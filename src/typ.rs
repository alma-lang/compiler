use crate::ast::ModuleFullName;
use crate::strings::{Strings, Symbol as StringSymbol};
use crate::type_env::{PolyTypeEnv, TypeEnv};
use fnv::FnvHashMap as HashMap;
use indexmap::IndexSet;
use std::cell::RefCell;
use std::char;
use std::rc::Rc;

#[derive(Hash, PartialEq, Eq, Debug, Copy, Clone)]
pub struct TypeVarId(pub u32);

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Copy, Clone)]
pub struct Level(pub u32);

#[derive(PartialEq, Eq, Debug)]
pub enum TypeVar {
    Bound(Rc<Type>),
    Unbound(TypeVarId, Level),
}

#[derive(PartialEq, Eq, Debug)]
pub enum Type {
    // Unit type ()
    Unit,

    // Named type (Int, Bool, List a, ...)
    Named {
        module: ModuleFullName,
        name: StringSymbol,
        params: Vec<Rc<Type>>,
    },

    /* a, b, etc.
     *
     * A reference to a bound or unbound TypeVar, set during unification.
     *
     * This is unique to algorithm J where mutation is needed to remember
     * substitutions.
     *
     * The level of this TypeVar identifies how many let-bindings deep it was
     * declared in. This is used to prevent generalization of TypeVars that
     * escape outside the current let-binding scope.
     */
    Var(Rc<RefCell<TypeVar>>),

    /* a1 a2 a3 ...an -> b
     *
     * e.g. \a b c -> c
     */
    Fn {
        params: Vec<Rc<Type>>,
        ret: Rc<Type>,
    },

    /* Records with field value pairs
     *
     * e.g. { x: Float, y: Float }
     */
    Record {
        fields: TypeEnv,
    },

    /* Extensible record with at least these field value pairs
     *
     * e.g. { a | x: Float, y: Float }
     */
    RecordExt {
        fields: TypeEnv,
        base_record: Rc<Type>,
    },

    // Alias to another type. Used when defining a record type with a name
    Alias {
        module: ModuleFullName,
        name: StringSymbol,
        params: Vec<(StringSymbol, Rc<Type>)>,
        destination: Rc<Type>,
    },
}

impl Type {
    /**
     * If this type is the a in a -> b, should it be parenthesized?
     * Note this is recursive in case bound type vars are used
     */
    fn should_parenthesize(&self) -> bool {
        use Type::*;
        match self {
            Var(var) => match &*var.borrow() {
                TypeVar::Bound(t) => t.should_parenthesize(),
                _ => false,
            },
            Fn { .. } => true,
            _ => false,
        }
    }

    pub fn parameters(&self) -> Vec<&Rc<Type>> {
        fn parameters_rec<'a>(mut all_params: Vec<&'a Rc<Type>>, t: &'a Type) -> Vec<&'a Rc<Type>> {
            match t {
                Type::Fn { params, ret, .. } => {
                    for param in params {
                        all_params.push(param);
                    }
                    parameters_rec(all_params, ret)
                }
                _ => all_params,
            }
        }

        parameters_rec(vec![], self)
    }

    pub fn to_string(&self, strings: &Strings) -> String {
        let mut s = String::new();
        self.to_string_rec(&mut vec!['a'], &mut HashMap::default(), &mut s, strings);
        s
    }

    pub fn to_string_rec<'a>(
        &self,
        cur_type_var_name: &'a mut Vec<char>,
        // keep track of number to character bindings for typevars
        // e.g. 2 => a, 5 => b, etc.
        type_var_names: &'a mut HashMap<u32, String>,
        s: &'a mut String,
        strings: &Strings,
    ) {
        fn fields_to_string<'a>(
            fields: &'a TypeEnv,
            cur_type_var_name: &'a mut Vec<char>,
            type_var_names: &'a mut HashMap<u32, String>,
            s: &'a mut String,
            strings: &Strings,
        ) {
            let mut keys: Vec<_> = fields.map().keys().collect();
            keys.sort();
            for (i, key) in keys.into_iter().enumerate() {
                let value = &fields.map()[key];

                if i != 0 {
                    s.push(',');
                    s.push(' ');
                }

                s.push_str(strings.resolve(*key));
                s.push_str(" : ");
                value.to_string_rec(cur_type_var_name, type_var_names, s, strings);
            }
        }

        use Type::*;
        match self {
            Unit => s.push_str("()"),

            Named { name, params, .. } => {
                s.push_str(strings.resolve(*name));
                for param in params.iter() {
                    s.push(' ');
                    param.to_string_rec(cur_type_var_name, type_var_names, s, strings);
                }
            }

            Var(var) => match &*var.borrow() {
                TypeVar::Bound(t) => t.to_string_rec(cur_type_var_name, type_var_names, s, strings),

                TypeVar::Unbound(TypeVarId(n), _) => match type_var_names.get(n) {
                    Some(name) => s.push_str(name),

                    None => {
                        let name: String = cur_type_var_name.iter().collect();
                        s.push_str(&name);

                        type_var_names.insert(*n, name);
                        next_letter(cur_type_var_name);
                    }
                },
            },

            Fn { params, ret } => {
                for (i, arg) in params.iter().enumerate() {
                    if i > 0 {
                        s.push_str(" -> ");
                    }

                    let parens = arg.should_parenthesize();

                    if parens {
                        s.push('(');
                    }

                    arg.to_string_rec(cur_type_var_name, type_var_names, s, strings);

                    if parens {
                        s.push(')');
                    }
                }
                s.push_str(" -> ");

                ret.to_string_rec(cur_type_var_name, type_var_names, s, strings);
            }

            Record { fields } => {
                s.push('{');

                if !fields.map().is_empty() {
                    s.push(' ');
                }

                fields_to_string(fields, cur_type_var_name, type_var_names, s, strings);

                if !fields.map().is_empty() {
                    s.push(' ');
                }
                s.push('}');
            }

            RecordExt { base_record, .. } => {
                let fields = get_extensible_record_fields(TypeEnv::new(), self);

                fn get_extensible_record_fields(mut all_fields: TypeEnv, typ: &Type) -> TypeEnv {
                    match typ {
                        Record { fields } => {
                            for (k, v) in fields.map() {
                                all_fields.insert(*k, Rc::clone(v));
                            }
                            all_fields
                        }
                        RecordExt {
                            fields,
                            base_record,
                        } => {
                            for (k, v) in fields.map() {
                                all_fields.insert(*k, Rc::clone(v));
                            }
                            get_extensible_record_fields(all_fields, base_record)
                        }
                        Var(var) => {
                            let var_read = (**var).borrow();
                            match &*var_read {
                                TypeVar::Unbound(_, _) => all_fields,
                                TypeVar::Bound(typ) => {
                                    get_extensible_record_fields(all_fields, typ)
                                }
                            }
                        }
                        _ => unreachable!(),
                    }
                }

                fn extensible_record_base_to_string<'a>(
                    typ: &Type,
                    cur_type_var_name: &'a mut Vec<char>,
                    type_var_names: &'a mut HashMap<u32, String>,
                    s: &'a mut String,
                    strings: &Strings,
                ) -> bool {
                    match typ {
                        Record { .. } => false,
                        RecordExt { base_record, .. } => extensible_record_base_to_string(
                            base_record,
                            cur_type_var_name,
                            type_var_names,
                            s,
                            strings,
                        ),
                        Var(var) => {
                            let var_read = (**var).borrow();
                            match &*var_read {
                                TypeVar::Unbound(_, _) => {
                                    typ.to_string_rec(
                                        cur_type_var_name,
                                        type_var_names,
                                        s,
                                        strings,
                                    );
                                    true
                                }
                                TypeVar::Bound(typ) => extensible_record_base_to_string(
                                    typ,
                                    cur_type_var_name,
                                    type_var_names,
                                    s,
                                    strings,
                                ),
                            }
                        }
                        _ => {
                            typ.to_string_rec(cur_type_var_name, type_var_names, s, strings);
                            true
                        }
                    }
                }

                s.push_str("{ ");

                let printed_extension = extensible_record_base_to_string(
                    base_record,
                    cur_type_var_name,
                    type_var_names,
                    s,
                    strings,
                );

                if printed_extension && !fields.map().is_empty() {
                    s.push_str(" | ");
                }
                if !fields.map().is_empty() {
                    fields_to_string(&fields, cur_type_var_name, type_var_names, s, strings);
                }

                s.push_str(" }");
            }

            Alias {
                name,
                params,
                destination,
                ..
            } => {
                s.push_str(strings.resolve(*name));
                for (_name, param) in params.iter() {
                    s.push(' ');
                    param.to_string_rec(cur_type_var_name, type_var_names, s, strings);
                }
                s.push_str(" (alias of ");
                destination.to_string_rec(cur_type_var_name, type_var_names, s, strings);
                s.push(')');
            }
        }
    }

    pub fn primitive_types(strings: &mut Strings) -> PolyTypeEnv {
        let module = strings.get_or_intern("Alma");
        let mut env = PolyTypeEnv::new();
        let float = strings.get_or_intern("Float");
        let bool_ = strings.get_or_intern("Bool");
        let string = strings.get_or_intern("String");
        env.insert(
            float,
            Rc::new(PolyType::new(
                IndexSet::new(),
                Rc::new(Type::Named {
                    module,
                    name: float,
                    params: vec![],
                }),
            )),
        );
        env.insert(
            bool_,
            Rc::new(PolyType::new(
                IndexSet::new(),
                Rc::new(Type::Named {
                    module,
                    name: bool_,
                    params: vec![],
                }),
            )),
        );
        env.insert(
            string,
            Rc::new(PolyType::new(
                IndexSet::new(),
                Rc::new(Type::Named {
                    module,
                    name: string,
                    params: vec![],
                }),
            )),
        );
        env
    }
}

/* PolyTypes in the form  forall 'a 'b ... 'y :: 'z
 * The TypeVar list will be a list of all monomorphic TypeVars in 'z
 * Used only in let-bindings to make the declaration polymorphic
 */
#[derive(PartialEq, Eq, Debug)]
pub struct PolyType {
    pub vars: IndexSet<TypeVarId>,
    pub typ: Rc<Type>,
}
impl PolyType {
    pub fn new(vars: IndexSet<TypeVarId>, typ: Rc<Type>) -> Self {
        PolyType { vars, typ }
    }

    pub fn to_string(&self, strings: &Strings) -> String {
        let mut s = String::new();
        let mut cur_type_var_name = vec!['a'];
        let mut type_var_names = HashMap::default();

        s.push('∀');
        if !self.vars.is_empty() {
            for var in self.vars.iter() {
                s.push(' ');
                let unbound_var = Type::Var(Rc::new(RefCell::new(TypeVar::Unbound(
                    *var,
                    Level(0), // This level doesn't matter for printing
                ))));
                unbound_var.to_string_rec(
                    &mut cur_type_var_name,
                    &mut type_var_names,
                    &mut s,
                    strings,
                );
            }
        } else {
            s.push_str(" ∅");
        }
        s.push_str(" . ");

        self.typ
            .to_string_rec(&mut cur_type_var_name, &mut type_var_names, &mut s, strings);

        s
    }
}

/* Return the next unique lowercase-letter string after the given one, e.g:
 *
 *   next_letter ['a'] = ['b']
 *   next_letter ['b'] = ['c']
 *   next_letter ['z'] = ['z', 'a']   This can be fixed but most examples shouldn't have > 26 typevars anyway
 */
fn next_letter(s: &mut Vec<char>) {
    let last_idx = s.len() - 1;
    let c: char = s[last_idx];
    let code = c as u32;
    if code + 1 > 'z' as u32 {
        s.push('a');
    } else {
        s[last_idx] = char::from_u32(code + 1).unwrap();
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::strings::Strings;
    use pretty_assertions::assert_eq;
    use std::iter::FromIterator;

    #[test]
    fn test_printing_types() {
        let mut strings = Strings::new();

        let module = strings.get_or_intern("Test");

        let tests = vec![
            (Type::Unit, "()"),
            (
                Type::Named {
                    module,
                    name: strings.get_or_intern("Float"),
                    params: vec![],
                },
                "Float",
            ),
            (
                Type::Named {
                    module,
                    name: strings.get_or_intern("Result"),
                    params: vec![
                        Rc::new(Type::Var(Rc::new(RefCell::new(TypeVar::Unbound(
                            TypeVarId(0),
                            Level(0),
                        ))))),
                        Rc::new(Type::Var(Rc::new(RefCell::new(TypeVar::Unbound(
                            TypeVarId(0),
                            Level(0),
                        ))))),
                    ],
                },
                "Result a a",
            ),
            (
                Type::Named {
                    module,
                    name: strings.get_or_intern("Result"),
                    params: vec![
                        Rc::new(Type::Var(Rc::new(RefCell::new(TypeVar::Unbound(
                            TypeVarId(0),
                            Level(0),
                        ))))),
                        Rc::new(Type::Var(Rc::new(RefCell::new(TypeVar::Unbound(
                            TypeVarId(1),
                            Level(0),
                        ))))),
                    ],
                },
                "Result a b",
            ),
            (
                Type::Var(Rc::new(RefCell::new(TypeVar::Unbound(
                    TypeVarId(0),
                    Level(0),
                )))),
                "a",
            ),
            (
                Type::Var(Rc::new(RefCell::new(TypeVar::Bound(Rc::new(Type::Unit))))),
                "()",
            ),
            (
                Type::Var(Rc::new(RefCell::new(TypeVar::Bound(Rc::new(Type::Unit))))),
                "()",
            ),
            (
                Type::Fn {
                    params: vec![Rc::new(Type::Named {
                        module,
                        name: strings.get_or_intern("Float"),
                        params: vec![],
                    })],
                    ret: Rc::new(Type::Named {
                        module,
                        name: strings.get_or_intern("String"),
                        params: vec![],
                    }),
                },
                "Float -> String",
            ),
            (
                Type::Fn {
                    params: vec![Rc::new(Type::Fn {
                        params: vec![Rc::new(Type::Named {
                            module,
                            name: strings.get_or_intern("Float"),
                            params: vec![],
                        })],
                        ret: Rc::new(Type::Named {
                            module,
                            name: strings.get_or_intern("String"),
                            params: vec![],
                        }),
                    })],
                    ret: Rc::new(Type::Named {
                        module,
                        name: strings.get_or_intern("String"),
                        params: vec![],
                    }),
                },
                "(Float -> String) -> String",
            ),
            (
                Type::Fn {
                    params: vec![Rc::new(Type::Named {
                        module,
                        name: strings.get_or_intern("String"),
                        params: vec![],
                    })],
                    ret: Rc::new(Type::Fn {
                        params: vec![Rc::new(Type::Named {
                            module,
                            name: strings.get_or_intern("Float"),
                            params: vec![],
                        })],
                        ret: Rc::new(Type::Named {
                            module,
                            name: strings.get_or_intern("String"),
                            params: vec![],
                        }),
                    }),
                },
                "String -> Float -> String",
            ),
            (
                Type::Fn {
                    params: vec![
                        Rc::new(Type::Named {
                            module,
                            name: strings.get_or_intern("String"),
                            params: vec![],
                        }),
                        Rc::new(Type::Named {
                            module,
                            name: strings.get_or_intern("Float"),
                            params: vec![],
                        }),
                    ],
                    ret: Rc::new(Type::Named {
                        module,
                        name: strings.get_or_intern("String"),
                        params: vec![],
                    }),
                },
                "String -> Float -> String",
            ),
            (
                Type::Record {
                    fields: TypeEnv::new(),
                },
                "{}",
            ),
            (
                Type::Record {
                    fields: {
                        let mut fields = TypeEnv::new();
                        fields.insert(
                            strings.get_or_intern("a"),
                            Rc::new(Type::Named {
                                module,
                                name: strings.get_or_intern("Int"),
                                params: vec![],
                            }),
                        );
                        fields
                    },
                },
                "{ a : Int }",
            ),
            (
                Type::Record {
                    fields: {
                        let mut fields = TypeEnv::new();
                        fields.insert(
                            strings.get_or_intern("age"),
                            Rc::new(Type::Named {
                                module,
                                name: strings.get_or_intern("Int"),
                                params: vec![],
                            }),
                        );
                        fields.insert(
                            strings.get_or_intern("extra"),
                            Rc::new(Type::Var(Rc::new(RefCell::new(TypeVar::Unbound(
                                TypeVarId(0),
                                Level(0),
                            ))))),
                        );
                        fields
                    },
                },
                "{ age : Int, extra : a }",
            ),
            (
                Type::RecordExt {
                    fields: TypeEnv::new(),
                    base_record: Rc::new(Type::Var(Rc::new(RefCell::new(TypeVar::Unbound(
                        TypeVarId(0),
                        Level(0),
                    ))))),
                },
                "{ a }",
            ),
            (
                Type::RecordExt {
                    fields: {
                        let mut fields = TypeEnv::new();
                        fields.insert(
                            strings.get_or_intern("age"),
                            Rc::new(Type::Named {
                                module,
                                name: strings.get_or_intern("Int"),
                                params: vec![],
                            }),
                        );
                        fields
                    },
                    base_record: Rc::new(Type::Var(Rc::new(RefCell::new(TypeVar::Unbound(
                        TypeVarId(0),
                        Level(0),
                    ))))),
                },
                "{ a | age : Int }",
            ),
            (
                Type::RecordExt {
                    fields: {
                        let mut fields = TypeEnv::new();
                        fields.insert(
                            strings.get_or_intern("age"),
                            Rc::new(Type::Named {
                                module,
                                name: strings.get_or_intern("Int"),
                                params: vec![],
                            }),
                        );
                        fields.insert(
                            strings.get_or_intern("extra"),
                            Rc::new(Type::Var(Rc::new(RefCell::new(TypeVar::Unbound(
                                TypeVarId(0),
                                Level(0),
                            ))))),
                        );
                        fields
                    },
                    base_record: Rc::new(Type::Var(Rc::new(RefCell::new(TypeVar::Unbound(
                        TypeVarId(1),
                        Level(0),
                    ))))),
                },
                "{ a | age : Int, extra : b }",
            ),
        ];

        for (value, expected) in tests {
            assert_eq!(value.to_string(&strings), expected, "\n\n{:#?}", value);
        }
    }

    #[test]
    fn test_printing_polytypes() {
        let mut strings = Strings::new();

        let _module = strings.get_or_intern("Test");

        let tests = vec![
            (
                PolyType::new(
                    IndexSet::from_iter(vec![TypeVarId(0)]),
                    Rc::new(Type::Var(Rc::new(RefCell::new(TypeVar::Unbound(
                        TypeVarId(0),
                        Level(0),
                    ))))),
                ),
                "∀ a . a",
            ),
            (
                PolyType::new(
                    IndexSet::from_iter(vec![TypeVarId(0)]),
                    Rc::new(Type::Var(Rc::new(RefCell::new(TypeVar::Unbound(
                        TypeVarId(1),
                        Level(0),
                    ))))),
                ),
                "∀ a . b",
            ),
            (
                PolyType::new(
                    IndexSet::from_iter(vec![TypeVarId(0)]),
                    Rc::new(Type::Fn {
                        params: vec![Rc::new(Type::Var(Rc::new(RefCell::new(TypeVar::Unbound(
                            TypeVarId(0),
                            Level(0),
                        )))))],
                        ret: Rc::new(Type::Var(Rc::new(RefCell::new(TypeVar::Unbound(
                            TypeVarId(0),
                            Level(0),
                        ))))),
                    }),
                ),
                "∀ a . a -> a",
            ),
        ];

        for (value, expected) in tests {
            assert_eq!(value.to_string(&strings), expected, "\n\n{:#?}", value);
        }
    }
}
