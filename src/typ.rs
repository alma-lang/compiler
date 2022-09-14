use crate::ast::ModuleFullName;
use crate::index;
use crate::strings::{Strings, Symbol as StringSymbol};
use crate::type_env::TypeEnv;
use fnv::FnvHashMap as HashMap;
use indexmap::IndexSet;
use std::char;
use std::rc::Rc;
use typed_index_collections::TiVec;

pub type Index = index::Index<Type>;
pub type Types = TiVec<Index, Type>;

#[derive(Hash, PartialEq, Eq, Debug, Copy, Clone)]
pub struct TypeVarId(pub u32);

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Copy, Clone)]
pub struct Level(pub u32);

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum TypeVar {
    Bound(Index),
    Unbound(TypeVarId, Level),
}

#[derive(PartialEq, Eq, Debug)]
pub enum Type {
    // Named type (Int, Bool, List a, ...)
    Named {
        module: ModuleFullName,
        name: StringSymbol,
        params: Rc<Vec<Index>>,
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
    Var(TypeVar),

    /* a1 a2 a3 ...an -> b
     *
     * e.g. \a b c -> c
     */
    Fn {
        params: Rc<Vec<Index>>,
        ret: Index,
    },

    /* Records with field value pairs
     *
     * e.g. { x: Float, y: Float }
     */
    Record {
        fields: Rc<TypeEnv>,
    },

    /* Extensible record with at least these field value pairs
     *
     * e.g. { a | x: Float, y: Float }
     */
    RecordExt {
        fields: Rc<TypeEnv>,
        base_record: Index,
    },

    // Alias to another type. Used when defining a record type with a name
    Alias {
        module: ModuleFullName,
        name: StringSymbol,
        params: Rc<Vec<(StringSymbol, Index)>>,
        destination: Index,
    },
}

impl Type {
    /**
     * If this type is the a in a -> b, should it be parenthesized?
     * Note this is recursive in case bound type vars are used
     */
    fn should_parenthesize(&self, types: &Types) -> bool {
        use Type::*;
        match self {
            Var(TypeVar::Bound(t)) => types[*t].should_parenthesize(types),
            Fn { .. } => true,
            _ => false,
        }
    }

    pub fn parameters(&self, types: &Types) -> Vec<Index> {
        fn parameters_rec<'a>(mut all_params: Vec<Index>, t: &Type, types: &Types) -> Vec<Index> {
            match t {
                Type::Fn { params, ret, .. } => {
                    for param in &**params {
                        all_params.push(*param);
                    }
                    parameters_rec(all_params, &types[*ret], types)
                }
                _ => all_params,
            }
        }

        parameters_rec(vec![], self, types)
    }

    pub fn to_string(&self, strings: &Strings, types: &Types) -> String {
        let mut out = String::new();
        self.to_string_rec(
            &mut vec!['a'],
            &mut HashMap::default(),
            &mut out,
            strings,
            types,
        );
        out
    }

    pub fn write_to_string<'a>(&self, out: &'a mut String, strings: &Strings, types: &Types) {
        self.to_string_rec(&mut vec!['a'], &mut HashMap::default(), out, strings, types);
    }

    pub fn to_string_rec<'a>(
        &self,
        cur_type_var_name: &'a mut Vec<char>,
        // keep track of number to character bindings for typevars
        // e.g. 2 => a, 5 => b, etc.
        type_var_names: &'a mut HashMap<u32, String>,
        s: &'a mut String,
        strings: &Strings,
        types: &Types,
    ) {
        fn fields_to_string<'a>(
            fields: &'a TypeEnv,
            cur_type_var_name: &'a mut Vec<char>,
            type_var_names: &'a mut HashMap<u32, String>,
            s: &'a mut String,
            strings: &Strings,
            types: &Types,
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
                types[*value].to_string_rec(cur_type_var_name, type_var_names, s, strings, types);
            }
        }

        use Type::*;
        match self {
            Named { name, params, .. } => {
                s.push_str(strings.resolve(*name));
                for param in params.iter() {
                    s.push(' ');
                    types[*param].to_string_rec(
                        cur_type_var_name,
                        type_var_names,
                        s,
                        strings,
                        types,
                    );
                }
            }

            Var(var) => match var {
                TypeVar::Bound(t) => {
                    types[*t].to_string_rec(cur_type_var_name, type_var_names, s, strings, types)
                }

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
                    let arg = &types[*arg];

                    if i > 0 {
                        s.push_str(" -> ");
                    }

                    let parens = arg.should_parenthesize(types);

                    if parens {
                        s.push('(');
                    }

                    arg.to_string_rec(cur_type_var_name, type_var_names, s, strings, types);

                    if parens {
                        s.push(')');
                    }
                }
                s.push_str(" -> ");

                types[*ret].to_string_rec(cur_type_var_name, type_var_names, s, strings, types);
            }

            Record { fields } => {
                s.push('{');

                if !fields.map().is_empty() {
                    s.push(' ');
                }

                fields_to_string(fields, cur_type_var_name, type_var_names, s, strings, types);

                if !fields.map().is_empty() {
                    s.push(' ');
                }
                s.push('}');
            }

            RecordExt { base_record, .. } => {
                let fields = get_extensible_record_fields(TypeEnv::new(), self, types);

                fn get_extensible_record_fields(
                    mut all_fields: TypeEnv,
                    typ: &Type,
                    types: &Types,
                ) -> TypeEnv {
                    match typ {
                        Record { fields } => {
                            for (k, v) in fields.map() {
                                all_fields.insert(*k, *v);
                            }
                            all_fields
                        }
                        RecordExt {
                            fields,
                            base_record,
                        } => {
                            for (k, v) in fields.map() {
                                all_fields.insert(*k, *v);
                            }
                            get_extensible_record_fields(all_fields, &types[*base_record], types)
                        }
                        Var(var) => match var {
                            TypeVar::Unbound(_, _) => all_fields,
                            TypeVar::Bound(typ) => {
                                get_extensible_record_fields(all_fields, &types[*typ], types)
                            }
                        },
                        _ => unreachable!(),
                    }
                }

                fn extensible_record_base_to_string<'a>(
                    typ: &Type,
                    cur_type_var_name: &'a mut Vec<char>,
                    type_var_names: &'a mut HashMap<u32, String>,
                    s: &'a mut String,
                    strings: &Strings,
                    types: &Types,
                ) -> bool {
                    match typ {
                        Record { .. } => false,
                        RecordExt { base_record, .. } => extensible_record_base_to_string(
                            &types[*base_record],
                            cur_type_var_name,
                            type_var_names,
                            s,
                            strings,
                            types,
                        ),
                        Var(var) => match var {
                            TypeVar::Unbound(_, _) => {
                                typ.to_string_rec(
                                    cur_type_var_name,
                                    type_var_names,
                                    s,
                                    strings,
                                    types,
                                );
                                true
                            }
                            TypeVar::Bound(typ) => extensible_record_base_to_string(
                                &types[*typ],
                                cur_type_var_name,
                                type_var_names,
                                s,
                                strings,
                                types,
                            ),
                        },
                        _ => {
                            typ.to_string_rec(cur_type_var_name, type_var_names, s, strings, types);
                            true
                        }
                    }
                }

                s.push_str("{ ");

                let printed_extension = extensible_record_base_to_string(
                    &types[*base_record],
                    cur_type_var_name,
                    type_var_names,
                    s,
                    strings,
                    types,
                );

                if printed_extension && !fields.map().is_empty() {
                    s.push_str(" | ");
                }
                if !fields.map().is_empty() {
                    fields_to_string(
                        &fields,
                        cur_type_var_name,
                        type_var_names,
                        s,
                        strings,
                        types,
                    );
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
                    types[*param].to_string_rec(
                        cur_type_var_name,
                        type_var_names,
                        s,
                        strings,
                        types,
                    );
                }
                s.push_str(" (alias of ");
                types[*destination].to_string_rec(
                    cur_type_var_name,
                    type_var_names,
                    s,
                    strings,
                    types,
                );
                s.push(')');
            }
        }
    }
}

/* PolyTypes in the form  forall 'a 'b ... 'y :: 'z
 * The TypeVar list will be a list of all monomorphic TypeVars in 'z
 * Used only in let-bindings to make the declaration polymorphic
 */
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct PolyType {
    pub vars: IndexSet<TypeVarId>,
    pub typ: Index,
}
impl PolyType {
    pub fn new(vars: IndexSet<TypeVarId>, typ: Index) -> Self {
        PolyType { vars, typ }
    }

    pub fn write_to_string(&self, s: &mut String, strings: &Strings, types: &Types) {
        let mut cur_type_var_name = vec!['a'];
        let mut type_var_names = HashMap::default();

        s.push('∀');
        if !self.vars.is_empty() {
            for var in self.vars.iter() {
                s.push(' ');
                let unbound_var = Type::Var(TypeVar::Unbound(
                    *var,
                    Level(0), // This level doesn't matter for printing
                ));
                unbound_var.to_string_rec(
                    &mut cur_type_var_name,
                    &mut type_var_names,
                    s,
                    strings,
                    types,
                );
            }
        } else {
            s.push_str(" ∅");
        }
        s.push_str(" . ");

        types[self.typ].to_string_rec(
            &mut cur_type_var_name,
            &mut type_var_names,
            s,
            strings,
            types,
        );
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

    #[test]
    fn test_printing_types() {
        let mut strings = Strings::new();
        let mut types = Types::new();
        let module = strings.get_or_intern("Test");

        let float = types.push_and_get_key(Type::Named {
            module,
            name: strings.get_or_intern("Float"),
            params: Rc::new(vec![]),
        });
        let string = types.push_and_get_key(Type::Named {
            module,
            name: strings.get_or_intern("String"),
            params: Rc::new(vec![]),
        });
        let int = types.push_and_get_key(Type::Named {
            module,
            name: strings.get_or_intern("Int"),
            params: Rc::new(vec![]),
        });
        let a = types.push_and_get_key(Type::Var(TypeVar::Unbound(TypeVarId(0), Level(0))));
        let b = types.push_and_get_key(Type::Var(TypeVar::Unbound(TypeVarId(1), Level(0))));

        let tests: Vec<(Index, &'static str)> = vec![
            (float, "Float"),
            (
                types.push_and_get_key(Type::Named {
                    module,
                    name: strings.get_or_intern("Result"),
                    params: Rc::new(vec![a, a]),
                }),
                "Result a a",
            ),
            (
                types.push_and_get_key(Type::Named {
                    module,
                    name: strings.get_or_intern("Result"),
                    params: Rc::new(vec![a, b]),
                }),
                "Result a b",
            ),
            (a, "a"),
            (
                types.push_and_get_key(Type::Var(TypeVar::Bound(float))),
                "Float",
            ),
            (
                types.push_and_get_key(Type::Fn {
                    params: Rc::new(vec![float]),
                    ret: string,
                }),
                "Float -> String",
            ),
            (
                {
                    let f = types.push_and_get_key(Type::Fn {
                        params: Rc::new(vec![float]),
                        ret: string,
                    });
                    types.push_and_get_key(Type::Fn {
                        params: Rc::new(vec![f]),
                        ret: string,
                    })
                },
                "(Float -> String) -> String",
            ),
            (
                {
                    let f = types.push_and_get_key(Type::Fn {
                        params: Rc::new(vec![float]),
                        ret: string,
                    });
                    types.push_and_get_key(Type::Fn {
                        params: Rc::new(vec![string]),
                        ret: f,
                    })
                },
                "String -> Float -> String",
            ),
            (
                types.push_and_get_key(Type::Fn {
                    params: Rc::new(vec![string, float]),
                    ret: string,
                }),
                "String -> Float -> String",
            ),
            (
                types.push_and_get_key(Type::Record {
                    fields: Rc::new(TypeEnv::new()),
                }),
                "{}",
            ),
            (
                types.push_and_get_key(Type::Record {
                    fields: {
                        let mut fields = TypeEnv::new();
                        fields.insert(strings.get_or_intern("a"), int);
                        Rc::new(fields)
                    },
                }),
                "{ a : Int }",
            ),
            (
                types.push_and_get_key(Type::Record {
                    fields: {
                        let mut fields = TypeEnv::new();
                        fields.insert(strings.get_or_intern("age"), int);
                        fields.insert(strings.get_or_intern("extra"), a);
                        Rc::new(fields)
                    },
                }),
                "{ age : Int, extra : a }",
            ),
            (
                types.push_and_get_key(Type::RecordExt {
                    fields: Rc::new(TypeEnv::new()),
                    base_record: a,
                }),
                "{ a }",
            ),
            (
                types.push_and_get_key(Type::RecordExt {
                    fields: {
                        let mut fields = TypeEnv::new();
                        fields.insert(strings.get_or_intern("age"), int);
                        Rc::new(fields)
                    },
                    base_record: a,
                }),
                "{ a | age : Int }",
            ),
            (
                types.push_and_get_key(Type::RecordExt {
                    fields: {
                        let mut fields = TypeEnv::new();
                        fields.insert(strings.get_or_intern("age"), int);
                        fields.insert(strings.get_or_intern("extra"), b);
                        Rc::new(fields)
                    },
                    base_record: a,
                }),
                "{ a | age : Int, extra : b }",
            ),
        ];

        for (value, expected) in tests {
            let mut out = String::new();
            types[value].write_to_string(&mut out, &strings, &types);
            assert_eq!(out, expected, "\n\n{:#?}", value);
        }
    }

    #[test]
    fn test_printing_polytypes() {
        let strings = Strings::new();

        let mut types = Types::new();
        let a = types.push_and_get_key(Type::Var(TypeVar::Unbound(TypeVarId(0), Level(0))));
        let b = types.push_and_get_key(Type::Var(TypeVar::Unbound(TypeVarId(1), Level(0))));

        let tests = vec![
            (
                PolyType::new(IndexSet::from_iter(vec![TypeVarId(0)]), a),
                "∀ a . a",
            ),
            (
                PolyType::new(IndexSet::from_iter(vec![TypeVarId(0)]), b),
                "∀ a . b",
            ),
            (
                PolyType::new(
                    IndexSet::from_iter(vec![TypeVarId(0)]),
                    types.push_and_get_key(Type::Fn {
                        params: Rc::new(vec![a]),
                        ret: a,
                    }),
                ),
                "∀ a . a -> a",
            ),
        ];

        for (value, expected) in tests {
            let mut actual = String::new();
            value.write_to_string(&mut actual, &strings, &types);
            assert_eq!(actual, expected, "\n\n{:#?}", value);
        }
    }
}
