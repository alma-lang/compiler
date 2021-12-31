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
    /* Unit type () */
    Unit,

    /* Named type (Int, Bool, List a, ...) */
    Named(ModuleFullName, StringSymbol, Vec<Rc<Type>>),

    /* 'a, 'b, etc.
     *
     * A reference to a bound or unbound TypeVar, set during unification.
     * This is unique to algorithm J where mutation is needed to remember
     * some substitutions.
     * The level of this TypeVar identifies how many let-bindings deep it was
     * declared in. This is used to prevent generalization of TypeVars that
     * escape outside the current let-binding scope.
     */
    Var(Rc<RefCell<TypeVar>>),

    /* 'a -> 'b, all functions are single-argument only
     * e.g. \a b c -> c  is automatically translated to \a -> \b -> \c -> c
     * Currying is also automatic
     */
    Fn(Vec<Rc<Type>>, Rc<Type>),

    Record(TypeEnv),
    RecordExt(TypeEnv, Rc<Type>),

    Alias(
        ModuleFullName,
        StringSymbol,
        Vec<(StringSymbol, Rc<Type>)>,
        Rc<Type>,
    ),
}

impl Type {
    /* If this type is the a in a -> b, should it be parenthesized?
     * Note this is recursive in case bound typevars are used
     */
    fn should_parenthesize(&self) -> bool {
        use Type::*;
        match self {
            Var(var) => match &*var.borrow() {
                TypeVar::Bound(t) => t.should_parenthesize(),
                _ => false,
            },
            Fn(..) => true,
            _ => false,
        }
    }

    pub fn parameters(&self) -> Vec<&Rc<Type>> {
        fn parameters_rec<'a>(mut params: Vec<&'a Rc<Type>>, t: &'a Type) -> Vec<&'a Rc<Type>> {
            match t {
                Type::Fn(args, body) => {
                    for arg in args {
                        params.push(arg);
                    }
                    parameters_rec(params, body)
                }
                _ => params,
            }
        }

        parameters_rec(vec![], self)
    }

    pub fn to_string(&self, strings: &Strings) -> String {
        let mut s = String::new();
        self.to_string_rec(&mut vec!['a'], &mut HashMap::default(), &mut s, strings);
        s
    }

    /* keep track of number to character bindings for typevars
     * e.g. '2 => 'a, '5 => 'b, etc.
     * letters are assigned to typevars by the order in which the typevars
     * appear in the type, left to right
     */
    pub fn to_string_rec<'a>(
        &self,
        cur_type_var_name: &'a mut Vec<char>,
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
                }
                s.push(' ');

                s.push_str(strings.resolve(*key));
                s.push_str(" : ");
                value.to_string_rec(cur_type_var_name, type_var_names, s, strings);
            }
        }

        use Type::*;
        match self {
            Unit => s.push_str("()"),

            Named(_module, name, params) => {
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

            Fn(args, body) => {
                for (i, arg) in args.iter().enumerate() {
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

                body.to_string_rec(cur_type_var_name, type_var_names, s, strings);
            }

            Record(fields) => {
                s.push('{');

                fields_to_string(fields, cur_type_var_name, type_var_names, s, strings);

                if !fields.map().is_empty() {
                    s.push(' ');
                }
                s.push('}');
            }

            RecordExt(fields, var) => {
                s.push_str("{ ");

                var.to_string_rec(cur_type_var_name, type_var_names, s, strings);

                if !fields.map().is_empty() {
                    s.push_str(" |");
                }

                fields_to_string(fields, cur_type_var_name, type_var_names, s, strings);

                s.push_str(" }");
            }

            Alias(_module, name, params, _typ) => {
                s.push_str(strings.resolve(*name));
                for (name, param) in params.iter() {
                    s.push(' ');
                    param.to_string_rec(cur_type_var_name, type_var_names, s, strings);
                }
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
                Rc::new(Type::Named(module, float, vec![])),
            )),
        );
        env.insert(
            bool_,
            Rc::new(PolyType::new(
                IndexSet::new(),
                Rc::new(Type::Named(module, bool_, vec![])),
            )),
        );
        env.insert(
            string,
            Rc::new(PolyType::new(
                IndexSet::new(),
                Rc::new(Type::Named(module, string, vec![])),
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
                    Level(0), /* This level doesn't matter for printing */
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
                Type::Named(module, strings.get_or_intern("Float"), vec![]),
                "Float",
            ),
            (
                Type::Named(
                    module,
                    strings.get_or_intern("Result"),
                    vec![
                        Rc::new(Type::Var(Rc::new(RefCell::new(TypeVar::Unbound(
                            TypeVarId(0),
                            Level(0),
                        ))))),
                        Rc::new(Type::Var(Rc::new(RefCell::new(TypeVar::Unbound(
                            TypeVarId(0),
                            Level(0),
                        ))))),
                    ],
                ),
                "Result a a",
            ),
            (
                Type::Named(
                    module,
                    strings.get_or_intern("Result"),
                    vec![
                        Rc::new(Type::Var(Rc::new(RefCell::new(TypeVar::Unbound(
                            TypeVarId(0),
                            Level(0),
                        ))))),
                        Rc::new(Type::Var(Rc::new(RefCell::new(TypeVar::Unbound(
                            TypeVarId(1),
                            Level(0),
                        ))))),
                    ],
                ),
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
                Type::Fn(
                    vec![Rc::new(Type::Named(
                        module,
                        strings.get_or_intern("Float"),
                        vec![],
                    ))],
                    Rc::new(Type::Named(module, strings.get_or_intern("String"), vec![])),
                ),
                "Float -> String",
            ),
            (
                Type::Fn(
                    vec![Rc::new(Type::Fn(
                        vec![Rc::new(Type::Named(
                            module,
                            strings.get_or_intern("Float"),
                            vec![],
                        ))],
                        Rc::new(Type::Named(module, strings.get_or_intern("String"), vec![])),
                    ))],
                    Rc::new(Type::Named(module, strings.get_or_intern("String"), vec![])),
                ),
                "(Float -> String) -> String",
            ),
            (
                Type::Fn(
                    vec![Rc::new(Type::Named(
                        module,
                        strings.get_or_intern("String"),
                        vec![],
                    ))],
                    Rc::new(Type::Fn(
                        vec![Rc::new(Type::Named(
                            module,
                            strings.get_or_intern("Float"),
                            vec![],
                        ))],
                        Rc::new(Type::Named(module, strings.get_or_intern("String"), vec![])),
                    )),
                ),
                "String -> Float -> String",
            ),
            (
                Type::Fn(
                    vec![
                        Rc::new(Type::Named(module, strings.get_or_intern("String"), vec![])),
                        Rc::new(Type::Named(module, strings.get_or_intern("Float"), vec![])),
                    ],
                    Rc::new(Type::Named(module, strings.get_or_intern("String"), vec![])),
                ),
                "String -> Float -> String",
            ),
            (Type::Record(TypeEnv::new()), "{}"),
            (
                Type::Record({
                    let mut fields = TypeEnv::new();
                    fields.insert(
                        strings.get_or_intern("a"),
                        Rc::new(Type::Named(module, strings.get_or_intern("Int"), vec![])),
                    );
                    fields
                }),
                "{ a : Int }",
            ),
            (
                Type::Record({
                    let mut fields = TypeEnv::new();
                    fields.insert(
                        strings.get_or_intern("age"),
                        Rc::new(Type::Named(module, strings.get_or_intern("Int"), vec![])),
                    );
                    fields.insert(
                        strings.get_or_intern("extra"),
                        Rc::new(Type::Var(Rc::new(RefCell::new(TypeVar::Unbound(
                            TypeVarId(0),
                            Level(0),
                        ))))),
                    );
                    fields
                }),
                "{ age : Int, extra : a }",
            ),
            (
                Type::RecordExt(
                    TypeEnv::new(),
                    Rc::new(Type::Var(Rc::new(RefCell::new(TypeVar::Unbound(
                        TypeVarId(0),
                        Level(0),
                    ))))),
                ),
                "{ a }",
            ),
            (
                Type::RecordExt(
                    {
                        let mut fields = TypeEnv::new();
                        fields.insert(
                            strings.get_or_intern("age"),
                            Rc::new(Type::Named(module, strings.get_or_intern("Int"), vec![])),
                        );
                        fields
                    },
                    Rc::new(Type::Var(Rc::new(RefCell::new(TypeVar::Unbound(
                        TypeVarId(0),
                        Level(0),
                    ))))),
                ),
                "{ a | age : Int }",
            ),
            (
                Type::RecordExt(
                    {
                        let mut fields = TypeEnv::new();
                        fields.insert(
                            strings.get_or_intern("age"),
                            Rc::new(Type::Named(module, strings.get_or_intern("Int"), vec![])),
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
                    Rc::new(Type::Var(Rc::new(RefCell::new(TypeVar::Unbound(
                        TypeVarId(1),
                        Level(0),
                    ))))),
                ),
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
                    Rc::new(Type::Fn(
                        vec![Rc::new(Type::Var(Rc::new(RefCell::new(TypeVar::Unbound(
                            TypeVarId(0),
                            Level(0),
                        )))))],
                        Rc::new(Type::Var(Rc::new(RefCell::new(TypeVar::Unbound(
                            TypeVarId(0),
                            Level(0),
                        ))))),
                    )),
                ),
                "∀ a . a -> a",
            ),
        ];

        for (value, expected) in tests {
            assert_eq!(value.to_string(&strings), expected, "\n\n{:#?}", value);
        }
    }
}
