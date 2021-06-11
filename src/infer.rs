use crate::ast::{self, Expression, Node};
use crate::source::Source;
use crate::typ::{Type::*, TypeVar::*, *};
use crate::type_env::TypeEnv;
use std::cell::RefCell;
use std::cmp::min;
use std::collections::HashMap;
use std::rc::Rc;

/*
 *  This implementation follows the type inference rules given at
 *  https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Algorithm_J
 *
 *  The algorithm itself uses most of the names from the above link, with
 *  a few changed for ease of typing:
 *       Γ (gamma) => env
 *       ⊢ⱼ (perpendicular symbol with j subscript, a.k.a. algorithm J) => infer
 *       Γ¯ (gamma bar) => generalize
 *
 *  And some expr constructors changed to match their more colloquial names
 *  to hopefully make this somewhat more approachable:
 *       Var => Identifier
 *       App => FnCall
 *       Abs => Lambda
 *
 *  Note that a let-binding (or Declaration here) can be of either
 *  a variable or a function
 *
 *  Additionally, implementation of "levels" for efficient generalization is
 *  taken from http://okmij.org/ftp/ML/generalization.html
 */

enum UnificationError {
    TypeMismatch,
    InfiniteType,
}

pub enum Error {
    UndefinedIdentifier(String, Expression),
    TypeMismatch(Expression, Rc<Type>, Option<Expression>, Rc<Type>),
    InfiniteType(Expression, Rc<Type>),
}

impl Error {
    pub fn to_string(&self, source: &Source) -> String {
        use Error::*;

        let mut s = String::new();

        let (position, line_number, column, end) = match self {
            UndefinedIdentifier(
                _,
                Node {
                    line,
                    column,
                    end,
                    start,
                    ..
                },
            ) => (start, line, column, end),
            TypeMismatch(
                Node {
                    line,
                    column,
                    end,
                    start,
                    ..
                },
                ..,
            ) => (start, line, column, end),
            InfiniteType(
                Node {
                    line,
                    column,
                    end,
                    start,
                    ..
                },
                _,
            ) => (start, line, column, end),
        };

        s.push_str(&source.to_string_with_line_and_col(*line_number, *column));
        s.push_str("\n\n");

        let code = source
            .lines_report_at_position_with_pointer(*position, Some(*end), *line_number)
            .unwrap();

        match self {
            UndefinedIdentifier(name, _expr) => {
                s.push_str(&format!("Undefined identifier `{}`\n\n{}", name, code));
            }
            InfiniteType(_, _) => {
                s.push_str(&format!("Infinite type\n\n{}", code));
            }

            TypeMismatch(_expr, typ, None, typ2) => {
                s.push_str(&format!(
                    "Type mismatch:  {0}  ≠  {1}

Expected

{2}

to be

{1}

but seems to be

{0}",
                    typ, typ2, code
                ));
            }

            TypeMismatch(_expr, typ, Some(expr2), typ2) => {
                let code2 = source
                    .lines_report_at_position_with_pointer(expr2.start, Some(expr2.end), expr2.line)
                    .unwrap();

                s.push_str(&format!(
                    "Type mismatch:  {0}  ≠  {1}

Expected

{2}

with type

{0}

to have the same type as

${3}

with type

{1}",
                    typ, typ2, code, code2
                ));
            }
        }

        s
    }
}

struct State {
    current_level: Level,
    current_type_var: TypeVarId,
}

impl State {
    fn new() -> Self {
        State {
            current_level: Level(1),
            current_type_var: TypeVarId(0),
        }
    }

    fn enter_level(&mut self) {
        self.current_level = Level(self.current_level.0 + 1);
    }
    fn exit_level(&mut self) {
        self.current_level = Level(self.current_level.0 - 1);
    }

    fn new_var_id(&mut self) -> TypeVarId {
        self.current_type_var = TypeVarId(self.current_type_var.0 + 1);
        self.current_type_var
    }

    fn new_type_var(&mut self) -> Rc<Type> {
        Rc::new(Type::Var(Rc::new(RefCell::new(TypeVar::Unbound(
            self.new_var_id(),
            self.current_level,
        )))))
    }

    /* specializes the polytype s by copying the term and replacing the
     * bound type variables consistently by new monotype variables
     * E.g.   inst (forall a b. a -> b -> a) = c -> d -> c */
    fn instantiate(&mut self, t: &Rc<Type>) -> Rc<Type> {
        /* Replace any typevars found in the Hashtbl with the
         * associated value in the same table, leave them otherwise */
        fn replace_type_vars(
            vars_to_replace: &HashMap<TypeVarId, Rc<Type>>,
            t: &Rc<Type>,
        ) -> Rc<Type> {
            match &**t {
                Unit => Rc::clone(&t),
                Named(..) => Rc::clone(&t),
                Var(var) => match &*var.borrow() {
                    Bound(t) => replace_type_vars(vars_to_replace, t),
                    Unbound(n, _level) => match vars_to_replace.get(&n) {
                        Some(t_) => Rc::clone(t_),
                        None => Rc::clone(&t),
                    },
                },

                Fn(a, b) => Rc::new(Fn(
                    replace_type_vars(vars_to_replace, a),
                    replace_type_vars(vars_to_replace, b),
                )),

                PolyType(type_vars, typ) => {
                    let mut vars_to_replace_copy = vars_to_replace.clone();
                    for var in type_vars.iter() {
                        vars_to_replace_copy.remove(var);
                    }
                    Rc::new(PolyType(
                        type_vars.clone(),
                        replace_type_vars(&vars_to_replace_copy, typ),
                    ))
                }
            }
        }

        if let Type::PolyType(vars, typ) = &**t {
            let mut vars_to_replace: HashMap<TypeVarId, Rc<Type>> = HashMap::new();
            for var in vars.iter() {
                vars_to_replace.insert(*var, self.new_type_var());
            }
            replace_type_vars(&vars_to_replace, typ)
        } else {
            Rc::clone(t)
        }
    }

    /* Find all typevars and wrap the type in a PolyType */
    /* e.g.  generalize (a -> b -> b) = forall a b. a -> b -> b */
    fn generalize(&self, t: &Rc<Type>) -> Rc<Type> {
        let current_level = self.current_level;

        /* collect all the monomorphic typevars */
        fn find_all_tvs(current_level: &Level, vars: &mut Vec<TypeVarId>, t: &Rc<Type>) {
            match &**t {
                Unit => (),

                Named(_, args) => {
                    for arg in args.iter() {
                        find_all_tvs(current_level, vars, arg);
                    }
                }

                Var(var) => match &*var.borrow() {
                    Bound(t) => find_all_tvs(current_level, vars, t),
                    Unbound(n, level) => {
                        if level > current_level {
                            vars.push(*n);
                        }
                    }
                },

                Fn(a, b) => {
                    find_all_tvs(current_level, vars, a);
                    find_all_tvs(current_level, vars, b);
                }

                /* Remove all of tvs from findAllTvs typ */
                PolyType(free_tvs, typ) => {
                    let mut typ_tvs = vec![];
                    find_all_tvs(current_level, &mut typ_tvs, typ);

                    vars.append(
                        &mut typ_tvs
                            .iter()
                            .filter(|var| !free_tvs.iter().any(|fvar| &fvar == var))
                            .map(|var| *var)
                            .collect::<Vec<TypeVarId>>(),
                    );
                }
            }
        }

        let mut tvs = vec![];
        find_all_tvs(&current_level, &mut tvs, t);
        Rc::new(PolyType(tvs, Rc::clone(&t)))
    }
}

/* The find for our union-find like algorithm */
/* Go through the given type, replacing all typevars with their bound types when possible */

/* Can a monomorphic Var(a) be found inside this type? */
fn occurs(a_id: TypeVarId, a_level: Level, t: &Rc<Type>) -> bool {
    match &**t {
        Unit => false,
        Named(..) => false,

        Var(var) => {
            let mut var = var.borrow_mut();
            match &*var {
                Bound(t) => occurs(a_id, a_level, t),

                Unbound(ref b_id, ref b_level) => {
                    let b_id = *b_id;
                    let b_level = *b_level;
                    let min_level = min(a_level, b_level);

                    *var = Unbound(b_id, min_level);

                    a_id == b_id
                }
            }
        }

        Fn(b, c) => occurs(a_id, a_level, b) || occurs(a_id, a_level, c),

        PolyType(tvs, t) => {
            if tvs.iter().any(|tv| a_id == *tv) {
                false
            } else {
                occurs(a_id, a_level, t)
            }
        }
    }
}

fn unify(
    ast: &Expression,
    t1: &Rc<Type>,
    ast2: Option<&Expression>,
    t2: &Rc<Type>,
) -> Result<(), Error> {
    fn unify_rec(t1: &Rc<Type>, t2: &Rc<Type>) -> Result<(), UnificationError> {
        use UnificationError::*;

        match (&**t1, &**t2) {
            (Unit, Unit) => Ok(()),

            (Named(name, args), Named(name2, args2)) => {
                if name != name2 {
                    Err(TypeMismatch)
                } else if args.len() != args2.len() {
                    Err(TypeMismatch)
                } else {
                    for (a1, a2) in args.iter().zip(args2.iter()) {
                        unify_rec(a1, a2)?;
                    }
                    Ok(())
                }
            }

            (Var(var1), b) => {
                let mut var1 = var1.borrow_mut();
                match &*var1 {
                    /* the 'find' in the union-find algorithm */
                    Bound(a1) => unify_rec(a1, t2),

                    /* create binding for boundTy that is currently empty */
                    Unbound(a_id, a_level) => {
                        let equal = match b {
                            Var(var2) => match &*var2.borrow() {
                                Unbound(b_id, b_level) => a_id == b_id && a_level == b_level,
                                _ => false,
                            },
                            _ => false,
                        };
                        if equal {
                            Ok(())
                        } else if occurs(*a_id, *a_level, t2) {
                            /* a = a, but dont create a recursive binding to itself */
                            Err(InfiniteType)
                        } else {
                            *var1 = Bound(Rc::clone(&t2));
                            Ok(())
                        }
                    }
                }
            }

            (a, Var(var2)) => {
                let mut var2 = var2.borrow_mut();
                match &*var2 {
                    /* the 'find' in the union-find algorithm */
                    Bound(b) => unify_rec(t1, b),

                    /* create binding for boundTy that is currently empty */
                    Unbound(b_id, b_level) => {
                        let equal = match a {
                            Var(var1) => match &*var1.borrow() {
                                Unbound(a_id, a_level) => a_id == b_id && a_level == b_level,
                                _ => false,
                            },
                            _ => false,
                        };
                        if equal {
                            Ok(())
                        } else if occurs(*b_id, *b_level, t1) {
                            /* a = a, but dont create a recursive binding to itself */
                            Err(InfiniteType)
                        } else {
                            *var2 = Bound(Rc::clone(&t1));
                            Ok(())
                        }
                    }
                }
            }

            (Fn(arg, body), Fn(arg2, body2)) => {
                unify_rec(arg, arg2)?;
                unify_rec(body, body2)
            }

            /* NOTE: Unneeded rule, never used due to [Var] and inst */
            (PolyType(_vars, typ1), PolyType(_vars2, typ2)) => unify_rec(typ1, typ2),

            (_a, _b) => Err(TypeMismatch),
        }
    }

    unify_rec(t1, t2).map_err(|e| match e {
        UnificationError::TypeMismatch => Error::TypeMismatch(
            ast.clone(),
            Rc::clone(&t1),
            ast2.map(|a| a.clone()),
            Rc::clone(&t2),
        ),
        UnificationError::InfiniteType => Error::InfiniteType(ast.clone(), Rc::clone(&t1)),
    })
}

fn base_env(state: &mut State, env: &mut TypeEnv) {
    // Primitive types
    let float = &Rc::new(Type::Named("Float".to_owned(), vec![]));
    let bool_ = &Rc::new(Type::Named("Bool".to_owned(), vec![]));
    let _string = &Rc::new(Type::Named("String".to_owned(), vec![]));

    env.insert(
        "(or)".to_owned(),
        Rc::new(Type::Fn(
            Rc::clone(bool_),
            Rc::new(Type::Fn(Rc::clone(bool_), Rc::clone(bool_))),
        )),
    );

    env.insert(
        "(or)".to_string(),
        Rc::new(Type::Fn(
            Rc::clone(bool_),
            Rc::new(Type::Fn(Rc::clone(bool_), Rc::clone(bool_))),
        )),
    );
    env.insert(
        "(and)".to_string(),
        Rc::new(Type::Fn(
            Rc::clone(bool_),
            Rc::new(Type::Fn(Rc::clone(bool_), Rc::clone(bool_))),
        )),
    );
    env.insert("(==)".to_string(), {
        let a = state.new_type_var();
        state.generalize(&Rc::new(Type::Fn(
            Rc::clone(&a),
            Rc::new(Type::Fn(Rc::clone(&a), Rc::clone(bool_))),
        )))
    });
    env.insert("(!=)".to_string(), {
        let a = state.new_type_var();
        state.generalize(&Rc::new(Type::Fn(
            Rc::clone(&a),
            Rc::new(Type::Fn(Rc::clone(&a), Rc::clone(bool_))),
        )))
    });
    env.insert(
        "(>)".to_string(),
        Rc::new(Type::Fn(
            Rc::clone(float),
            Rc::new(Type::Fn(Rc::clone(float), Rc::clone(bool_))),
        )),
    );
    env.insert(
        "(>=)".to_string(),
        Rc::new(Type::Fn(
            Rc::clone(float),
            Rc::new(Type::Fn(Rc::clone(float), Rc::clone(bool_))),
        )),
    );
    env.insert(
        "(<)".to_string(),
        Rc::new(Type::Fn(
            Rc::clone(float),
            Rc::new(Type::Fn(Rc::clone(float), Rc::clone(bool_))),
        )),
    );
    env.insert(
        "(<=)".to_string(),
        Rc::new(Type::Fn(
            Rc::clone(float),
            Rc::new(Type::Fn(Rc::clone(float), Rc::clone(bool_))),
        )),
    );
    env.insert(
        "(+)".to_string(),
        Rc::new(Type::Fn(
            Rc::clone(float),
            Rc::new(Type::Fn(Rc::clone(float), Rc::clone(float))),
        )),
    );
    env.insert(
        "(-)".to_string(),
        Rc::new(Type::Fn(
            Rc::clone(float),
            Rc::new(Type::Fn(Rc::clone(float), Rc::clone(float))),
        )),
    );
    env.insert(
        "(*)".to_string(),
        Rc::new(Type::Fn(
            Rc::clone(float),
            Rc::new(Type::Fn(Rc::clone(float), Rc::clone(float))),
        )),
    );
    env.insert(
        "(/)".to_string(),
        Rc::new(Type::Fn(
            Rc::clone(float),
            Rc::new(Type::Fn(Rc::clone(float), Rc::clone(float))),
        )),
    );
}

/* The main entry point to type inference */
/* All branches (except for the trivial Unit) of the first match in this function
are translated directly from the rules for algorithm J, given in comments */
pub fn infer(ast: &Expression) -> Result<Rc<Type>, Vec<Error>> {
    let mut state = State::new();
    let mut env = TypeEnv::new();

    base_env(&mut state, &mut env);

    let mut errors: Vec<Error> = vec![];

    let t = infer_rec(ast, &mut state, &mut env, &mut errors);

    if errors.len() == 0 {
        Ok(t)
    } else {
        Err(errors)
    }
}

fn infer_rec(
    ast: &Expression,
    state: &mut State,
    env: &mut TypeEnv,
    errors: &mut Vec<Error>,
) -> Rc<Type> {
    use ast::{Expression_ as E, Pattern_ as P, Unary_ as U};

    // Primitive types
    // TODO: This is horrible, copypastad because globals with Rc in Rust are ASLDKFJNQASLKJDFG
    let float = &Rc::new(Type::Named("Float".to_owned(), vec![]));
    let bool_ = &Rc::new(Type::Named("Bool".to_owned(), vec![]));
    let string = &Rc::new(Type::Named("String".to_owned(), vec![]));

    match &ast.value {
        E::Unit => Rc::new(Unit),

        E::Bool(_) => Rc::clone(bool_),

        E::Float(_) => Rc::clone(float),

        E::String_(_) => Rc::clone(string),

        E::Unary(op, e) => {
            let t = infer_rec(e, state, env, errors);

            add_error(
                match op.value {
                    U::Not => unify(e, &t, None, bool_),
                    U::Minus => unify(e, &t, None, float),
                },
                errors,
            );

            t
        }

        E::Binary(left, op, right) => {
            // Convert the binary AST to function calls
            let fn_call = Expression {
                value: E::FnCall(
                    Box::new(Expression {
                        value: E::Identifier(op.value.fn_.clone()),
                        start: op.start,
                        end: op.end,
                        line: op.line,
                        column: op.column,
                    }),
                    // TODO: I'm copying two possibly big chunks of Ast here just for fun...
                    // Should the Ast also be Rc?
                    vec![(**left).clone(), (**right).clone()],
                ),
                ..*ast
            };

            // Infer the binary op as a function call
            infer_rec(&fn_call, state, env, errors)
        }

        /* If
         * infer env condition = t0
         * unify t0 bool
         * infer env then = t1
         * infer env else_ = t2
         * unify t1 t2
         * infer env (if condition then else) = t2
         */
        E::If(condition, then, else_) => {
            let t = infer_rec(condition, state, env, errors);
            add_error(unify(condition, &t, None, bool_), errors);

            let t1 = infer_rec(then, state, env, errors);
            let t2 = infer_rec(else_, state, env, errors);
            add_error(unify(then, &t1, Some(else_), &t2), errors);

            t2
        }

        /*
         * Var
         *   x : s ∊ env
         *   t = inst s
         *   -----------
         *   infer env x = t
         */
        E::Identifier(x) => match env.get(x) {
            Some(s) => {
                let t = state.instantiate(&s);
                t
            }
            None => {
                add_error(
                    Err(Error::UndefinedIdentifier(x.clone(), ast.clone())),
                    errors,
                );
                state.new_type_var()
            }
        },

        /* App
         *   infer env f = t0
         *   infer env x = t1
         *   t' = newVar ()
         *   unify t0 (t1 -> t')
         *   ---------------
         *   infer env (f x) = t'
         */
        E::FnCall(f, args) => {
            let fn_type = infer_rec(f, state, env, errors);
            let param_types = fn_type.parameters();

            let arg_types: Vec<Rc<Type>> = args
                .iter()
                .map(|arg| infer_rec(arg, state, env, errors))
                .collect();

            let return_type = state.new_type_var();

            let call_type: Rc<Type> = arg_types
                .iter()
                .rev()
                .fold(Rc::clone(&return_type), |ret, typ| {
                    Rc::new(Fn(Rc::clone(typ), ret))
                });

            // Unify the arguments separately for nicer error messages
            let res = arg_types
                .iter()
                .zip(args.iter())
                .zip(param_types)
                .fold(Ok(()), |result, ((arg_type, arg), param_type)| {
                    result.and_then(|_| unify(arg, arg_type, None, param_type))
                })
                // If there weren't any failures, unify the Fn and return types
                .and_then(|_| unify(ast, &call_type, None, &fn_type));
            add_error(res, errors);

            return_type
        }

        /* Abs
         *   t = newVar ()
         *   infer (SMap.add x t env) e = t'
         *   -------------
         *   infer env (fun x -> e) = t -> t'
         */
        E::Lambda(params, e) => {
            let params_with_type: Vec<(&ast::Pattern, Rc<Type>)> =
                params.iter().map(|p| (p, state.new_type_var())).collect();

            let mut env =
                params_with_type
                    .iter()
                    .fold(env.clone(), |mut env, (param, param_type)| {
                        match param {
                            Node {
                                value: P::Identifier(x),
                                ..
                            } => env.insert(x.clone(), Rc::clone(param_type)),
                        };
                        env
                    });

            let return_type = infer_rec(e, state, &mut env, errors);

            params_with_type
                .iter()
                .rev()
                .fold(return_type, |return_type, (_, param_type)| {
                    Rc::new(Fn(Rc::clone(param_type), return_type))
                })
        }

        /* Let
         *   infer env e0 = t
         *   infer (SMap.add x (generalize t) env) e1 = t'
         *   -----------------
         *   infer env (let x = e0 in e1) = t'
         *
         * enter/exitLevel optimizations are from
         * http://okmij.org/ftp/ML/generalization.html
         * In this implementation, they're required so we don't generalize types
         * that escape into the environment.
         */
        E::Let(p, e0, e1) => {
            state.enter_level();
            let t = infer_rec(e0, state, env, errors);
            state.exit_level();

            let mut new_env = env.clone();
            match &p.value {
                P::Identifier(x) => new_env.insert(x.clone(), state.generalize(&t)),
            };

            infer_rec(e1, state, &mut new_env, errors)
        }
    }
}

fn add_error(result: Result<(), Error>, errors: &mut Vec<Error>) {
    match result {
        Err(e) => errors.push(e),
        _ => (),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser;
    use crate::tokenizer;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_infer() {
        let tests = vec![
            ("\\f -> \\x -> f x", "(a -> b) -> a -> b"),
            ("\\f -> \\x -> f (f x)", "(a -> a) -> a -> a"),
            // (+):
            (
                "\\m -> \\n -> \\f -> \\x -> m f (n f x)",
                "(a -> b -> c) -> (a -> d -> b) -> a -> d -> c",
            ),
            // succ:
            (
                "\\n -> \\f -> \\x -> f (n f x)",
                "((a -> b) -> c -> a) -> (a -> b) -> c -> b",
            ),
            // mult:
            (
                "\\m -> \\n -> \\f -> \\x -> m (n f) x",
                "(a -> b -> c) -> (d -> a) -> d -> b -> c",
            ),
            // pred:
            (
                "\\n -> \\f -> \\x -> n (\\g -> \\h -> h (g f)) (\\u -> x) (\\u -> u)",
                "(((a -> b) -> (b -> c) -> c) -> (d -> e) -> (f -> f) -> g) -> a -> e -> g",
            ),
            // let generalization tests
            (
                "
      \\x ->
        let y = x
        y
    ",
                "a -> a",
            ),
            (
                "
      \\x ->
        let y = \\z -> x
        y
      ",
                "a -> b -> a",
            ),
            // if:
            ("if 1 == 1 then True else False", "Bool"),
            ("if 1 == 1 then 1 else 2", "Float"),
            // errors:
            (
                "if 1 == 1 then if 1 + 1 > 2 then 5 else 1 / 1 else 2 + 2",
                "Float",
            ),
            (
                "if 1 then 5 else 1",
                "[1:3]

Type mismatch:  Float  ≠  Bool

Expected

  1│  if 1 then 5 else 1
   │     ↑

to be

Bool

but seems to be

Float",
            ),
            (
                "let incr = \\n -> n + 1

incr True",
                "[3:5]

Type mismatch:  Bool  ≠  Float

Expected

  1│  let incr = \\n -> n + 1
  2│  
  3│  incr True
   │       ↑↑↑↑

to be

Float

but seems to be

Bool",
            ),
            (
                "\\x ->
    let a = x + 1
    let b = not x
    x",
                "[3:16]

Type mismatch:  Float  ≠  Bool

Expected

  1│  \\x ->
  2│      let a = x + 1
  3│      let b = not x
   │                  ↑
  4│      x

to be

Bool

but seems to be

Float",
            ),
            (
                "let a = bar\nbar",
                "[1:8]

Undefined identifier `bar`

  1│  let a = bar
   │          ↑↑↑
  2│  bar

[2:0]

Undefined identifier `bar`

  1│  let a = bar
  2│  bar
   │  ↑↑↑",
            ),
        ];

        for (input, expected) in tests {
            let source = Source::new_orphan(&input);

            let tokens = tokenizer::parse(&source)
                .map_err(|errors| {
                    errors
                        .iter()
                        .map(|e| e.to_string(&source))
                        .collect::<Vec<String>>()
                        .join("\n\n")
                })
                .unwrap();

            let ast = parser::parse(&source, tokens)
                .map_err(|errors| {
                    errors
                        .iter()
                        .map(|e| e.to_string(&source))
                        .collect::<Vec<String>>()
                        .join("\n\n")
                })
                .unwrap();

            let s = match infer(&ast) {
                Ok(typ) => format!("{}", typ),
                Err(errs) => errs
                    .iter()
                    .map(|e| e.to_string(&source))
                    .collect::<Vec<String>>()
                    .join("\n\n"),
            };

            assert_eq!(s, expected, "\n\n{}\n\n{}", &input, s);
        }
    }
}