use crate::ast::{
    self, Definition, Expression, ExpressionType as ET, Identifier, Import, Module, ModuleName,
    Pattern_ as P, Unary_ as U,
};
use crate::compiler::types::ModuleInterfaces;
use crate::source::Source;
use crate::strings::Strings;
use crate::typ::{Type::*, TypeVar::*, *};
use crate::type_env::TypeEnv;
use fnv::FnvHashMap as HashMap;
use indexmap::IndexSet;
use std::cell::RefCell;
use std::cmp::min;
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

#[derive(Debug)]
pub enum Error<'ast> {
    UndefinedIdentifier(&'ast Identifier, &'ast Expression),
    UndefinedModule(&'ast ModuleName, &'ast Expression),
    DuplicateField(&'ast Identifier, &'ast Expression),
    TypeMismatch(
        &'ast Expression,
        Rc<Type>,
        Option<&'ast Expression>,
        Rc<Type>,
    ),
    InfiniteType(&'ast Expression, Rc<Type>),
    UndefinedExport(&'ast Identifier),
    UnknownImport(&'ast Import),
    UnknownImportDefinition(&'ast Identifier, &'ast Import),
}

impl<'ast> Error<'ast> {
    pub fn to_string(&self, source: &Source, strings: &Strings) -> String {
        use Error::*;

        let mut s = String::new();

        let (position, line_number, column, end) = match self {
            UndefinedIdentifier(_, node) => (node.start, node.line, node.column, node.end),
            UndefinedModule(_, node) => (node.start, node.line, node.column, node.end),
            DuplicateField(node, _) => (node.start, node.line, node.column, node.end),
            TypeMismatch(node, ..) => (node.start, node.line, node.column, node.end),
            InfiniteType(node, _) => (node.start, node.line, node.column, node.end),
            UndefinedExport(node) => (node.start, node.line, node.column, node.end),
            UnknownImport(node) => (node.start, node.line, node.column, node.end),
            UnknownImportDefinition(node, _) => (node.start, node.line, node.column, node.end),
        };

        s.push_str(&source.to_string_with_line_and_col(line_number, column));
        s.push_str("\n\n");

        let code = source
            .lines_report_at_position_with_pointer(position, Some(end), line_number)
            .unwrap();

        match self {
            UndefinedIdentifier(identifier, _expr) => {
                s.push_str(&format!(
                    "Undefined identifier `{}`\n\n{}",
                    identifier.value.to_string(strings),
                    code
                ));
            }

            UndefinedModule(module_name, _expr) => {
                s.push_str(&format!(
                    "Undefined module `{}`. Did you forget to import it?\n\n{}",
                    module_name.to_string(strings),
                    code
                ));
            }

            DuplicateField(identifier, expr) => {
                let record_code = source
                    .lines_report_at_position(expr.start, Some(expr.end), expr.line, false)
                    .unwrap();
                s.push_str(&format!(
                    "Duplicate field `{0}`

{1}

in record

{2}",
                    identifier.value.to_string(strings),
                    code,
                    record_code
                ));
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
                    typ.to_string(strings),
                    typ2.to_string(strings),
                    code
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

{3}

with type

{1}",
                    typ.to_string(strings),
                    typ2.to_string(strings),
                    code,
                    code2
                ));
            }

            UndefinedExport(export) => {
                s.push_str(&format!(
                    "Undefined identifier `{}`\n\n{}",
                    export.value.to_string(strings),
                    code
                ));
            }

            UnknownImport(import) => {
                s.push_str(&format!(
                    "Couldn't find module `{}`\n\n{}",
                    import.value.module_name.to_string(strings),
                    code
                ));
            }

            UnknownImportDefinition(export, import) => {
                s.push_str(&format!(
                    "Module `{}` doesn't appear to expose `{}`\n\n{}",
                    import.value.module_name.to_string(strings),
                    export.value.to_string(strings),
                    code
                ));
            }
        };

        s
    }
}

pub struct State {
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
                Unit => Rc::clone(t),

                Named(module, name, args) => Rc::new(Named(
                    *module,
                    *name,
                    args.iter()
                        .map(|arg| replace_type_vars(vars_to_replace, arg))
                        .collect(),
                )),

                Var(var) => match &*((**var).borrow()) {
                    Bound(t) => replace_type_vars(vars_to_replace, t),
                    Unbound(n, _level) => match vars_to_replace.get(n) {
                        Some(t_) => Rc::clone(t_),
                        None => Rc::clone(t),
                    },
                },

                Fn(args, b) => Rc::new(Fn(
                    {
                        let mut new_args = vec![];
                        for arg in args {
                            new_args.push(replace_type_vars(vars_to_replace, arg));
                        }
                        new_args
                    },
                    replace_type_vars(vars_to_replace, b),
                )),

                Record(fields) => {
                    let mut new_fields = fields.clone();
                    for (key, value) in fields.map() {
                        new_fields.insert(*key, replace_type_vars(vars_to_replace, value));
                    }
                    Rc::new(Record(new_fields))
                }

                RecordExt(fields, ext) => {
                    let mut new_fields = fields.clone();
                    for (key, value) in fields.map() {
                        new_fields.insert(*key, replace_type_vars(vars_to_replace, value));
                    }
                    Rc::new(RecordExt(
                        new_fields,
                        replace_type_vars(vars_to_replace, ext),
                    ))
                }

                Poly(type_vars, typ) => {
                    let mut vars_to_replace_copy = vars_to_replace.clone();
                    for var in type_vars.iter() {
                        vars_to_replace_copy.remove(var);
                    }
                    Rc::new(Poly(
                        type_vars.clone(),
                        replace_type_vars(&vars_to_replace_copy, typ),
                    ))
                }
            }
        }

        if let Type::Poly(vars, typ) = &**t {
            let mut vars_to_replace: HashMap<TypeVarId, Rc<Type>> = HashMap::default();
            for var in vars.iter() {
                vars_to_replace.insert(*var, self.new_type_var());
            }
            replace_type_vars(&vars_to_replace, typ)
        } else {
            Rc::clone(t)
        }
    }

    /* Find all typevars and wrap the type in a Poly */
    /* e.g.  generalize (a -> b -> b) = forall a b. a -> b -> b */
    fn generalize(&self, t: &Rc<Type>) -> Rc<Type> {
        let current_level = self.current_level;

        /* collect all the monomorphic typevars */
        fn find_all_tvs(current_level: &Level, vars: &mut IndexSet<TypeVarId>, t: &Rc<Type>) {
            match &**t {
                Unit => (),

                Named(_, _, args) => {
                    for arg in args.iter() {
                        find_all_tvs(current_level, vars, arg);
                    }
                }

                Var(var) => match &*((**var).borrow()) {
                    Bound(t) => find_all_tvs(current_level, vars, t),
                    Unbound(n, level) => {
                        if level > current_level {
                            vars.insert(*n);
                        }
                    }
                },

                Fn(args, b) => {
                    for arg in args {
                        find_all_tvs(current_level, vars, arg);
                    }
                    find_all_tvs(current_level, vars, b);
                }

                Record(fields) => {
                    for t in fields.map().values() {
                        find_all_tvs(current_level, vars, t);
                    }
                }

                RecordExt(fields, ext) => {
                    for t in fields.map().values() {
                        find_all_tvs(current_level, vars, t);
                    }
                    find_all_tvs(current_level, vars, ext);
                }

                /* Remove all of tvs from findAllTvs typ */
                Poly(free_tvs, typ) => {
                    let mut typ_tvs = IndexSet::new();
                    find_all_tvs(current_level, &mut typ_tvs, typ);

                    for tv in typ_tvs {
                        if !free_tvs.contains(&tv) {
                            vars.insert(tv);
                        }
                    }
                }
            }
        }

        let mut tvs = IndexSet::new();
        find_all_tvs(&current_level, &mut tvs, t);
        Rc::new(Poly(tvs, Rc::clone(t)))
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
            let var_read = (**var).borrow();
            match &*var_read {
                Bound(t) => occurs(a_id, a_level, t),

                Unbound(b_id, b_level) => {
                    let (b_id, b_level) = (*b_id, *b_level);
                    drop(var_read);

                    let min_level = min(a_level, b_level);

                    let mut var = var.borrow_mut();
                    *var = Unbound(b_id, min_level);

                    a_id == b_id
                }
            }
        }

        Fn(args, ret) => {
            args.iter().any(|arg| occurs(a_id, a_level, arg)) || occurs(a_id, a_level, ret)
        }

        Record(fields) => fields.map().values().any(|tv| occurs(a_id, a_level, tv)),

        RecordExt(fields, record_t) => {
            if fields.map().values().any(|tv| occurs(a_id, a_level, tv)) {
                true
            } else {
                occurs(a_id, a_level, record_t)
            }
        }

        Poly(tvs, t) => {
            if tvs.iter().any(|tv| a_id == *tv) {
                false
            } else {
                occurs(a_id, a_level, t)
            }
        }
    }
}

fn flatten_record(typ: &Rc<Type>) -> Option<Rc<Type>> {
    let typ = find(typ);
    if let RecordExt(_, _) = &*typ {
        let mut all_fields = TypeEnv::new();
        let mut typ = typ;

        loop {
            match &*typ {
                Record(fields) => {
                    for (k, v) in fields.map() {
                        all_fields.insert(*k, Rc::clone(v));
                    }
                    return Some(Rc::new(Record(all_fields)));
                }
                RecordExt(fields, ext) => {
                    for (k, v) in fields.map() {
                        all_fields.insert(*k, Rc::clone(v));
                    }
                    typ = find(ext);
                }
                Var(var) => {
                    let var_read = (**var).borrow();
                    match &*var_read {
                        Unbound(_, _) => {
                            return Some(Rc::new(RecordExt(all_fields, Rc::clone(&typ))))
                        }
                        Bound(_) => unreachable!(),
                    };
                }
                _ => return None,
            }
        }
    } else {
        None
    }
}

fn find(typ: &Rc<Type>) -> Rc<Type> {
    match &**typ {
        Var(var) => {
            let var_read = (**var).borrow();
            match &*var_read {
                Bound(t) => find(t),
                Unbound(_, _) => Rc::clone(typ),
            }
        }
        _ => Rc::clone(typ),
    }
}

fn unify<'ast>(
    state: &mut State,
    ast: &'ast Expression,
    t1: &Rc<Type>,
    ast2: Option<&'ast Expression>,
    t2: &Rc<Type>,
) -> Result<(), Error<'ast>> {
    fn unify_var(
        state: &mut State,
        typ: &Rc<Type>,
        var: &Rc<RefCell<TypeVar>>,
        other_type: &Rc<Type>,
    ) -> Result<(), UnificationError> {
        let var_read = (**var).borrow();
        match &*var_read {
            // The 'find' in the union-find algorithm
            Bound(dest_var) => unify_rec(state, dest_var, other_type),

            // Create binding for unbound type variable
            Unbound(a_id, a_level) => {
                let (a_id, a_level) = (*a_id, *a_level);
                // Drop the read borrow before the occurs check since it is not used and
                // can panic the occurs check if type 1 and type 2 point to the same thing
                drop(var_read);

                if typ == other_type {
                    Ok(())
                } else if occurs(a_id, a_level, other_type) {
                    /* a = a, but dont create a recursive binding to itself */
                    Err(UnificationError::InfiniteType)
                } else {
                    let mut var = var.borrow_mut();
                    let new_dest_type =
                        flatten_record(other_type).unwrap_or_else(|| Rc::clone(other_type));
                    *var = Bound(new_dest_type);
                    Ok(())
                }
            }
        }
    }

    fn unify_rec(state: &mut State, t1: &Rc<Type>, t2: &Rc<Type>) -> Result<(), UnificationError> {
        use UnificationError::*;

        match (&**t1, &**t2) {
            (Unit, Unit) => Ok(()),

            (Named(module, name, args), Named(module2, name2, args2)) => {
                if module != module2 || name != name2 || args.len() != args2.len() {
                    Err(TypeMismatch)
                } else {
                    for (a1, a2) in args.iter().zip(args2.iter()) {
                        unify_rec(state, a1, a2)?;
                    }
                    Ok(())
                }
            }

            (Var(var), _) => unify_var(state, t1, var, t2),

            (_, Var(var)) => unify_var(state, t2, var, t1),

            (Fn(args, body), Fn(args2, body2)) => {
                if args.len() != args2.len() {
                    Err(TypeMismatch)
                } else {
                    for (a1, a2) in args.iter().zip(args2.iter()) {
                        unify_rec(state, a1, a2)?;
                    }
                    unify_rec(state, body, body2)
                }
            }

            (Record(fields1), Record(fields2)) => {
                // Check fields on each record, and unify types of the values against the other
                // record. If the field doesn't exist then it is a type mismatch.

                for (key, value) in fields1.map() {
                    if fields2.map().contains_key(key) {
                        unify_rec(state, value, fields2.get(key).unwrap())?;
                    } else {
                        return Err(TypeMismatch);
                    }
                }
                for (key, value) in fields2.map() {
                    if fields1.map().contains_key(key) {
                        unify_rec(state, value, fields1.get(key).unwrap())?;
                    } else {
                        return Err(TypeMismatch);
                    }
                }

                Ok(())
            }

            (Record(fields), RecordExt(ext_fields, ext))
            | (RecordExt(ext_fields, ext), Record(fields)) => {
                // Check the fields on the open record and match them against the fixed record. If
                // a field doesn't exist it is a mismatch
                for (key, value) in ext_fields.map() {
                    if fields.map().contains_key(key) {
                        unify_rec(state, value, fields.get(key).unwrap())?;
                    } else {
                        return Err(TypeMismatch);
                    }
                }

                // Unify the open record type variable with a record of the remaining fields from
                // the fixed record.
                let mut rem_fields = TypeEnv::new();
                for (key, value) in fields.map() {
                    if !ext_fields.map().contains_key(key) {
                        rem_fields.insert(*key, Rc::clone(value));
                    }
                }
                let rem_rec = Rc::new(Record(rem_fields));
                unify_rec(state, ext, &rem_rec)
            }

            (RecordExt(fields1, var1), RecordExt(fields2, var2)) => {
                // Check fields on each record, and unify types of the values against the other
                // record. If the field doesn't exist then it is a type mismatch.
                for (key, value) in fields1.map() {
                    if fields2.map().contains_key(key) {
                        unify_rec(state, value, fields2.get(key).unwrap())?;
                    } else {
                        return Err(TypeMismatch);
                    }
                }
                for (key, value) in fields2.map() {
                    if fields1.map().contains_key(key) {
                        unify_rec(state, value, fields1.get(key).unwrap())?;
                    } else {
                        return Err(TypeMismatch);
                    }
                }

                let var = state.new_type_var();

                // unify { <fields-only-found-on-the-left-side> | new-type-var } varRight
                {
                    let mut rem_fields1 = TypeEnv::new();
                    for (key, value) in fields1.map() {
                        if !fields2.map().contains_key(key) {
                            rem_fields1.insert(*key, Rc::clone(value));
                        }
                    }
                    let rem_rec1 = Rc::new(RecordExt(rem_fields1, Rc::clone(&var)));
                    unify_rec(state, var2, &rem_rec1)?;
                }

                // unify { <fields-only-found-on-the-right-side> | new-type-var } varLeft
                {
                    let mut rem_fields2 = TypeEnv::new();
                    for (key, value) in fields2.map() {
                        if !fields1.map().contains_key(key) {
                            rem_fields2.insert(*key, Rc::clone(value));
                        }
                    }
                    let rem_rec2 = Rc::new(RecordExt(rem_fields2, Rc::clone(&var)));
                    unify_rec(state, var1, &rem_rec2)?;
                }

                Ok(())
            }

            /* NOTE: Unneeded rule, never used due to [Var] and inst */
            (Poly(_vars, typ1), Poly(_vars2, typ2)) => unify_rec(state, typ1, typ2),

            (Unit, _)
            | (Named(..), _)
            | (Fn(..), _)
            | (Poly(..), _)
            | (Record(_), _)
            | (RecordExt(..), _) => Err(TypeMismatch),
        }
    }

    unify_rec(state, t1, t2).map_err(|e| match e {
        UnificationError::TypeMismatch => {
            Error::TypeMismatch(ast, Rc::clone(t1), ast2, Rc::clone(t2))
        }
        UnificationError::InfiniteType => Error::InfiniteType(ast, Rc::clone(t1)),
    })
}

fn base_env(
    state: &mut State,
    env: &mut TypeEnv,
    primitive_types: &PrimitiveTypes,
    strings: &mut Strings,
) {
    env.insert(
        strings.get_or_intern("__op__or"),
        Rc::new(Type::Fn(
            vec![
                Rc::clone(&primitive_types.bool),
                Rc::clone(&primitive_types.bool),
            ],
            Rc::clone(&primitive_types.bool),
        )),
    );

    env.insert(
        strings.get_or_intern("__op__or"),
        Rc::new(Type::Fn(
            vec![
                Rc::clone(&primitive_types.bool),
                Rc::clone(&primitive_types.bool),
            ],
            Rc::clone(&primitive_types.bool),
        )),
    );
    env.insert(
        strings.get_or_intern("__op__and"),
        Rc::new(Type::Fn(
            vec![
                Rc::clone(&primitive_types.bool),
                Rc::clone(&primitive_types.bool),
            ],
            Rc::clone(&primitive_types.bool),
        )),
    );
    env.insert(strings.get_or_intern("__op__eq"), {
        let a = state.new_type_var();
        state.generalize(&Rc::new(Type::Fn(
            vec![Rc::clone(&a), Rc::clone(&a)],
            Rc::clone(&primitive_types.bool),
        )))
    });
    env.insert(strings.get_or_intern("__op__ne"), {
        let a = state.new_type_var();
        state.generalize(&Rc::new(Type::Fn(
            vec![Rc::clone(&a), Rc::clone(&a)],
            Rc::clone(&primitive_types.bool),
        )))
    });
    env.insert(
        strings.get_or_intern("__op__gt"),
        Rc::new(Type::Fn(
            vec![
                Rc::clone(&primitive_types.float),
                Rc::clone(&primitive_types.float),
            ],
            Rc::clone(&primitive_types.bool),
        )),
    );
    env.insert(
        strings.get_or_intern("__op__ge"),
        Rc::new(Type::Fn(
            vec![
                Rc::clone(&primitive_types.float),
                Rc::clone(&primitive_types.float),
            ],
            Rc::clone(&primitive_types.bool),
        )),
    );
    env.insert(
        strings.get_or_intern("__op__lt"),
        Rc::new(Type::Fn(
            vec![
                Rc::clone(&primitive_types.float),
                Rc::clone(&primitive_types.float),
            ],
            Rc::clone(&primitive_types.bool),
        )),
    );
    env.insert(
        strings.get_or_intern("__op__le"),
        Rc::new(Type::Fn(
            vec![
                Rc::clone(&primitive_types.float),
                Rc::clone(&primitive_types.float),
            ],
            Rc::clone(&primitive_types.bool),
        )),
    );
    env.insert(
        strings.get_or_intern("__op__add"),
        Rc::new(Type::Fn(
            vec![
                Rc::clone(&primitive_types.float),
                Rc::clone(&primitive_types.float),
            ],
            Rc::clone(&primitive_types.float),
        )),
    );
    env.insert(
        strings.get_or_intern("__op__sub"),
        Rc::new(Type::Fn(
            vec![
                Rc::clone(&primitive_types.float),
                Rc::clone(&primitive_types.float),
            ],
            Rc::clone(&primitive_types.float),
        )),
    );
    env.insert(
        strings.get_or_intern("__op__mult"),
        Rc::new(Type::Fn(
            vec![
                Rc::clone(&primitive_types.float),
                Rc::clone(&primitive_types.float),
            ],
            Rc::clone(&primitive_types.float),
        )),
    );
    env.insert(
        strings.get_or_intern("__op__div"),
        Rc::new(Type::Fn(
            vec![
                Rc::clone(&primitive_types.float),
                Rc::clone(&primitive_types.float),
            ],
            Rc::clone(&primitive_types.float),
        )),
    );
}

/* The main entry point to type inference */
pub fn infer<'interfaces, 'ast>(
    module_interfaces: &'interfaces ModuleInterfaces,
    module: &'ast Module,
    primitive_types: &PrimitiveTypes,
    strings: &mut Strings,
) -> Result<Rc<TypeEnv>, (Rc<TypeEnv>, Vec<Error<'ast>>)> {
    let mut state = State::new();
    let mut env = TypeEnv::new();
    base_env(&mut state, &mut env, primitive_types, strings);
    let mut errors: Vec<Error> = vec![];

    let mut module_type = TypeEnv::new();

    // Check imports and add them to the env to type check this module
    for import in &module.imports {
        let module_full_name = &import.value.module_name.full_name;
        match module_interfaces.get(module_full_name) {
            Some(imported) => {
                if let Some(alias) = &import.value.alias {
                    env.insert(alias.value.name, Rc::new(Record((**imported).clone())));
                } else {
                    env.insert(
                        import.value.module_name.full_name,
                        Rc::new(Record((**imported).clone())),
                    );
                };

                for exposed in &import.value.exposing {
                    for identifier in exposed.value.identifiers() {
                        let name = &identifier.value.name;
                        match imported.get(name) {
                            Some(definition) => env.insert(*name, Rc::clone(definition)),
                            None => errors.push(Error::UnknownImportDefinition(identifier, import)),
                        };
                    }
                }
            }
            None => errors.push(Error::UnknownImport(import)),
        }
    }

    // Type check the definitions in the module
    infer_definitions(
        &module.definitions,
        &mut state,
        &mut env,
        primitive_types,
        &mut errors,
    );

    // Check exports against this module type to see everything exists and is valid
    for export in &module.exports {
        for identifier in export.value.identifiers() {
            let name = &identifier.value.name;
            match env.get(name) {
                Some(typ) => module_type.insert(*name, Rc::clone(typ)),
                None => errors.push(Error::UndefinedExport(identifier)),
            }
        }
    }

    if errors.is_empty() {
        Ok(Rc::new(module_type))
    } else {
        Err((Rc::new(module_type), errors))
    }
}

fn infer_rec<'ast>(
    ast: &'ast Expression,
    state: &mut State,
    env: &mut TypeEnv,
    primitive_types: &PrimitiveTypes,
    errors: &mut Vec<Error<'ast>>,
) -> Rc<Type> {
    let typ = match &ast.value.expr {
        ET::Unit => Rc::new(Unit),

        ET::Bool(_) => Rc::clone(&primitive_types.bool),

        ET::Float(_) => Rc::clone(&primitive_types.float),

        ET::String_(_) => Rc::clone(&primitive_types.string),

        ET::ModuleAccess(module_name) => match env.get(&module_name.full_name) {
            Some(s) => Rc::clone(s),
            None => {
                add_error(Err(Error::UndefinedModule(module_name, ast)), errors);
                state.new_type_var()
            }
        },

        ET::Record(fields) => {
            let mut typed_fields = TypeEnv::new();

            for (name, value) in fields {
                let t = infer_rec(value, state, env, primitive_types, errors);
                if typed_fields.map().contains_key(&name.value.name) {
                    errors.push(Error::DuplicateField(name, ast));
                } else {
                    typed_fields.insert(name.value.name, t);
                }
            }

            Rc::new(Record(typed_fields))
        }

        ET::RecordUpdate(record, fields) => {
            let mut typed_fields = TypeEnv::new();
            for (name, value) in fields {
                let t = infer_rec(value, state, env, primitive_types, errors);
                if typed_fields.map().contains_key(&name.value.name) {
                    errors.push(Error::DuplicateField(name, ast));
                } else {
                    typed_fields.insert(name.value.name, t);
                }
            }

            let inferred_type = Rc::new(RecordExt(typed_fields, Rc::clone(&state.new_type_var())));

            let record_type = infer_rec(record, state, env, primitive_types, errors);

            // Unify the base record with the extensible record represented by this update
            add_error(
                unify(state, record, &record_type, None, &inferred_type),
                errors,
            );

            // Unify the type of this expression with the inferred type after the unification with
            // the base record. Gives a chance to flatten the inner record types when substituting.
            let t = state.new_type_var();
            add_error(unify(state, record, &t, None, &inferred_type), errors);

            t
        }

        ET::PropAccess(expr, field) => {
            let field_type = state.new_type_var();
            let remaining_fields_type = state.new_type_var();
            let record_type = Rc::new(RecordExt(
                {
                    let mut fields = TypeEnv::new();
                    fields.insert(field.value.name, Rc::clone(&field_type));
                    fields
                },
                remaining_fields_type,
            ));

            let expr_type = infer_rec(expr, state, env, primitive_types, errors);

            add_error(unify(state, expr, &expr_type, None, &record_type), errors);

            field_type
        }

        ET::PropAccessLambda(field) => {
            let field_type = state.new_type_var();
            let remaining_fields_type = state.new_type_var();

            Rc::new(Fn(
                vec![Rc::new(RecordExt(
                    {
                        let mut fields = TypeEnv::new();
                        fields.insert(field.value.name, Rc::clone(&field_type));
                        fields
                    },
                    remaining_fields_type,
                ))],
                field_type,
            ))
        }

        ET::Unary(op, e) => {
            let t = infer_rec(e, state, env, primitive_types, errors);

            add_error(
                match op.value {
                    U::Not => unify(state, e, &t, None, &primitive_types.bool),
                    U::Minus => unify(state, e, &t, None, &primitive_types.float),
                },
                errors,
            );

            t
        }

        ET::Binary(fn_, _op, args) => {
            // Infer the binary op as a function call
            infer_fn_call(fn_, args.iter(), ast, state, env, primitive_types, errors)
        }

        /* If
         * infer env condition = t0
         * unify t0 bool
         * infer env then = t1
         * infer env else_ = t2
         * unify t1 t2
         * infer env (if condition then else) = t2
         */
        ET::If(condition, then, else_) => {
            let t = infer_rec(condition, state, env, primitive_types, errors);
            add_error(
                unify(state, condition, &t, None, &primitive_types.bool),
                errors,
            );

            let t1 = infer_rec(then, state, env, primitive_types, errors);
            let t2 = infer_rec(else_, state, env, primitive_types, errors);
            add_error(unify(state, then, &t1, Some(else_), &t2), errors);

            t2
        }

        /*
         * Var
         *   x : s ∊ env
         *   t = inst s
         *   -----------
         *   infer env x = t
         */
        ET::Identifier(x) => match env.get(&x.value.name) {
            Some(s) => state.instantiate(s),
            None => {
                add_error(Err(Error::UndefinedIdentifier(x, ast)), errors);
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
        ET::FnCall(f, args) => {
            infer_fn_call(f, args.iter(), ast, state, env, primitive_types, errors)
        }

        /* Abs
         *   t = newVar ()
         *   infer (SMap.add x t env) e = t'
         *   -------------
         *   infer env (fun x -> e) = t -> t'
         */
        ET::Lambda(params, body) => infer_lambda(params, body, state, env, primitive_types, errors),

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
        ET::Let(bindings, e) => {
            let mut new_env = env.clone();

            infer_definitions(bindings, state, &mut new_env, primitive_types, errors);

            infer_rec(e, state, &mut new_env, primitive_types, errors)
        }
    };

    ast.value.set_type(Rc::clone(&typ));

    typ
}

fn infer_definitions<'ast>(
    definitions: &'ast [Definition],
    state: &mut State,
    env: &mut TypeEnv,
    primitive_types: &PrimitiveTypes,
    errors: &mut Vec<Error<'ast>>,
) {
    for definition in definitions {
        match &definition {
            Definition::Lambda(identifier, expression) => match &expression.value.expr {
                ET::Lambda(params, body) => {
                    state.enter_level();
                    let t = infer_lambda(params, body, state, env, primitive_types, errors);
                    state.exit_level();

                    env.insert(identifier.value.name, state.generalize(&t));
                }
                _ => panic!("Top level definitions classified as Lambda should always have a Lambda expression on the right hand side. This is a compiler bug. Please report it!"),
            },
            Definition::Pattern(pattern, value) => {
                state.enter_level();
                let t = infer_rec(value, state, env, primitive_types, errors);
                state.exit_level();

                match &pattern.value {
                    P::Hole => (),
                    P::Identifier(x) => env.insert(x.value.name, state.generalize(&t)),
                };
            }
        };
    }
}

fn infer_lambda<'ast>(
    params: &'ast [ast::Pattern],
    body: &'ast Expression,
    state: &mut State,
    env: &mut TypeEnv,
    primitive_types: &PrimitiveTypes,
    errors: &mut Vec<Error<'ast>>,
) -> Rc<Type> {
    let params_with_type: Vec<(&ast::Pattern, Rc<Type>)> =
        params.iter().map(|p| (p, state.new_type_var())).collect();

    let mut env = params_with_type
        .iter()
        .fold(env.clone(), |mut env, (param, param_type)| {
            match &(**param).value {
                P::Hole => (),
                P::Identifier(x) => env.insert(x.value.name, Rc::clone(param_type)),
            };
            env
        });

    let return_type = infer_rec(body, state, &mut env, primitive_types, errors);

    Rc::new(Fn(
        params_with_type
            .into_iter()
            .map(|(_, param_type)| param_type)
            .collect(),
        return_type,
    ))
}

fn infer_fn_call<'ast, Args>(
    f: &'ast Expression,
    args: Args,
    ast: &'ast Expression,
    state: &mut State,
    env: &mut TypeEnv,
    primitive_types: &PrimitiveTypes,
    errors: &mut Vec<Error<'ast>>,
) -> Rc<Type>
where
    Args: Iterator<Item = &'ast Expression> + Clone,
{
    let fn_type = infer_rec(f, state, env, primitive_types, errors);
    let param_types = fn_type.parameters();

    let arg_types: Vec<Rc<Type>> = args
        .clone()
        .map(|arg| infer_rec(arg, state, env, primitive_types, errors))
        .collect();

    let return_type = state.new_type_var();

    let call_type: Rc<Type> = Rc::new(Fn(arg_types.clone(), Rc::clone(&return_type)));

    // Unify the arguments separately for nicer error messages
    let res = arg_types
        .iter()
        .zip(args)
        .zip(param_types)
        .fold(Ok(()), |result, ((arg_type, arg), param_type)| {
            result.and_then(|_| unify(state, arg, arg_type, None, param_type))
        })
        // If there weren't any failures, unify the Fn and return types
        .and_then(|_| unify(state, ast, &call_type, None, &fn_type));
    add_error(res, errors);

    return_type
}

fn add_error<'ast>(result: Result<(), Error<'ast>>, errors: &mut Vec<Error<'ast>>) {
    if let Err(e) = result {
        errors.push(e);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::stages as compiler;
    use crate::parser;
    use crate::tokenizer;
    use insta::assert_snapshot;

    pub fn infer_expression<'ast>(
        ast: &'ast Expression,
        state: &mut State,
        env: &mut TypeEnv,
        primitive_types: &PrimitiveTypes,
    ) -> Result<Rc<Type>, Vec<Error<'ast>>> {
        let mut errors: Vec<Error<'ast>> = vec![];
        let t = infer_rec(ast, state, env, &primitive_types, &mut errors);

        if errors.is_empty() {
            Ok(t)
        } else {
            Err(errors)
        }
    }

    #[test]
    fn test_infer_expr() {
        assert_snapshot!(infer(r"\f -> \x -> f x",));

        assert_snapshot!(infer(r"\f -> \x -> f (f x)",));

        // (+):

        assert_snapshot!(infer(r"\m -> \n -> \f -> \x -> m f (n f x)",));

        // succ:

        assert_snapshot!(infer(r"\n -> \f -> \x -> f (n f x)",));

        // mult:

        assert_snapshot!(infer(r"\m -> \n -> \f -> \x -> m (n f) x",));

        // pred:

        assert_snapshot!(infer(
            r"\n -> \f -> \x -> n (\g -> \h -> h (g f)) (\u -> x) (\u -> u)",
        ));

        // let generalization tests

        assert_snapshot!(infer(
            r"
      \x ->
        let y = x
        y
    ",
        ));

        assert_snapshot!(infer(
            r"
      \x ->
        let y = \z -> x
        y
      ",
        ));

        // if:

        assert_snapshot!(infer("if 1 == 1 then True else False"));

        assert_snapshot!(infer("if 1 == 1 then 1 else 2"));

        // errors:

        assert_snapshot!(infer(
            "if 1 == 1 then if 1 + 1 > 2 then 5 else 1 / 1 else 2 + 2",
        ));

        assert_snapshot!(infer("if 1 then 5 else 1"));

        // let:

        assert_snapshot!(infer(
            r"let incr = \n -> n + 1

incr True"
        ));

        assert_snapshot!(infer(
            r"\x ->
    let a = x + 1
    let b = not x
    x"
        ));

        assert_snapshot!(infer("let a = bar\nbar"));

        assert_snapshot!(infer(
            "\
let add x y = x + y

add 5"
        ));

        // Infinite type:

        assert_snapshot!(infer(r"\a -> a 1 a",));

        // Record literal

        assert_snapshot!(infer(r"{}"));

        assert_snapshot!(infer(r"{ x = 1 }"));

        assert_snapshot!(infer(r"{ x = 1, y = True }"));

        assert_snapshot!(infer("{ x = { x = 5 } }"));

        // Property access

        assert_snapshot!(infer(r#"{ age = 1, msg = "Hello" }.age"#));

        assert_snapshot!(infer(r#"{ age = 1, msg = "Hello" }.msg"#));

        assert_snapshot!(infer(r#"{ age = 1, msg = "Hello" }.wat"#));

        assert_snapshot!(infer(r#"let a = "Hi" in a.wat"#));

        // Property access shorthand lambda

        assert_snapshot!(infer(".name"));

        assert_snapshot!(infer(".name { name = 1 }"));

        assert_snapshot!(infer(".name { age = 1 }"));

        // Record update

        assert_snapshot!(infer("{ {} | age = 1 }"));

        assert_snapshot!(infer("{ { age = 5 } | age = 1 }"));

        assert_snapshot!(infer("{ { age = 5 } | age = \"Hi\" }"));

        assert_snapshot!(infer(r"\thing -> { thing | age = 5 }"));

        assert_snapshot!(infer(
            r#"(\thing -> { thing | age = 5 }) { name = "Joe", age = 1 }"#
        ));

        // Duplicate fields in records

        assert_snapshot!(infer("{ age = 1, age = 1 }"));

        assert_snapshot!(infer("{ name = 1, age = 1, name = 1 }"));

        assert_snapshot!(infer("{ { name = 1, age = 1 } | name = 2, name = 3 }"));

        fn infer(code: &str) -> String {
            let mut strings = Strings::new();
            let source = Source::new_orphan(code.to_string());

            let tokens = tokenizer::parse(&source)
                .map_err(|errors| {
                    errors
                        .iter()
                        .map(|e| e.to_string(&source))
                        .collect::<Vec<String>>()
                        .join("\n\n")
                })
                .unwrap();

            let ast = parser::tests::parse_expression(&source, &tokens, &mut strings)
                .map_err(|error| error.to_string(&source))
                .unwrap();

            let mut state = State::new();
            let mut env = TypeEnv::new();
            let primitive_types = Type::primitive_types(&mut strings);
            base_env(&mut state, &mut env, &primitive_types, &mut strings);
            let s = match infer_expression(&ast, &mut state, &mut env, &primitive_types) {
                Ok(typ) => typ.to_string(&strings),
                Err(errs) => errs
                    .iter()
                    .map(|e| e.to_string(&source, &strings))
                    .collect::<Vec<String>>()
                    .join("\n\n"),
            };

            format!("Input:\n\n{}\n\n---\nOutput:\n\n{}", code, s)
        }
    }

    #[test]
    fn test_infer() {
        assert_snapshot!(infer(
            "
module Test

a = 1

b = 2
           "
        ));

        assert_snapshot!(infer(
            "
module Test exposing (a)

a = 1

b = 2
           "
        ));

        assert_snapshot!(infer(
            "
module Test exposing (a, b)

a = 1

b = 2
           "
        ));

        assert_snapshot!(infer(
            "
module Test exposing (c)

a = 1

b = 2
           "
        ));

        assert_snapshot!(infer(
            r#"
module Test exposing (a, b)

a = 1

b = 2

module Test.TestInner exposing (a, b)
  a = \x y -> x + y
  b = \c -> c

c = "hi"
"#
        ));

        assert_snapshot!(infer(
            r#"
module Parent

import Parent.Test exposing (test)

add = \x -> x + test

module Parent.Test exposing (test)

    test = 5
"#
        ));

        assert_snapshot!(infer(
            r#"
module Parent

import Parent.Test exposing (test)

add = \x -> x + test

module Parent.Test exposing (test)

    test = "hi"
"#
        ));

        assert_snapshot!(infer(
            r#"
module Parent

import Parent.Test exposing (nope)

add = \x -> x + test

module Parent.Test exposing (test)

    test = "hi"
"#
        ));

        assert_snapshot!(infer(
            r#"
module Parent

import Parent.Test exposing (test)

add = \x -> x + test x

module Parent.Test exposing (test)

    test = \x -> x + "hi"
"#
        ));

        assert_snapshot!(infer(
            r#"
module Parent

import Nope exposing (test)

test = 1
"#
        ));

        assert_snapshot!(infer(
            "\
module Test exposing (main, add)

add x y = x + y

main = add 5
"
        ));

        assert_snapshot!(infer(
            "module Test exposing (last)

last _ y = y
"
        ));

        assert_snapshot!(infer(
            "\
module User exposing (id, new)

import User.Id

id = User.Id

new = { id = User.Id.new }

module User.Id exposing (new)
    new = 42
"
        ));

        assert_snapshot!(infer(
            "\
module User exposing (new)

import User.Id

new = { id = User.Id.new }

module User.Id exposing (new)
    new = 42
"
        ));

        assert_snapshot!(infer(
            "\
module User exposing (new)

import User.Id as UserId

new = { id = UserId.new }

module User.Id exposing (new)
    new = 42
"
        ));

        assert_snapshot!(infer(
            "\
module User exposing (new)

import User.Id as UserId

new = { id = User.Id.new }

module User.Id exposing (new)
    new = 42
"
        ));

        assert_snapshot!(infer(
            "\
module User exposing (new)

import User.Attributes
import User.Attributes.Id

new = { id = User.Attributes.Id.new }

module User.Attributes
    module User.Attributes.Id exposing (new)
        new = 42
"
        ));

        assert_snapshot!(infer(
            "\
module User exposing (new)

import User.Id

new = User.Id.wat

module User.Id exposing (new)
    new = 42
"
        ));

        assert_snapshot!(infer(
            "\
module User exposing (new)

import User.Id

new = User.Id

module User.Id exposing (new)
    new = 42
"
        ));

        fn infer(code: &str) -> String {
            let mut strings = Strings::new();
            let source = Source::new_orphan(code.to_string());

            let primitive_types = Type::primitive_types(&mut strings);

            let (entry_sources, sources) = compiler::process_sources(vec![source]);

            let (entry_modules, module_sources, module_asts) =
                compiler::parse_files(&entry_sources, &sources, &mut strings).unwrap();

            let actual = match compiler::infer(
                entry_modules,
                &module_sources,
                &module_asts,
                &primitive_types,
                &mut strings,
            ) {
                Ok(module_interfaces) => module_interfaces.to_string(&strings),
                Err(err) => err,
            };

            format!("Input:\n\n{}\n\n---\nOutput:\n\n{}", &code, actual)
        }
    }
}
