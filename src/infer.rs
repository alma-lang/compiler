use crate::ast::{
    self, CapitalizedIdentifier, Definition, Expression, ExpressionType as ET, Identifier, Import,
    Lambda, Node, Pattern_ as P, TypeSignature, TypedDefinition, Unary_ as U,
};
use crate::compiler;
use crate::compiler::state::ModuleIndex;
use crate::compiler::types::{HashMap, ModuleInterface};
use crate::source::Source;
use crate::strings::{Strings, Symbol as StringSymbol};
use crate::token::Tokens;
use crate::typ::{Type::*, TypeVar::*, *};
use crate::type_env::{PolyTypeEnv, TypeEnv};
use indexmap::IndexSet;
use std::cell::RefCell;
use std::cmp::min;
use std::rc::Rc;

/*
 * Resources:
 *   - https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Algorithm_J
 *   - http://okmij.org/ftp/ML/generalization.html
 */

#[derive(Debug)]
pub enum Error {
    UndefinedIdentifier {
        identifier: StringSymbol,
        location: Node<()>,
    },
    DuplicateField {
        field: Identifier,
        expr: Expression,
    },
    TypeMismatch {
        actual_type_location: Node<()>,
        actual_type: Rc<Type>,
        expected_type_location: Option<Node<()>>,
        expected_type: Rc<Type>,
    },
    WrongArity {
        location: Node<()>,
        num_params_applied: usize,
        num_params: usize,
        typ: Rc<Type>,
    },
    SignatureMismatch {
        signature_type_location: Node<()>,
        signature_type: Rc<Type>,
        inferred_type: Rc<Type>,
    },
    SignatureTooGeneral {
        signature_type_location: Node<()>,
        signature_type: Rc<Type>,
        inferred_type: Rc<Type>,
    },
    InfiniteType {
        location: Node<()>,
        typ: Rc<Type>,
    },
    UndefinedExport(Identifier),
    UndefinedExportConstructor(CapitalizedIdentifier),
    UnknownImport(Import),
    UnknownImportDefinition {
        export: Identifier,
        import: Import,
    },
    UnknownImportConstructor {
        constructor: CapitalizedIdentifier,
        import: Import,
    },
    UnknownType(CapitalizedIdentifier),
    UnknownTypeVar(Identifier),
}

impl Error {
    pub fn to_string(&self, source: &Source, strings: &Strings, tokens: &Tokens) -> String {
        use Error::*;

        let mut s = String::new();

        let (position, line_number, column, end) = {
            let location = match self {
                UndefinedIdentifier { location, .. } => location.unit(),
                DuplicateField { field, .. } => field.unit(),
                TypeMismatch {
                    actual_type_location,
                    ..
                } => actual_type_location.unit(),
                WrongArity { location, .. } => location.unit(),
                SignatureMismatch {
                    signature_type_location,
                    ..
                } => signature_type_location.unit(),
                SignatureTooGeneral {
                    signature_type_location,
                    ..
                } => signature_type_location.unit(),
                InfiniteType { location, .. } => location.unit(),
                UndefinedExport(node) => node.unit(),
                UndefinedExportConstructor(node) => node.unit(),
                UnknownImport(node) => node.unit(),
                UnknownImportDefinition { export, .. } => export.unit(),
                UnknownImportConstructor { constructor, .. } => constructor.unit(),
                UnknownType(node) => node.unit(),
                UnknownTypeVar(node) => node.unit(),
            };
            let start_token = tokens[location.start];
            let end_token = tokens[location.end];
            (
                start_token.start,
                start_token.line,
                start_token.column,
                end_token.end,
            )
        };

        s.push_str(&source.to_string_with_line_and_col(line_number, column));
        s.push_str("\n\n");

        let code = source
            .lines_report_at_position_with_pointer(position, Some(end), line_number)
            .unwrap();

        match self {
            UndefinedIdentifier { identifier, .. } => {
                s.push_str(&format!(
                    "Undefined identifier `{identifier}`\n\n{code}",
                    identifier = strings.resolve(*identifier),
                ));
            }

            DuplicateField { field, expr } => {
                let expr_token = tokens[expr.start];
                let expr_end_token = tokens[expr.end];
                let record_code = source
                    .lines_report_at_position(
                        expr_token.start,
                        Some(expr_end_token.end),
                        expr_token.line,
                        false,
                    )
                    .unwrap();
                let identifier = field.value.to_string(strings);

                s.push_str(&format!(
                    "Duplicate field `{identifier}`

{code}

in record

{record_code}",
                ));
            }

            InfiniteType { .. } => {
                s.push_str(&format!("Infinite type\n\n{code}"));
            }

            TypeMismatch {
                actual_type,
                expected_type_location: None,
                expected_type: typ2,
                ..
            } => {
                let typ = actual_type.to_string(strings);
                let typ2 = typ2.to_string(strings);
                s.push_str(&format!(
                    "Type mismatch:  {typ}  ≠  {typ2}

Expected

{code}

to be

{typ2}

but seems to be

{typ}",
                ));
            }

            TypeMismatch {
                actual_type,
                expected_type_location: Some(expected_type_location),
                expected_type,
                ..
            } => {
                let node_token = tokens[expected_type_location.start];
                let node_end_token = tokens[expected_type_location.end];
                let code2 = source
                    .lines_report_at_position_with_pointer(
                        node_token.start,
                        Some(node_end_token.end),
                        node_token.line,
                    )
                    .unwrap();
                let actual_type = actual_type.to_string(strings);
                let expected_type = expected_type.to_string(strings);

                s.push_str(&format!(
                    "Type mismatch:  {actual_type}  ≠  {expected_type}

Expected

{code}

with type

{actual_type}

to have the same type as

{code2}

with type

{expected_type}",
                ));
            }

            WrongArity {
                num_params_applied,
                num_params,
                typ,
                ..
            } => {
                let plural = if *num_params == 1 { "" } else { "s" };
                let typ = typ.to_string(strings);
                s.push_str(&format!(
                    "Type `{typ}` accepts {num_params} parameter{plural} but it was called with {num_params_applied}.

{code}",
                ));
            }

            SignatureMismatch {
                signature_type,
                inferred_type,
                ..
            } => {
                let signature_type = signature_type.to_string(strings);
                let inferred_type = inferred_type.to_string(strings);
                s.push_str(&format!(
                    "The type signature and inferred type don't match

{code}

The type signature says the type is

{signature_type}

but it seems to be

{inferred_type}",
                ));
            }

            SignatureTooGeneral {
                signature_type,
                inferred_type,
                ..
            } => {
                let signature_type = signature_type.to_string(strings);
                let inferred_type = inferred_type.to_string(strings);
                s.push_str(&format!(
                    "The type signature is more generic than the inferred type:

{code}

The type signature says the type is

{signature_type}

which it is more general than

{inferred_type}

which was inferred from the code.

Change the signature to be more specific or try to make your code more generic.",
                ));
            }

            UndefinedExport(export) => {
                let export = export.value.to_string(strings);
                s.push_str(&format!("Undefined identifier `{export}`\n\n{code}"));
            }

            UndefinedExportConstructor(constructor) => {
                let constructor = constructor.value.to_string(strings);
                s.push_str(&format!("Undefined identifier `{constructor}`\n\n{code}"));
            }

            UnknownImport(import) => {
                let module = import.value.module_name.to_string(strings);
                s.push_str(&format!("Couldn't find module `{module}`\n\n{code}"));
            }

            UnknownImportDefinition { export, import } => {
                let module = import.value.module_name.to_string(strings);
                let export = export.value.to_string(strings);
                s.push_str(&format!(
                    "Module `{module}` doesn't appear to expose `{export}`\n\n{code}"
                ));
            }

            UnknownImportConstructor {
                constructor,
                import,
            } => {
                let module = import.value.module_name.to_string(strings);
                let export = constructor.value.to_string(strings);
                s.push_str(&format!(
                    "Module `{module}` doesn't appear to expose `{export}`\n\n{code}"
                ));
            }

            UnknownType(name) => {
                let type_name = name.value.to_string(strings);
                s.push_str(&format!("Couldn't find type `{type_name}`\n\n{code}"));
            }

            UnknownTypeVar(name) => {
                let type_name = name.value.to_string(strings);
                s.push_str(&format!(
                    "Type variable `{type_name}` has not been declared\n\n{code}"
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

    /**
     * Specializes the PolyType s by copying the term and replacing the
     * bound type variables consistently by new monomorphic type variables
     *
     * E.g. instantiate (forall a b. a -> b -> a) = c -> d -> c
     */
    fn instantiate(&mut self, t: &Rc<PolyType>) -> Rc<Type> {
        /* Replace any type vars found in the hashtable with the
         * associated value in the same table, leave them otherwise
         */
        fn replace_type_vars(
            vars_to_replace: &HashMap<TypeVarId, Rc<Type>>,
            t: &Rc<Type>,
        ) -> Rc<Type> {
            match &**t {
                Unit => Rc::clone(t),

                Named {
                    module,
                    name,
                    params,
                } => Rc::new(Named {
                    module: *module,
                    name: *name,
                    params: params
                        .iter()
                        .map(|arg| replace_type_vars(vars_to_replace, arg))
                        .collect(),
                }),

                Var(var) => match &*((**var).borrow()) {
                    Bound(t) => replace_type_vars(vars_to_replace, t),
                    Unbound(n, _level) => match vars_to_replace.get(n) {
                        Some(t_) => Rc::clone(t_),
                        None => Rc::clone(t),
                    },
                },

                Fn { params, ret } => Rc::new(Fn {
                    params: {
                        let mut new_args = vec![];
                        for param in params {
                            new_args.push(replace_type_vars(vars_to_replace, param));
                        }
                        new_args
                    },
                    ret: replace_type_vars(vars_to_replace, ret),
                }),

                Record { fields } => {
                    let mut new_fields = fields.clone();
                    for (key, value) in fields.map() {
                        new_fields.insert(*key, replace_type_vars(vars_to_replace, value));
                    }
                    Rc::new(Record { fields: new_fields })
                }

                RecordExt {
                    fields,
                    base_record,
                } => {
                    let mut new_fields = fields.clone();
                    for (key, value) in fields.map() {
                        new_fields.insert(*key, replace_type_vars(vars_to_replace, value));
                    }
                    Rc::new(RecordExt {
                        fields: new_fields,
                        base_record: replace_type_vars(vars_to_replace, base_record),
                    })
                }

                Alias {
                    module,
                    name,
                    params,
                    destination,
                } => Rc::new(Alias {
                    module: *module,
                    name: *name,
                    params: {
                        let mut new_params = Vec::new();
                        for (name, param) in params.iter() {
                            new_params.push((*name, replace_type_vars(vars_to_replace, param)));
                        }
                        new_params
                    },
                    destination: replace_type_vars(vars_to_replace, destination),
                }),
            }
        }

        let mut vars_to_replace: HashMap<TypeVarId, Rc<Type>> = HashMap::default();
        for var in t.vars.iter() {
            vars_to_replace.insert(*var, self.new_type_var());
        }
        replace_type_vars(&vars_to_replace, &t.typ)
    }

    /* Find all type variables and wrap the type in a PolyType
     * e.g. generalize (a -> b -> b) = forall a b. a -> b -> b
     */
    fn generalize(&self, t: &Rc<Type>) -> Rc<PolyType> {
        let current_level = self.current_level;

        // Collect all the monomorphic typevars
        fn find_all_tvs(current_level: &Level, vars: &mut IndexSet<TypeVarId>, t: &Rc<Type>) {
            match &**t {
                Unit => (),

                Named { params, .. } => {
                    for param in params.iter() {
                        find_all_tvs(current_level, vars, param);
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

                Fn { params, ret } => {
                    for param in params {
                        find_all_tvs(current_level, vars, param);
                    }
                    find_all_tvs(current_level, vars, ret);
                }

                Record { fields } => {
                    for t in fields.map().values() {
                        find_all_tvs(current_level, vars, t);
                    }
                }

                RecordExt {
                    fields,
                    base_record,
                } => {
                    for t in fields.map().values() {
                        find_all_tvs(current_level, vars, t);
                    }
                    find_all_tvs(current_level, vars, base_record);
                }

                Alias {
                    params,
                    destination,
                    ..
                } => {
                    for (_name, param) in params.iter() {
                        find_all_tvs(current_level, vars, param);
                    }
                    find_all_tvs(current_level, vars, destination)
                }
            }
        }

        let mut tvs = IndexSet::new();
        find_all_tvs(&current_level, &mut tvs, t);
        Rc::new(PolyType::new(tvs, Rc::clone(t)))
    }
}

/**
 * Can a monomorphic Var(a) be found inside this type?
 */
fn occurs(a_id: TypeVarId, a_level: Level, t: &Rc<Type>) -> bool {
    match &**t {
        Unit => false,
        Named { .. } => false,

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

        Fn { params, ret } => {
            params.iter().any(|param| occurs(a_id, a_level, param)) || occurs(a_id, a_level, ret)
        }

        Record { fields } => fields.map().values().any(|tv| occurs(a_id, a_level, tv)),

        RecordExt {
            fields,
            base_record,
        } => {
            if fields.map().values().any(|tv| occurs(a_id, a_level, tv)) {
                true
            } else {
                occurs(a_id, a_level, base_record)
            }
        }

        Alias { destination, .. } => occurs(a_id, a_level, destination),
    }
}

fn flatten_record(typ: &Rc<Type>) -> Option<Rc<Type>> {
    let typ = find(typ);
    if let RecordExt { .. } = &*typ {
        let mut all_fields = TypeEnv::new();
        let mut typ = typ;

        loop {
            match &*typ {
                Record { fields } => {
                    for (k, v) in fields.map() {
                        all_fields.insert(*k, Rc::clone(v));
                    }
                    return Some(Rc::new(Record { fields: all_fields }));
                }
                RecordExt {
                    fields,
                    base_record,
                } => {
                    for (k, v) in fields.map() {
                        all_fields.insert(*k, Rc::clone(v));
                    }
                    typ = find(base_record);
                }
                Var(var) => {
                    let var_read = (**var).borrow();
                    match &*var_read {
                        Unbound(_, _) => {
                            return Some(Rc::new(RecordExt {
                                fields: all_fields,
                                base_record: Rc::clone(&typ),
                            }))
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

fn unify<'ast, T>(
    state: &mut State,
    ast: &Node<T>,
    typ: &Rc<Type>,
    ast2: Option<&Node<T>>,
    typ2: &Rc<Type>,
) -> Result<(), Error> {
    unify_rec(state, typ, typ2, ast, typ, ast2, typ2)
}

fn unify_var<'ast, T>(
    state: &mut State,
    t: &Rc<Type>,
    var: &Rc<RefCell<TypeVar>>,
    other_type: &Rc<Type>,
    ast: &Node<T>,
    typ: &Rc<Type>,
    ast2: Option<&Node<T>>,
    typ2: &Rc<Type>,
) -> Result<(), Error> {
    let var_read = (**var).borrow();
    match &*var_read {
        // The 'find' in the union-find algorithm
        Bound(dest_var) => unify_rec(state, dest_var, other_type, ast, typ, ast2, typ2),

        // Create binding for unbound type variable
        Unbound(a_id, a_level) => {
            let (a_id, a_level) = (*a_id, *a_level);
            // Drop the read borrow before the occurs check since it is not used and
            // can panic the occurs check if type 1 and type 2 point to the same thing
            drop(var_read);

            if t == other_type {
                Ok(())
            } else if occurs(a_id, a_level, other_type) {
                // a = a, but dont create a recursive binding to itself
                Err(Error::InfiniteType {
                    location: ast.unit(),
                    typ: Rc::clone(t),
                })
            } else {
                let mut var = var.borrow_mut();
                *var = Bound(Rc::clone(other_type));
                Ok(())
            }
        }
    }
}

fn unify_rec<'ast, T>(
    state: &mut State,
    t1: &Rc<Type>,
    t2: &Rc<Type>,
    ast: &Node<T>,
    typ: &Rc<Type>,
    ast2: Option<&Node<T>>,
    typ2: &Rc<Type>,
) -> Result<(), Error> {
    match (&**t1, &**t2) {
        (Unit, Unit) => Ok(()),

        (
            Named {
                module,
                name,
                params,
            },
            Named {
                module: module2,
                name: name2,
                params: params2,
            },
        ) => {
            // TODO: Specialize the arity error message instead of the generic TypeMismatch. We
            // could make unify_rec get the ast and original types to pass around and improve many
            // error messages around the whole function.
            if module != module2 || name != name2 || params.len() != params2.len() {
                Err(Error::TypeMismatch {
                    actual_type_location: ast.unit(),
                    actual_type: Rc::clone(typ),
                    expected_type_location: ast2.map(|ast2| ast2.unit()),
                    expected_type: Rc::clone(typ2),
                })
            } else {
                for (p1, p2) in params.iter().zip(params2.iter()) {
                    unify_rec(state, p1, p2, ast, typ, ast2, typ2)?;
                }
                Ok(())
            }
        }

        (Var(var), _) => unify_var(state, t1, var, t2, ast, typ, ast2, typ2),

        (_, Var(var)) => unify_var(state, t2, var, t1, ast, typ, ast2, typ2),

        (
            Fn { params, ret },
            Fn {
                params: params2,
                ret: ret2,
            },
        ) => {
            if params.len() != params2.len() {
                Err(Error::TypeMismatch {
                    actual_type_location: ast.unit(),
                    actual_type: Rc::clone(typ),
                    expected_type_location: ast2.map(|ast2| ast2.unit()),
                    expected_type: Rc::clone(typ2),
                })
            } else {
                for (p1, p2) in params.iter().zip(params2.iter()) {
                    unify_rec(state, p1, p2, ast, typ, ast2, typ2)?;
                }
                unify_rec(state, ret, ret2, ast, typ, ast2, typ2)
            }
        }

        (Record { fields: fields1 }, Record { fields: fields2 }) => {
            // Check fields on each record, and unify types of the values against the other
            // record. If the field doesn't exist then it is a type mismatch.

            for (key, value) in fields1.map() {
                if fields2.map().contains_key(key) {
                    unify_rec(
                        state,
                        value,
                        fields2.get(key).unwrap(),
                        ast,
                        typ,
                        ast2,
                        typ2,
                    )?;
                } else {
                    return Err(Error::TypeMismatch {
                        actual_type_location: ast.unit(),
                        actual_type: Rc::clone(typ),
                        expected_type_location: ast2.map(|ast2| ast2.unit()),
                        expected_type: Rc::clone(typ2),
                    });
                }
            }
            for (key, value) in fields2.map() {
                if fields1.map().contains_key(key) {
                    unify_rec(
                        state,
                        value,
                        fields1.get(key).unwrap(),
                        ast,
                        typ,
                        ast2,
                        typ2,
                    )?;
                } else {
                    return Err(Error::TypeMismatch {
                        actual_type_location: ast.unit(),
                        actual_type: Rc::clone(typ),
                        expected_type_location: ast2.map(|ast2| ast2.unit()),
                        expected_type: Rc::clone(typ2),
                    });
                }
            }

            Ok(())
        }

        (
            Record { fields },
            RecordExt {
                fields: ext_fields,
                base_record,
            },
        )
        | (
            RecordExt {
                fields: ext_fields,
                base_record,
            },
            Record { fields },
        ) => {
            // Check the fields on the open record and match them against the fixed record. If
            // a field doesn't exist it is a mismatch
            for (key, value) in ext_fields.map() {
                if fields.map().contains_key(key) {
                    unify_rec(state, value, fields.get(key).unwrap(), ast, typ, ast2, typ2)?;
                } else {
                    return Err(Error::TypeMismatch {
                        actual_type_location: ast.unit(),
                        actual_type: Rc::clone(typ),
                        expected_type_location: ast2.map(|ast2| ast2.unit()),
                        expected_type: Rc::clone(typ2),
                    });
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
            let rem_rec = Rc::new(Record { fields: rem_fields });
            unify_rec(state, base_record, &rem_rec, ast, typ, ast2, typ2)
        }

        (
            RecordExt {
                fields: fields1,
                base_record: base_record1,
            },
            RecordExt {
                fields: fields2,
                base_record: base_record2,
            },
        ) => {
            // Check common fields on the records, and unify types
            for (key, value) in fields1.map() {
                if fields2.map().contains_key(key) {
                    unify_rec(
                        state,
                        value,
                        fields2.get(key).unwrap(),
                        ast,
                        typ,
                        ast2,
                        typ2,
                    )?;
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
                let rem_rec1 = Rc::new(RecordExt {
                    fields: rem_fields1,
                    base_record: Rc::clone(&var),
                });
                unify_rec(state, base_record2, &rem_rec1, ast, typ, ast2, typ2)?;
            }

            // unify { <fields-only-found-on-the-right-side> | new-type-var } varLeft
            {
                let mut rem_fields2 = TypeEnv::new();
                for (key, value) in fields2.map() {
                    if !fields1.map().contains_key(key) {
                        rem_fields2.insert(*key, Rc::clone(value));
                    }
                }
                let rem_rec2 = Rc::new(RecordExt {
                    fields: rem_fields2,
                    base_record: Rc::clone(&var),
                });
                unify_rec(state, base_record1, &rem_rec2, ast, typ, ast2, typ2)?;
            }

            Ok(())
        }

        (Alias { destination: t, .. }, _) => unify_rec(state, t, t2, ast, typ, ast2, typ2),
        (_, Alias { destination: t, .. }) => unify_rec(state, t1, t, ast, typ, ast2, typ2),

        (Unit, _)
        | (Named { .. }, _)
        | (Fn { .. }, _)
        | (Record { .. }, _)
        | (RecordExt { .. }, _) => Err(Error::TypeMismatch {
            actual_type_location: ast.unit(),
            actual_type: Rc::clone(typ),
            expected_type_location: ast2.map(|ast2| ast2.unit()),
            expected_type: Rc::clone(typ2),
        }),
    }
}

/**
 * This function must be called after the types have been unified and are compatible
 */
fn signature_is_too_generic(signature: &Rc<Type>, inferred: &Rc<Type>) -> bool {
    let signature = find(signature);
    let inferred = find(inferred);
    let signature = flatten_record(&signature).unwrap_or(signature);
    let inferred = flatten_record(&inferred).unwrap_or(inferred);

    match (&*signature, &*inferred) {
        (
            Named { params, .. },
            Named {
                params: params2, ..
            },
        ) => {
            for (p1, p2) in params.iter().zip(params2.iter()) {
                if signature_is_too_generic(p1, p2) {
                    return true;
                }
            }
            false
        }

        // Two vars resolved by find above must be unbound, hence not too general
        (Var(_), Var(_)) => false,
        // Anything else not a variable means this is a variable in the signature but inferred to
        // something more specific, hence the signature is too general
        (Var(_), _) => true,

        (
            Fn { params, ret },
            Fn {
                params: params2,
                ret: ret2,
            },
        ) => {
            for (p1, p2) in params.iter().zip(params2.iter()) {
                if signature_is_too_generic(p1, p2) {
                    return true;
                }
            }
            signature_is_too_generic(ret, ret2)
        }

        (Record { fields: fields1 }, Record { fields: fields2 }) => {
            for (key, value) in fields1.map() {
                if signature_is_too_generic(value, fields2.get(key).unwrap()) {
                    return true;
                }
            }
            false
        }

        (
            Record { fields },
            RecordExt {
                fields: ext_fields, ..
            },
        ) => {
            for (key, value) in fields.map() {
                if signature_is_too_generic(value, ext_fields.get(key).unwrap()) {
                    return true;
                }
            }
            false
        }

        (RecordExt { .. }, Record { .. }) => true,

        (
            RecordExt {
                fields: signature_fields,
                base_record: signature_base_record,
            },
            RecordExt {
                fields: inferred_fields,
                base_record: inferred_base_record,
            },
        ) => {
            for (key, signature_field_value) in signature_fields.map() {
                if signature_is_too_generic(
                    signature_field_value,
                    inferred_fields.get(key).unwrap(),
                ) {
                    return true;
                }
            }
            signature_is_too_generic(signature_base_record, inferred_base_record)
        }

        (Alias { destination, .. }, _) => signature_is_too_generic(destination, &inferred),

        (_, Alias { destination, .. }) => signature_is_too_generic(&signature, destination),

        (Unit, _)
        | (Named { .. }, _)
        | (Fn { .. }, _)
        | (Record { .. }, _)
        | (RecordExt { .. }, _) => false,
    }
}

fn base_env(
    state: &mut State,
    env: &mut PolyTypeEnv,
    primitive_types: &PolyTypeEnv,
    strings: &mut Strings,
) {
    let bool_ =
        Rc::new(state.instantiate(primitive_types.get(&strings.get_or_intern("Bool")).unwrap()));
    let float = Rc::new(
        state.instantiate(
            primitive_types
                .get(&strings.get_or_intern("Float"))
                .unwrap(),
        ),
    );
    let _string = Rc::new(
        state.instantiate(
            primitive_types
                .get(&strings.get_or_intern("String"))
                .unwrap(),
        ),
    );

    env.insert(
        strings.get_or_intern("__op__or"),
        state.generalize(&Rc::new(Type::Fn {
            params: vec![Rc::clone(&bool_), Rc::clone(&bool_)],
            ret: Rc::clone(&bool_),
        })),
    );

    env.insert(
        strings.get_or_intern("__op__or"),
        state.generalize(&Rc::new(Type::Fn {
            params: vec![Rc::clone(&bool_), Rc::clone(&bool_)],
            ret: Rc::clone(&bool_),
        })),
    );
    env.insert(
        strings.get_or_intern("__op__and"),
        state.generalize(&Rc::new(Type::Fn {
            params: vec![Rc::clone(&bool_), Rc::clone(&bool_)],
            ret: Rc::clone(&bool_),
        })),
    );
    env.insert(strings.get_or_intern("__op__eq"), {
        let a = state.new_type_var();
        state.generalize(&Rc::new(Type::Fn {
            params: vec![Rc::clone(&a), Rc::clone(&a)],
            ret: Rc::clone(&bool_),
        }))
    });
    env.insert(strings.get_or_intern("__op__ne"), {
        let a = state.new_type_var();
        state.generalize(&Rc::new(Type::Fn {
            params: vec![Rc::clone(&a), Rc::clone(&a)],
            ret: Rc::clone(&bool_),
        }))
    });
    env.insert(
        strings.get_or_intern("__op__gt"),
        state.generalize(&Rc::new(Type::Fn {
            params: vec![Rc::clone(&float), Rc::clone(&float)],
            ret: Rc::clone(&bool_),
        })),
    );
    env.insert(
        strings.get_or_intern("__op__ge"),
        state.generalize(&Rc::new(Type::Fn {
            params: vec![Rc::clone(&float), Rc::clone(&float)],
            ret: Rc::clone(&bool_),
        })),
    );
    env.insert(
        strings.get_or_intern("__op__lt"),
        state.generalize(&Rc::new(Type::Fn {
            params: vec![Rc::clone(&float), Rc::clone(&float)],
            ret: Rc::clone(&bool_),
        })),
    );
    env.insert(
        strings.get_or_intern("__op__le"),
        state.generalize(&Rc::new(Type::Fn {
            params: vec![Rc::clone(&float), Rc::clone(&float)],
            ret: Rc::clone(&bool_),
        })),
    );
    env.insert(
        strings.get_or_intern("__op__add"),
        state.generalize(&Rc::new(Type::Fn {
            params: vec![Rc::clone(&float), Rc::clone(&float)],
            ret: Rc::clone(&float),
        })),
    );
    env.insert(
        strings.get_or_intern("__op__sub"),
        state.generalize(&Rc::new(Type::Fn {
            params: vec![Rc::clone(&float), Rc::clone(&float)],
            ret: Rc::clone(&float),
        })),
    );
    env.insert(
        strings.get_or_intern("__op__mult"),
        state.generalize(&Rc::new(Type::Fn {
            params: vec![Rc::clone(&float), Rc::clone(&float)],
            ret: Rc::clone(&float),
        })),
    );
    env.insert(
        strings.get_or_intern("__op__div"),
        state.generalize(&Rc::new(Type::Fn {
            params: vec![Rc::clone(&float), Rc::clone(&float)],
            ret: Rc::clone(&float),
        })),
    );
}

/**
 * The main entry point to type inference.
 *
 * Infer the types of the whole module and produce its module interface with types.
 */
pub fn infer<'state>(
    compiler_state: &'state mut compiler::State,
    module_idx: ModuleIndex,
    primitive_types: &PolyTypeEnv,
) -> Result<(), Vec<Error>> {
    let strings = &mut compiler_state.strings;
    let module_ast = &compiler_state.asts[module_idx];
    let mut state = State::new();
    let mut env = PolyTypeEnv::new();
    let mut types_env = primitive_types.clone();
    base_env(&mut state, &mut env, primitive_types, strings);
    let mut errors: Vec<Error> = vec![];

    let mut module_definitions = PolyTypeEnv::new();
    let mut module_types = TypeEnv::new();
    let mut module_type_constructors = HashMap::default();

    // Check imports and add them to the env to type check this module
    for import in &module_ast.imports {
        let module_full_name = import.value.module_name.full_name;
        let import_types: Option<&ModuleInterface> = compiler_state
            .module_name_to_module_idx
            .get(&module_full_name)
            .and_then(|idx| compiler_state.types.get(*idx))
            .and_then(|maybe_types| maybe_types.as_ref());
        match import_types {
            Some(imported) => {
                let module_ident = if let Some(alias) = &import.value.alias {
                    alias.value.name
                } else {
                    import.value.module_name.full_name
                };

                let mut imported_env = (*imported.definitions).clone();
                for constructors in imported.type_constructors.values() {
                    for (constructor_name, constructor_type) in constructors.map() {
                        imported_env.insert(*constructor_name, Rc::clone(constructor_type));
                    }
                }

                for (ident, typ) in imported_env.map() {
                    let full_name = {
                        let module = strings.resolve(module_ident);
                        let ident = strings.resolve(*ident);
                        format!("{module}.{ident}")
                    };
                    let full_name = strings.get_or_intern(full_name);
                    env.insert(full_name, Rc::clone(typ));
                }

                for exposed in &import.value.exposing {
                    match &exposed.value {
                        ast::Export_::Identifier(identifier) => {
                            let name = &identifier.value.name;
                            match imported.definitions.get(name) {
                                Some(definition) => env.insert(*name, Rc::clone(definition)),
                                None => errors.push(Error::UnknownImportDefinition {
                                    export: identifier.to_owned(),
                                    import: import.to_owned(),
                                }),
                            };
                        }
                        ast::Export_::Type {
                            name: name_ast,
                            constructors,
                        } => {
                            let name = &name_ast.value.name;
                            match imported.types.get(name) {
                                Some(typ) => {
                                    types_env.insert(*name, state.generalize(&typ));

                                    let constructor_types =
                                        imported.type_constructors.get(name).unwrap();
                                    for constructor in constructors {
                                        let name = &constructor.value.name;
                                        match constructor_types.get(name) {
                                            Some(definition) => {
                                                env.insert(*name, Rc::clone(definition))
                                            }
                                            None => errors.push(Error::UnknownImportConstructor {
                                                constructor: constructor.to_owned(),
                                                import: import.to_owned(),
                                            }),
                                        };
                                    }
                                }
                                None => errors.push(Error::UnknownType(name_ast.to_owned())),
                            }
                        }
                    }
                }
            }
            None => errors.push(Error::UnknownImport(import.to_owned())),
        }
    }

    // Add local module types to environment
    for type_def in &module_ast.type_definitions {
        let mut type_vars = Vec::new();
        let mut type_vars_env = TypeEnv::new();
        state.enter_level();
        for var in &type_def.value.vars {
            let tvar = state.new_type_var();
            type_vars.push((var.value.name, Rc::clone(&tvar)));
            type_vars_env.insert(var.value.name, tvar);
        }
        state.exit_level();
        let name = type_def.value.name.value.name;

        match &type_def.value.typ {
            ast::types::TypeDefinitionType::Union { constructors } => {
                let type_def_type = Rc::new(Type::Named {
                    module: module_ast.name.full_name,
                    name,
                    params: type_vars.iter().map(|(_, t)| Rc::clone(t)).collect(),
                });
                types_env.insert(name, state.generalize(&type_def_type));

                for constructor in constructors {
                    let constructor = &constructor.value;
                    let name = &constructor.name.value.name;
                    let mut param_types = vec![];

                    for param in &constructor.params {
                        let typ = match ast_type_to_type(
                            &param,
                            &type_vars_env,
                            &mut errors,
                            &mut state,
                            &types_env,
                        ) {
                            Ok(t) => t,
                            Err(err) => {
                                errors.push(err);
                                state.new_type_var()
                            }
                        };
                        param_types.push(typ);
                    }

                    let typ = if param_types.is_empty() {
                        Rc::clone(&type_def_type)
                    } else {
                        Rc::new(Type::Fn {
                            params: param_types,
                            ret: Rc::clone(&type_def_type),
                        })
                    };

                    env.insert(*name, state.generalize(&typ));
                }
            }
            ast::types::TypeDefinitionType::Record(record_type) => {
                let typ = match ast_record_type_to_type(
                    record_type,
                    &type_vars_env,
                    &mut errors,
                    &mut state,
                    &types_env,
                ) {
                    Ok(t) => t,
                    Err(err) => {
                        errors.push(err);
                        state.new_type_var()
                    }
                };
                let typ = Rc::new(Alias {
                    module: module_ast.name.full_name,
                    name,
                    params: type_vars,
                    destination: typ,
                });
                types_env.insert(name, state.generalize(&typ));
            }
        }
    }

    // Type check the definitions in the module
    infer_definitions(
        &module_ast.definitions,
        &mut state,
        &mut env,
        &types_env,
        strings,
        &mut errors,
    );

    // Check exports against this module type to see everything exists and is valid
    for export in &module_ast.exports {
        match &export.value {
            ast::Export_::Identifier(identifier) => {
                let name = &identifier.value.name;
                match env.get(name) {
                    Some(poly_type) => module_definitions.insert(*name, Rc::clone(poly_type)),
                    None => errors.push(Error::UndefinedExport(identifier.to_owned())),
                }
            }
            ast::Export_::Type {
                name: name_ast,
                constructors,
            } => {
                let name = &name_ast.value.name;
                if let Some(typ) = types_env.get(name) {
                    module_types.insert(*name, Rc::clone(&typ.typ));

                    let mut constructor_types = PolyTypeEnv::new();
                    for constructor in constructors {
                        let name = &constructor.value.name;
                        match env.get(name) {
                            Some(poly_type) => {
                                constructor_types.insert(*name, Rc::clone(poly_type));
                            }
                            None => errors
                                .push(Error::UndefinedExportConstructor(constructor.to_owned())),
                        }
                    }
                    module_type_constructors.insert(*name, Rc::new(constructor_types));
                } else {
                    errors.push(Error::UnknownType(name_ast.to_owned()));
                }
            }
        }
    }

    let module_interface = ModuleInterface {
        types: Rc::new(module_types),
        type_constructors: module_type_constructors,
        definitions: Rc::new(module_definitions),
    };
    compiler_state.types[module_idx] = Some(module_interface);
    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

fn ast_type_to_type<'ast>(
    ast_type: &'ast ast::types::Type,
    type_vars: &TypeEnv,
    errors: &mut Vec<Error>,
    state: &mut State,
    types_env: &PolyTypeEnv,
) -> Result<Rc<Type>, Error> {
    match ast_type {
        ast::types::Type::Fun { params, ret } => {
            let mut params_types = vec![];
            for param in params {
                params_types.push(ast_type_to_type(
                    &param, type_vars, errors, state, types_env,
                )?);
            }
            let ret_type = ast_type_to_type(&ret, type_vars, errors, state, types_env)?;
            Ok(Rc::new(Fn {
                params: params_types,
                ret: ret_type,
            }))
        }

        ast::types::Type::App(typ) => match types_env.get(&typ.value.name.value.name) {
            Some(t) => {
                state.enter_level();
                let t = state.instantiate(t);
                state.exit_level();
                match &*t {
                    Named {
                        module: module_name,
                        ..
                    } => {
                        let mut params_types: Vec<Rc<Type>> = vec![];
                        for param in &typ.value.params {
                            params_types.push(ast_type_to_type(
                                param, type_vars, errors, state, types_env,
                            )?);
                        }
                        let ast_typ = Rc::new(Named {
                            module: *module_name,
                            name: typ.value.name.value.name,
                            params: params_types,
                        });

                        unify(state, typ, &ast_typ, None, &t)?;

                        Ok(ast_typ)
                    }
                    Alias {
                        module,
                        name,
                        params,
                        destination: dest_typ,
                    } => {
                        if params.len() != typ.value.params.len() {
                            return Err(Error::WrongArity {
                                location: typ.unit(),
                                num_params_applied: typ.value.params.len(),
                                num_params: params.len(),
                                typ: t,
                            });
                        }

                        let mut params_types = vec![];
                        for (param, (name, dest_param_typ)) in typ.value.params.iter().zip(params) {
                            let param_typ =
                                ast_type_to_type(param, type_vars, errors, state, types_env)?;
                            // TODO: param ast (`typ.unit()`) here should have node to point at it
                            // directly in the error message
                            unify(state, &typ.unit(), &param_typ, None, &dest_param_typ)?;
                            params_types.push((*name, param_typ));
                        }

                        Ok(Rc::new(Alias {
                            module: *module,
                            name: *name,
                            params: params_types,
                            destination: Rc::clone(dest_typ),
                        }))
                    }
                    _ => Err(Error::WrongArity {
                        location: typ.unit(),
                        num_params_applied: typ.value.params.len(),
                        num_params: 0,
                        typ: t,
                    }),
                }
            }
            None => Err(Error::UnknownType(typ.value.name.to_owned())),
        },

        ast::types::Type::Var(identifier) => match type_vars.get(&identifier.value.name) {
            Some(t) => Ok(Rc::clone(t)),
            None => Err(Error::UnknownTypeVar(identifier.to_owned())),
        },

        ast::types::Type::Record(record_type) => {
            ast_record_type_to_type(record_type, type_vars, errors, state, types_env)
        }
    }
}
fn ast_record_type_to_type<'ast>(
    record_type: &'ast ast::types::RecordType,
    type_vars: &TypeEnv,
    errors: &mut Vec<Error>,
    state: &mut State,
    types_env: &PolyTypeEnv,
) -> Result<Rc<Type>, Error> {
    match record_type {
        ast::types::RecordType::Record(record) => {
            let mut fields = TypeEnv::new();
            for (name, ast_type) in &record.value.fields {
                let typ = ast_type_to_type(ast_type, type_vars, errors, state, types_env)?;
                fields.insert(name.value.name, typ);
            }
            Ok(Rc::new(Record { fields }))
        }
        ast::types::RecordType::RecordExt(record_ext) => {
            let base_record = if let Some(t) = type_vars.get(&record_ext.value.extension.value.name)
            {
                Rc::clone(t)
            } else {
                return Err(Error::UnknownTypeVar(record_ext.value.extension.to_owned()));
            };

            let mut fields = TypeEnv::new();
            for (name, ast_type) in &record_ext.value.fields {
                let typ = ast_type_to_type(&ast_type, type_vars, errors, state, types_env)?;
                fields.insert(name.value.name, typ);
            }

            Ok(Rc::new(RecordExt {
                fields,
                base_record,
            }))
        }
    }
}

fn infer_rec<'ast>(
    ast: &'ast Expression,
    state: &mut State,
    env: &mut PolyTypeEnv,
    types_env: &PolyTypeEnv,
    strings: &mut Strings,
    errors: &mut Vec<Error>,
) -> Rc<Type> {
    let typ = match &ast.value.expr {
        ET::Unit => Rc::new(Unit),

        ET::Bool(_) => Rc::clone(&types_env.get(&strings.get_or_intern("Bool")).unwrap().typ),

        ET::Float(_) => Rc::clone(&types_env.get(&strings.get_or_intern("Float")).unwrap().typ),

        ET::String_(_) => Rc::clone(&types_env.get(&strings.get_or_intern("String")).unwrap().typ),

        ET::Record { fields } => {
            let mut typed_fields = TypeEnv::new();

            for (name, value) in fields {
                let t = infer_rec(value, state, env, types_env, strings, errors);
                if typed_fields.map().contains_key(&name.value.name) {
                    errors.push(Error::DuplicateField {
                        field: name.to_owned(),
                        expr: ast.to_owned(),
                    });
                } else {
                    typed_fields.insert(name.value.name, t);
                }
            }

            Rc::new(Record {
                fields: typed_fields,
            })
        }

        ET::RecordUpdate { record, fields } => {
            let mut typed_fields = TypeEnv::new();
            for (name, value) in fields {
                let t = infer_rec(value, state, env, types_env, strings, errors);
                if typed_fields.map().contains_key(&name.value.name) {
                    errors.push(Error::DuplicateField {
                        field: name.to_owned(),
                        expr: ast.to_owned(),
                    });
                } else {
                    typed_fields.insert(name.value.name, t);
                }
            }

            let inferred_type = Rc::new(RecordExt {
                fields: typed_fields,
                base_record: Rc::clone(&state.new_type_var()),
            });

            let record_type = infer_rec(record, state, env, types_env, strings, errors);

            // Unify the base record with the extensible record represented by this update
            add_error(
                unify(state, record, &record_type, None, &inferred_type),
                errors,
            );

            inferred_type
        }

        ET::PropertyAccess {
            expression,
            property,
        } => {
            let field_type = state.new_type_var();
            let remaining_fields_type = state.new_type_var();
            let record_type = Rc::new(RecordExt {
                fields: {
                    let mut fields = TypeEnv::new();
                    fields.insert(property.value.name, Rc::clone(&field_type));
                    fields
                },
                base_record: remaining_fields_type,
            });

            let expr_type = infer_rec(expression, state, env, types_env, strings, errors);

            add_error(
                unify(state, expression, &expr_type, None, &record_type),
                errors,
            );

            field_type
        }

        ET::PropertyAccessLambda { property } => {
            let field_type = state.new_type_var();
            let remaining_fields_type = state.new_type_var();

            Rc::new(Fn {
                params: vec![Rc::new(RecordExt {
                    fields: {
                        let mut fields = TypeEnv::new();
                        fields.insert(property.value.name, Rc::clone(&field_type));
                        fields
                    },
                    base_record: remaining_fields_type,
                })],
                ret: field_type,
            })
        }

        ET::Unary { op, expression } => {
            let t = infer_rec(expression, state, env, types_env, strings, errors);

            add_error(
                match op.value {
                    U::Not => unify(
                        state,
                        expression,
                        &t,
                        None,
                        &types_env.get(&strings.get_or_intern("Bool")).unwrap().typ,
                    ),
                    U::Minus => unify(
                        state,
                        expression,
                        &t,
                        None,
                        &types_env.get(&strings.get_or_intern("Bool")).unwrap().typ,
                    ),
                },
                errors,
            );

            t
        }

        ET::Binary {
            expression: function,
            arguments,
            ..
        } => {
            // Infer the binary op as a function call
            infer_fn_call(
                function,
                arguments.iter(),
                ast,
                state,
                env,
                types_env,
                strings,
                errors,
            )
        }

        /* If
         *
         * Condition must be boolean:
         *
         *     t0 = infer condition
         *     unify t0 bool
         *
         * then and else branches must have the same type:
         *
         *     t1 = infer then
         *     t2 = infer else
         *     unify t1 t2
         *
         * The final type is any of t1 or t2
         */
        ET::If {
            condition,
            then,
            else_,
        } => {
            let t = infer_rec(condition, state, env, types_env, strings, errors);
            add_error(
                unify(
                    state,
                    condition,
                    &t,
                    None,
                    &types_env.get(&strings.get_or_intern("Bool")).unwrap().typ,
                ),
                errors,
            );

            let t1 = infer_rec(then, state, env, types_env, strings, errors);
            let t2 = infer_rec(else_, state, env, types_env, strings, errors);
            add_error(unify(state, then, &t1, Some(else_), &t2), errors);

            t2
        }

        /*
         * Identifier
         *
         * Given x of type s in env:
         *
         *     t = inst s
         */
        ET::Identifier { module, identifier } => {
            let name = if let Some(module) = module {
                // TODO: Check the module if it was imported, would make for a better error message
                // TODO: This should be pre-computed in the identifier to avoid doing this with
                // each node in the AST.
                let full_name = {
                    let module = strings.resolve(module.full_name);
                    let name = strings.resolve(identifier.name());
                    format!("{module}.{name}")
                };
                strings.get_or_intern(full_name)
            } else {
                identifier.name()
            };

            match env.get(&name) {
                Some(s) => state.instantiate(s),
                None => {
                    add_error(
                        Err(Error::UndefinedIdentifier {
                            identifier: name,
                            location: ast.unit(),
                        }),
                        errors,
                    );
                    state.new_type_var()
                }
            }
        }

        /* FnCall
         *
         * Given `f ...args`
         *
         * Infer the type of the function and arguments
         *
         *     t0 = infer f
         *     targs = map infer args
         *
         * Unify the function type of targs -> t' with the inferred f type
         *
         *     t' = new_var ()
         *     unify t0 (targs -> t')
         */
        ET::FnCall {
            function,
            arguments,
        } => infer_fn_call(
            function,
            arguments.iter(),
            ast,
            state,
            env,
            types_env,
            strings,
            errors,
        ),

        /* Lambda
         *
         * Given (lambda ...params -> body)
         *
         * Collect the param types and add them to scope
         *
         *     tparams = []
         *     for param in params
         *         t = new_var ()
         *         add env param (generalize t)
         *         add tparams t
         *
         * Infer the type of the body with the new scope
         *
         *     t' = infer env body
         *
         *  The lambda type is then:
         *
         *     tparams -> t'
         */
        ET::Lambda(lambda) => infer_lambda(lambda, state, env, types_env, strings, errors),

        /* Let
         *
         * Given `let x = e0, x2 = e1, ... in body`
         *
         * Infer binding expressions and add them to scope
         *
         *     for (x, e) in bindings
         *         t = infer env e
         *         add env x (generalize t)
         *
         * Infer the body with the new env
         *
         *     infer env e1
         *
         * enter_level/exit_level optimizations are from
         *
         *     http://okmij.org/ftp/ML/generalization.html
         *
         * They're required so we don't generalize types that escape into
         * the environment.
         */
        ET::Let { definitions, body } => {
            let mut new_env = env.clone();

            infer_definitions(
                definitions,
                state,
                &mut new_env,
                &types_env,
                strings,
                errors,
            );

            infer_rec(body, state, &mut new_env, types_env, strings, errors)
        }
    };

    ast.value.set_type(Rc::clone(&typ));

    typ
}

fn infer_definitions<'ast>(
    definitions: &'ast [TypedDefinition],
    state: &mut State,
    env: &mut PolyTypeEnv,
    types_env: &PolyTypeEnv,
    strings: &mut Strings,
    errors: &mut Vec<Error>,
) {
    for typed_definition in definitions {
        if let Some(definition) = typed_definition.definition() {
            match &definition {
                Definition::Lambda(identifier, lambda) => {
                    state.enter_level();
                    let t = infer_lambda(lambda, state, env, types_env, strings, errors);
                    state.exit_level();

                    let t = check_signature(&typed_definition, &t, state, types_env, errors);

                    let t = state.generalize(&t);
                    env.insert(identifier.value.name, t);
                }
                Definition::Pattern(pattern, value) => {
                    state.enter_level();
                    let t = infer_rec(value, state, env, types_env, strings, errors);
                    state.exit_level();

                    let t = check_signature(&typed_definition, &t, state, types_env, errors);

                    let t = state.generalize(&t);
                    match &pattern.value {
                        P::Hole => (),
                        P::Identifier(x) => env.insert(x.value.name, t),
                    }
                }
            }
        } else if let Some(typ) = typed_definition.typ() {
            let t = match type_from_signature(typ, state, types_env, errors) {
                Ok(t) => t,
                Err(err) => {
                    errors.push(err);
                    state.enter_level();
                    let t = state.new_type_var();
                    state.exit_level();
                    t
                }
            };
            let t = state.generalize(&t);
            env.insert(typ.name.value.name, t);
        }
    }
}

fn type_from_signature<'ast>(
    signature: &'ast TypeSignature,
    state: &mut State,
    types_env: &PolyTypeEnv,
    errors: &mut Vec<Error>,
) -> Result<Rc<Type>, Error> {
    let mut type_vars = TypeEnv::new();
    state.enter_level();
    for var in signature.typ.vars() {
        type_vars.insert(var.value.name, state.new_type_var());
    }
    let signature_type_result =
        ast_type_to_type(&signature.typ, &type_vars, errors, state, types_env);
    state.exit_level();
    signature_type_result
}

fn check_signature<'ast>(
    typed_definition: &'ast TypedDefinition,
    typ: &Rc<Type>,
    state: &mut State,
    types_env: &PolyTypeEnv,
    errors: &mut Vec<Error>,
) -> Rc<Type> {
    if let Some(signature) = typed_definition.typ() {
        let signature_type = match type_from_signature(signature, state, types_env, errors) {
            Ok(t) => t,
            Err(err) => {
                errors.push(err);
                return Rc::clone(typ);
            }
        };

        // TODO: This is kind of nasty
        // Clone the signature to avoid unifying the signature_type which we use to check if the
        // type is to general below
        let signature_type_to_unify = state.generalize(&signature_type);
        state.enter_level();
        let signature_type_to_unify = state.instantiate(&signature_type_to_unify);
        state.exit_level();

        if let Err(e) = unify(state, &signature.name, &typ, None, &signature_type_to_unify) {
            errors.push(match e {
                Error::TypeMismatch {
                    actual_type_location,
                    ..
                } => Error::SignatureMismatch {
                    signature_type_location: actual_type_location,
                    signature_type: Rc::clone(&signature_type_to_unify),
                    inferred_type: Rc::clone(typ),
                },
                _ => e,
            });
            Rc::clone(typ)
        } else {
            if signature_is_too_generic(&signature_type, &typ) {
                errors.push(Error::SignatureTooGeneral {
                    signature_type_location: signature.name.unit(),
                    signature_type: Rc::clone(&signature_type),
                    inferred_type: Rc::clone(&typ),
                });
            }
            signature_type_to_unify
        }
    } else {
        Rc::clone(typ)
    }
}

fn infer_lambda<'ast>(
    lambda: &'ast Lambda,
    state: &mut State,
    env: &mut PolyTypeEnv,
    types_env: &PolyTypeEnv,
    strings: &mut Strings,
    errors: &mut Vec<Error>,
) -> Rc<Type> {
    let params_with_type: Vec<(&ast::Pattern, Rc<PolyType>)> = lambda
        .parameters
        .iter()
        .map(|p| {
            (
                p,
                Rc::new(PolyType::new(IndexSet::new(), state.new_type_var())),
            )
        })
        .collect();

    let mut env = params_with_type
        .iter()
        .fold(env.clone(), |mut env, (param, param_type)| {
            match &(**param).value {
                P::Hole => (),
                P::Identifier(x) => env.insert(x.value.name, Rc::clone(param_type)),
            };
            env
        });

    let return_type = infer_rec(&lambda.body, state, &mut env, types_env, strings, errors);

    Rc::new(Fn {
        params: params_with_type
            .iter()
            .map(|(_, param_type)| Rc::clone(&param_type.typ))
            .collect(),
        ret: return_type,
    })
}

fn infer_fn_call<'ast, Args>(
    f: &'ast Expression,
    args: Args,
    ast: &'ast Expression,
    state: &mut State,
    env: &mut PolyTypeEnv,
    types_env: &PolyTypeEnv,
    strings: &mut Strings,
    errors: &mut Vec<Error>,
) -> Rc<Type>
where
    Args: Iterator<Item = &'ast Expression> + Clone,
{
    let fn_type = infer_rec(f, state, env, types_env, strings, errors);
    let param_types = fn_type.parameters();

    let arg_types: Vec<Rc<Type>> = args
        .clone()
        .map(|arg| infer_rec(arg, state, env, types_env, strings, errors))
        .collect();

    let return_type = state.new_type_var();

    let call_type: Rc<Type> = Rc::new(Fn {
        params: arg_types.clone(),
        ret: Rc::clone(&return_type),
    });

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

fn add_error<'ast>(result: Result<(), Error>, errors: &mut Vec<Error>) {
    if let Err(e) = result {
        errors.push(e);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser;
    use crate::tokenizer;

    pub fn infer_expression<'ast>(
        ast: &'ast Expression,
        state: &mut State,
        env: &mut PolyTypeEnv,
        types_env: &PolyTypeEnv,
        strings: &mut Strings,
    ) -> Result<Rc<Type>, Vec<Error>> {
        let mut errors: Vec<Error> = vec![];
        let t = infer_rec(ast, state, env, types_env, strings, &mut errors);

        if errors.is_empty() {
            Ok(t)
        } else {
            Err(errors)
        }
    }

    mod test_infer_expression {
        use super::*;
        use insta::assert_snapshot;

        #[test]
        fn test_lambdas_and_application() {
            assert_snapshot!(infer(r"\f -> \x -> f x"));
            assert_snapshot!(infer(r"\f -> \x -> f (f x)"));
            // (+):
            assert_snapshot!(infer(r"\m -> \n -> \f -> \x -> m f (n f x)"));
            // succ:
            assert_snapshot!(infer(r"\n -> \f -> \x -> f (n f x)"));
            // mult:
            assert_snapshot!(infer(r"\m -> \n -> \f -> \x -> m (n f) x"));
            // pred:
            assert_snapshot!(infer(
                r"\n -> \f -> \x -> n (\g -> \h -> h (g f)) (\u -> x) (\u -> u)"
            ));
        }

        #[test]
        fn test_let_generalization() {
            assert_snapshot!(infer(
                r"
      \x ->
        let y = x
        y
    "
            ));
            assert_snapshot!(infer(
                r"
      \x ->
        let y = \z -> x
        y
      "
            ));
        }

        #[test]
        fn test_if() {
            assert_snapshot!(infer("if 1 == 1 then True else False"));
            assert_snapshot!(infer("if 1 == 1 then 1 else 2"));
            assert_snapshot!(infer(
                "if 1 == 1 then if 1 + 1 > 2 then 5 else 1 / 1 else 2 + 2"
            ));
            assert_snapshot!("bad condition type", infer("if 1 then 5 else 1"));
        }

        #[test]
        fn test_let() {
            assert_snapshot!(
                "function binding",
                infer(
                    r"
let incr = \n -> n + 1

incr True"
                )
            );
            assert_snapshot!(infer(
                r"
\x ->
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
        }

        #[test]
        fn test_infinite_type() {
            assert_snapshot!(infer(r"\a -> a 1 a"));
        }

        #[test]
        fn test_record_literal() {
            assert_snapshot!(infer(r"{}"));
            assert_snapshot!(infer(r"{ x = 1 }"));
            assert_snapshot!(infer(r"{ x = 1, y = True }"));
            assert_snapshot!(infer("{ x = { x = 5 } }"));
        }

        #[test]
        fn test_property_access() {
            assert_snapshot!(infer(r#"{ age = 1, msg = "Hello" }.age"#));
            assert_snapshot!(infer(r#"{ age = 1, msg = "Hello" }.msg"#));
            assert_snapshot!(infer(r#"{ age = 1, msg = "Hello" }.wat"#));
            assert_snapshot!(infer(r#"let a = "Hi" in a.wat"#));
            assert_snapshot!(infer(r"\r -> r.x + r.y"));
        }

        #[test]
        fn test_property_access_shorthand_lambda() {
            assert_snapshot!(infer(".name"));
            assert_snapshot!(infer(".name { name = 1 }"));
            assert_snapshot!(infer(".name { age = 1 }"));
        }

        #[test]
        fn test_record_update() {
            assert_snapshot!(infer("{ {} | age = 1 }"));
            assert_snapshot!(infer("{ { age = 5 } | age = 1 }"));
            assert_snapshot!(infer("{ { age = 5 } | age = \"Hi\" }"));
            assert_snapshot!(infer(r"\thing -> { thing | age = 5 }"));
            assert_snapshot!(infer(
                r#"(\thing -> { thing | age = 5 }) { name = "Joe", age = 1 }"#
            ));
        }

        #[test]
        fn test_duplicate_fields_in_records() {
            assert_snapshot!(infer("{ age = 1, age = 1 }"));
            assert_snapshot!(infer("{ name = 1, age = 1, name = 1 }"));
            assert_snapshot!(infer("{ { name = 1, age = 1 } | name = 2, name = 3 }"));
        }

        fn infer(code: &str) -> String {
            let mut strings = Strings::new();
            let source = Source::new_orphan(code.to_string());
            let mut tokens = Tokens::new();

            tokenizer::parse(&source, &mut strings, &mut tokens)
                .map_err(|errors| {
                    errors
                        .iter()
                        .map(|e| e.to_string(&source))
                        .collect::<Vec<String>>()
                        .join("\n\n")
                })
                .unwrap();

            let ast = parser::tests::parse_expression(&source, &tokens, &mut strings)
                .map_err(|error| error.to_string(&source, &strings))
                .unwrap();

            let mut state = State::new();
            let mut env = PolyTypeEnv::new();
            let primitive_types = Type::primitive_types(&mut strings);
            base_env(&mut state, &mut env, &primitive_types, &mut strings);
            let result = match infer_expression(
                &ast,
                &mut state,
                &mut env,
                &primitive_types,
                &mut strings,
            ) {
                Ok(typ) => typ.to_string(&strings),
                Err(errs) => errs
                    .iter()
                    .map(|e| e.to_string(&source, &strings, &tokens))
                    .collect::<Vec<String>>()
                    .join("\n\n"),
            };

            format!("Input:\n\n{code}\n\n---\nOutput:\n\n{result}")
        }
    }

    mod test_infer_module {
        use super::*;
        use crate::compiler;
        use insta::assert_snapshot;

        #[test]
        fn test_module_definitions() {
            assert_snapshot!(infer(
                "
module Test

a = 1

b = 2
           "
            ));
        }

        #[test]
        fn test_exposing() {
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
        }

        #[test]
        fn test_exposing_submodules() {
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
        }

        #[test]
        fn test_importing_from_submodule() {
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
        }

        #[test]
        fn test_import_nonexistent_module() {
            assert_snapshot!(infer(
                r#"
module Parent

import Nope exposing (test)

test = 1
"#
            ));
        }

        #[test]
        fn test_exposing_lambdas() {
            assert_snapshot!(infer(
                "\
module Test exposing (main, add)

add x y = x + y

main = add 5
"
            ));
        }

        #[test]
        fn test_exposing_lambdas_with_patterns_in_params() {
            assert_snapshot!(infer(
                "module Test exposing (last)

last _ y = y
"
            ));
        }

        #[test]
        fn test_import_fully_qualified_name() {
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

import User.Id

new = User.Id

module User.Id exposing (new)
    new = 42
"
            ));
        }

        #[test]
        fn test_import_alias() {
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
        }

        #[test]
        fn test_import_nested_fully_qualified_name() {
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
        }

        #[test]
        fn test_use_nonexistent_import_from_fully_qualified_module() {
            assert_snapshot!(infer(
                "\
module User exposing (new)

import User.Id

new = User.Id.wat

module User.Id exposing (new)
    new = 42
"
            ));
        }

        #[test]
        fn test_exposing_type() {
            assert_snapshot!(infer(
                "\
module Test exposing (Fruit)

type Fruit = Banana
"
            ));
            assert_snapshot!(infer(
                "\
module Test exposing (Fruit)

type Fruit = Banana a
"
            ));
            assert_snapshot!(infer(
                "\
module Test exposing (Fruita)

type Fruit = Banana
"
            ));
            assert_snapshot!(infer(
                "\
module Test exposing (Fruit)

type Fruit a = Banana a
"
            ));
            assert_snapshot!(infer(
                "\
module Test exposing (Fruit)

type Fruit a b = Banana a
"
            ));
        }

        #[test]
        fn test_using_union_type() {
            assert_snapshot!(infer(
                "\
module Test exposing (main)

type Fruit = Banana

main = Banana
"
            ));
            assert_snapshot!(infer(
                "\
module Test exposing (main)

type Banana = Banana

type Fruit a = Fruit a

main = Fruit
"
            ));
            assert_snapshot!(infer(
                "\
module Test exposing (main)

type Banana = Banana

type Fruit a = Fruit a

main = Fruit Banana
"
            ));
        }

        #[test]
        fn test_exposing_union_type_constructor() {
            assert_snapshot!(infer(
                "\
module Test exposing (Banana)

type Fruit = Banana
"
            ));
            assert_snapshot!(infer(
                "\
module Test exposing (Fruit(Banana))

type Fruit = Banana
"
            ));
            assert_snapshot!(infer(
                "\
module Test exposing (main)

import Test.Fruits

main = Test.Fruits.test

module Test.Fruits exposing (test, Fruit(Banana))
    type Fruit = Banana

    test = 1
"
            ));
        }

        #[test]
        fn test_import_union_type_constructor() {
            assert_snapshot!(infer(
                "\
module Test exposing (main)

import Test.Fruits exposing (Fruit(Banana))

main = Banana

module Test.Fruits exposing (Fruit(Banana))
    type Fruit = Banana
"
            ));
            assert_snapshot!(infer(
                "\
module Test exposing (main)

import Test.Fruits exposing (Banana)

main = Banana

module Test.Fruits exposing (Fruit(Banana))
    type Fruit = Banana
"
            ));
        }

        #[test]
        fn test_use_fully_qualified_union_type_constructor() {
            assert_snapshot!(infer(
                "\
module Test exposing (main)

import Test.Fruits exposing (Fruit)

main = Test.Fruits.Banana

module Test.Fruits exposing (Fruit(Banana))
    type Fruit = Banana
"
            ));
        }

        #[test]
        fn test_use_union_type_constructor_from_module_alias() {
            assert_snapshot!(infer(
                "\
module Test exposing (main)

import Test.Fruits as Fruits

main = Fruits.Banana

module Test.Fruits exposing (Fruit(Banana))
    type Fruit = Banana
"
            ));
            assert_snapshot!(infer(
                "\
module Test exposing (main)

import Test.Fruits as Fruits

main = Fruits.Banana

module Test.Fruits exposing (Fruit)
    type Fruit = Banana
"
            ));
        }

        #[test]
        fn test_constructing_a_union_type() {
            assert_snapshot!(infer(
                "\
module Test exposing (main)

type Fruit a = Banana (a -> a -> a)

main = Banana 1
"
            ));
            assert_snapshot!(infer(
                "\
module Test exposing (main)

type Fruit a = Banana (a -> a -> a)

main = Banana (\\x y -> x + y)
"
            ));
        }

        #[test]
        fn test_constructing_a_union_type_with_records() {
            assert_snapshot!(infer(
                "\
module Test exposing (main)

type Math num =
    { add : num -> num -> num
    , sub : num -> num -> num
    }

type M num = M (Math num)

main = M { add: \\x y -> x + y, sub: \\x y -> x - y }
"
            ));
            assert_snapshot!(infer(
                "\
module Test exposing (main)

type Math num =
    { add : num -> num -> num
    , sub : num -> num -> num
    }

type M num = M (Math num)

main = M { add: \\x y -> x + y, sub: \\x -> x }
"
            ));
        }

        #[test]
        fn test_typed_definition() {
            assert_snapshot!(infer(
                "\
module Test exposing (main)

main : a -> a
main a = a
"
            ));
            assert_snapshot!(infer(
                "\
module Test exposing (main)

main : Float
main = 5
"
            ));
        }

        #[test]
        fn test_typed_definition_with_mismatched_signature() {
            assert_snapshot!(infer(
                "\
module Test exposing (main)

main : String
main = 5
"
            ));
        }

        #[test]
        fn test_signature_too_generic() {
            assert_snapshot!(infer(
                "\
module Test exposing (main)

main : a
main = 5
"
            ));
            assert_snapshot!(infer(
                "\
module Test exposing (main)

type List a = List a

main : List a
main = List 1
"
            ));
        }

        #[test]
        fn test_function_signature() {
            assert_snapshot!(infer(
                "\
module Test exposing (main)

type List a = List a

main : Float -> List Float
main a = List a
"
            ));
        }

        #[test]
        fn test_union_type_application_arity_mismatch() {
            assert_snapshot!(infer(
                "\
module Test exposing (main)

type List a = List a

main : List a Float
main = List 1
"
            ));
        }

        #[test]
        fn test_record_type_application() {
            assert_snapshot!(infer(
                "\
module Test exposing (main)

type List a = { x : a }

main : List Float
main = { x : 1 }
"
            ));
        }

        #[test]
        fn test_record_type_application_arity_mismatch() {
            assert_snapshot!(infer(
                "\
module Test exposing (main)

type List a = { x : a }

main : List
main = { x : 1 }
"
            ));
            assert_snapshot!(infer(
                "\
module Test exposing (main)

type List a = { x : a }

main : List Float String
main = { x : 1 }
"
            ));
        }

        #[test]
        fn test_exposing_and_importing_a_record_type() {
            assert_snapshot!(infer(
                "\
module Test exposing (main)

import Test.Record exposing (Record)

module Test.Record exposing (Record)
    type Record a = { x : a }

main : Record Float
main = { x : 5 }
"
            ));
        }

        #[test]
        fn test_signature_records_too_generic_check() {
            assert_snapshot!(infer(
                "\
module Test exposing (main)

main : { x : a } -> Float
main r = r.x + 1
"
            ));
        }

        #[test]
        fn test_signature_extensible_records_too_generic_check() {
            assert_snapshot!(infer(
                "\
module Test exposing (main)

main : { r | x : a, y: a } -> Float
main r = r.x + r.y + 1
"
            ));
        }

        fn infer(code: &str) -> String {
            let mut state = compiler::State::new();
            let source = Source::new_orphan(code.to_string());

            let primitive_types = Type::primitive_types(&mut state.strings);

            let entry_sources = compiler::stages::process_sources(vec![source], &mut state);

            let entry_modules = compiler::stages::parse_files(&entry_sources, &mut state).unwrap();

            let actual = match compiler::stages::infer(entry_modules, &mut state, &primitive_types)
            {
                Ok(()) => {
                    let mut out = String::new();
                    state.types_to_string(&mut out);
                    out
                }
                Err(err) => err,
            };

            format!("Input:\n\n{code}\n\n---\nOutput:\n\n{actual}")
        }
    }
}
