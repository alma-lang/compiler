use crate::ast::{
    self,
    expression::{
        self, AnyIdentifier, CapitalizedIdentifier, Expression as E, ExpressionSpans,
        ExpressionTypes, ExpressionValues, Identifier, IdentifierName, Lambda, Pattern,
        PatternData as P, UnaryData as U,
    },
    span::Span,
    Definition, Import, ModuleName, TypeSignature, TypedDefinition,
};
use crate::compiler;
use crate::compiler::state::ModuleIndex;
use crate::module_interface::{HashMap, ModuleInterface};
use crate::source::Source;
use crate::strings::Strings;
use crate::token::Tokens;
use crate::typ::{self, Type::*, TypeVar::*, *};
use crate::type_env::{PolyTypeEnv, TypeEnv};
use fnv::FnvHashSet;
use indexmap::IndexSet;
use std::cmp::min;
use std::fmt::Write;
use std::rc::Rc;

/*
 * Resources:
 *   - https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Algorithm_J
 *   - http://okmij.org/ftp/ML/generalization.html
 *   - https://github.com/jfecher/algorithm-j
 */

#[derive(Debug)]
pub enum Error {
    UndefinedIdentifier {
        identifier: IdentifierName,
        location: Span,
    },
    DuplicateField {
        field: Identifier,
        expr: expression::Index,
    },
    TypeMismatch {
        actual_type_location: Span,
        actual_type: typ::Index,
        expected_type_location: Option<Span>,
        expected_type: typ::Index,
    },
    WrongArity {
        location: Span,
        num_params_applied: usize,
        num_params: usize,
        typ: typ::Index,
    },
    SignatureMismatch {
        signature_type_location: Span,
        signature_type: typ::Index,
        inferred_type: typ::Index,
    },
    SignatureTooGeneral {
        signature_type_location: Span,
        signature_type: typ::Index,
        inferred_type: typ::Index,
    },
    InfiniteType {
        location: Span,
        typ: typ::Index,
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
    PatternsIntroduceDifferentBindings {
        names1: FnvHashSet<IdentifierName>,
        span1: Span,
        names2: FnvHashSet<IdentifierName>,
        span2: Span,
    },
}

impl Error {
    pub fn to_string(
        &self,
        source: &Source,
        strings: &Strings,
        tokens: &Tokens,
        types: &Types,
        expression_spans: &ExpressionSpans,
    ) -> String {
        use Error::*;

        let mut s = String::new();

        let (position, line_number, column, end) = {
            let location = match self {
                UndefinedIdentifier { location, .. } => location,
                DuplicateField { field, .. } => &field.span,
                TypeMismatch {
                    actual_type_location,
                    ..
                } => actual_type_location,
                WrongArity { location, .. } => location,
                SignatureMismatch {
                    signature_type_location,
                    ..
                } => signature_type_location,
                SignatureTooGeneral {
                    signature_type_location,
                    ..
                } => signature_type_location,
                InfiniteType { location, .. } => location,
                UndefinedExport(export) => &export.span,
                UndefinedExportConstructor(constructor) => &constructor.span,
                UnknownImport(import) => &import.span,
                UnknownImportDefinition { export, .. } => &export.span,
                UnknownImportConstructor { constructor, .. } => &constructor.span,
                UnknownType(name) => &name.span,
                UnknownTypeVar(var) => &var.span,
                PatternsIntroduceDifferentBindings { span1, .. } => span1,
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
                write!(
                    s,
                    "Undefined identifier `{identifier}`\n\n{code}",
                    identifier = strings.resolve(*identifier),
                )
                .unwrap();
            }

            DuplicateField { field, expr } => {
                let expr_token = tokens[expression_spans[*expr].start];
                let expr_end_token = tokens[expression_spans[*expr].end];
                let record_code = source
                    .lines_report_at_position(
                        expr_token.start,
                        Some(expr_end_token.end),
                        expr_token.line,
                        false,
                    )
                    .unwrap();
                let identifier = field.to_string(strings);

                write!(
                    s,
                    "Duplicate field `{identifier}`

{code}

in record

{record_code}",
                )
                .unwrap();
            }

            InfiniteType { .. } => {
                write!(s, "Infinite type\n\n{code}").unwrap();
            }

            TypeMismatch {
                actual_type,
                expected_type_location: None,
                expected_type: typ2,
                ..
            } => {
                let typ = types[*actual_type].to_string(strings, types);
                let typ2 = types[*typ2].to_string(strings, types);
                write!(
                    s,
                    "Type mismatch:  {typ}  ???  {typ2}

Expected

{code}

to be

    {typ2}

but seems to be

    {typ}",
                )
                .unwrap();
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
                let actual_type = types[*actual_type].to_string(strings, types);
                let expected_type = types[*expected_type].to_string(strings, types);

                write!(
                    s,
                    "Type mismatch:  {actual_type}  ???  {expected_type}

Expected

{code}

with type

    {actual_type}

to have the same type as

{code2}

with type

    {expected_type}",
                )
                .unwrap();
            }

            WrongArity {
                num_params_applied,
                num_params,
                typ,
                ..
            } => {
                let plural = if *num_params == 1 { "" } else { "s" };
                let typ = types[*typ].to_string(strings, types);
                write!(
                    s,
                    "Type `{typ}` accepts {num_params} parameter{plural} \
                    but it was called with {num_params_applied}.

{code}",
                )
                .unwrap();
            }

            SignatureMismatch {
                signature_type,
                inferred_type,
                ..
            } => {
                let signature_type = types[*signature_type].to_string(strings, types);
                let inferred_type = types[*inferred_type].to_string(strings, types);
                write!(
                    s,
                    "The type signature and inferred type don't match

{code}

The type signature says the type is

    {signature_type}

but it seems to be

    {inferred_type}",
                )
                .unwrap();
            }

            SignatureTooGeneral {
                signature_type,
                inferred_type,
                ..
            } => {
                let signature_type = types[*signature_type].to_string(strings, types);
                let inferred_type = types[*inferred_type].to_string(strings, types);
                write!(
                    s,
                    "The type signature is more generic than the inferred type:

{code}

The type signature says the type is

    {signature_type}

which it is more general than

    {inferred_type}

which was inferred from the code.

Change the signature to be more specific or try to make your code more generic.",
                )
                .unwrap();
            }

            UndefinedExport(export) => {
                let export = export.to_string(strings);
                write!(s, "Undefined identifier `{export}`\n\n{code}").unwrap();
            }

            UndefinedExportConstructor(constructor) => {
                let constructor = constructor.to_string(strings);
                write!(s, "Undefined identifier `{constructor}`\n\n{code}").unwrap();
            }

            UnknownImport(import) => {
                let module = import.module_name.to_string(strings);
                write!(s, "Couldn't find module `{module}`\n\n{code}").unwrap();
            }

            UnknownImportDefinition { export, import } => {
                let module = import.module_name.to_string(strings);
                let export = export.to_string(strings);
                write!(
                    s,
                    "Module `{module}` doesn't appear to expose `{export}`\n\n{code}"
                )
                .unwrap();
            }

            UnknownImportConstructor {
                constructor,
                import,
            } => {
                let module = import.module_name.to_string(strings);
                let export = constructor.to_string(strings);
                write!(
                    s,
                    "Module `{module}` doesn't appear to expose `{export}`\n\n{code}"
                )
                .unwrap();
            }

            UnknownType(name) => {
                let type_name = name.to_string(strings);
                write!(s, "Couldn't find type `{type_name}`\n\n{code}").unwrap();
            }

            UnknownTypeVar(name) => {
                let type_name = name.to_string(strings);
                write!(
                    s,
                    "Type variable `{type_name}` has not been declared\n\n{code}"
                )
                .unwrap();
            }

            PatternsIntroduceDifferentBindings {
                names1,
                names2,
                span2,
                ..
            } => {
                let node_token = tokens[span2.start];
                let node_end_token = tokens[span2.end];
                let code2 = source
                    .lines_report_at_position_with_pointer(
                        node_token.start,
                        Some(node_end_token.end),
                        node_token.line,
                    )
                    .unwrap();
                let names1 = names1
                    .iter()
                    .map(|n| strings.resolve(*n))
                    .collect::<Vec<&str>>()
                    .join(", ");
                let names2 = names2
                    .iter()
                    .map(|n| strings.resolve(*n))
                    .collect::<Vec<&str>>()
                    .join(", ");
                write!(
                    s,
                    "Patterns introduce different bindings:

Expected

{code}

which introduces

    {names1}

to introduce the same bindings as

{code2}

which introduces

    {names2}",
                )
                .unwrap();
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

    fn new_type_var(&mut self) -> Type {
        Type::Var(TypeVar::Unbound(self.new_var_id(), self.current_level))
    }

    /**
     * Specializes the PolyType s by copying the term and replacing the
     * bound type variables consistently by new monomorphic type variables
     *
     * E.g. instantiate (forall a b. a -> b -> a) = c -> d -> c
     */
    fn instantiate(&mut self, t: &PolyType, types: &mut Types) -> typ::Index {
        /* Replace any type vars found in the hashtable with the
         * associated value in the same table, leave them otherwise
         */
        fn replace_type_vars(
            vars_to_replace: &HashMap<TypeVarId, typ::Index>,
            t: typ::Index,
            types: &mut Types,
        ) -> typ::Index {
            match &types[t] {
                Named {
                    module,
                    name,
                    params,
                } => {
                    let module = *module;
                    let name = *name;
                    let mut new_params = vec![];
                    for param in params.clone().iter() {
                        new_params.push(replace_type_vars(vars_to_replace, *param, types));
                    }

                    types.push_and_get_key(Named {
                        module,
                        name,
                        params: Rc::new(new_params),
                    })
                }

                Var(var) => match var {
                    Bound(t) => replace_type_vars(vars_to_replace, *t, types),
                    Unbound(n, _level) => match vars_to_replace.get(n) {
                        Some(t_) => *t_,
                        None => t,
                    },
                },

                Fn { params, ret } => {
                    let ret = *ret;
                    let params = {
                        let mut new_args = vec![];
                        for param in params.clone().iter() {
                            new_args.push(replace_type_vars(vars_to_replace, *param, types));
                        }
                        Rc::new(new_args)
                    };
                    let ret = replace_type_vars(vars_to_replace, ret, types);
                    types.push_and_get_key(Fn { params, ret })
                }

                Record { fields } => {
                    let mut new_fields = (**fields).clone();
                    for (key, value) in fields.clone().map() {
                        new_fields.insert(*key, replace_type_vars(vars_to_replace, *value, types));
                    }
                    types.push_and_get_key(Record {
                        fields: Rc::new(new_fields),
                    })
                }

                RecordExt {
                    fields,
                    base_record,
                } => {
                    let base_record = *base_record;
                    let mut new_fields = (**fields).clone();
                    for (key, value) in fields.clone().map() {
                        new_fields.insert(*key, replace_type_vars(vars_to_replace, *value, types));
                    }
                    let base_record = replace_type_vars(vars_to_replace, base_record, types);
                    types.push_and_get_key(RecordExt {
                        fields: Rc::new(new_fields),
                        base_record,
                    })
                }

                Alias {
                    module,
                    name,
                    params,
                    destination,
                } => {
                    let module = *module;
                    let name = *name;
                    let destination = *destination;
                    let params = {
                        let mut new_params = Vec::new();
                        for (name, param) in params.clone().iter() {
                            new_params
                                .push((*name, replace_type_vars(vars_to_replace, *param, types)));
                        }
                        Rc::new(new_params)
                    };
                    let destination = replace_type_vars(vars_to_replace, destination, types);
                    types.push_and_get_key(Alias {
                        module,
                        name,
                        params,
                        destination,
                    })
                }
            }
        }

        let mut vars_to_replace: HashMap<TypeVarId, typ::Index> = HashMap::default();
        for var in t.vars.iter() {
            vars_to_replace.insert(*var, types.push_and_get_key(self.new_type_var()));
        }
        replace_type_vars(&vars_to_replace, t.typ, types)
    }

    /* Find all type variables and wrap the type in a PolyType
     * e.g. generalize (a -> b -> b) = forall a b. a -> b -> b
     */
    fn generalize(&self, t: typ::Index, types: &Types) -> PolyType {
        let current_level = self.current_level;

        fn find_all_tvs(
            current_level: &Level,
            vars: &mut IndexSet<TypeVarId>,
            t: &Type,
            types: &Types,
        ) {
            match t {
                Named { params, .. } => {
                    for param in params.iter() {
                        find_all_tvs(current_level, vars, &types[*param], types);
                    }
                }

                Var(var) => match var {
                    Bound(t) => find_all_tvs(current_level, vars, &types[*t], types),
                    Unbound(n, level) => {
                        if level > current_level {
                            vars.insert(*n);
                        }
                    }
                },

                Fn { params, ret } => {
                    for param in &**params {
                        find_all_tvs(current_level, vars, &types[*param], types);
                    }
                    find_all_tvs(current_level, vars, &types[*ret], types);
                }

                Record { fields } => {
                    for t in fields.map().values() {
                        find_all_tvs(current_level, vars, &types[*t], types);
                    }
                }

                RecordExt {
                    fields,
                    base_record,
                } => {
                    for t in fields.map().values() {
                        find_all_tvs(current_level, vars, &types[*t], types);
                    }
                    find_all_tvs(current_level, vars, &types[*base_record], types);
                }

                Alias {
                    params,
                    destination,
                    ..
                } => {
                    for (_name, param) in params.iter() {
                        find_all_tvs(current_level, vars, &types[*param], types);
                    }
                    find_all_tvs(current_level, vars, &types[*destination], types)
                }
            }
        }

        let mut tvs = IndexSet::new();
        find_all_tvs(&current_level, &mut tvs, &types[t], types);
        PolyType::new(tvs, t)
    }
}

/**
 * Can a monomorphic Var(a) be found inside this type?
 */
fn occurs(a_id: TypeVarId, a_level: Level, t: typ::Index, types: &mut Types) -> bool {
    match &types[t] {
        Named { params, .. } => params
            .clone()
            .iter()
            .any(|param| occurs(a_id, a_level, *param, types)),

        Var(var) => match var {
            Bound(t) => occurs(a_id, a_level, *t, types),

            Unbound(b_id, b_level) => {
                let (b_id, b_level) = (*b_id, *b_level);

                let min_level = min(a_level, b_level);

                types[t] = Var(Unbound(b_id, min_level));

                a_id == b_id
            }
        },

        Fn { params, ret } => {
            let ret = *ret;
            params
                .clone()
                .iter()
                .any(|param| occurs(a_id, a_level, *param, types))
                || occurs(a_id, a_level, ret, types)
        }

        Record { fields } => fields
            .clone()
            .map()
            .values()
            .any(|tv| occurs(a_id, a_level, *tv, types)),

        RecordExt {
            fields,
            base_record,
        } => {
            let base_record = *base_record;
            let fields_occur = fields
                .clone()
                .map()
                .values()
                .any(|tv| occurs(a_id, a_level, *tv, types));
            if fields_occur {
                true
            } else {
                occurs(a_id, a_level, base_record, types)
            }
        }

        Alias { destination, .. } => occurs(a_id, a_level, *destination, types),
    }
}

pub fn find(typ: typ::Index, types: &Types) -> typ::Index {
    match &types[typ] {
        Var(var) => match var {
            Bound(t) => find(*t, types),
            Unbound(_, _) => typ,
        },
        _ => typ,
    }
}

fn flatten_record(typ: typ::Index, types: &mut Types) -> Option<typ::Index> {
    let typ = find(typ, types);
    if let RecordExt { .. } = &types[typ] {
        let mut all_fields = TypeEnv::new();
        let mut typ = typ;

        loop {
            match &types[typ] {
                Record { fields } => {
                    for (k, v) in fields.map() {
                        all_fields.insert(*k, *v);
                    }
                    return Some(types.push_and_get_key(Record {
                        fields: Rc::new(all_fields),
                    }));
                }
                RecordExt {
                    fields,
                    base_record,
                } => {
                    for (k, v) in fields.map() {
                        all_fields.insert(*k, *v);
                    }
                    typ = find(*base_record, types);
                }
                Var(var) => match var {
                    Unbound(_, _) => {
                        return Some(types.push_and_get_key(RecordExt {
                            fields: Rc::new(all_fields),
                            base_record: typ,
                        }))
                    }
                    Bound(_) => unreachable!(),
                },
                _ => return None,
            }
        }
    } else {
        None
    }
}

fn unify(
    state: &mut State,
    span: Span,
    typ: typ::Index,
    span2: Option<Span>,
    typ2: typ::Index,
    types: &mut Types,
) -> Result<(), Error> {
    unify_rec(state, typ, typ2, span, typ, span2, typ2, types)
}

fn unify_var(
    t: typ::Index,
    id: TypeVarId,
    level: Level,
    other_type: typ::Index,
    span: Span,
    types: &mut Types,
) -> Result<(), Error> {
    if t == other_type {
        Ok(())
    } else if occurs(id, level, other_type, types) {
        Err(Error::InfiniteType {
            location: span,
            typ: t,
        })
    } else {
        // Create binding for unbound type variable
        types[t] = Var(Bound(other_type));
        Ok(())
    }
}

fn unify_rec(
    state: &mut State,
    t1: typ::Index,
    t2: typ::Index,
    span: Span,
    typ: typ::Index,
    span2: Option<Span>,
    typ2: typ::Index,
    types: &mut Types,
) -> Result<(), Error> {
    match (&types[t1], &types[t2]) {
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
                    actual_type_location: span,
                    actual_type: typ,
                    expected_type_location: span2,
                    expected_type: typ2,
                })
            } else {
                for (p1, p2) in params.clone().iter().zip(params2.clone().iter()) {
                    unify_rec(state, *p1, *p2, span, typ, span2, typ2, types)?;
                }
                Ok(())
            }
        }

        // The 'find' in the union-find algorithm
        (Var(Bound(dest)), _) => unify_rec(state, *dest, t2, span, typ, span2, typ2, types),
        (_, Var(Bound(dest))) => unify_rec(state, t1, *dest, span, typ, span2, typ2, types),

        (Var(Unbound(id, level)), _) => unify_var(t1, *id, *level, t2, span, types),
        (_, Var(Unbound(id, level))) => unify_var(t2, *id, *level, t1, span, types),

        (
            Fn { params, ret },
            Fn {
                params: params2,
                ret: ret2,
            },
        ) => {
            if params.len() != params2.len() {
                Err(Error::TypeMismatch {
                    actual_type_location: span,
                    actual_type: typ,
                    expected_type_location: span2,
                    expected_type: typ2,
                })
            } else {
                let ret = *ret;
                let ret2 = *ret2;
                for (p1, p2) in params.clone().iter().zip(params2.clone().iter()) {
                    unify_rec(state, *p1, *p2, span, typ, span2, typ2, types)?;
                }
                unify_rec(state, ret, ret2, span, typ, span2, typ2, types)
            }
        }

        (Record { fields: fields1 }, Record { fields: fields2 }) => {
            let fields1 = fields1.clone();
            let fields2 = fields2.clone();

            // Check fields on each record, and unify types of the values against the other
            // record. If the field doesn't exist then it is a type mismatch.

            for (key, value) in fields1.map() {
                if fields2.map().contains_key(key) {
                    unify_rec(
                        state,
                        *value,
                        fields2.get(key).unwrap(),
                        span,
                        typ,
                        span2,
                        typ2,
                        types,
                    )?;
                } else {
                    return Err(Error::TypeMismatch {
                        actual_type_location: span,
                        actual_type: typ,
                        expected_type_location: span2,
                        expected_type: typ2,
                    });
                }
            }
            for (key, value) in fields2.map() {
                if fields1.map().contains_key(key) {
                    unify_rec(
                        state,
                        *value,
                        fields1.get(key).unwrap(),
                        span,
                        typ,
                        span2,
                        typ2,
                        types,
                    )?;
                } else {
                    return Err(Error::TypeMismatch {
                        actual_type_location: span,
                        actual_type: typ,
                        expected_type_location: span2,
                        expected_type: typ2,
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
            let base_record = *base_record;
            let fields = fields.clone();
            let ext_fields = ext_fields.clone();

            // Check the fields on the open record and match them against the fixed record. If
            // a field doesn't exist it is a mismatch
            for (key, value) in ext_fields.map() {
                if fields.map().contains_key(key) {
                    unify_rec(
                        state,
                        *value,
                        fields.get(key).unwrap(),
                        span,
                        typ,
                        span2,
                        typ2,
                        types,
                    )?;
                } else {
                    return Err(Error::TypeMismatch {
                        actual_type_location: span,
                        actual_type: typ,
                        expected_type_location: span2,
                        expected_type: typ2,
                    });
                }
            }

            // Unify the open record type variable with a record of the remaining fields from
            // the fixed record.
            let mut rem_fields = TypeEnv::new();
            for (key, value) in fields.map() {
                if !ext_fields.map().contains_key(key) {
                    rem_fields.insert(*key, *value);
                }
            }
            let rem_rec = types.push_and_get_key(Record {
                fields: Rc::new(rem_fields),
            });
            unify_rec(state, base_record, rem_rec, span, typ, span2, typ2, types)
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
            let base_record1 = *base_record1;
            let base_record2 = *base_record2;
            let fields1 = fields1.clone();
            let fields2 = fields2.clone();

            // Check common fields on the records, and unify types
            for (key, value) in fields1.map() {
                if fields2.map().contains_key(key) {
                    unify_rec(
                        state,
                        *value,
                        fields2.get(key).unwrap(),
                        span,
                        typ,
                        span2,
                        typ2,
                        types,
                    )?;
                }
            }

            let var = types.push_and_get_key(state.new_type_var());

            // unify { <fields-only-found-on-the-left-side> | new-type-var } varRight
            {
                let mut rem_fields1 = TypeEnv::new();
                for (key, value) in fields1.map() {
                    if !fields2.map().contains_key(key) {
                        rem_fields1.insert(*key, *value);
                    }
                }
                let rem_rec1 = types.push_and_get_key(RecordExt {
                    fields: Rc::new(rem_fields1),
                    base_record: var,
                });
                unify_rec(state, base_record2, rem_rec1, span, typ, span2, typ2, types)?;
            }

            // unify { <fields-only-found-on-the-right-side> | new-type-var } varLeft
            {
                let mut rem_fields2 = TypeEnv::new();
                for (key, value) in fields2.map() {
                    if !fields1.map().contains_key(key) {
                        rem_fields2.insert(*key, *value);
                    }
                }
                let rem_rec2 = types.push_and_get_key(RecordExt {
                    fields: Rc::new(rem_fields2),
                    base_record: var,
                });
                unify_rec(state, base_record1, rem_rec2, span, typ, span2, typ2, types)?;
            }

            Ok(())
        }

        (Alias { destination: t, .. }, _) => {
            unify_rec(state, *t, t2, span, typ, span2, typ2, types)
        }
        (_, Alias { destination: t, .. }) => {
            unify_rec(state, t1, *t, span, typ, span2, typ2, types)
        }

        (Named { .. }, _) | (Fn { .. }, _) | (Record { .. }, _) | (RecordExt { .. }, _) => {
            Err(Error::TypeMismatch {
                actual_type_location: span,
                actual_type: typ,
                expected_type_location: span2,
                expected_type: typ2,
            })
        }
    }
}

/**
 * This function must be called after the types have been unified and are compatible
 */
fn signature_is_too_generic(
    signature: typ::Index,
    inferred: typ::Index,
    types: &mut Types,
) -> bool {
    let signature = find(signature, types);
    let inferred = find(inferred, types);
    let signature = flatten_record(signature, types).unwrap_or(signature);
    let inferred = flatten_record(inferred, types).unwrap_or(inferred);

    match (&types[signature], &types[inferred]) {
        (
            Named { params, .. },
            Named {
                params: params2, ..
            },
        ) => {
            for (p1, p2) in params.clone().iter().zip(params2.clone().iter()) {
                if signature_is_too_generic(*p1, *p2, types) {
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
            let ret = *ret;
            let ret2 = *ret2;
            for (p1, p2) in params.clone().iter().zip(params2.clone().iter()) {
                if signature_is_too_generic(*p1, *p2, types) {
                    return true;
                }
            }
            signature_is_too_generic(ret, ret2, types)
        }

        (Record { fields: fields1 }, Record { fields: fields2 }) => {
            let fields1 = fields1.clone();
            let fields2 = fields2.clone();
            for (key, value) in fields1.map() {
                if signature_is_too_generic(*value, fields2.get(key).unwrap(), types) {
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
            let fields = fields.clone();
            let ext_fields = ext_fields.clone();
            for (key, value) in fields.map() {
                if signature_is_too_generic(*value, ext_fields.get(key).unwrap(), types) {
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
            let signature_base_record = *signature_base_record;
            let inferred_base_record = *inferred_base_record;
            let signature_fields = signature_fields.clone();
            let inferred_fields = inferred_fields.clone();

            for (key, signature_field_value) in signature_fields.map() {
                if signature_is_too_generic(
                    *signature_field_value,
                    inferred_fields.get(key).unwrap(),
                    types,
                ) {
                    return true;
                }
            }
            signature_is_too_generic(signature_base_record, inferred_base_record, types)
        }

        (Alias { destination, .. }, _) => signature_is_too_generic(*destination, inferred, types),

        (_, Alias { destination, .. }) => signature_is_too_generic(signature, *destination, types),

        (Named { .. }, _) | (Fn { .. }, _) | (Record { .. }, _) | (RecordExt { .. }, _) => false,
    }
}

/**
 * The main entry point to type inference.
 *
 * Infer the types of the whole module and produce its module interface with types.
 */
pub fn infer(
    compiler_state: &mut compiler::State,
    module_idx: ModuleIndex,
) -> Result<(), Vec<Error>> {
    let strings = &mut compiler_state.strings;
    let module_ast = &mut compiler_state.modules[module_idx];
    let expressions = &module_ast.expressions.values;
    let expression_types = &mut module_ast.expressions.types;
    let expression_spans = &module_ast.expressions.spans;
    let mut state = State::new();
    let mut env = PolyTypeEnv::new();
    let mut types_env = PolyTypeEnv::new();
    let types_vec = &mut compiler_state.types;
    let mut errors: Vec<Error> = vec![];

    let mut module_definitions = PolyTypeEnv::new();
    let mut module_types = TypeEnv::new();
    let mut module_type_constructors = HashMap::default();

    // Check imports and add them to the env to type check this module
    for import in &module_ast.imports {
        let module_full_name = import.module_name.full_name;
        let import_types: Option<&ModuleInterface> = compiler_state
            .module_name_to_module_idx
            .get(&module_full_name)
            .and_then(|idx| compiler_state.module_interfaces.get(*idx))
            .and_then(|maybe_types| maybe_types.as_ref());
        match import_types {
            Some(import_types) => {
                let module_ident = if let Some(alias) = &import.alias {
                    alias.name
                } else {
                    import.module_name.full_name
                };

                // Fully qualified constructor functions
                for constructors in import_types.type_constructors.values() {
                    for (constructor_name, constructor_type) in constructors.map() {
                        let full_name = {
                            let module = strings.resolve(module_ident);
                            let ident = strings.resolve(*constructor_name);
                            // TODO: This should be done for all identifiers at some point instead
                            // of every time they are imported
                            format!("{module}.{ident}")
                        };
                        let full_name = strings.get_or_intern(full_name);
                        env.insert(full_name, constructor_type.clone());
                    }
                }

                // Fully qualified definitions
                for (ident, typ) in import_types.definitions.map() {
                    let full_name = {
                        let module = strings.resolve(module_ident);
                        let ident = strings.resolve(*ident);
                        format!("{module}.{ident}")
                    };
                    let full_name = strings.get_or_intern(full_name);
                    env.insert(full_name, typ.clone());
                }

                for exposed in &import.exposing {
                    match &exposed.typ {
                        ast::ExportData::Identifier(identifier) => {
                            let name = &identifier.name;
                            match import_types.definitions.get(name) {
                                // TODO: I'm cloning poly envs galore? it was rc before, why dont
                                // i generalize here when I import the types?
                                Some(definition) => env.insert(*name, definition.clone()),
                                None => errors.push(Error::UnknownImportDefinition {
                                    export: identifier.to_owned(),
                                    import: import.to_owned(),
                                }),
                            };
                        }
                        ast::ExportData::Type {
                            name: name_ast,
                            constructors,
                        } => {
                            let name = &name_ast.name;
                            match import_types.types.get(name) {
                                Some(typ) => {
                                    types_env.insert(*name, state.generalize(typ, types_vec));

                                    let constructor_types =
                                        import_types.type_constructors.get(name).unwrap();
                                    for constructor in constructors {
                                        let name = &constructor.name;
                                        match constructor_types.get(name) {
                                            Some(definition) => {
                                                env.insert(*name, definition.clone())
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
        for var in &type_def.vars {
            let tvar = types_vec.push_and_get_key(state.new_type_var());
            type_vars.push((var.name, tvar));
            type_vars_env.insert(var.name, tvar);
        }
        state.exit_level();
        let name = type_def.name.name;

        match &type_def.typ {
            ast::types::TypeDefinitionData::Empty => {
                let type_def_type = types_vec.push_and_get_key(Type::Named {
                    module: module_ast.name.full_name,
                    name,
                    params: Rc::new(type_vars.iter().map(|(_, t)| *t).collect()),
                });
                types_env.insert(name, state.generalize(type_def_type, types_vec));
            }
            ast::types::TypeDefinitionData::Union { constructors } => {
                let type_def_type = types_vec.push_and_get_key(Type::Named {
                    module: module_ast.name.full_name,
                    name,
                    params: Rc::new(type_vars.iter().map(|(_, t)| *t).collect()),
                });
                types_env.insert(name, state.generalize(type_def_type, types_vec));

                for constructor in constructors {
                    let name = &constructor.name.name;
                    let mut param_types = vec![];

                    for param in &constructor.params {
                        let typ = match ast_type_to_type(
                            param,
                            &type_vars_env,
                            &mut errors,
                            &mut state,
                            &types_env,
                            types_vec,
                        ) {
                            Ok(t) => t,
                            Err(err) => {
                                errors.push(err);
                                types_vec.push_and_get_key(state.new_type_var())
                            }
                        };
                        param_types.push(typ);
                    }

                    let typ = if param_types.is_empty() {
                        type_def_type
                    } else {
                        types_vec.push_and_get_key(Type::Fn {
                            params: Rc::new(param_types),
                            ret: type_def_type,
                        })
                    };

                    env.insert(*name, state.generalize(typ, types_vec));
                }
            }
            ast::types::TypeDefinitionData::Record(record_type) => {
                let typ = match ast_record_type_to_type(
                    record_type,
                    &type_vars_env,
                    &mut errors,
                    &mut state,
                    &types_env,
                    types_vec,
                ) {
                    Ok(t) => t,
                    Err(err) => {
                        errors.push(err);
                        types_vec.push_and_get_key(state.new_type_var())
                    }
                };
                let typ = types_vec.push_and_get_key(Alias {
                    module: module_ast.name.full_name,
                    name,
                    params: Rc::new(type_vars),
                    destination: typ,
                });
                types_env.insert(name, state.generalize(typ, types_vec));
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
        expressions,
        expression_types,
        expression_spans,
        types_vec,
        &mut errors,
    );

    // Check exports against this module type to see everything exists and is valid
    for export in &module_ast.exports {
        match &export.typ {
            ast::ExportData::Identifier(identifier) => {
                let name = &identifier.name;
                match env.get(name) {
                    Some(poly_type) => module_definitions.insert(*name, poly_type.clone()),
                    None => errors.push(Error::UndefinedExport(identifier.to_owned())),
                }
            }
            ast::ExportData::Type {
                name: name_ast,
                constructors,
            } => {
                let name = &name_ast.name;
                if let Some(typ) = types_env.get(name) {
                    module_types.insert(*name, typ.typ);

                    let mut constructor_types = PolyTypeEnv::new();
                    for constructor in constructors {
                        let name = &constructor.name;
                        match env.get(name) {
                            Some(poly_type) => {
                                constructor_types.insert(*name, poly_type.clone());
                            }
                            None => errors
                                .push(Error::UndefinedExportConstructor(constructor.to_owned())),
                        }
                    }
                    module_type_constructors.insert(*name, constructor_types);
                } else {
                    errors.push(Error::UnknownType(name_ast.to_owned()));
                }
            }
        }
    }

    let module_interface = ModuleInterface {
        types: module_types,
        type_constructors: module_type_constructors,
        definitions: module_definitions,
    };
    compiler_state.module_interfaces[module_idx] = Some(module_interface);
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
    types: &mut Types,
) -> Result<typ::Index, Error> {
    match ast_type {
        ast::types::Type::Fun { params, ret } => {
            let mut params_types = vec![];
            for param in params {
                params_types.push(ast_type_to_type(
                    param, type_vars, errors, state, types_env, types,
                )?);
            }
            let ret_type = ast_type_to_type(ret, type_vars, errors, state, types_env, types)?;
            Ok(types.push_and_get_key(Fn {
                params: Rc::new(params_types),
                ret: ret_type,
            }))
        }

        ast::types::Type::App(typ) => match types_env.get(&typ.name.name) {
            Some(t) => {
                state.enter_level();
                let t = state.instantiate(t, types);
                state.exit_level();
                match &types[t] {
                    Named { module, .. } => {
                        let module = *module;
                        let mut params_types = vec![];
                        for param in &typ.params {
                            params_types.push(ast_type_to_type(
                                param, type_vars, errors, state, types_env, types,
                            )?);
                        }
                        let ast_typ = types.push_and_get_key(Named {
                            module,
                            name: typ.name.name,
                            params: Rc::new(params_types),
                        });

                        unify(state, typ.span, ast_typ, None, t, types)?;

                        Ok(ast_typ)
                    }
                    Alias {
                        module,
                        name,
                        params,
                        destination,
                    } => {
                        let module = *module;
                        let name = *name;
                        let destination = *destination;
                        let params = params.clone();

                        if params.len() != typ.params.len() {
                            return Err(Error::WrongArity {
                                location: typ.span,
                                num_params_applied: typ.params.len(),
                                num_params: params.len(),
                                typ: t,
                            });
                        }

                        let mut params_types = vec![];
                        for (param, (name, dest_param_typ)) in typ.params.iter().zip(&**params) {
                            let param_typ = ast_type_to_type(
                                param, type_vars, errors, state, types_env, types,
                            )?;
                            // TODO: param span (`typ.span`) here should have node to point at it
                            // directly in the error message
                            unify(state, typ.span, param_typ, None, *dest_param_typ, types)?;
                            params_types.push((*name, param_typ));
                        }

                        Ok(types.push_and_get_key(Alias {
                            module,
                            name,
                            params: Rc::new(params_types),
                            destination,
                        }))
                    }
                    _ => Err(Error::WrongArity {
                        location: typ.span,
                        num_params_applied: typ.params.len(),
                        num_params: 0,
                        typ: t,
                    }),
                }
            }
            None => Err(Error::UnknownType(typ.name.to_owned())),
        },

        ast::types::Type::Var(identifier) => match type_vars.get(&identifier.name) {
            Some(t) => Ok(t),
            None => Err(Error::UnknownTypeVar(identifier.to_owned())),
        },

        ast::types::Type::Record(record_type) => {
            ast_record_type_to_type(record_type, type_vars, errors, state, types_env, types)
        }
    }
}
fn ast_record_type_to_type<'ast>(
    record_type: &'ast ast::types::RecordType,
    type_vars: &TypeEnv,
    errors: &mut Vec<Error>,
    state: &mut State,
    types_env: &PolyTypeEnv,
    types: &mut Types,
) -> Result<typ::Index, Error> {
    match record_type {
        ast::types::RecordType::Record(record) => {
            let mut fields = TypeEnv::new();
            for (name, ast_type) in &record.fields {
                let typ = ast_type_to_type(ast_type, type_vars, errors, state, types_env, types)?;
                fields.insert(name.name, typ);
            }
            Ok(types.push_and_get_key(Record {
                fields: Rc::new(fields),
            }))
        }
        ast::types::RecordType::RecordExt(record_ext) => {
            let base_record = if let Some(t) = type_vars.get(&record_ext.extension.name) {
                t
            } else {
                return Err(Error::UnknownTypeVar(record_ext.extension.to_owned()));
            };

            let mut fields = TypeEnv::new();
            for (name, ast_type) in &record_ext.fields {
                let typ = ast_type_to_type(ast_type, type_vars, errors, state, types_env, types)?;
                fields.insert(name.name, typ);
            }

            Ok(types.push_and_get_key(RecordExt {
                fields: Rc::new(fields),
                base_record,
            }))
        }
    }
}

fn infer_rec<'ast>(
    expression_index: expression::Index,
    state: &mut State,
    env: &mut PolyTypeEnv,
    types_env: &PolyTypeEnv,
    strings: &mut Strings,
    expressions: &'ast ExpressionValues,
    expression_types: &'ast mut ExpressionTypes,
    expression_spans: &'ast ExpressionSpans,
    types: &mut Types,
    errors: &mut Vec<Error>,
) -> typ::Index {
    let expression = &expressions[expression_index];
    let typ = match expression {
        E::Float(_) => types_env.get(&strings.get_or_intern("Float")).unwrap().typ,

        E::String_(_) => types_env.get(&strings.get_or_intern("String")).unwrap().typ,

        E::Record { fields } => {
            let mut typed_fields = TypeEnv::new();

            for (name, value) in fields {
                let t = infer_rec(
                    *value,
                    state,
                    env,
                    types_env,
                    strings,
                    expressions,
                    expression_types,
                    expression_spans,
                    types,
                    errors,
                );
                if typed_fields.map().contains_key(&name.name) {
                    errors.push(Error::DuplicateField {
                        field: name.to_owned(),
                        expr: expression_index.to_owned(),
                    });
                } else {
                    typed_fields.insert(name.name, t);
                }
            }

            types.push_and_get_key(Record {
                fields: Rc::new(typed_fields),
            })
        }

        E::RecordUpdate { record, fields } => {
            let mut typed_fields = TypeEnv::new();
            for (name, value) in fields {
                let t = infer_rec(
                    *value,
                    state,
                    env,
                    types_env,
                    strings,
                    expressions,
                    expression_types,
                    expression_spans,
                    types,
                    errors,
                );
                if typed_fields.map().contains_key(&name.name) {
                    errors.push(Error::DuplicateField {
                        field: name.to_owned(),
                        expr: expression_index.to_owned(),
                    });
                } else {
                    typed_fields.insert(name.name, t);
                }
            }

            let base_record = types.push_and_get_key(state.new_type_var());
            let inferred_type = types.push_and_get_key(RecordExt {
                fields: Rc::new(typed_fields),
                base_record,
            });

            let record_index = *record;

            let record_type = infer_rec(
                record_index,
                state,
                env,
                types_env,
                strings,
                expressions,
                expression_types,
                expression_spans,
                types,
                errors,
            );

            // Unify the base record with the extensible record represented by this update
            add_error(
                unify(
                    state,
                    expression_spans[record_index],
                    record_type,
                    None,
                    inferred_type,
                    types,
                ),
                errors,
            );

            inferred_type
        }

        E::PropertyAccess {
            expression: lhs_index,
            property,
        } => {
            let field_type = types.push_and_get_key(state.new_type_var());
            let remaining_fields_type = types.push_and_get_key(state.new_type_var());
            let record_type = types.push_and_get_key(RecordExt {
                fields: {
                    let mut fields = TypeEnv::new();
                    fields.insert(property.name, field_type);
                    Rc::new(fields)
                },
                base_record: remaining_fields_type,
            });

            let lhs_type = infer_rec(
                *lhs_index,
                state,
                env,
                types_env,
                strings,
                expressions,
                expression_types,
                expression_spans,
                types,
                errors,
            );

            add_error(
                unify(
                    state,
                    expression_spans[*lhs_index],
                    lhs_type,
                    None,
                    record_type,
                    types,
                ),
                errors,
            );

            field_type
        }

        E::PropertyAccessLambda { property } => {
            let field_type = types.push_and_get_key(state.new_type_var());
            let remaining_fields_type = types.push_and_get_key(state.new_type_var());

            let param = types.push_and_get_key(RecordExt {
                fields: {
                    let mut fields = TypeEnv::new();
                    fields.insert(property.name, field_type);
                    Rc::new(fields)
                },
                base_record: remaining_fields_type,
            });
            types.push_and_get_key(Fn {
                params: Rc::new(vec![param]),
                ret: field_type,
            })
        }

        E::Unary {
            op,
            expression: rhs_index,
        } => {
            let rhs_type = infer_rec(
                *rhs_index,
                state,
                env,
                types_env,
                strings,
                expressions,
                expression_types,
                expression_spans,
                types,
                errors,
            );

            add_error(
                unify(
                    state,
                    expression_spans[*rhs_index],
                    rhs_type,
                    None,
                    types_env
                        .get(&strings.get_or_intern(match op.typ {
                            U::Not => "Bool",
                            U::Minus => "Float",
                        }))
                        .unwrap()
                        .typ,
                    types,
                ),
                errors,
            );

            rhs_type
        }

        E::Binary {
            expression: function,
            arguments,
            ..
        } => {
            // Infer the binary op as a function call
            infer_fn_call(
                *function,
                arguments.iter().copied(),
                expression_index,
                state,
                env,
                types_env,
                strings,
                expressions,
                expression_types,
                expression_spans,
                types,
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
        E::If {
            condition: condition_index,
            then: then_index,
            else_: else_index,
        } => {
            let t = infer_rec(
                *condition_index,
                state,
                env,
                types_env,
                strings,
                expressions,
                expression_types,
                expression_spans,
                types,
                errors,
            );
            add_error(
                unify(
                    state,
                    expression_spans[*condition_index],
                    t,
                    None,
                    types_env.get(&strings.get_or_intern("Bool")).unwrap().typ,
                    types,
                ),
                errors,
            );

            let t1 = infer_rec(
                *then_index,
                state,
                env,
                types_env,
                strings,
                expressions,
                expression_types,
                expression_spans,
                types,
                errors,
            );
            let t2 = infer_rec(
                *else_index,
                state,
                env,
                types_env,
                strings,
                expressions,
                expression_types,
                expression_spans,
                types,
                errors,
            );
            add_error(
                unify(
                    state,
                    expression_spans[*then_index],
                    t1,
                    Some(expression_spans[*else_index]),
                    t2,
                    types,
                ),
                errors,
            );

            t2
        }

        /*
         * Identifier
         *
         * Given x of type s in env:
         *
         *     t = inst s
         */
        E::Identifier { module, identifier } => infer_identifier(
            module,
            identifier,
            expression_spans[expression_index],
            state,
            env,
            strings,
            types,
            errors,
        ),

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
        E::FnCall {
            function,
            arguments,
        } => infer_fn_call(
            *function,
            arguments.iter().copied(),
            expression_index,
            state,
            env,
            types_env,
            strings,
            expressions,
            expression_types,
            expression_spans,
            types,
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
        E::Lambda(lambda) => infer_lambda(
            lambda,
            state,
            env,
            types_env,
            strings,
            expressions,
            expression_types,
            expression_spans,
            types,
            errors,
        ),

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
        E::Let { definitions, body } => {
            let mut new_env = env.clone();

            infer_definitions(
                definitions,
                state,
                &mut new_env,
                types_env,
                strings,
                expressions,
                expression_types,
                expression_spans,
                types,
                errors,
            );

            infer_rec(
                *body,
                state,
                &mut new_env,
                types_env,
                strings,
                expressions,
                expression_types,
                expression_spans,
                types,
                errors,
            )
        }

        E::PatternMatching {
            conditions,
            branches,
        } => {
            let return_type = types.push_and_get_key(state.new_type_var());

            let expression_types_and_spans: Vec<_> = conditions
                .iter()
                .map(|expression| {
                    let typ = infer_rec(
                        *expression,
                        state,
                        env,
                        types_env,
                        strings,
                        expressions,
                        expression_types,
                        expression_spans,
                        types,
                        errors,
                    );
                    let span = Some(expression_spans[*expression]);
                    (typ, span)
                })
                .collect();

            let mut branch_types = Vec::with_capacity(branches.len());
            for branch in branches {
                let mut env = env.clone();

                for (pattern, (expression_type, expression_type_span)) in
                    branch.patterns.iter().zip(&expression_types_and_spans)
                {
                    check_pattern_and_add_bindings(
                        pattern,
                        *expression_type,
                        *expression_type_span,
                        state,
                        &mut env,
                        &mut Vec::new(),
                        types_env,
                        strings,
                        types,
                        errors,
                    );
                }

                if let Some(condition_idx) = &branch.condition {
                    let condition_type = infer_rec(
                        *condition_idx,
                        state,
                        &mut env,
                        types_env,
                        strings,
                        expressions,
                        expression_types,
                        expression_spans,
                        types,
                        errors,
                    );
                    add_error(
                        unify(
                            state,
                            expression_spans[*condition_idx],
                            condition_type,
                            None,
                            types_env.get(&strings.get_or_intern("Bool")).unwrap().typ,
                            types,
                        ),
                        errors,
                    );
                }

                branch_types.push(infer_rec(
                    branch.body,
                    state,
                    &mut env,
                    types_env,
                    strings,
                    expressions,
                    expression_types,
                    expression_spans,
                    types,
                    errors,
                ));
            }

            let mut last_unified_span = None;
            for (branch, branch_type) in branches.iter().zip(branch_types) {
                if let Err(e) = unify(
                    state,
                    branch.span,
                    branch_type,
                    last_unified_span,
                    return_type,
                    types,
                ) {
                    // TODO: Right now we error with only the first branch to have a type mismatch,
                    // but ideally we would unify all and store the branches that have different
                    // types, eg: (all_that_unified, all_that_didnt) and emit a specific nice error
                    // message for it. For now this can do but it can be improved.
                    add_error(Err(e), errors);
                    return types.push_and_get_key(state.new_type_var());
                } else {
                    last_unified_span = Some(branch.span);
                }
            }

            return_type
        }
    };

    expression_types[expression_index] = Some(typ);

    typ
}

fn infer_definitions<'ast>(
    definitions: &'ast [TypedDefinition],
    state: &mut State,
    env: &mut PolyTypeEnv,
    types_env: &PolyTypeEnv,
    strings: &mut Strings,
    expressions: &'ast ExpressionValues,
    expression_types: &'ast mut ExpressionTypes,
    expression_spans: &'ast ExpressionSpans,
    types: &mut Types,
    errors: &mut Vec<Error>,
) {
    for typed_definition in definitions {
        if let Some(definition) = typed_definition.definition() {
            match &definition {
                Definition::Lambda(identifier, lambda) => {
                    state.enter_level();
                    let t = infer_lambda(
                        lambda,
                        state,
                        env,
                        types_env,
                        strings,
                        expressions,
                        expression_types,
                        expression_spans,
                        types,
                        errors,
                    );
                    state.exit_level();

                    let t = check_signature(typed_definition, t, state, types_env, types, errors);

                    let t = state.generalize(t, types);
                    env.insert(identifier.name, t);
                }
                Definition::Pattern(pattern, value) => {
                    state.enter_level();
                    let t = infer_rec(
                        *value,
                        state,
                        env,
                        types_env,
                        strings,
                        expressions,
                        expression_types,
                        expression_spans,
                        types,
                        errors,
                    );
                    state.exit_level();

                    let t = check_signature(typed_definition, t, state, types_env, types, errors);

                    check_pattern_and_add_bindings(
                        pattern,
                        t,
                        None,
                        state,
                        env,
                        &mut Vec::new(),
                        types_env,
                        strings,
                        types,
                        errors,
                    );
                }
            }
        } else if let Some(typ) = typed_definition.typ() {
            let t = match type_from_signature(typ, state, types_env, types, errors) {
                Ok(t) => t,
                Err(err) => {
                    errors.push(err);
                    state.enter_level();
                    let t = state.new_type_var();
                    state.exit_level();
                    types.push_and_get_key(t)
                }
            };
            let t = state.generalize(t, types);
            env.insert(typ.name.name, t);
        }
    }
}

fn type_from_signature<'ast>(
    signature: &'ast TypeSignature,
    state: &mut State,
    types_env: &PolyTypeEnv,
    types: &mut Types,
    errors: &mut Vec<Error>,
) -> Result<typ::Index, Error> {
    let mut type_vars = TypeEnv::new();
    state.enter_level();
    for var in signature.typ.vars() {
        type_vars.insert(var.name, types.push_and_get_key(state.new_type_var()));
    }
    let signature_type_result =
        ast_type_to_type(&signature.typ, &type_vars, errors, state, types_env, types);
    state.exit_level();
    signature_type_result
}

fn check_signature<'ast>(
    typed_definition: &'ast TypedDefinition,
    typ: typ::Index,
    state: &mut State,
    types_env: &PolyTypeEnv,
    types: &mut Types,
    errors: &mut Vec<Error>,
) -> typ::Index {
    if let Some(signature) = typed_definition.typ() {
        let signature_type = match type_from_signature(signature, state, types_env, types, errors) {
            Ok(t) => t,
            Err(err) => {
                errors.push(err);
                return typ;
            }
        };

        // TODO: This is kind of nasty
        // Clone the signature to avoid unifying the signature_type which we use to check if the
        // type is to general below
        let signature_type_to_unify = state.generalize(signature_type, types);
        state.enter_level();
        let signature_type_to_unify = state.instantiate(&signature_type_to_unify, types);
        state.exit_level();

        if let Err(e) = unify(
            state,
            signature.name.span,
            typ,
            None,
            signature_type_to_unify,
            types,
        ) {
            errors.push(match e {
                Error::TypeMismatch {
                    actual_type_location,
                    ..
                } => Error::SignatureMismatch {
                    signature_type_location: actual_type_location,
                    signature_type: signature_type_to_unify,
                    inferred_type: typ,
                },
                _ => e,
            });
            typ
        } else {
            if signature_is_too_generic(signature_type, typ, types) {
                errors.push(Error::SignatureTooGeneral {
                    signature_type_location: signature.name.span,
                    signature_type,
                    inferred_type: typ,
                });
            }
            signature_type_to_unify
        }
    } else {
        typ
    }
}

fn check_pattern_and_add_bindings(
    pattern: &Pattern,
    expected_pattern_type: typ::Index,
    expected_pattern_type_span: Option<Span>,
    state: &mut State,
    env: &mut PolyTypeEnv,
    introduced_bindings: &mut Vec<(Identifier, typ::Index)>,
    types_env: &PolyTypeEnv,
    strings: &mut Strings,
    types: &mut Types,
    errors: &mut Vec<Error>,
) {
    match &pattern.data {
        P::Hole => (),
        P::Identifier(x) => {
            let t = state.generalize(expected_pattern_type, types);
            introduced_bindings.push((*x, t.typ));
            env.insert(x.name, t);
        }
        P::String_(_) => add_error(
            unify(
                state,
                pattern.span,
                types_env.get(&strings.get_or_intern("String")).unwrap().typ,
                expected_pattern_type_span,
                expected_pattern_type,
                types,
            ),
            errors,
        ),
        P::Float(_) => add_error(
            unify(
                state,
                pattern.span,
                types_env.get(&strings.get_or_intern("Float")).unwrap().typ,
                expected_pattern_type_span,
                expected_pattern_type,
                types,
            ),
            errors,
        ),
        P::Type {
            module,
            constructor,
            params: pattern_params,
        } => {
            // TODO: This is ugly and unnecessary, we could have AnyIdentifier be a trait, or
            // make the function take an thing that complies with an identifier trait.
            let identifier = AnyIdentifier::CapitalizedIdentifier(constructor.clone());
            let constructor_type = infer_identifier(
                module,
                &identifier,
                identifier.span(),
                state,
                env,
                strings,
                types,
                errors,
            );

            let pattern_arity = pattern_params.len();

            let constructor_return_type = match (pattern_arity, &types[constructor_type]) {
                // Arities match, then check params against each other, and return the return type
                (
                    n,
                    Fn {
                        params: constructor_type_params,
                        ret,
                    },
                ) if n == constructor_type_params.len() => {
                    let constructor_type_params = constructor_type_params.clone();
                    let ret = *ret;
                    for (pattern_param, constructor_type_param) in
                        pattern_params.iter().zip(&*constructor_type_params)
                    {
                        check_pattern_and_add_bindings(
                            pattern_param,
                            *constructor_type_param,
                            None,
                            state,
                            env,
                            introduced_bindings,
                            types_env,
                            strings,
                            types,
                            errors,
                        );
                    }
                    ret
                }

                // Arities dont match. Check the arguments anyways to introduce bindings and avoid
                // extra error messages, and make up variables as needed. Return a new variable to
                // avoid unnecessary errors when matching against the expected type.
                (
                    _n,
                    Fn {
                        params: constructor_type_params,
                        ..
                    },
                ) => {
                    let constructor_type_params = constructor_type_params.clone();

                    errors.push(Error::WrongArity {
                        location: identifier.span(),
                        num_params_applied: pattern_arity,
                        num_params: constructor_type_params.len(),
                        typ: constructor_type,
                    });

                    for (i, param) in pattern_params.iter().enumerate() {
                        let expected_param_type = constructor_type_params
                            .get(i)
                            .copied()
                            .unwrap_or_else(|| types.push_and_get_key(state.new_type_var()));

                        check_pattern_and_add_bindings(
                            param,
                            expected_param_type,
                            None,
                            state,
                            env,
                            introduced_bindings,
                            types_env,
                            strings,
                            types,
                            errors,
                        );
                    }

                    types.push_and_get_key(state.new_type_var())
                }

                // Just a type without arguments, arities match
                (0, Named { .. }) => constructor_type,

                // Used the pattern with arguments, but the type is just a type not a function
                (_n, Named { .. }) => {
                    let mut params = Vec::with_capacity(pattern_params.len());
                    let ret = types.push_and_get_key(state.new_type_var());

                    for param in pattern_params {
                        let expected_param_type = types.push_and_get_key(state.new_type_var());
                        params.push(expected_param_type);
                        check_pattern_and_add_bindings(
                            param,
                            expected_param_type,
                            None,
                            state,
                            env,
                            introduced_bindings,
                            types_env,
                            strings,
                            types,
                            errors,
                        );
                    }

                    let pattern_type = types.push_and_get_key(Fn {
                        params: Rc::new(params),
                        ret,
                    });

                    add_error(
                        unify(
                            state,
                            identifier.span(),
                            pattern_type,
                            None,
                            constructor_type,
                            types,
                        ),
                        errors,
                    );

                    ret
                }

                (_, _) => constructor_type,
            };

            add_error(
                unify(
                    state,
                    constructor.span,
                    constructor_return_type,
                    expected_pattern_type_span,
                    expected_pattern_type,
                    types,
                ),
                errors,
            );
        }
        P::Named { pattern, name } => {
            let t = state.generalize(expected_pattern_type, types);
            introduced_bindings.push((*name, t.typ));
            env.insert(name.name, t);

            check_pattern_and_add_bindings(
                pattern,
                expected_pattern_type,
                expected_pattern_type_span,
                state,
                env,
                introduced_bindings,
                types_env,
                strings,
                types,
                errors,
            )
        }
        P::Or(patterns) => {
            let mut introduced_bindings = Vec::new();

            // Infer types for the different patterns separately
            // Then unify the patterns against each other, for better errors
            // And if they did, then unify with the expected_pattern_type

            let all_patterns_type = types.push_and_get_key(state.new_type_var());
            let mut pattern_types = Vec::with_capacity(patterns.len());
            let mut last_unified_span = None;
            for pattern in patterns {
                let pattern_type = types.push_and_get_key(state.new_type_var());
                check_pattern_and_add_bindings(
                    pattern,
                    pattern_type,
                    None,
                    state,
                    env,
                    &mut introduced_bindings,
                    types_env,
                    strings,
                    types,
                    errors,
                );
                if let Err(e) = unify(
                    state,
                    pattern.span,
                    pattern_type,
                    last_unified_span,
                    all_patterns_type,
                    types,
                ) {
                    // TODO: Right now we error with only the first pattern to have a type mismatch,
                    // but ideally we would unify all and store the patterns that have different
                    // types, eg: (all_that_unified, all_that_didnt) and emit a specific nice error
                    // message for it. For now this can do but it can be improved.
                    add_error(Err(e), errors);
                    return;
                } else {
                    last_unified_span = Some(pattern.span);
                    pattern_types.push(pattern_type);
                }
            }

            for (pattern, pattern_type) in patterns.iter().zip(pattern_types) {
                let result = unify(
                    state,
                    pattern.span,
                    pattern_type,
                    expected_pattern_type_span,
                    expected_pattern_type,
                    types,
                );
                if result.is_err() {
                    add_error(result, errors);
                    return;
                }
            }

            // If all unified fine, then check all branches introduce the same bindings
            let mut names: Option<FnvHashSet<IdentifierName>> = None;
            for pattern in patterns {
                if let Some(names) = &names {
                    let mut pattern_names = FnvHashSet::default();
                    pattern.data.get_bindings(&mut pattern_names);
                    if names.symmetric_difference(&pattern_names).count() > 0 {
                        add_error(
                            Err(Error::PatternsIntroduceDifferentBindings {
                                names1: names.clone(),
                                span1: patterns.first().unwrap().span,
                                names2: pattern_names,
                                span2: pattern.span,
                            }),
                            errors,
                        );
                        return;
                    }
                } else {
                    let mut names_set = FnvHashSet::default();
                    pattern.data.get_bindings(&mut names_set);
                    names = Some(names_set);
                }
            }

            for (i, (identifier, type_idx)) in introduced_bindings.iter().enumerate() {
                for (j, (identifier2, type_idx2)) in introduced_bindings.iter().enumerate().skip(i)
                {
                    if i != j && identifier.name == identifier2.name && type_idx != type_idx2 {
                        let result = unify(
                            state,
                            identifier.span,
                            *type_idx,
                            Some(identifier2.span),
                            *type_idx2,
                            types,
                        );
                        if result.is_err() {
                            add_error(result, errors);
                            return;
                        }
                    }
                }
            }
        }
        P::Record(fields) => {
            let mut field_types = TypeEnv::new();
            for (identifier, field) in fields {
                let expected_field_type = types.push_and_get_key(state.new_type_var());
                field_types.insert(identifier.name, expected_field_type);
                check_pattern_and_add_bindings(
                    field,
                    expected_field_type,
                    None,
                    state,
                    env,
                    introduced_bindings,
                    types_env,
                    strings,
                    types,
                    errors,
                );
            }

            let base_record = types.push_and_get_key(state.new_type_var());
            let pattern_type = types.push_and_get_key(RecordExt {
                base_record,
                fields: Rc::new(field_types),
            });

            add_error(
                unify(
                    state,
                    pattern.span,
                    pattern_type,
                    expected_pattern_type_span,
                    expected_pattern_type,
                    types,
                ),
                errors,
            );
        }
    }
}

fn infer_identifier(
    module: &Option<ModuleName>,
    identifier: &AnyIdentifier,
    span: Span,
    state: &mut State,
    env: &mut PolyTypeEnv,
    strings: &mut Strings,
    types: &mut Types,
    errors: &mut Vec<Error>,
) -> typ::Index {
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
        Some(s) => state.instantiate(s, types),
        None => {
            add_error(
                Err(Error::UndefinedIdentifier {
                    identifier: name,
                    location: span,
                }),
                errors,
            );
            types.push_and_get_key(state.new_type_var())
        }
    }
}

fn infer_lambda<'ast>(
    lambda: &'ast Lambda,
    state: &mut State,
    env: &mut PolyTypeEnv,
    types_env: &PolyTypeEnv,
    strings: &mut Strings,
    expressions: &'ast ExpressionValues,
    expression_types: &'ast mut ExpressionTypes,
    expression_spans: &'ast ExpressionSpans,
    types: &mut Types,
    errors: &mut Vec<Error>,
) -> typ::Index {
    let param_types: Vec<typ::Index> = lambda
        .parameters
        .iter()
        .map(|_| types.push_and_get_key(state.new_type_var()))
        .collect();

    let mut env = lambda.parameters.iter().zip(&param_types).fold(
        env.clone(),
        |mut env, (param, param_type)| {
            check_pattern_and_add_bindings(
                param,
                *param_type,
                None,
                state,
                &mut env,
                &mut Vec::new(),
                types_env,
                strings,
                types,
                errors,
            );
            env
        },
    );

    let return_type = infer_rec(
        lambda.body,
        state,
        &mut env,
        types_env,
        strings,
        expressions,
        expression_types,
        expression_spans,
        types,
        errors,
    );

    types.push_and_get_key(Fn {
        params: Rc::new(param_types),
        ret: return_type,
    })
}

fn infer_fn_call<'ast, Args>(
    f: expression::Index,
    args: Args,
    expression: expression::Index,
    state: &mut State,
    env: &mut PolyTypeEnv,
    types_env: &PolyTypeEnv,
    strings: &mut Strings,
    expressions: &'ast ExpressionValues,
    expression_types: &'ast mut ExpressionTypes,
    expression_spans: &'ast ExpressionSpans,
    types: &mut Types,
    errors: &mut Vec<Error>,
) -> typ::Index
where
    Args: Iterator<Item = expression::Index> + Clone,
{
    let fn_type = infer_rec(
        f,
        state,
        env,
        types_env,
        strings,
        expressions,
        expression_types,
        expression_spans,
        types,
        errors,
    );
    let param_types = match &types[fn_type] {
        Fn { params, .. } => params.clone(),
        _ => Rc::new(Vec::new()),
    };

    let arg_types: Vec<typ::Index> = args
        .clone()
        .map(|arg| {
            infer_rec(
                arg,
                state,
                env,
                types_env,
                strings,
                expressions,
                expression_types,
                expression_spans,
                types,
                errors,
            )
        })
        .collect();

    let return_type = types.push_and_get_key(state.new_type_var());

    let call_type: typ::Index = types.push_and_get_key(Fn {
        params: Rc::new(arg_types.clone()),
        ret: return_type,
    });

    // Unify the arguments separately for nicer error messages
    let res = arg_types
        .iter()
        .zip(args)
        .zip(param_types.iter())
        .fold(Ok(()), |result, ((arg_type, arg), param_type)| {
            result.and_then(|_| {
                unify(
                    state,
                    expression_spans[arg],
                    *arg_type,
                    None,
                    *param_type,
                    types,
                )
            })
        })
        // If there weren't any failures, unify the Fn and return types
        .and_then(|_| {
            unify(
                state,
                expression_spans[expression],
                call_type,
                None,
                fn_type,
                types,
            )
        });
    add_error(res, errors);

    return_type
}

fn add_error(result: Result<(), Error>, errors: &mut Vec<Error>) {
    if let Err(e) = result {
        errors.push(e);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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

        #[test]
        fn test_pattern_matching_floats() {
            assert_snapshot!(infer(
                "\
when 5 is
    1 -> True
    1.2 -> True
    _ -> False
            "
            ));
        }

        #[test]
        fn test_pattern_matching_strings() {
            assert_snapshot!(infer(
                "\
when \"hi\" is
    \"hi\" -> True
    \"\" -> False
            "
            ));
        }

        #[test]
        fn test_pattern_matching_expr_type_matches_branches_type() {
            assert_snapshot!(infer(
                "\
when 5 is
    1 -> True
            "
            ));
            assert_snapshot!(infer(
                "\
when True is
    1 -> True
    \"hi\" -> True
            "
            ));
            assert_snapshot!(infer(
                "\
when {x = 1} is
    True -> True
            "
            ));
        }

        #[test]
        fn test_pattern_matching_all_branches_return_type() {
            assert_snapshot!(infer(
                "\
when 5 is
    1 -> True
    1.2 -> 1 == 1
            "
            ));
            assert_snapshot!(infer(
                "\
when 5 is
    1 -> True
    1.2 -> 2
            "
            ));
        }

        #[test]
        fn test_pattern_matching_named_pattern() {
            assert_snapshot!(infer(
                "\
when 5 is
    1 as a -> a
            "
            ));
        }

        #[test]
        fn test_pattern_matching_or_pattern() {
            assert_snapshot!(infer(
                "\
when 5 is
    1 | 2 | 3 -> True
    _ -> False
            "
            ));
            assert_snapshot!(infer(
                "\
when 5 is
    1 | False | 3 -> True
    _ -> False
            "
            ));
            assert_snapshot!(infer(
                "\
let
    f x = when x is
        1 | False | 3 -> True
        _ -> False
f 5
            "
            ));
            assert_snapshot!(infer(
                "\
when True is
    1 | 2 | 3 -> True
    _ -> False
            "
            ));
            assert_snapshot!(infer(
                "\
when 1 is
    a | 2 as a | 3 as a -> True
            "
            ));
            assert_snapshot!(infer(
                "\
when 1 is
    a | 2 as b | 3 as c -> True
            "
            ));
        }

        #[test]
        fn test_pattern_matching_if_condition() {
            assert_snapshot!(infer(
                "\
when 5 is
    n if n > 3 -> n
            "
            ));
            assert_snapshot!(infer(
                "\
when 5 is
    n if n - 5 -> n
            "
            ));
        }

        #[test]
        fn test_pattern_matching_multiple_patterns() {
            assert_snapshot!(infer(
                "\
when 5, False is
    x, y -> x > 3 and y
"
            ));
            assert_snapshot!(infer(
                "\
when 5, 3, 4 is
    x, y, z if x + y + z > 0 -> x + y - z
"
            ));
        }

        #[test]
        fn test_pattern_matching_records() {
            assert_snapshot!(infer(
                "\
when {x: 5} is
    {x} -> x
"
            ));
            assert_snapshot!(infer(
                "\
when 5 is
    {x} -> x
"
            ));
            assert_snapshot!(infer(
                "\
when {x: 5, y: 3} is
    {x} -> x
"
            ));
            assert_snapshot!(infer(
                "\
when {x: 5, y: 3} is
    {x: 5 | 7, y} -> y
    {x: ( 1 | 3 ) as x} -> x
"
            ));
        }

        fn infer(code: &str) -> String {
            let mut compiler_state = compiler::State::new();
            let alma_source = Source::new_file("Alma.alma").unwrap();
            let source = Source::new(
                "Test.alma",
                format!(
                    "\
module Test exposing (test)

test = {}",
                    code.to_string()
                ),
            );

            let entry_sources =
                compiler::stages::process_sources(vec![alma_source, source], &mut compiler_state);

            let entry_modules =
                compiler::stages::parse_files(&entry_sources, &mut compiler_state).unwrap();

            let actual = match compiler::stages::infer(entry_modules, &mut compiler_state) {
                Ok(()) => {
                    let mut out = String::new();
                    compiler_state.types_to_string(&mut out);
                    out
                }
                Err(err) => err,
            };

            format!("Input:\n\n{code}\n\n---\nOutput:\n\n{actual}")
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
module Test

import Test.TestInner exposing (test)

add = \x -> x + test

module Test.TestInner exposing (test)

    test = 5
"#
            ));
            assert_snapshot!(infer(
                r#"
module Test

import Test.TestInner exposing (test)

add = \x -> x + test

module Test.TestInner exposing (test)

    test = "hi"
"#
            ));
            assert_snapshot!(infer(
                r#"
module Test

import Test.TestInner exposing (nope)

add = \x -> x + test

module Test.TestInner exposing (test)

    test = "hi"
"#
            ));
            assert_snapshot!(infer(
                r#"
module Test

import Test.TestInner exposing (test)

add = \x -> x + test x

module Test.TestInner exposing (test)

    test = \x -> x + "hi"
"#
            ));
        }

        #[test]
        fn test_import_nonexistent_module() {
            assert_snapshot!(infer(
                r#"
module Test

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
module Test exposing (new)

import Test.Id

new = { id = Test.Id.new }

module Test.Id exposing (new)
    new = 42
"
            ));
            assert_snapshot!(infer(
                "\
module Test exposing (new)

import Test.Id

new = Test.Id

module Test.Id exposing (new)
    new = 42
"
            ));
        }

        #[test]
        fn test_import_alias() {
            assert_snapshot!(infer(
                "\
module Test exposing (new)

import Test.Id as UserId

new = { id = UserId.new }

module Test.Id exposing (new)
    new = 42
"
            ));
            assert_snapshot!(infer(
                "\
module Test exposing (new)

import Test.Id as UserId

new = { id = Test.Id.new }

module Test.Id exposing (new)
    new = 42
"
            ));
        }

        #[test]
        fn test_import_nested_fully_qualified_name() {
            assert_snapshot!(infer(
                "\
module Test exposing (new)

import Test.Attributes
import Test.Attributes.Id

new = { id = Test.Attributes.Id.new }

module Test.Attributes
    module Test.Attributes.Id exposing (new)
        new = 42
"
            ));
        }

        #[test]
        fn test_use_nonexistent_import_from_fully_qualified_module() {
            assert_snapshot!(infer(
                "\
module Test exposing (new)

import Test.Id

new = Test.Id.wat

module Test.Id exposing (new)
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

alias Math num =
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

alias Math num =
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
        fn test_function_signature_only_definition_usage_from_the_same_module() {
            assert_snapshot!(infer(
                "\
module Test exposing (main)

test : Float -> Float

main = test 5
"
            ));
        }

        #[test]
        fn test_function_signature_only_definition_usage_from_another_module() {
            assert_snapshot!(infer(
                "\
module Test exposing (main)

import Test.Help exposing (test)

module Test.Help exposing (test)
    test : Float -> Float

main = test 5
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

alias List a = { x : a }

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

alias List a = { x : a }

main : List
main = { x : 1 }
"
            ));
            assert_snapshot!(infer(
                "\
module Test exposing (main)

alias List a = { x : a }

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
    alias Record a = { x : a }

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

        #[test]
        fn test_external_function_signature() {
            assert_snapshot!(infer(
                "\
module Test exposing (test)

external test : Float -> String
"
            ));
        }

        #[test]
        fn test_external_function_signature_usage_from_the_same_module() {
            assert_snapshot!(infer(
                "\
module Test exposing (main)

external test : Float -> String

main = test 5
"
            ));
        }

        #[test]
        fn test_external_function_signature_usage_from_another_module() {
            assert_snapshot!(infer(
                "\
module Test exposing (main)

import Test.Help exposing (test)

module Test.Help exposing (test)
    external test : Float -> String

main = test 5
"
            ));
        }

        #[test]
        fn test_pattern_matching_constructor_types_wrong_arity() {
            assert_snapshot!(infer(
                "\
module Test exposing (test)

type Test a = X a | Y

test = when X 1 is
    X -> True
"
            ));
            assert_snapshot!(infer(
                "\
module Test exposing (test)

type Test a = X a | Y

test = when X 1 is
    X a b -> a + b
"
            ));
            assert_snapshot!(infer(
                "\
module Test exposing (test)

type Test a = X a | Y

test = when X 1 is
    X a -> a
"
            ));
            assert_snapshot!(infer(
                "\
module Test exposing (test)

type Test a = X a | Y

test = when X 1 is
    Y a -> a
"
            ));
            assert_snapshot!(infer(
                "\
module Test exposing (test)

type Test a = X a | Y

test = when X 1 is
    Y -> True
"
            ));
        }

        #[test]
        fn test_patterns_in_function_parameters() {
            assert_snapshot!(infer(
                "\
module Test exposing (main)

type Test a = Test a

main (Test a) = a
"
            ));
        }

        #[test]
        fn test_patterns_in_top_level_definitions() {
            assert_snapshot!(infer(
                "\
module Test exposing (main)

type Test a = Test a

Test a = Test 1

main = a
"
            ));
        }

        #[test]
        fn test_qualified_constructor_in_pattern_matching() {
            assert_snapshot!(infer(
                "\
module Test exposing (main)

import Test.Types

module Test.Types exposing (Test(Test))
    type Test a = Test a

Test.Types.Test a = Test.Types.Test 1

test = when Test.Types.Test 2 is
    Test.Types.Test a -> a

main = a + test
"
            ));
            assert_snapshot!(infer(
                "\
module Test exposing (main)

import Test.Types as Types

module Test.Types exposing (Test(Test))
    type Test a = Test a

Types.Test a = Types.Test 1

test = when Types.Test 2 is
    Types.Test a -> a

main = a + test
"
            ));
        }

        #[test]
        fn test_introduced_bindings_type_in_pattern_matching() {
            assert_snapshot!(infer(
                "\
module Test exposing (test)

type Option a = Some a | None 

test = when Some (Some 1) is
    Some a | Some (Some a) -> a
"
            ));
            assert_snapshot!(infer(
                "\
module Test exposing (test)

type List a = Cons a (List a) | Nil

test = when Nil is
    Cons a Nil |
    Cons _ (Cons a Nil) |
    Cons _ (Cons _ (Cons a Nil)) -> a
    _ -> 5
"
            ));
        }

        #[test]
        fn test_infinite_type_shenanigans_in_pattern_matching() {
            assert_snapshot!(infer(
                "\
module Test exposing (test)

type List a = Cons a (List a) | Nil

test = when Nil is
    Cons a Nil -> a
    Cons (Cons a Nil) Nil -> a
    Cons (Cons (Cons a Nil) Nil) Nil -> a
    _ -> 5
"
            ));
        }

        fn infer(code: &str) -> String {
            let mut state = compiler::State::new();
            let alma_source = Source::new_file("Alma.alma").unwrap();
            let source = Source::new("Test.alma", code.to_string());

            let entry_sources =
                compiler::stages::process_sources(vec![alma_source, source], &mut state);

            let entry_modules = compiler::stages::parse_files(&entry_sources, &mut state).unwrap();

            let actual = match compiler::stages::infer(entry_modules, &mut state) {
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
