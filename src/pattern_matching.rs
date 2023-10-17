use crate::{
    ast::expression::{self, IdentifierName, Pattern, PatternData},
    compiler,
    strings::Strings,
    typ::{self, Type, Types},
};
use fnv::{FnvHashMap, FnvHashSet};

#[derive(Debug)]
pub struct CompiledPatternMatch {
    pub tree: Decision,
    pub diagnostics: Diagnostics,
}

impl CompiledPatternMatch {
    /// Returns a list of patterns not covered by the match expression.
    pub fn missing_patterns(&self, compiler: &compiler::State) -> Vec<String> {
        let mut names = FnvHashSet::default();
        let mut steps = Vec::new();

        self.add_missing_patterns(&self.tree, &mut steps, &mut names, compiler);

        let mut missing: Vec<String> = names.into_iter().collect();

        // Sorting isn't necessary, but it makes it a bit easier to write tests.
        missing.sort();
        missing
    }

    fn add_missing_patterns(
        &self,
        node: &Decision,
        missing_patterns: &mut Vec<MissingPattern>,
        missing: &mut FnvHashSet<String>,
        compiler: &compiler::State,
    ) {
        match node {
            Decision::Success(_) => {}
            Decision::Failure => {
                let mut mapping = FnvHashMap::default();

                // At this point the missing_patterns stack looks something like this:
                // `[missing_pattern, missing_pattern + arguments, missing_pattern, ...]`. To construct a pattern
                // name from this stack, we first map all variables to their
                // missing_pattern indexes. This is needed because when a missing_pattern defines
                // arguments, the missing_patterns for those arguments don't necessarily
                // appear in order in the missing_pattern stack.
                //
                // This mapping is then used when (recursively) generating a
                // pattern name.
                //
                // This approach could probably be done more efficiently, so if
                // you're reading this and happen to know of a way, please
                // submit a merge request :)
                for (index, step) in missing_patterns.iter().enumerate() {
                    mapping.insert(&step.variable, index);
                }

                let name = missing_patterns
                    .first()
                    .map(|missing_pattern| {
                        missing_pattern.pattern_name(missing_patterns, &mapping, &compiler.strings)
                    })
                    .unwrap_or_else(|| "_".to_string());

                missing.insert(name);
            }
            Decision::Guard(_, _, fallback) => {
                self.add_missing_patterns(&*fallback, missing_patterns, missing, compiler);
            }
            Decision::Switch(var, cases, fallback) => {
                for case in cases {
                    match &case.condition {
                        // Constructor::True => {
                        //     let name = "true".to_string();

                        //     missing_patterns.push(MissingPattern::new(*var, name, Vec::new()));
                        // }
                        // Constructor::False => {
                        //     let name = "false".to_string();

                        //     missing_patterns.push(MissingPattern::new(*var, name, Vec::new()));
                        // }
                        // Constructor::Int(_) | Constructor::Range(_, _) => {
                        //     let name = "_".to_string();

                        //     missing_patterns.push(MissingPattern::new(*var, name, Vec::new()));
                        // }
                        // Constructor::Pair(_, _) => {
                        //     let args = case.arguments.clone();

                        //     missing_patterns.push(MissingPattern::new(
                        //         *var,
                        //         String::new(),
                        //         args,
                        //     ));
                        // }
                        DecisionCaseCondition::Constructor(name, _idx) => {
                            let args = case.arguments.clone();
                            missing_patterns.push(MissingPattern::new(*var, *name, args));
                        }
                    }

                    self.add_missing_patterns(&case.body, missing_patterns, missing, compiler);
                    missing_patterns.pop();
                }

                if let Some(node) = fallback {
                    self.add_missing_patterns(&*node, missing_patterns, missing, compiler);
                }
            }
        }
    }
}

/// Information about a single constructor/value (aka missing_pattern) being tested, used
/// to build a list of names of missing patterns.
#[derive(Debug)]
struct MissingPattern {
    variable: PatternVariable,
    name: IdentifierName,
    arguments: Vec<PatternVariable>,
}

impl MissingPattern {
    fn new(
        variable: PatternVariable,
        name: IdentifierName,
        arguments: Vec<PatternVariable>,
    ) -> Self {
        Self {
            variable,
            name,
            arguments,
        }
    }

    fn pattern_name(
        &self,
        terms: &[MissingPattern],
        mapping: &FnvHashMap<&PatternVariable, usize>,
        strings: &Strings,
    ) -> String {
        let name = strings.resolve(self.name);
        if self.arguments.is_empty() {
            name.to_owned()
        } else {
            let args = self
                .arguments
                .iter()
                .map(|arg| {
                    mapping
                        .get(arg)
                        .map(|&idx| terms[idx].pattern_name(terms, mapping, strings))
                        .unwrap_or_else(|| "_".to_string())
                })
                .collect::<Vec<_>>()
                .join(", ");

            format!("{}({})", name, args)
        }
    }
}

#[derive(Debug)]
pub enum Decision {
    Success(Body),
    Failure,
    Guard(expression::Index, Body, Box<Decision>),
    Switch(
        PatternVariable,
        Vec<DecisionSwitchCase>,
        Option<Box<Decision>>,
    ),
}

#[derive(Debug)]
pub struct DecisionSwitchCase {
    condition: DecisionCaseCondition,
    arguments: Vec<PatternVariable>,
    body: Decision,
}

impl DecisionSwitchCase {
    fn new(
        condition: DecisionCaseCondition,
        arguments: Vec<PatternVariable>,
        body: Decision,
    ) -> Self {
        Self {
            condition,
            arguments,
            body,
        }
    }
}

#[derive(Debug)]
pub enum DecisionCaseCondition {
    Constructor(IdentifierName, usize),
}

#[derive(Debug)]
pub struct Diagnostics {
    incomplete_pattern_match: bool,
    reachable_branches: Vec<expression::Index>,
}

#[derive(PartialEq, Hash, Eq, Debug, Copy, Clone)]
pub struct PatternVariable {
    id: usize,
    type_id: typ::Index,
}

#[derive(Clone, Debug)]
pub struct Body {
    bindings: Vec<(IdentifierName, PatternVariable)>,
    expression: expression::Index,
}

#[derive(Clone)]
pub struct Row {
    columns: Vec<Column>,
    guard: Option<expression::Index>,
    body: Body,
}

impl Row {
    fn new(columns: Vec<Column>, guard: Option<expression::Index>, body: Body) -> Self {
        Self {
            columns,
            guard,
            body,
        }
    }

    fn remove_column(&mut self, variable: &PatternVariable) -> Option<Column> {
        self.columns
            .iter()
            .position(|c| &c.variable == variable)
            .map(|idx| self.columns.remove(idx))
    }
}

#[derive(Clone)]
pub struct Column {
    variable: PatternVariable,
    pattern: Pattern,
}

impl Column {
    fn new(variable: PatternVariable, pattern: Pattern) -> Self {
        Self { variable, pattern }
    }
}

pub struct Compiler<'a> {
    variable_id: usize,
    types: &'a Types,
    compiler_state: &'a compiler::State,
    diagnostics: Diagnostics,
}

impl<'a> Compiler<'a> {
    pub fn new(types: &'a Types, compiler_state: &'a compiler::State) -> Self {
        Self {
            variable_id: 0,
            types,
            compiler_state,
            diagnostics: Diagnostics {
                incomplete_pattern_match: false,
                reachable_branches: Vec::new(),
            },
        }
    }

    pub fn compile(
        mut self,
        types: Vec<typ::Index>,
        branches: Vec<(Vec<Pattern>, Body)>,
    ) -> CompiledPatternMatch {
        let mut rows = Vec::new();
        for (patterns, body) in branches {
            let mut columns = vec![];
            for (typ, pattern) in types.iter().zip(patterns) {
                let var = self.new_variable(*typ);
                columns.push(Column::new(var, pattern));
            }
            rows.push(Row::new(columns, None, body));
        }

        CompiledPatternMatch {
            tree: self.compile_rows(rows),
            diagnostics: self.diagnostics,
        }
    }

    fn compile_rows(&mut self, rows: Vec<Row>) -> Decision {
        if rows.is_empty() {
            self.diagnostics.incomplete_pattern_match = true;

            return Decision::Failure;
        }

        let mut rows = rows
            .into_iter()
            .map(|row| self.move_variable_patterns(row))
            .collect::<Vec<_>>();

        // There may be multiple rows, but if the first one has no patterns
        // those extra rows are redundant, as a row without columns/patterns
        // always matches.
        if rows.first().map_or(false, |c| c.columns.is_empty()) {
            let row = rows.remove(0);

            self.diagnostics
                .reachable_branches
                .push(row.body.expression);

            return self.get_success_or_guard(row, rows);
        }

        let branch_var = self.branch_variable(&rows[0], &rows);

        match self.variable_type(branch_var) {
            Type::Named { module, name, .. } => {
                let cases = self
                    .compiler_state
                    .module_interfaces
                    .get(
                        *self
                            .compiler_state
                            .module_name_to_module_idx
                            .get(module)
                            .unwrap(),
                    )
                    .unwrap()
                    .as_ref()
                    .unwrap()
                    .type_constructors
                    .get(name)
                    .unwrap()
                    .map()
                    .iter()
                    .enumerate()
                    .map(|(_idx, (name, poly_type))| {
                        (
                            *name,
                            match &self.types[poly_type.typ] {
                                Type::Fn { params, .. } => Some(self.new_variables(params)),
                                Type::Named { .. } => None,
                                _ => panic!("Expected a constructor type to be a function"),
                            },
                            Vec::new(),
                        )
                    })
                    .collect();

                Decision::Switch(
                    branch_var,
                    self.compile_constructor_cases(rows, branch_var, cases),
                    None,
                )
            }
            Type::Var(_) => todo!(),
            // Can get here with named patterns and identifier patterns
            Type::Fn { params: _, ret: _ } => todo!(),
            Type::Record { fields: _ } => todo!(),
            Type::RecordExt {
                fields: _,
                base_record: _,
            } => todo!(),
            Type::Alias {
                module: _,
                name: _,
                params: _,
                destination: _,
            } => todo!(),
        }
    }

    /*
    /// Compiles the cases and fallback cases for integer and range patterns.
    ///
    /// Integers have an infinite number of constructors, so we specialise the
    /// compilation of integer and range patterns.
    fn compile_int_cases(
        &mut self,
        rows: Vec<Row>,
        branch_var: PatternVariable,
    ) -> (Vec<DecisionSwitchCase>, Box<Decision>) {
        let mut raw_cases: Vec<(Constructor, Vec<PatternVariable>, Vec<Row>)> = Vec::new();
        let mut fallback_rows = Vec::new();
        let mut tested: FnvHashMap<(i64, i64), usize> = FnvHashMap::default();

        for mut row in rows {
            if let Some(col) = row.remove_column(&branch_var) {
                for (pat, row) in flatten_or(col.pattern, row) {
                    let (key, cons) = match pat {
                        Pattern::Int(val) => ((val, val), Constructor::Int(val)),
                        Pattern::Range(start, stop) => {
                            ((start, stop), Constructor::Range(start, stop))
                        }
                        _ => unreachable!(),
                    };

                    if let Some(index) = tested.get(&key) {
                        raw_cases[*index].2.push(row);
                        continue;
                    }

                    tested.insert(key, raw_cases.len());
                    raw_cases.push((cons, Vec::new(), vec![row]));
                }
            } else {
                fallback_rows.push(row);
            }
        }

        for (_, _, rows) in &mut raw_cases {
            rows.append(&mut fallback_rows.clone());
        }

        let cases = raw_cases
            .into_iter()
            .map(|(cons, vars, rows)| {
                DecisionSwitchCase::new(cons, vars, self.compile_rows(rows))
            })
            .collect();

        (cases, Box::new(self.compile_rows(fallback_rows)))
    }
    */

    /// Compiles the cases and sub cases for the constructor located at the
    /// column of the branching variable.
    ///
    /// What exactly this method does may be a bit hard to understand from the
    /// code, as there's simply quite a bit going on. Roughly speaking, it does
    /// the following:
    ///
    /// 1. It takes the column we're branching on (based on the branching
    ///    variable) and removes it from every row.
    /// 2. We add additional columns to this row, if the constructor takes any
    ///    arguments (which we'll handle in a nested match).
    /// 3. We turn the resulting list of rows into a list of cases, then compile
    ///    those into decision (sub) trees.
    ///
    /// If a row didn't include the branching variable, we simply copy that row
    /// into the list of rows for every constructor to test.
    ///
    /// For this to work, the `cases` variable must be prepared such that it has
    /// a triple for every constructor we need to handle. For an ADT with 10
    /// constructors, that means 10 triples. This is needed so this method can
    /// assign the correct sub matches to these constructors.
    ///
    /// Types with infinite constructors (e.g. int and string) are handled
    /// separately; they don't need most of this work anyway.
    fn compile_constructor_cases(
        &mut self,
        rows: Vec<Row>,
        branch_var: PatternVariable,
        mut cases: Vec<(IdentifierName, Option<Vec<PatternVariable>>, Vec<Row>)>,
    ) -> Vec<DecisionSwitchCase> {
        for mut row in rows {
            if let Some(col) = row.remove_column(&branch_var) {
                for (pat, row) in flatten_or(col.pattern, row) {
                    if let PatternData::Type {
                        constructor,
                        params,
                        ..
                    } = &pat.data
                    {
                        let idx = cases
                            .iter()
                            .position(|(c, _, _)| *c == constructor.name)
                            .unwrap();
                        let mut cols = row.columns;

                        if let Some(type_params) = &cases[idx].1 {
                            for (var, pat) in type_params.iter().zip(params.iter()) {
                                cols.push(Column::new(*var, pat.clone()));
                            }
                        }

                        cases[idx].2.push(Row::new(cols, row.guard, row.body));
                    }
                }
            } else {
                for (_, _, rows) in &mut cases {
                    rows.push(row.clone());
                }
            }
        }

        cases
            .into_iter()
            .enumerate()
            .map(|(idx, (constructor, vars, rows))| {
                DecisionSwitchCase::new(
                    DecisionCaseCondition::Constructor(constructor, idx),
                    vars.unwrap_or(Vec::new()),
                    self.compile_rows(rows),
                )
            })
            .collect()
    }

    fn get_success_or_guard(&mut self, row: Row, rows: Vec<Row>) -> Decision {
        return if let Some(guard) = row.guard {
            Decision::Guard(guard, row.body, Box::new(self.compile_rows(rows)))
        } else {
            Decision::Success(row.body)
        };
    }

    /// Moves variable-only patterns/tests into the right-hand side/body of a
    /// case.
    ///
    /// This turns cases like this:
    ///
    ///     case foo -> print(foo)
    ///
    /// Into this:
    ///
    ///     case -> {
    ///       let foo = it
    ///       print(foo)
    ///     }
    ///
    /// Where `it` is a variable holding the value `case foo` is compared
    /// against, and the case/row has no patterns (i.e. always matches).
    fn move_variable_patterns(&self, row: Row) -> Row {
        let mut bindings = row.body.bindings;

        for col in &row.columns {
            if let PatternData::Named { name, .. } | PatternData::Identifier(name) =
                &col.pattern.data
            {
                bindings.push((name.name, col.variable));
            }
        }

        let columns = row
            .columns
            .into_iter()
            .filter(|col| !matches!(col.pattern.data, PatternData::Identifier(_)))
            .collect();

        Row {
            columns,
            guard: row.guard,
            body: Body {
                bindings,
                expression: row.body.expression,
            },
        }
    }

    fn branch_variable(&self, row: &Row, rows: &[Row]) -> PatternVariable {
        let mut counts = FnvHashMap::default();

        for row in rows {
            for col in &row.columns {
                *counts.entry(&col.variable).or_insert(0_usize) += 1
            }
        }

        row.columns
            .iter()
            .map(|col| col.variable)
            .max_by_key(|var| counts[var])
            .unwrap()
    }

    fn new_variable(&mut self, type_id: typ::Index) -> PatternVariable {
        let var = PatternVariable {
            id: self.variable_id,
            type_id,
        };

        self.variable_id += 1;
        var
    }

    fn new_variables(&mut self, type_ids: &[typ::Index]) -> Vec<PatternVariable> {
        type_ids.iter().map(|t| self.new_variable(*t)).collect()
    }

    fn variable_type(&self, id: PatternVariable) -> &Type {
        &self.types[id.type_id]
    }
}

fn flatten_or(pattern: Pattern, row: Row) -> Vec<(Pattern, Row)> {
    if let PatternData::Or(args) = pattern.data {
        args.into_iter().map(|p| (p, row.clone())).collect()
    } else {
        vec![(pattern, row)]
    }
}
