use crate::ast::expression::{self, binop, Expressions, IdentifierName};
use crate::compiler::state::ModuleIndex;
use crate::module_interface::ModuleInterface;
use crate::strings::Strings;
use crate::{
    ast::{
        expression::{Expression as E, Lambda, Pattern, PatternData as P, UnaryData as U},
        types, Definition, ExportData, Module, ModuleName, TypedDefinition,
    },
    compiler,
};
use fnv::FnvHashSet;
use pathdiff::diff_paths;
use std::cmp::min;
use std::fmt::Write;
use std::path::{Path, PathBuf};

const INDENT: usize = 4;
const UNION_TYPE_FIELD: &str = "__tag";

pub enum OutputFile {
    File(String),
    CopyFrom(String),
}

struct State<'a> {
    code: String,
    indent: usize,
    path: &'a PathBuf,
    module_output_path: PathBuf,
    output: &'a Path,
    compiler_state: &'a compiler::State,
    module: &'a Module,
    interface: &'a ModuleInterface,
    expressions: &'a Expressions,
    strings: &'a Strings,
}
impl<'a> State<'a> {
    fn new(
        module_idx: ModuleIndex,
        output: &'a Path,
        compiler_state: &'a compiler::State,
        strings: &'a Strings,
    ) -> Self {
        let module = &compiler_state.modules[module_idx];
        let path = &compiler_state.sources[compiler_state.module_to_source_idx[module_idx]].path;
        let module_output_path = output
            .join(&path)
            .with_file_name(&format!("{}.js", module.name.to_string(strings)));
        let interface = compiler_state.module_interfaces[module_idx]
            .as_ref()
            .unwrap_or_else(|| {
                let name = module.name.to_string(strings);
                panic!("Couldn't find the types for module {name}");
            });
        let expressions = &module.expressions;

        Self {
            indent: 0,
            code: String::new(),
            path,
            module_output_path,
            output,
            compiler_state,
            module,
            interface,
            expressions,
            strings,
        }
    }

    fn indent(&mut self) {
        self.indent += INDENT;
    }
    fn outdent(&mut self) {
        self.indent -= INDENT;
    }
}

pub fn generate(
    sorted_modules: &[ModuleIndex],
    state: &compiler::State,
    output: &Path,
) -> Vec<(String, OutputFile)> {
    let mut files = vec![];
    let strings = &state.strings;

    for module_idx in sorted_modules {
        let mut state = State::new(*module_idx, output, state, strings);

        // Print types
        {
            state.code.push_str("/*\n\nmodule ");
            state
                .code
                .push_str(state.module.name.to_string(state.strings));
            state.code.push_str("\n\n");
            state
                .interface
                .write_to_string(&mut state.code, strings, &state.compiler_state.types);

            state.code.push_str(
                "\n\n*/
\n\n",
            );
        }

        let maybe_out_ffi_file = generate_file(&mut state);

        if let Some(out_ffi_file) = maybe_out_ffi_file {
            let ffi_file = {
                let ffi_path = state.path.with_file_name(module_ffi_file_name(
                    state
                        .module_output_path
                        .file_stem()
                        .unwrap()
                        .to_str()
                        .unwrap(),
                ));
                ffi_path.to_str().unwrap().to_owned()
            };
            files.push((
                out_ffi_file,
                if ffi_file == "Alma.ffi.js" {
                    OutputFile::File(include_str!("./alma/Alma.ffi.js").to_owned())
                } else {
                    OutputFile::CopyFrom(ffi_file)
                },
            ));
        }

        files.push((
            state.module_output_path.to_str().unwrap().to_owned(),
            OutputFile::File(state.code),
        ))
    }

    files
}

fn generate_file(state: &mut State) -> Option<String> {
    let mut ffi_file = None;

    if state.module.has_externals() {
        state.code.push_str("import * as ");
        module_ffi_full_name(&mut state.code, &state.module.name, state.strings);

        let ffi_path = state
            .module_output_path
            .with_file_name(module_ffi_file_name(
                state
                    .module_output_path
                    .file_stem()
                    .unwrap()
                    .to_str()
                    .unwrap(),
            ));
        let module_output_dir = state.module_output_path.parent().unwrap();
        let relative_path = get_relative_path(module_output_dir, &ffi_path);
        let ffi_path = ffi_path.to_str().unwrap();
        let relative_path = relative_path.to_str().unwrap();
        generate_import_from_part_with_newline(&mut state.code, relative_path);

        ffi_file = Some(ffi_path.to_owned());
    }

    if !state.module.imports.is_empty() {
        state.code.push('\n');
        generate_imports(state);
    }

    if !state.module.type_definitions.is_empty() {
        state.code.push('\n');
        generate_types(state);
    }

    if !state.module.definitions.is_empty() {
        state.code.push('\n');
        generate_definitions(state, &state.module.definitions, true);
    }

    if !state.module.exports.is_empty() {
        state.code.push('\n');
        generate_exports(state);
    }

    ffi_file
}

fn generate_imports(state: &mut State) {
    let module_output_dir = state.module_output_path.parent().unwrap().to_path_buf();
    for import in &state.module.imports {
        let import = &import;
        let import_output_path = {
            let compiler_state = state.compiler_state;
            let path = compiler_state.sources[compiler_state.module_to_source_idx[*compiler_state
                .module_name_to_module_idx
                .get(&import.module_name.full_name)
                .unwrap()]]
            .path
            .with_file_name(format!(
                "{}.js",
                import.module_name.to_string(state.strings)
            ));
            let path = state.output.to_path_buf().join(path);
            get_relative_path(&module_output_dir, &path)
        };
        let import_output_path = import_output_path.to_str().unwrap();

        indented(state, "");
        state.code.push_str("import * as ");
        match &import.alias {
            Some(alias) => {
                let alias = alias.to_string(state.strings);
                state.code.push_str(alias);
            }
            None => {
                module_full_name(&mut state.code, &import.module_name, state.strings);
            }
        }
        generate_import_from_part_with_newline(&mut state.code, import_output_path);

        if !import.exposing.is_empty() {
            indented(state, "import { ");
            let mut first = true;
            for exposing in &import.exposing {
                match &exposing.typ {
                    ExportData::Identifier(identifier) => {
                        if !first {
                            state.code.push_str(", ");
                        } else {
                            first = false;
                        }
                        state.code.push_str(identifier.to_string(state.strings));
                    }
                    ExportData::Type { constructors, .. } => {
                        if !constructors.is_empty() {
                            if !first {
                                state.code.push_str(", ");
                            } else {
                                first = false;
                            }
                            for (i, constructor) in constructors.iter().enumerate() {
                                if i > 0 {
                                    state.code.push_str(", ");
                                }
                                state.code.push_str(constructor.to_string(state.strings));
                            }
                        }
                    }
                }
            }
            state.code.push_str(" }");
            generate_import_from_part_with_newline(&mut state.code, import_output_path);
        }
    }
}

fn generate_import_from_part_with_newline(code: &mut String, path: &str) {
    let dot_slash = if path.starts_with("./") || path.starts_with("../") || path.starts_with('/') {
        ""
    } else {
        "./"
    };
    writeln!(code, " from \"{dot_slash}{path}\"").unwrap();
}

fn generate_union_discriminant_field_value(code: &mut String, constructor_name: &str) {
    write!(code, "\"{constructor_name}\"").unwrap();
}

fn generate_types(state: &mut State) {
    for (i, type_def) in state.module.type_definitions.iter().enumerate() {
        let type_def = &type_def;
        if i > 0 {
            state.code.push('\n')
        }
        match &type_def.typ {
            types::TypeDefinitionData::Empty => (),
            types::TypeDefinitionData::Union { constructors } => {
                let type_name = type_def.name.to_string(state.strings);
                indented(state, "");
                writeln!(state.code, "// type {type_name}\n").unwrap();

                let max_params = constructors
                    .iter()
                    .map(|c| c.params.len())
                    .max()
                    .unwrap_or(0);

                for (i, constructor) in constructors.iter().enumerate() {
                    if i > 0 {
                        state.code.push('\n')
                    }
                    let constructor = &constructor;
                    let constructor_name = constructor.name.to_string(state.strings);
                    let num_params = constructor.params.len();

                    if num_params > 0 {
                        indented(state, "function ");
                        state.code.push_str(constructor_name);
                        state.code.push('(');
                        for i in 0..min(num_params, max_params) {
                            if i > 0 {
                                state.code.push_str(", ");
                            }
                            generate_union_field(&mut state.code, i);
                        }
                        state.code.push_str(") {\n");

                        {
                            state.indent();
                            line(state, "return {");
                            {
                                state.indent();
                                indented(state, "");
                                write!(state.code, "{UNION_TYPE_FIELD}: ").unwrap();
                                generate_union_discriminant_field_value(
                                    &mut state.code,
                                    constructor_name,
                                );
                                state.code.push_str(",\n");

                                for i in 0..max_params {
                                    indented(state, "");
                                    if i < num_params {
                                        generate_union_field(&mut state.code, i);
                                        state.code.push_str(",\n");
                                    } else {
                                        generate_union_field(&mut state.code, i);
                                        state.code.push_str(": null,\n");
                                    }
                                }
                                state.outdent();
                            }
                            line(state, "}");
                            state.outdent();
                        }
                    } else {
                        indented(state, "let ");
                        state.code.push_str(constructor_name);
                        state.code.push_str(" = {\n");
                        {
                            state.indent();
                            indented(state, "");
                            write!(state.code, "{UNION_TYPE_FIELD}: ").unwrap();
                            generate_union_discriminant_field_value(
                                &mut state.code,
                                constructor_name,
                            );
                            state.code.push_str(",\n");

                            for i in 0..max_params {
                                indented(state, "");
                                generate_union_field(&mut state.code, i);
                                state.code.push_str(": null,\n");
                            }
                            state.outdent();
                        }
                    }

                    line(state, "}");
                }
            }
            types::TypeDefinitionData::Record(_) => (),
        }
    }
}

fn generate_union_field(code: &mut String, i: usize) {
    write!(code, "_{i}").unwrap();
}

fn generate_definitions(state: &mut State, definitions: &[TypedDefinition], space_between: bool) {
    for (i, definition) in definitions.iter().enumerate() {
        if i > 0 && space_between {
            state.code.push('\n')
        }
        match definition {
            TypedDefinition::External(typ) => {
                let name = typ.name.to_string(state.strings);

                indented(state, "");
                write!(state.code, "let {name} = ").unwrap();
                module_ffi_full_name(&mut state.code, &state.module.name, state.strings);
                writeln!(state.code, ".{name}").unwrap();
            }
            TypedDefinition::TypeSignature(typ) => {
                indented(state, "");
                writeln!(
                    state.code,
                    "let {type_name} = new Error(\"Unimplemented\")",
                    type_name = typ.name.to_string(state.strings)
                )
                .unwrap();
            }
            TypedDefinition::Typed(_typ, def) => generate_definition(state, def),
            TypedDefinition::Untyped(def) => generate_definition(state, def),
        }
    }
}

fn generate_definition(state: &mut State, definition: &Definition) {
    match definition {
        Definition::Lambda(name, lambda) => {
            indented(state, "");
            generate_function(state, name.to_string(state.strings), lambda);
            state.code.push('\n');
        }
        Definition::Pattern(pattern, expression) => generate_let(state, pattern, *expression),
    }
}

fn generate_exports(state: &mut State) {
    line(state, "export {");

    state.indent();
    for export in &state.module.exports {
        match &export.typ {
            ExportData::Identifier(identifier) => {
                indented(state, "");
                writeln!(state.code, "{},", identifier.to_string(state.strings)).unwrap();
            }
            ExportData::Type { constructors, name } => {
                if !constructors.is_empty() {
                    indented(state, "");
                    writeln!(state.code, "// type {}", name.to_string(state.strings)).unwrap();
                }
                for constructor in constructors {
                    indented(state, "");
                    writeln!(state.code, "{},", constructor.to_string(state.strings)).unwrap();
                }
                if !constructors.is_empty() {
                    indented(state, "");
                    writeln!(state.code, "// end type {}", name.to_string(state.strings)).unwrap();
                }
            }
        }
    }
    state.outdent();

    line(state, "}");
}

fn generate_function(state: &mut State, name: &str, lambda: &Lambda) {
    write!(state.code, "function {name}(").unwrap();

    for (i, pattern) in lambda.parameters.iter().enumerate() {
        if i > 0 {
            state.code.push_str(", ");
        }
        if pattern_needs_bindings(pattern) {
            generate_binding_destructuring(&mut state.code, pattern, state.strings);
        } else {
            state.code.push('_');
        }
    }

    state.code.push_str(") {\n");

    {
        state.indent();
        indented(state, "return ");
        generate_expression(state, lambda.body);
        state.code.push('\n');
        state.outdent();
    }

    indented(state, "}");
}

fn generate_binding_destructuring(code: &mut String, pattern: &Pattern, strings: &Strings) {
    match &pattern.data {
        P::Identifier(identifier) => code.push_str(identifier.to_string(strings)),
        P::Type { params, .. } => {
            code.push_str("{ ");
            let mut first = true;
            for (i, param) in params.iter().enumerate() {
                if pattern_needs_bindings(param) {
                    if !first {
                        code.push_str(", ");
                    }
                    first = false;
                    generate_union_field(code, i);
                    code.push_str(": ");
                    generate_binding_destructuring(code, param, strings);
                }
            }
            code.push_str(" }");
        }
        P::Hole | P::String_(_) | P::Float(_) => (),
        P::Named { pattern, name } => {
            code.push_str(name.to_string(strings));
            if pattern_needs_bindings(pattern) {
                code.push_str(", ");
                generate_binding_destructuring(code, pattern, strings);
            }
        }
        P::Or(patterns) => {
            let mut first = true;
            for pattern in patterns {
                if pattern_needs_bindings(pattern) {
                    if !first {
                        code.push_str(", ");
                    }
                    first = false;
                    generate_binding_destructuring(code, pattern, strings);
                }
            }
        }
        P::Record(fields) => {
            code.push_str("{ ");
            let mut first = true;
            for (name, value) in fields {
                if pattern_needs_bindings(value) {
                    if !first {
                        code.push_str(", ");
                    }
                    first = false;
                    let name = name.to_string(strings);
                    write!(code, "{name}: ").unwrap();
                    generate_binding_destructuring(code, value, strings);
                }
            }
            code.push_str(" }");
        }
    }
}

fn generate_let(state: &mut State, pattern: &Pattern, expression: expression::Index) {
    indented(state, "");
    if pattern_needs_bindings(pattern) {
        state.code.push_str("let ");
        generate_binding_destructuring(&mut state.code, pattern, state.strings);
        state.code.push_str(" = ");
    }
    generate_expression(state, expression);
    state.code.push('\n');
}

fn generate_expression(state: &mut State, expression: expression::Index) {
    match &state.expressions.values[expression] {
        E::Float(float) => write!(state.code, "{float}").unwrap(),

        E::String_(string) => {
            let escaped_string = state.strings.resolve(*string).replace('\n', "\\n");
            write!(state.code, "\"{escaped_string}\"").unwrap()
        }

        E::Identifier { module, identifier } => {
            // TODO: Generate True and False as true and false
            if let Some(module) = module {
                module_full_name(&mut state.code, module, state.strings);
                state.code.push('.');
            }

            state.code.push_str(identifier.to_string(state.strings));
        }

        E::Record { fields } => {
            state.code.push_str("{\n");

            {
                state.indent();
                for (key, value) in fields {
                    indented(state, key.to_string(state.strings));
                    state.code.push_str(": ");
                    generate_expression(state, *value);
                    state.code.push_str(",\n");
                }
                state.outdent();
            }

            indented(state, "}");
        }

        E::RecordUpdate { record, fields } => {
            state.code.push_str("{\n");

            {
                state.indent();
                indented(state, "...");
                generate_expression(state, *record);
                state.code.push('\n');

                for (key, value) in fields {
                    indented(state, key.to_string(state.strings));
                    state.code.push_str(": ");
                    generate_expression(state, *value);
                    state.code.push_str(",\n");
                }
                state.outdent();
            }

            indented(state, "}");
        }

        E::PropertyAccess {
            expression,
            property,
        } => {
            generate_expression(state, *expression);
            state.code.push('.');
            state.code.push_str(property.to_string(state.strings));
        }

        E::PropertyAccessLambda { property } => {
            let property = property.to_string(state.strings);
            write!(state.code, "(r => r.{property})").unwrap()
        }

        E::Unary { op, expression } => {
            state.code.push(match &op.typ {
                U::Not => '!',
                U::Minus => '-',
            });
            generate_expression(state, *expression);
        }

        E::Binary {
            expression,
            op,
            arguments,
        } => {
            let binop = match op.typ {
                binop::Type::And => Some("&&"),
                binop::Type::Or => Some("||"),
                binop::Type::LessThan => Some("<"),
                binop::Type::LessEqualThan => Some("<="),
                binop::Type::GreaterThan => Some(">"),
                binop::Type::GreaterEqualThan => Some(">="),
                binop::Type::Addition => Some("+"),
                binop::Type::Substraction => Some("-"),
                binop::Type::Multiplication => Some("*"),
                binop::Type::Division => Some("/"),
                // TODO: Specialize equal and not equal too when the types are primitives
                _ => None,
            };

            if let Some(binop) = binop {
                let [left, right] = arguments;
                generate_binary_operator(state, binop, *left, *right)
            } else {
                generate_fn_call(state, *expression, arguments)
            }
        }

        E::Lambda(lambda) => {
            generate_function(state, "", lambda);
        }

        E::FnCall {
            function,
            arguments,
        } => generate_fn_call(state, *function, arguments),

        E::Let { definitions, body } => {
            state.code.push_str("function() {\n");

            {
                state.indent();
                generate_definitions(state, definitions, false);

                indented(state, "return ");
                generate_expression(state, *body);
                state.code.push('\n');
                state.outdent();
            }

            indented(state, "}()");
        }

        E::If {
            condition,
            then,
            else_,
        } => {
            state.code.push_str("function () {\n");
            {
                state.indent();
                indented(state, "if (");
                generate_expression(state, *condition);
                state.code.push_str(") {\n");

                {
                    state.indent();
                    indented(state, "return ");
                    generate_expression(state, *then);
                    state.code.push('\n');
                    state.outdent();
                }

                line(state, "} else {");

                {
                    state.indent();
                    indented(state, "return ");
                    generate_expression(state, *else_);
                    state.code.push('\n');
                    state.outdent();
                }

                line(state, "}");
                state.outdent();
            }

            indented(state, "}()");
        }

        E::PatternMatching {
            conditions,
            branches,
        } => {
            state.code.push_str("function () {\n");
            {
                state.indent();

                for (i, condition) in conditions.iter().enumerate() {
                    indented(state, "let ");
                    pattern_matching_expression_result(&mut state.code, i);
                    state.code.push_str(" = ");
                    generate_expression(state, *condition);
                    state.code.push('\n');
                }

                let mut names = FnvHashSet::default();
                for branch in branches {
                    for pattern in &branch.patterns {
                        pattern.data.get_bindings(&mut names);
                    }
                }
                generate_pattern_matching_bindings(state, &names);

                for branch in branches {
                    generate_pattern_matching_if(state, &branch.patterns, |state| {
                        if let Some(condition) = branch.condition {
                            indented(state, "if (");
                            generate_expression(state, condition);
                            state.code.push_str(") {\n");

                            {
                                state.indent();
                                indented(state, "return ");
                                generate_expression(state, branch.body);
                                state.code.push('\n');
                                state.outdent();
                            }

                            line(state, "}");
                        } else {
                            indented(state, "return ");
                            generate_expression(state, branch.body);
                            state.code.push('\n');
                        }
                    });
                }

                state.code.push('\n');
                line(state, "throw new Error(\"Incomplete pattern match\")");
                state.outdent();
            }
            indented(state, "}()");
        }
    };
}

fn generate_pattern_matching_if<'a, F>(
    state: &mut State,
    patterns: &'a Vec<Pattern>,
    generate_body: F,
) where
    F: Fn(&mut State),
{
    let has_conditions = patterns
        .iter()
        .any(|pattern| pattern_has_pattern_matching_conditions(pattern));
    if has_conditions {
        indented(state, "if (");
        let mut first = true;
        for (i, pattern) in patterns.iter().enumerate() {
            if pattern_has_pattern_matching_conditions(pattern) {
                if first {
                    first = false;
                } else {
                    state.code.push_str(" && ");
                }
                let result = {
                    let mut r = String::new();
                    pattern_matching_expression_result(&mut r, i);
                    r
                };
                generate_pattern_matching_conditions(
                    &mut state.code,
                    &mut vec![result.to_owned()],
                    pattern,
                    state.strings,
                );
            }
        }
        state.code.push_str(") {\n");
        state.indent();
    }
    generate_body(state);
    if has_conditions {
        state.outdent();
        line(state, "}");
    }
}

fn generate_pattern_matching_bindings(state: &mut State, names: &FnvHashSet<IdentifierName>) {
    if !names.is_empty() {
        indented(state, "let ");
        for (i, name) in names.iter().enumerate() {
            if i > 0 {
                state.code.push_str(", ");
            }
            state.code.push_str(state.strings.resolve(*name));
        }
        state.code.push('\n');
    }
}

fn generate_pattern_matching_conditions(
    code: &mut String,
    path: &mut Vec<String>,
    pattern: &Pattern,
    strings: &Strings,
) {
    match &pattern.data {
        P::Hole => write!(code, "true").unwrap(),
        P::Identifier(identifier) => write!(
            code,
            "({name} = {path}, true)",
            name = identifier.to_string(strings),
            path = path.join(".")
        )
        .unwrap(),
        P::String_(string) => {
            let string = strings.resolve(*string);
            let prop = path.join(".");
            write!(code, "{prop} === \"{string}\"").unwrap();
        }
        P::Float(num) => {
            let num = strings.resolve(*num);
            let prop = path.join(".");
            write!(code, "{prop} === {num}").unwrap();
        }
        P::Type {
            constructor,
            params,
            ..
        } => {
            let prop = path.join(".");
            write!(code, "{prop}.{UNION_TYPE_FIELD} === ").unwrap();
            generate_union_discriminant_field_value(code, constructor.to_string(strings));

            for (i, param) in params.iter().enumerate() {
                if pattern_has_pattern_matching_conditions(param) {
                    code.push_str(" && ");
                    path.push({
                        let mut field = String::new();
                        generate_union_field(&mut field, i);
                        field
                    });
                    generate_pattern_matching_conditions(code, path, param, strings);
                    path.pop();
                }
            }
        }
        P::Named {
            pattern,
            name: identifier,
        } => {
            if pattern_has_pattern_matching_conditions(pattern) {
                code.push('(');
                code.push('(');
                generate_pattern_matching_conditions(code, path, pattern, strings);
                code.push(')');
                write!(
                    code,
                    " ? ({name} = {path}, true) : false)",
                    name = identifier.to_string(strings),
                    path = path.join(".")
                )
                .unwrap();
            } else {
                write!(
                    code,
                    "({name} = {path}, true)",
                    name = identifier.to_string(strings),
                    path = path.join(".")
                )
                .unwrap();
            }
        }
        P::Or(patterns) => {
            let mut first = true;
            for pattern in patterns {
                if pattern_has_pattern_matching_conditions(pattern) {
                    if first {
                        code.push('(');
                        first = false;
                    } else {
                        code.push_str(" || ");
                    }
                    code.push('(');
                    generate_pattern_matching_conditions(code, path, pattern, strings);
                    code.push(')');
                }
            }
            if !first {
                code.push(')');
            }
        }
        P::Record(fields) => {
            let mut first = true;
            for (name, value) in fields {
                if pattern_has_pattern_matching_conditions(value) {
                    if first {
                        first = false;
                    } else {
                        code.push_str(" && ");
                    }
                    path.push(name.to_string(strings).to_owned());
                    generate_pattern_matching_conditions(code, path, value, strings);
                    path.pop();
                }
            }
        }
    }
}

fn pattern_has_pattern_matching_conditions(pattern: &Pattern) -> bool {
    match &pattern.data {
        P::Hole => false,
        P::Identifier(_) => true,
        P::String_(_) => true,
        P::Float(_) => true,
        P::Type { .. } => true,
        P::Named { .. } => true,
        P::Or(patterns) => patterns.iter().any(pattern_has_pattern_matching_conditions),
        P::Record(fields) => fields
            .iter()
            .any(|(_, v)| pattern_has_pattern_matching_conditions(v)),
    }
}

fn pattern_needs_bindings(pattern: &Pattern) -> bool {
    match &pattern.data {
        P::Hole => false,
        P::Identifier(_) => true,
        P::String_(_) => false,
        P::Float(_) => false,
        P::Type { params, .. } => params.iter().any(pattern_needs_bindings),
        P::Named { .. } => true,
        P::Or(patterns) => patterns.iter().any(pattern_needs_bindings),
        P::Record(fields) => fields.iter().any(|(_, v)| pattern_needs_bindings(v)),
    }
}

fn generate_binary_operator(
    state: &mut State,
    binop: &str,
    left: expression::Index,
    right: expression::Index,
) {
    state.code.push('(');
    generate_expression(state, left);
    write!(state.code, " {binop} ").unwrap();
    generate_expression(state, right);
    state.code.push(')');
}

fn generate_fn_call(state: &mut State, fun: expression::Index, params: &[expression::Index]) {
    generate_expression(state, fun);
    state.code.push('(');
    for (i, param) in params.iter().enumerate() {
        if i > 0 {
            state.code.push_str(", ");
        }
        generate_expression(state, *param);
    }
    state.code.push(')');
}

fn indented(state: &mut State, line: &str) {
    write!(state.code, "{0:indent$}{line}", "", indent = state.indent).unwrap();
}
fn line(state: &mut State, line: &str) {
    indented(state, line);
    state.code.push('\n');
}

fn module_full_name(out: &mut String, name: &ModuleName, strings: &Strings) {
    for (i, module) in name.parts.iter().enumerate() {
        if i > 0 {
            out.push_str("__");
        }
        let module = module.to_string(strings);
        write!(out, "{module}").unwrap();
    }
}

fn module_ffi_full_name(out: &mut String, name: &ModuleName, strings: &Strings) {
    for (i, module) in name.parts.iter().enumerate() {
        if i > 0 {
            out.push_str("__");
        }
        let module = module.to_string(strings);
        write!(out, "{module}").unwrap();
    }
    out.push_str("__ffi");
}

fn module_ffi_file_name(name: &str) -> String {
    format!("{name}.ffi.js")
}

fn get_relative_path<P, B>(from_dir: P, to: B) -> PathBuf
where
    P: AsRef<Path>,
    B: AsRef<Path>,
{
    diff_paths(&to, &from_dir).unwrap_or_else(|| {
        let from = from_dir.as_ref().to_str().unwrap();
        let to = to.as_ref().to_str().unwrap();
        panic!("Couldn't find a relative path from {from} to {to}")
    })
}

fn pattern_matching_expression_result(out: &mut String, i: usize) {
    write!(out, "__result_{i}").unwrap();
}
