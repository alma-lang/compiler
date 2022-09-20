use crate::ast::expression::{binop, Expressions};
use crate::compiler::state::ModuleIndex;
use crate::compiler::types::ModuleInterface;
use crate::strings::Strings;
use crate::{
    ast::{
        expression::{
            Expression, ExpressionData as E, Lambda, Pattern, PatternData as P, UnaryData as U,
        },
        types::{self, TypeDefinition},
        Definition, ExportData, Module, ModuleName, TypedDefinition,
    },
    compiler,
};
use pathdiff::diff_paths;
use std::cmp::min;
use std::fmt::Write;
use std::path::{Path, PathBuf};

const INDENT: usize = 4;
const UNION_TYPE_FIELD: &str = "__tag";
const PATTERN_MATCHING_EXPRESSION_RESULT: &str = "__result";

pub enum OutputFile {
    File(String),
    CopyFrom(String),
}

pub fn generate(
    sorted_modules: &[ModuleIndex],
    state: &compiler::State,
    output: &Path,
) -> Vec<(String, OutputFile)> {
    let mut files = vec![];
    let strings = &state.strings;

    for module_idx in sorted_modules {
        let mut code = String::new();
        let module_ast = &state.modules[*module_idx];
        let path = &state.sources[state.module_to_source_idx[*module_idx]].path;
        let module_output_path = output
            .join(&path)
            .with_file_name(&format!("{}.js", module_ast.name.to_string(strings)));

        // Print types
        {
            code.push_str("/*\n\nmodule ");
            code.push_str(module_ast.name.to_string(strings));
            code.push_str("\n\n");
            state.module_interfaces[*module_idx]
                .as_ref()
                .unwrap()
                .write_to_string(&mut code, strings, &state.types);

            code.push_str(
                "\n\n*/
\n\n",
            );
        }

        let expressions = &module_ast.expressions;
        let maybe_out_ffi_file = generate_file(
            &module_output_path,
            &mut code,
            output,
            state,
            module_ast,
            &state.module_interfaces[*module_idx]
                .as_ref()
                .unwrap_or_else(|| {
                    let name = module_ast.name.to_string(strings);
                    panic!("Couldn't find the types for module {name}");
                }),
            strings,
            expressions,
        );

        if let Some(out_ffi_file) = maybe_out_ffi_file {
            let ffi_file = {
                let ffi_path = path.with_file_name(module_ffi_file_name(
                    module_output_path.file_stem().unwrap().to_str().unwrap(),
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
            module_output_path.to_str().unwrap().to_owned(),
            OutputFile::File(code),
        ))
    }

    files
}

fn generate_file(
    module_output_path: &PathBuf,
    code: &mut String,
    output: &Path,
    state: &compiler::State,
    module: &Module,
    _interface: &ModuleInterface,
    strings: &Strings,
    expressions: &Expressions,
) -> Option<String> {
    let indent = 0;
    let mut ffi_file = None;

    if module.has_externals() {
        code.push_str("import * as ");
        module_ffi_full_name(code, &module.name, strings);

        let ffi_path = module_output_path.with_file_name(module_ffi_file_name(
            module_output_path.file_stem().unwrap().to_str().unwrap(),
        ));
        let module_output_dir = module_output_path.parent().unwrap();
        let relative_path = get_relative_path(module_output_dir, &ffi_path);
        let ffi_path = ffi_path.to_str().unwrap();
        let relative_path = relative_path.to_str().unwrap();
        generate_import_from_part_with_newline(code, relative_path);

        ffi_file = Some(ffi_path.to_owned());
    }

    if !module.imports.is_empty() {
        code.push('\n');
        generate_imports(
            indent,
            code,
            module_output_path,
            output,
            state,
            module,
            strings,
        );
    }

    if !module.type_definitions.is_empty() {
        code.push('\n');
        generate_types(indent, code, &module.type_definitions, strings);
    }

    if !module.definitions.is_empty() {
        code.push('\n');
        generate_definitions(
            indent,
            code,
            true,
            &module.name,
            &module.definitions,
            strings,
            expressions,
        );
    }

    if !module.exports.is_empty() {
        code.push('\n');
        generate_exports(indent, code, module, strings);
    }

    ffi_file
}

fn generate_imports(
    indent: usize,
    code: &mut String,
    module_output_path: &PathBuf,
    output: &Path,
    state: &compiler::State,
    module: &Module,
    strings: &Strings,
) {
    let module_output_dir = module_output_path.parent().unwrap();
    for import in &module.imports {
        let import = &import;
        let import_output_path = {
            let path = state.sources[state.module_to_source_idx[*state
                .module_name_to_module_idx
                .get(&import.module_name.full_name)
                .unwrap()]]
            .path
            .with_file_name(format!("{}.js", import.module_name.to_string(strings)));
            let path = output.to_path_buf().join(path);
            let relative_path = get_relative_path(module_output_dir, &path);
            relative_path
        };
        let import_output_path = import_output_path.to_str().unwrap();

        indented(code, indent, "");
        code.push_str("import * as ");
        match &import.alias {
            Some(alias) => {
                let alias = alias.to_string(strings);
                code.push_str(alias);
            }
            None => {
                module_full_name(code, &import.module_name, strings);
            }
        }
        generate_import_from_part_with_newline(code, import_output_path);

        if !import.exposing.is_empty() {
            indented(code, indent, "import { ");
            let mut first = true;
            for exposing in &import.exposing {
                match &exposing.typ {
                    ExportData::Identifier(identifier) => {
                        if !first {
                            code.push_str(", ");
                        } else {
                            first = false;
                        }
                        code.push_str(identifier.to_string(strings));
                    }
                    ExportData::Type { constructors, .. } => {
                        if !constructors.is_empty() {
                            if !first {
                                code.push_str(", ");
                            } else {
                                first = false;
                            }
                            for (i, constructor) in constructors.iter().enumerate() {
                                if i > 0 {
                                    code.push_str(", ");
                                }
                                code.push_str(constructor.to_string(strings));
                            }
                        }
                    }
                }
            }
            code.push_str(" }");
            generate_import_from_part_with_newline(code, import_output_path);
        }
    }
}

fn generate_import_from_part_with_newline(code: &mut String, path: &str) {
    let dot_slash = if path.starts_with("./") || path.starts_with("../") || path.starts_with("/") {
        ""
    } else {
        "./"
    };
    write!(code, " from \"{dot_slash}{path}\"\n").unwrap();
}

fn generate_union_discriminant_field_value(code: &mut String, constructor_name: &str) {
    write!(code, "\"{constructor_name}\"").unwrap();
}

fn generate_types(indent: usize, code: &mut String, types: &[TypeDefinition], strings: &Strings) {
    for (i, type_def) in types.iter().enumerate() {
        let type_def = &type_def;
        if i > 0 {
            code.push('\n')
        }
        match &type_def.typ {
            types::TypeDefinitionData::Empty => (),
            types::TypeDefinitionData::Union { constructors } => {
                let type_name = type_def.name.to_string(strings);
                indented(code, indent, "");
                writeln!(code, "// type {type_name}\n").unwrap();

                let max_params = constructors
                    .iter()
                    .map(|c| c.params.len())
                    .max()
                    .unwrap_or(0);

                for (i, constructor) in constructors.iter().enumerate() {
                    if i > 0 {
                        code.push('\n')
                    }
                    let constructor = &constructor;
                    let constructor_name = constructor.name.to_string(strings);
                    let num_params = constructor.params.len();

                    if num_params > 0 {
                        indented(code, indent, "function ");
                        code.push_str(constructor_name);
                        code.push('(');
                        for i in 0..min(num_params, max_params) {
                            if i > 0 {
                                code.push_str(", ");
                            }
                            generate_union_field(code, i);
                        }
                        code.push_str(") {\n");

                        {
                            let indent = add_indent(indent);
                            line(code, indent, "return {");
                            {
                                let indent = add_indent(indent);
                                indented(code, indent, "");
                                write!(code, "{UNION_TYPE_FIELD}: ").unwrap();
                                generate_union_discriminant_field_value(code, constructor_name);
                                code.push_str(",\n");

                                for i in 0..max_params {
                                    indented(code, indent, "");
                                    if i < num_params {
                                        generate_union_field(code, i);
                                        code.push_str(",\n");
                                    } else {
                                        generate_union_field(code, i);
                                        code.push_str(": null,\n");
                                    }
                                }
                            }
                            line(code, indent, "}");
                        }
                    } else {
                        indented(code, indent, "let ");
                        code.push_str(constructor_name);
                        code.push_str(" = {\n");
                        {
                            let indent = add_indent(indent);
                            indented(code, indent, "");
                            write!(code, "{UNION_TYPE_FIELD}: ").unwrap();
                            generate_union_discriminant_field_value(code, constructor_name);
                            code.push_str(",\n");

                            for i in 0..max_params {
                                indented(code, indent, "");
                                generate_union_field(code, i);
                                code.push_str(": null,\n");
                            }
                        }
                    }

                    line(code, indent, "}");
                }
            }
            types::TypeDefinitionData::Record(_) => (),
        }
    }
}

fn generate_union_field(code: &mut String, i: usize) {
    write!(code, "_{i}").unwrap();
}

fn generate_definitions(
    indent: usize,
    code: &mut String,
    space_between: bool,
    module_name: &ModuleName,
    definitions: &[TypedDefinition],
    strings: &Strings,
    expressions: &Expressions,
) {
    for (i, definition) in definitions.iter().enumerate() {
        if i > 0 && space_between {
            code.push('\n')
        }
        match definition {
            TypedDefinition::External(typ) => {
                let name = typ.name.to_string(strings);

                indented(code, indent, "");
                write!(code, "let {name} = ").unwrap();
                module_ffi_full_name(code, module_name, strings);
                writeln!(code, ".{name}").unwrap();
            }
            TypedDefinition::TypeSignature(typ) => {
                indented(code, indent, "");
                writeln!(
                    code,
                    "let {type_name} = new Error(\"Unimplemented\")",
                    type_name = typ.name.to_string(strings)
                )
                .unwrap();
            }
            TypedDefinition::Typed(_typ, def) => {
                generate_definition(indent, code, module_name, def, strings, expressions)
            }
            TypedDefinition::Untyped(def) => {
                generate_definition(indent, code, module_name, def, strings, expressions)
            }
        }
    }
}

fn generate_definition(
    indent: usize,
    code: &mut String,
    module_name: &ModuleName,
    definition: &Definition,
    strings: &Strings,
    expressions: &Expressions,
) {
    match definition {
        Definition::Lambda(name, lambda) => {
            indented(code, indent, "");
            generate_function(
                indent,
                code,
                module_name,
                name.to_string(strings),
                lambda,
                strings,
                expressions,
            );
            code.push('\n');
        }
        Definition::Pattern(pattern, expression) => generate_let(
            indent,
            code,
            module_name,
            pattern,
            &expressions[*expression],
            strings,
            expressions,
        ),
    }
}

fn generate_exports(indent: usize, code: &mut String, module: &Module, strings: &Strings) {
    line(code, indent, "export {");

    for export in &module.exports {
        match &export.typ {
            ExportData::Identifier(identifier) => {
                indented(code, add_indent(indent), "");
                writeln!(code, "{},", identifier.to_string(strings)).unwrap();
            }
            ExportData::Type { constructors, name } => {
                let indent = add_indent(indent);
                if !constructors.is_empty() {
                    indented(code, indent, "");
                    writeln!(code, "// type {}", name.to_string(strings)).unwrap();
                }
                for constructor in constructors {
                    indented(code, indent, "");
                    writeln!(code, "{},", constructor.to_string(strings)).unwrap();
                }
                if !constructors.is_empty() {
                    indented(code, indent, "");
                    writeln!(code, "// end type {}", name.to_string(strings)).unwrap();
                }
            }
        }
    }

    line(code, indent, "}");
}

fn generate_function(
    indent: usize,
    code: &mut String,
    module_name: &ModuleName,
    name: &str,
    lambda: &Lambda,
    strings: &Strings,
    expressions: &Expressions,
) {
    write!(code, "function {name}(").unwrap();

    for (i, pattern) in lambda.parameters.iter().enumerate() {
        if i > 0 {
            code.push_str(", ");
        }
        generate_binding_destructuring(code, pattern, strings);
    }

    code.push_str(") {\n");

    {
        let indent = add_indent(indent);
        indented(code, indent, "return ");
        generate_expression(
            indent,
            code,
            module_name,
            &expressions[lambda.body],
            strings,
            expressions,
        );
        code.push('\n');
    }

    indented(code, indent, "}");
}

fn generate_binding_destructuring(code: &mut String, pattern: &Pattern, strings: &Strings) {
    match &pattern.typ {
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
    }
}

fn generate_let(
    indent: usize,
    code: &mut String,
    module_name: &ModuleName,
    pattern: &Pattern,
    expression: &Expression,
    strings: &Strings,
    expressions: &Expressions,
) {
    indented(code, indent, "let ");
    generate_binding_destructuring(code, pattern, strings);
    code.push_str(" = ");
    generate_expression(indent, code, module_name, expression, strings, expressions);
    code.push('\n');
}

fn generate_expression(
    indent: usize,
    code: &mut String,
    module_name: &ModuleName,
    expression: &Expression,
    strings: &Strings,
    expressions: &Expressions,
) {
    match &expression.expr {
        E::Float(float) => write!(code, "{float}").unwrap(),

        E::String_(string) => {
            let escaped_string = strings.resolve(*string).replace("\n", "\\n");
            write!(code, "\"{escaped_string}\"").unwrap()
        }

        E::Identifier { module, identifier } => {
            // TODO: Generate True and False as true and false
            if let Some(module) = module {
                module_full_name(code, module, strings);
                code.push('.');
            }

            code.push_str(identifier.to_string(strings));
        }

        E::Record { fields } => {
            code.push_str("{\n");

            {
                let indent = add_indent(indent);

                for (key, value) in fields {
                    indented(code, indent, key.to_string(strings));
                    code.push_str(": ");
                    generate_expression(
                        indent,
                        code,
                        module_name,
                        &expressions[*value],
                        strings,
                        expressions,
                    );
                    code.push_str(",\n");
                }
            }

            indented(code, indent, "}");
        }

        E::RecordUpdate { record, fields } => {
            code.push_str("{\n");

            {
                let indent = add_indent(indent);

                indented(code, indent, "...");
                generate_expression(
                    indent,
                    code,
                    module_name,
                    &expressions[*record],
                    strings,
                    expressions,
                );
                code.push('\n');

                for (key, value) in fields {
                    indented(code, indent, key.to_string(strings));
                    code.push_str(": ");
                    generate_expression(
                        indent,
                        code,
                        module_name,
                        &expressions[*value],
                        strings,
                        expressions,
                    );
                    code.push_str(",\n");
                }
            }

            indented(code, indent, "}");
        }

        E::PropertyAccess {
            expression,
            property,
        } => {
            generate_expression(
                indent,
                code,
                module_name,
                &expressions[*expression],
                strings,
                expressions,
            );
            code.push('.');
            code.push_str(property.to_string(strings));
        }

        E::PropertyAccessLambda { property } => {
            let property = property.to_string(strings);
            write!(code, "(r => r.{property})").unwrap()
        }

        E::Unary { op, expression } => {
            code.push(match &op.typ {
                U::Not => '!',
                U::Minus => '-',
            });
            generate_expression(
                indent,
                code,
                module_name,
                &expressions[*expression],
                strings,
                expressions,
            );
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
                generate_binary_operator(
                    indent,
                    code,
                    module_name,
                    binop,
                    &expressions[*left],
                    &expressions[*right],
                    strings,
                    expressions,
                )
            } else {
                generate_fn_call(
                    indent,
                    code,
                    module_name,
                    &expressions[*expression],
                    arguments.iter().map(|a| &expressions[*a]),
                    strings,
                    expressions,
                )
            }
        }

        E::Lambda(lambda) => {
            generate_function(indent, code, module_name, "", lambda, strings, expressions);
        }

        E::FnCall {
            function,
            arguments,
        } => generate_fn_call(
            indent,
            code,
            module_name,
            &expressions[*function],
            arguments.iter().map(|a| &expressions[*a]),
            strings,
            expressions,
        ),

        E::Let { definitions, body } => {
            code.push_str("function() {\n");

            {
                let indent = add_indent(indent);
                generate_definitions(
                    indent,
                    code,
                    false,
                    module_name,
                    definitions,
                    strings,
                    expressions,
                );

                indented(code, indent, "return ");
                generate_expression(
                    indent,
                    code,
                    module_name,
                    &expressions[*body],
                    strings,
                    expressions,
                );
                code.push('\n');
            }

            indented(code, indent, "}()");
        }

        E::If {
            condition,
            then,
            else_,
        } => {
            code.push_str("function () {\n");
            {
                let indent = add_indent(indent);

                indented(code, indent, "if (");
                generate_expression(
                    indent,
                    code,
                    module_name,
                    &expressions[*condition],
                    strings,
                    expressions,
                );
                code.push_str(") {\n");

                {
                    let indent = add_indent(indent);
                    indented(code, indent, "return ");
                    generate_expression(
                        indent,
                        code,
                        module_name,
                        &expressions[*then],
                        strings,
                        expressions,
                    );
                    code.push('\n');
                }

                line(code, indent, "} else {");

                {
                    let indent = add_indent(indent);
                    indented(code, indent, "return ");
                    generate_expression(
                        indent,
                        code,
                        module_name,
                        &expressions[*else_],
                        strings,
                        expressions,
                    );
                    code.push('\n');
                }

                line(code, indent, "}");
            }

            indented(code, indent, "}()");
        }

        E::PatternMatching {
            expression,
            branches,
        } => {
            code.push_str("function () {\n");
            {
                let indent = add_indent(indent);

                indented(code, indent, "");
                write!(code, "let {PATTERN_MATCHING_EXPRESSION_RESULT} = ").unwrap();
                generate_expression(
                    indent,
                    code,
                    module_name,
                    &expressions[*expression],
                    strings,
                    expressions,
                );
                code.push_str("\n");

                for branch in branches {
                    generate_pattern_matching_if(
                        code,
                        indent,
                        &branch.pattern,
                        |code, indent, module_name, expressions, strings| {
                            indented(code, indent, "return ");
                            generate_expression(
                                indent,
                                code,
                                module_name,
                                &expressions[branch.body],
                                strings,
                                expressions,
                            );
                            code.push('\n');
                        },
                        module_name,
                        expressions,
                        strings,
                    );
                }

                code.push('\n');
                line(
                    code,
                    indent,
                    "throw new Error(\"Incomplete pattern match\")",
                )
            }
            indented(code, indent, "}()");
        }
    };
}

fn generate_pattern_matching_if<'a, F>(
    code: &mut String,
    indent: usize,
    pattern: &'a Pattern,
    generate_body: F,
    module_name: &ModuleName,
    expressions: &Expressions,
    strings: &Strings,
) where
    F: Fn(&mut String, usize, &ModuleName, &Expressions, &Strings),
{
    let has_conditions = pattern_has_pattern_matching_conditions(pattern);
    if has_conditions {
        indented(code, indent, "if (");
        generate_pattern_matching_conditions(
            code,
            &mut vec![PATTERN_MATCHING_EXPRESSION_RESULT.to_owned()],
            pattern,
            strings,
        );
        code.push_str(") {\n");
    }
    {
        let indent = if has_conditions {
            add_indent(indent)
        } else {
            indent
        };
        generate_pattern_matching_bindings(
            code,
            indent,
            PATTERN_MATCHING_EXPRESSION_RESULT,
            pattern,
            strings,
        );

        generate_body(code, indent, module_name, expressions, strings);
    }
    if has_conditions {
        line(code, indent, "}");
    }
}

fn generate_pattern_matching_bindings(
    code: &mut String,
    indent: usize,
    result: &str,
    pattern: &Pattern,
    strings: &Strings,
) {
    if pattern_needs_bindings(pattern) {
        indented(code, indent, "let ");
        generate_binding_destructuring(code, pattern, strings);
        writeln!(code, " = {result}").unwrap();
    }
}

fn generate_pattern_matching_conditions(
    code: &mut String,
    path: &mut Vec<String>,
    pattern: &Pattern,
    strings: &Strings,
) {
    match &pattern.typ {
        P::Hole | P::Identifier(_) => (),
        P::String_(string) => {
            let string = strings.resolve(*string);
            let prop = path.join(".");
            write!(code, "{prop} === \"{string}\"").unwrap();
        }
        P::Float(num) => {
            let num = num.to_string();
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
    }
}

fn pattern_has_pattern_matching_conditions(pattern: &Pattern) -> bool {
    match &pattern.typ {
        P::Hole => false,
        P::Identifier(_) => false,
        P::String_(_) => true,
        P::Float(_) => true,
        P::Type { .. } => true,
    }
}

fn pattern_needs_bindings(pattern: &Pattern) -> bool {
    match &pattern.typ {
        P::Hole => false,
        P::Identifier(_) => true,
        P::String_(_) => false,
        P::Float(_) => false,
        P::Type { params, .. } => params.iter().any(|p| pattern_needs_bindings(p)),
    }
}

fn generate_binary_operator(
    indent: usize,
    code: &mut String,
    module_name: &ModuleName,
    binop: &str,
    left: &Expression,
    right: &Expression,
    strings: &Strings,
    expressions: &Expressions,
) {
    code.push('(');
    generate_expression(indent, code, module_name, left, strings, expressions);
    write!(code, " {binop} ").unwrap();
    generate_expression(indent, code, module_name, right, strings, expressions);
    code.push(')');
}

fn generate_fn_call<'ast, Args>(
    indent: usize,
    code: &mut String,
    module_name: &ModuleName,
    fun: &Expression,
    params: Args,
    strings: &Strings,
    expressions: &Expressions,
) where
    Args: Iterator<Item = &'ast Expression>,
{
    generate_expression(indent, code, module_name, fun, strings, expressions);
    code.push('(');
    for (i, param) in params.enumerate() {
        if i > 0 {
            code.push_str(", ");
        }
        generate_expression(indent, code, module_name, param, strings, expressions);
    }
    code.push(')');
}

fn add_indent(level: usize) -> usize {
    level + INDENT
}

fn indented(out: &mut String, indent: usize, line: &str) {
    write!(out, "{0:indent$}{line}", "").unwrap();
}
fn line(out: &mut String, indent: usize, line: &str) {
    indented(out, indent, line);
    out.push('\n');
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
