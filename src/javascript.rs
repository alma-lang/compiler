use crate::ast::expression::Expressions;
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
use std::cmp::min;
use std::fmt::Write;
use std::path::{Path, PathBuf};

const INDENT: usize = 4;

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

pub fn generate(
    sorted_modules: &[ModuleIndex],
    state: &compiler::State,
    output: &Path,
) -> Vec<(String, String)> {
    let mut files = vec![];
    let strings = &state.strings;

    for module_idx in sorted_modules {
        let mut code = String::new();
        let module_ast = &state.modules[*module_idx];
        let dir = {
            let mut path = state.sources[state.module_to_source_idx[*module_idx]]
                .path
                .to_path_buf();
            // Get rid of the source file part and keep the directory
            path.pop();
            path
        };
        let module_output_path = {
            let mut path = output.to_path_buf().join(&dir);
            path.push(&format!("{}.js", module_ast.name.to_string(strings)));
            path
        };

        dbg!(
            &state.sources[state.module_to_source_idx[*module_idx]].path,
            (&dir).to_str().unwrap(),
            module_output_path.to_str().unwrap(),
            module_ast.name.to_string(strings)
        );

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
        generate_file(
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

        files.push((module_output_path.to_str().unwrap().to_owned(), code))
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
) {
    let indent = 0;

    if module.has_externals() {
        code.push_str("import * as ");
        module_ffi_full_name(code, &module.name, strings);
        let mut ffi_path = module_output_path.clone();
        ffi_path.set_file_name(module_ffi_file_name(
            module_output_path.file_stem().unwrap().to_str().unwrap(),
        ));
        let ffi_path = ffi_path.to_str().unwrap();
        write!(code, " from \"{ffi_path}\"\n\n").unwrap();
    }

    if !module.imports.is_empty() {
        code.push('\n');
        generate_imports(indent, code, output, state, module, strings);
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
}

fn generate_imports(
    indent: usize,
    code: &mut String,
    output: &Path,
    state: &compiler::State,
    module: &Module,
    strings: &Strings,
) {
    for import in &module.imports {
        let import = &import;
        let import_output_path = {
            let mut path = state.sources[state.module_to_source_idx[*state
                .module_name_to_module_idx
                .get(&import.module_name.full_name)
                .unwrap()]]
            .path
            .to_path_buf();
            // Get rid of the source file part and keep the directory
            path.pop();
            path.push(format!("{}.js", import.module_name.to_string(strings)));
            output.to_path_buf().join(path)
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
        write!(code, " from \"{import_output_path}\"\n").unwrap();

        if !import.exposing.is_empty() {
            let mut identifiers = vec![];
            for exposing in &import.exposing {
                match &exposing.typ {
                    ExportData::Identifier(identifier) => {
                        identifiers.push(identifier.to_string(strings));
                    }
                    ExportData::Type { constructors, .. } => {
                        for constructor in constructors {
                            identifiers.push(constructor.to_string(strings));
                        }
                    }
                }
            }
            indented(code, indent, "");
            let fields = identifiers.join(", ");
            write!(
                code,
                "import {{ {fields} }} from \"{import_output_path}\"\n"
            )
            .unwrap();
        }
    }
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
                writeln!(code, "// type {}\n", type_name).unwrap();

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
                            write!(code, "_{i}").unwrap();
                        }
                        code.push_str(") {\n");

                        {
                            let indent = add_indent(indent);
                            line(code, indent, "return {");
                            {
                                let indent = add_indent(indent);
                                indented(code, indent, "");
                                writeln!(code, "tag: \"{}\",", constructor_name).unwrap();

                                for i in 0..max_params {
                                    indented(code, indent, "");
                                    if i < num_params {
                                        writeln!(code, "_{},", i).unwrap();
                                    } else {
                                        writeln!(code, "_{}: null,", i).unwrap();
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
                            writeln!(code, "tag: \"{}\",", constructor_name).unwrap();

                            for i in 0..max_params {
                                indented(code, indent, "");
                                writeln!(code, "_{}: null,", i).unwrap();
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
                write!(code, "let {} = ", name).unwrap();
                module_ffi_full_name(code, module_name, strings);
                writeln!(code, ".{}", name).unwrap();
            }
            TypedDefinition::TypeSignature(typ) => {
                indented(code, indent, "");
                writeln!(
                    code,
                    "let {} = new Error(\"Unimplemented\")",
                    typ.name.to_string(strings)
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
            ExportData::Identifier(ident) => {
                indented(code, add_indent(indent), "");
                writeln!(code, "{},", ident.to_string(strings)).unwrap();
            }
            ExportData::Type { constructors, name } => {
                if !constructors.is_empty() {
                    indented(code, add_indent(indent), "");
                    writeln!(code, "// type {}", name.to_string(strings)).unwrap();
                }
                for constructor in constructors {
                    indented(code, add_indent(indent), "");
                    writeln!(code, "{},", constructor.to_string(strings)).unwrap();
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
        generate_pattern(code, pattern, strings);
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

fn generate_pattern(code: &mut String, pattern: &Pattern, strings: &Strings) {
    match &pattern.typ {
        P::Hole => code.push('_'),
        P::Identifier(identifier) => code.push_str(identifier.to_string(strings)),
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
    generate_pattern(code, pattern, strings);
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
        E::Unit => code.push_str("()"),

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
            arguments,
            ..
        } => generate_fn_call(
            indent,
            code,
            module_name,
            &expressions[*expression],
            arguments.iter().map(|a| &expressions[*a]),
            strings,
            expressions,
        ),

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
                code.push_str(") {");

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
                }

                line(code, indent, "}");
            }

            indented(code, indent, "}()");
        }
    };
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
