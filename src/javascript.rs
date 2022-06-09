use crate::compiler::state::ModuleIndex;
use crate::compiler::types::ModuleInterface;
use crate::strings::Strings;
use crate::{
    ast::{
        types::{self, TypeDefinition},
        Definition, Export_, Expression, ExpressionType as ET, Lambda, Module, ModuleName, Pattern,
        Pattern_ as P, TypedDefinition, Unary_ as U,
    },
    compiler,
};
use std::cmp::min;
use std::fmt::Write;

const INDENT: usize = 4;

fn module_full_name(out: &mut String, name: &ModuleName, strings: &Strings) {
    for (i, module) in name.parts.iter().enumerate() {
        if i > 0 {
            out.push_str("__");
        }
        let module = module.value.to_string(strings);
        write!(out, "{module}").unwrap();
    }
}

pub fn generate(sorted_modules: &[ModuleIndex], state: &compiler::State) -> String {
    let mut code = String::new();
    let strings = &state.strings;

    // Print types
    {
        code.push_str("/*\n\n");
        state.types_to_string(&mut code);
        code.push_str("\n\n*/\n\n");
    }

    code.push_str("\nglobalThis.Alma = Object.assign(globalThis.Alma ?? {}, function () {\n\n");

    // for module_name in sorted_modules.iter().rev() {
    //     let name = strings.resolve(*module_name);
    //     writeln!(code, "let {} = ", alias.value.to_string(strings)).unwrap();
    // }

    for module_idx in sorted_modules {
        let module_ast = &state.asts[*module_idx];

        code.push_str("\nlet ");
        module_full_name(&mut code, &module_ast.name, strings);
        code.push_str(" = function () {\n");

        generate_file(
            &mut code,
            module_ast,
            &state.types[*module_idx].as_ref().unwrap_or_else(|| {
                let name = strings.resolve(module_ast.name.full_name);
                panic!("Couldn't find the types for module {name}");
            }),
            strings,
        );

        code.push_str("\n}();\n");
    }

    code.push_str("\nreturn {\n");
    for module in &state.asts {
        indented(&mut code, add_indent(0), "");
        module_full_name(&mut code, &module.name, strings);
        code.push_str(",\n");
    }
    code.push_str("};\n");

    code.push_str("\n}());\n");

    code
}

fn generate_file(
    code: &mut String,
    module: &Module,
    _interface: &ModuleInterface,
    strings: &Strings,
) {
    let indent = add_indent(0);
    if !module.imports.is_empty() {
        code.push('\n');
        generate_imports(indent, code, module, strings);
    }

    if !module.type_definitions.is_empty() {
        code.push('\n');
        generate_types(indent, code, &module.type_definitions, strings);
    }

    if !module.definitions.is_empty() {
        code.push('\n');
        generate_definitions(indent, code, true, &module.definitions, strings);
    }

    if !module.exports.is_empty() {
        code.push('\n');
        generate_exports(indent, code, module, strings);
    }
}

fn generate_imports(indent: usize, code: &mut String, module: &Module, strings: &Strings) {
    for import in &module.imports {
        let import = &import.value;

        match &import.alias {
            Some(alias) => {
                indented(code, indent, "");
                let alias = alias.value.to_string(strings);
                write!(code, "let {alias} = ").unwrap();
                module_full_name(code, &import.module_name, strings);
                code.push('\n');
            }
            None => {
                // Modules are already in scope, no need to print anything
            }
        }

        if !import.exposing.is_empty() {
            let mut identifiers = vec![];
            for exposing in &import.exposing {
                match &exposing.value {
                    Export_::Identifier(identifier) => {
                        identifiers.push(identifier.value.to_string(strings));
                    }
                    Export_::Type(_typ, constructors) => {
                        for constructor in constructors {
                            identifiers.push(constructor.value.to_string(strings));
                        }
                    }
                }
            }
            indented(code, indent, "");
            let fields = identifiers.join(", ");
            write!(code, "let {{ {fields} }} = ").unwrap();
            module_full_name(code, &import.module_name, strings);
            code.push('\n')
        }
    }
}

fn generate_types(indent: usize, code: &mut String, types: &[TypeDefinition], strings: &Strings) {
    for (i, type_def) in types.iter().enumerate() {
        let type_def = &type_def.value;
        if i > 0 {
            code.push('\n')
        }
        match &type_def.typ {
            types::TypeDefinitionType::Union(constructors) => {
                let type_name = type_def.name.value.to_string(strings);
                indented(code, indent, "");
                writeln!(code, "// type {}\n", type_name).unwrap();

                let max_params = constructors
                    .iter()
                    .map(|c| c.value.params.len())
                    .max()
                    .unwrap_or(0);

                for (i, constructor) in constructors.iter().enumerate() {
                    if i > 0 {
                        code.push('\n')
                    }
                    let constructor = &constructor.value;
                    let constructor_name = constructor.name.value.to_string(strings);
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
            types::TypeDefinitionType::Record(_) => (),
        }
    }
}

fn generate_definitions(
    indent: usize,
    code: &mut String,
    space_between: bool,
    definitions: &[TypedDefinition],
    strings: &Strings,
) {
    for (i, definition) in definitions.iter().enumerate() {
        if i > 0 && space_between {
            code.push('\n')
        }
        match definition.definition() {
            Some(Definition::Lambda(name, lambda)) => {
                indented(code, indent, "");
                generate_function(indent, code, name.value.to_string(strings), lambda, strings);
                code.push('\n');
            }
            Some(Definition::Pattern(pattern, expression)) => {
                generate_let(indent, code, pattern, expression, strings)
            }
            None => (),
        }
    }
}

fn generate_exports(indent: usize, code: &mut String, module: &Module, strings: &Strings) {
    line(code, indent, "return {");

    for export in &module.exports {
        match &export.value {
            Export_::Identifier(ident) => {
                indented(code, add_indent(indent), "");
                writeln!(code, "{},", ident.value.to_string(strings)).unwrap();
            }
            Export_::Type(_, constructors) => {
                for constructor in constructors {
                    indented(code, add_indent(indent), "");
                    writeln!(code, "{},", constructor.value.to_string(strings)).unwrap();
                }
            }
        }
    }

    line(code, indent, "};");
}

fn generate_function(
    indent: usize,
    code: &mut String,
    name: &str,
    lambda: &Lambda,
    strings: &Strings,
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
        generate_expression(indent, code, &lambda.body, strings);
        code.push('\n');
    }

    indented(code, indent, "}");
}

fn generate_pattern(code: &mut String, pattern: &Pattern, strings: &Strings) {
    match &pattern.value {
        P::Hole => code.push('_'),
        P::Identifier(identifier) => code.push_str(identifier.value.to_string(strings)),
    }
}

fn generate_let(
    indent: usize,
    code: &mut String,
    pattern: &Pattern,
    expression: &Expression,
    strings: &Strings,
) {
    indented(code, indent, "let ");
    generate_pattern(code, pattern, strings);
    code.push_str(" = ");
    generate_expression(indent, code, expression, strings);
    code.push('\n');
}

fn generate_expression(
    indent: usize,
    code: &mut String,
    expression: &Expression,
    strings: &Strings,
) {
    match &expression.value.expr {
        ET::Unit => code.push_str("()"),

        ET::Bool(bool_) => {
            if *bool_ {
                code.push_str("true");
            } else {
                code.push_str("false");
            }
        }

        ET::Float(float) => write!(code, "{float}").unwrap(),

        ET::String_(string) => {
            let escaped_string = strings.resolve(*string).replace("\n", "\\n");
            write!(code, "\"{escaped_string}\"").unwrap()
        }

        ET::Identifier(module, identifier) => {
            if let Some(module) = module {
                module_full_name(code, module, strings);
                code.push('.');
            }

            code.push_str(identifier.to_string(strings));
        }

        ET::Record(fields) => {
            code.push_str("{\n");

            {
                let indent = add_indent(indent);

                for (key, value) in fields {
                    indented(code, indent, key.value.to_string(strings));
                    code.push_str(": ");
                    generate_expression(indent, code, value, strings);
                    code.push_str(",\n");
                }
            }

            indented(code, indent, "}");
        }

        ET::RecordUpdate(record, fields) => {
            code.push_str("{\n");

            {
                let indent = add_indent(indent);

                indented(code, indent, "...");
                generate_expression(indent, code, record, strings);
                code.push('\n');

                for (key, value) in fields {
                    indented(code, indent, key.value.to_string(strings));
                    code.push_str(": ");
                    generate_expression(indent, code, value, strings);
                    code.push_str(",\n");
                }
            }

            indented(code, indent, "}");
        }

        ET::PropAccess(expr, field) => {
            generate_expression(indent, code, expr, strings);
            code.push('.');
            code.push_str(field.value.to_string(strings));
        }

        ET::PropAccessLambda(field) => {
            let field = field.value.to_string(strings);
            write!(code, "(r => r.{field})").unwrap()
        }

        ET::Unary(unary, expression) => {
            code.push(match &unary.value {
                U::Not => '!',
                U::Minus => '-',
            });
            generate_expression(indent, code, expression, strings);
        }

        ET::Binary(binop_expression, _binop, arg_expressions) => generate_fn_call(
            indent,
            code,
            binop_expression,
            arg_expressions.iter(),
            strings,
        ),

        ET::Lambda(lambda) => {
            generate_function(indent, code, "", lambda, strings);
        }

        ET::FnCall(fun, params) => generate_fn_call(indent, code, fun, params.iter(), strings),

        ET::Let(definitions, body) => {
            code.push_str("function() {\n");

            {
                let indent = add_indent(indent);
                generate_definitions(indent, code, false, definitions, strings);

                indented(code, indent, "return ");
                generate_expression(indent, code, body, strings);
                code.push('\n');
            }

            indented(code, indent, "}()");
        }

        ET::If(condition, then, else_) => {
            code.push_str("function () {\n");
            {
                let indent = add_indent(indent);

                indented(code, indent, "if (");
                generate_expression(indent, code, condition, strings);
                code.push_str(") {");

                {
                    let indent = add_indent(indent);
                    indented(code, indent, "return ");
                    generate_expression(indent, code, then, strings);
                }

                line(code, indent, "} else {");

                {
                    let indent = add_indent(indent);
                    indented(code, indent, "return ");
                    generate_expression(indent, code, else_, strings);
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
    fun: &Expression,
    params: Args,
    strings: &Strings,
) where
    Args: Iterator<Item = &'ast Expression>,
{
    generate_expression(indent, code, fun, strings);
    code.push('(');
    for (i, param) in params.enumerate() {
        if i > 0 {
            code.push_str(", ");
        }
        generate_expression(indent, code, param, strings);
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
