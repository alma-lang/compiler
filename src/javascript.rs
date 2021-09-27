use crate::ast::{
    Definition, Expression, ExpressionType as ET, Module, Pattern, Pattern_ as P, Unary_ as U,
};
use crate::compiler::types::{ModuleAsts, ModuleInterfaces};
use crate::strings::Strings;
use crate::type_env::TypeEnv;
use std::fmt::Write;

pub struct File {
    pub name: String,
    pub contents: String,
}

const INDENT: usize = 4;

pub fn files_to_bundle(files: &[File]) -> String {
    let mut out = String::new();

    for file in files {
        writeln!(out, "\nfunction {} () {{", &file.name).unwrap();
        out.push_str(&file.contents);
        out.push_str("}();\n");
    }

    out
}

pub fn generate(
    module_asts: &ModuleAsts,
    module_interfaces: &ModuleInterfaces,
    strings: &Strings,
) -> Vec<File> {
    let mut files = vec![];
    for (name, module) in module_asts {
        let file = generate_file(module, module_interfaces.get(&name).unwrap(), strings);
        files.push(file);
    }
    files
}

fn generate_file(module: &Module, _interface: &TypeEnv, strings: &Strings) -> File {
    let mut code = String::new();

    generate_imports(&mut code, module, strings);

    code.push('\n');

    generate_definitions(0, &mut code, true, &module.definitions, strings);

    File {
        name: module.name.to_string(strings).to_owned(),
        contents: code,
    }
}

fn generate_imports(code: &mut String, module: &Module, strings: &Strings) {
    for import in &module.imports {
        let import = &import.value;

        // TODO: import the alias or full name module in to the JS
        // let alias = &import
        //     .alias
        //     .as_ref()
        //     .unwrap_or(&import.module_name)
        //     .value
        //     .name;
        // write!(
        //     code,
        //     "import * as {} from \"{}\"\n",
        //     alias,
        //     import.module_name.value.name, // TODO: Wrong path
        // )
        // .unwrap();

        if !import.exposing.is_empty() {
            let mut identifiers = vec![];
            for exposing in &import.exposing {
                identifiers.append(
                    &mut exposing
                        .value
                        .identifiers()
                        .into_iter()
                        .map(|i| i.value.to_string(strings))
                        .collect(),
                );
            }
            writeln!(
                code,
                "import {{{}}} from \"{}\"",
                identifiers.join(", "),
                import.module_name.to_string(strings)
            )
            .unwrap();
        }
    }
}

fn generate_definitions(
    indent: usize,
    code: &mut String,
    space_between: bool,
    definitions: &[Definition],
    strings: &Strings,
) {
    for definition in definitions {
        match definition {
            Definition::Lambda(name, expression) => match &expression.value.expr {
                ET::Lambda(params, body) => {
                    generate_function(indent, code, name.value.to_string(strings), params, body, strings)
                }
                _ => panic!("Top level definitions classified as Lambda should always have a Lambda expression on the right hand side. This is a compiler bug. Please report it!"),
            },
            Definition::Pattern(pattern, expression) => {
                generate_let(indent, code, pattern, expression, strings)
            }
        }
        if space_between {
            code.push('\n')
        }
    }
}

fn generate_function(
    indent: usize,
    code: &mut String,
    name: &str,
    params: &[Pattern],
    body: &Expression,
    strings: &Strings,
) {
    write!(code, "function {}(", name).unwrap();

    for (i, pattern) in params.iter().enumerate() {
        if i > 0 {
            code.push_str(", ");
        }
        generate_pattern(code, pattern, strings);
    }

    line(code, indent, ") {");

    {
        let indent = add_indent(indent);
        indented(code, indent, "return ");
        generate_expression(indent, code, body, strings);
        code.push('\n');
    }

    line(code, indent, "}");
}

fn generate_pattern(code: &mut String, pattern: &Pattern, strings: &Strings) {
    match &pattern.value {
        P::Hole => code.push('_'),
        P::Identifier(identifier) => code.push_str(&identifier.value.to_string(strings)),
    }
}

fn generate_let(
    indent: usize,
    code: &mut String,
    pattern: &Pattern,
    expression: &Expression,
    strings: &Strings,
) {
    indented(code, indent, "var ");
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

        ET::Float(float) => write!(code, "{}", float).unwrap(),

        ET::String_(string) => write!(code, "\"{}\"", strings.resolve(*string)).unwrap(),

        ET::Identifier(identifier) => code.push_str(identifier.value.to_string(strings)),

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
                code.push_str("\n");

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
            code.push_str(".");
            code.push_str(field.value.to_string(strings));
        }

        ET::PropAccessLambda(field) => {
            write!(code, "(r => r.{})", field.value.to_string(strings)).unwrap()
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

        ET::Lambda(patterns, body) => {
            generate_function(indent, code, "", patterns, body, strings);
        }

        ET::FnCall(fun, params) => generate_fn_call(indent, code, fun, params.iter(), strings),

        ET::Let(definitions, body) => {
            code.push_str("function() {\n");

            {
                let indent = add_indent(indent);
                generate_definitions(indent, code, false, definitions, strings);

                code.push_str("return ");
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
    write!(out, "{0:1$}{2}", "", indent, line).unwrap();
}
fn line(out: &mut String, indent: usize, line: &str) {
    indented(out, indent, line);
    out.push('\n');
}
