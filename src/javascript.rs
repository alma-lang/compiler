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
        out.push_str(&format!("\nfunction {} () {{\n", &file.name));
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
    code.push_str(&format!("function {}(", name));

    for (i, pattern) in params.iter().enumerate() {
        if i > 0 {
            code.push_str(", ");
        }
        generate_pattern(code, pattern, strings);
    }

    line(code, indent, ") {");

    let expr = generate_expression(add_indent(indent), code, body, strings);
    line(code, add_indent(indent), &format!("return {}", expr));

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
    let expr = generate_expression(indent, code, expression, strings);

    indented(code, indent, "var ");
    generate_pattern(code, pattern, strings);
    writeln!(code, " = {}", expr).unwrap();
}

fn generate_expression(
    indent: usize,
    code: &mut String,
    expression: &Expression,
    strings: &Strings,
) -> String {
    match &expression.value.expr {
        ET::Unit => "()".to_string(),

        ET::Bool(bool_) => {
            if *bool_ {
                "true".to_string()
            } else {
                "false".to_string()
            }
        }

        ET::Float(float) => float.to_string(),

        ET::String_(string) => format!("\"{}\"", strings.resolve(*string)),

        ET::Identifier(identifier) => identifier.value.to_string(strings).to_owned(),

        ET::Record(fields) => {
            let mut record = String::new();
            record.push_str("{\n");

            {
                let indent = add_indent(indent);

                for (key, value) in fields {
                    let value = generate_expression(indent, code, value, strings);
                    line(
                        &mut record,
                        indent,
                        &format!("{}: {},", key.value.to_string(strings), value),
                    );
                }
            }

            indented(&mut record, indent, "}");

            record
        }

        ET::RecordUpdate(record, fields) => {
            let mut record_update = String::new();
            record_update.push_str("{\n");

            {
                let indent = add_indent(indent);

                let record_str = generate_expression(indent, code, record, strings);
                line(&mut record_update, indent, &format!("...{},", record_str));

                for (key, value) in fields {
                    let value = generate_expression(indent, code, value, strings);
                    line(
                        &mut record_update,
                        indent,
                        &format!("{}: {},", key.value.to_string(strings), value),
                    );
                }
            }

            indented(&mut record_update, indent, "}");

            record_update
        }

        ET::PropAccess(expr, field) => format!(
            "{}.{}",
            generate_expression(indent, code, expr, strings),
            field.value.to_string(strings)
        ),

        ET::PropAccessLambda(field) => format!("(r => r.{})", field.value.to_string(strings)),

        ET::Unary(unary, expression) => format!(
            "{}{}",
            match &unary.value {
                U::Not => "!",
                U::Minus => "-",
            },
            generate_expression(indent, code, expression, strings)
        ),

        ET::Binary(binop_expression, _binop, arg_expressions) => generate_fn_call(
            indent,
            code,
            binop_expression,
            arg_expressions.iter(),
            strings,
        ),

        ET::Lambda(patterns, body) => {
            let mut lambda = String::new();
            generate_function(indent, &mut lambda, "", patterns, body, strings);
            lambda
        }

        ET::FnCall(fun, params) => generate_fn_call(indent, code, fun, params.iter(), strings),

        ET::Let(definitions, body) => {
            let mut let_ = String::new();
            let_.push_str("function() {\n");

            {
                let indent = add_indent(indent);
                generate_definitions(indent, &mut let_, false, definitions, strings);
                let ret = generate_expression(indent, &mut let_, body, strings);
                line(&mut let_, indent, &format!("return {}", ret));
            }

            indented(&mut let_, indent, "}()");
            let_
        }

        ET::If(condition, then, else_) => {
            // TODO: Use ternary if the if doesn't need many statements
            let mut if_ = String::new();
            if_.push_str("function () {\n");
            {
                let indent = add_indent(indent);

                let condition = generate_expression(indent, &mut if_, condition, strings);
                line(&mut if_, indent, &format!("if ({}) {{", condition));
                {
                    let indent = add_indent(indent);
                    let then = generate_expression(indent, &mut if_, then, strings);
                    line(&mut if_, indent, &format!("return {}", then));
                }
                line(&mut if_, indent, "} else {");
                {
                    let indent = add_indent(indent);
                    let else_ = generate_expression(indent, &mut if_, else_, strings);
                    line(&mut if_, indent, &format!("return {}", else_));
                }
                line(&mut if_, indent, "}");
            }

            indented(&mut if_, indent, "}()");

            if_
        }
    }
}

fn generate_fn_call<'ast, Args>(
    indent: usize,
    code: &mut String,
    fun: &Expression,
    params: Args,
    strings: &Strings,
) -> String
where
    Args: Iterator<Item = &'ast Expression>,
{
    let mut call = String::new();
    let fun = generate_expression(indent, code, fun, strings);

    let params = params
        .map(|expression| generate_expression(indent, code, expression, strings))
        .collect::<Vec<String>>()
        .join(", ");

    call.push_str(&fun);
    call.push('(');
    call.push_str(&params);
    call.push(')');
    call
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
