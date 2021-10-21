use crate::ast::{
    Definition, Export_, Expression, ExpressionType as ET, Module, ModuleFullName, ModuleName,
    Pattern, Pattern_ as P, Unary_ as U,
};
use crate::compiler::types::{ModuleAsts, ModuleInterface, ModuleInterfaces};
use crate::strings::Strings;
use std::fmt::Write;

const INDENT: usize = 4;

fn module_full_name(out: &mut String, name: &ModuleName, strings: &Strings) {
    for (i, module) in name.parts.iter().enumerate() {
        if i > 0 {
            out.push_str("__");
        }
        write!(out, "{}", module.value.to_string(strings)).unwrap();
    }
}

pub fn generate(
    sorted_modules: &[ModuleFullName],
    module_asts: &ModuleAsts,
    module_interfaces: &ModuleInterfaces,
    strings: &Strings,
) -> String {
    let mut code = String::new();

    // Print types
    write!(
        &mut code,
        "/*\n\n{}\n\n*/\n\n",
        &module_interfaces.to_string(strings)
    )
    .unwrap();

    code.push_str("\nglobalThis.Alma = Object.assign(globalThis.Alma ?? {}, function () {\n\n");

    // for module_name in sorted_modules.iter().rev() {
    //     let name = strings.resolve(*module_name);
    //     writeln!(code, "let {} = ", alias.value.to_string(strings),).unwrap();
    // }

    for module_name in sorted_modules {
        let module = module_asts
            .get(module_name)
            .unwrap_or_else(|| panic!("Couldn't get module {}", strings.resolve(*module_name)));

        code.push_str("\nlet ");
        module_full_name(&mut code, &module.name, strings);
        code.push_str(" = function () {\n");

        generate_file(
            &mut code,
            module,
            module_interfaces.get(module_name).unwrap(),
            strings,
        );

        code.push_str("\n}();\n");
    }

    code.push_str("\nreturn {\n");
    for module in module_asts.values() {
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
    if !module.imports.is_empty() {
        code.push('\n');
        generate_imports(code, module, strings);
    }

    code.push('\n');
    generate_definitions(add_indent(0), code, true, &module.definitions, strings);

    if !module.exports.is_empty() {
        code.push('\n');
        generate_exports(code, module, strings);
    }
}

fn generate_imports(code: &mut String, module: &Module, strings: &Strings) {
    let indent = add_indent(0);

    for import in &module.imports {
        let import = &import.value;

        match &import.alias {
            Some(alias) => {
                indented(code, indent, "");
                write!(code, "let {} = ", alias.value.to_string(strings),).unwrap();
                module_full_name(code, &import.module_name, strings);
                code.push('\n');
            }
            None => {
                // TODO: This is wrong, and code will be accessing this via normal prop access so
                // here we need to do something like:
                //     let Modules;
                //     (Constants = (Modules = Modules ?? {}).Constants ?? {});
                //
                // write!(
                //     code,
                //     "let {0} = {0}\n",
                //     import
                //         .module_name
                //         .parts
                //         .get(0)
                //         .unwrap_or_else(|| panic!(
                //             "A module name should never be empty. {}",
                //             import.module_name.to_string(strings)
                //         ))
                //         .value
                //         .to_string(strings)
                // )
                // .unwrap();
                // todo!()
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
            write!(code, "let {{ {} }} = ", identifiers.join(", "),).unwrap();
            module_full_name(code, &import.module_name, strings);
            code.push('\n');
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
    for (i, definition) in definitions.iter().enumerate() {
        if i > 0 && space_between {
            code.push('\n')
        }
        match definition {
            Definition::Lambda(name, expression) => match &expression.value.expr {
                ET::Lambda(params, body) => {
                    indented(code, indent, "");
                    generate_function(indent, code, name.value.to_string(strings), params, body, strings)
                }
                _ => panic!("Top level definitions classified as Lambda should always have a Lambda expression on the right hand side. This is a compiler bug. Please report it!"),
            },
            Definition::Pattern(pattern, expression) => {
                generate_let(indent, code, pattern, expression, strings)
            }
        }
    }
}

fn generate_exports(code: &mut String, module: &Module, strings: &Strings) {
    let indent = add_indent(0);
    line(code, indent, "return {");

    for export in &module.exports {
        match &export.value {
            Export_::Identifier(ident) => {
                indented(code, add_indent(indent), "");
                writeln!(code, "{},", ident.value.to_string(strings)).unwrap();
            }
            Export_::Type(_, _) =>
            // TODO: Generate type constructors
            {
                ()
            }
        }
    }

    line(code, indent, "};");
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

    code.push_str(") {\n");

    {
        let indent = add_indent(indent);
        indented(code, indent, "return ");
        generate_expression(indent, code, body, strings);
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

        ET::Float(float) => write!(code, "{}", float).unwrap(),

        ET::String_(string) => write!(code, "\"{}\"", strings.resolve(*string)).unwrap(),

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
    write!(out, "{0:1$}{2}", "", indent, line).unwrap();
}
fn line(out: &mut String, indent: usize, line: &str) {
    indented(out, indent, line);
    out.push('\n');
}
