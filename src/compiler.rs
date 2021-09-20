use crate::ast::{self, Module, ReplEntry};
use crate::infer;
use crate::javascript;
use crate::parser;
use crate::source::Source;
use crate::tokenizer;
use crate::type_env::TypeEnv;
use std::collections::HashMap;
use std::rc::Rc;

pub fn compile(source: &Source) -> Result<String, String> {
    let tokens = tokenizer::parse(&source).map_err(|errors| {
        errors
            .iter()
            .map(|e| e.to_string(&source))
            .collect::<Vec<String>>()
            .join("\n\n")
    })?;

    let modules = parser::parse(source, &tokens).map_err(|error| error.to_string(&source))?;

    let mut module_interfaces: HashMap<String, Rc<TypeEnv>> = HashMap::new();
    let mut errors = vec![];

    for module in &modules {
        match infer::infer(&module_interfaces, &module) {
            Ok(typ) => {
                module_interfaces.insert(module.name.to_string(), typ);
            }
            Err((typ, mut module_errors)) => {
                module_interfaces.insert(module.name.to_string(), typ);
                errors.append(&mut module_errors);
            }
        }
    }

    if !errors.is_empty() {
        return Err(errors
            .iter()
            .map(|e| e.to_string(&source))
            .collect::<Vec<String>>()
            .join("\n\n"));
    }

    let javascript_files = javascript::generate(&modules, &module_interfaces);

    let mut out = String::new();

    // Print types
    out.push_str(
        &modules
            .iter()
            .map(|m| {
                format!(
                    "{}\n\n{}\n",
                    m.name.to_string(),
                    module_interfaces
                        .get(&m.name.to_string())
                        .unwrap_or(&Rc::new(TypeEnv::new()))
                )
            })
            .collect::<Vec<String>>()
            .join("\n\n"),
    );

    // Print code
    out.push_str(&javascript::files_to_bundle(&javascript_files));

    Ok(out)
}

pub fn compile_repl_entry(
    module: &mut Module,
    module_interfaces: &mut HashMap<String, Rc<TypeEnv>>,
    source: &Source,
) -> Result<String, String> {
    let tokens = tokenizer::parse(&source).map_err(|errors| {
        errors
            .iter()
            .map(|e| e.to_string(&source))
            .collect::<Vec<String>>()
            .join("\n\n")
    })?;

    let entry = parser::parse_repl(source, &tokens).map_err(|error| error.to_string(&source))?;

    let mut errors = vec![];

    // TODO: Annotate the AST with types. After inference, fetch what we stored in the module and
    // print the type of it
    match entry {
        ReplEntry::Import(import) => {
            module.imports.push(import);
            let result =
                compile_repl_entry_helper(&module, module_interfaces, &source, &mut errors);
            if result.is_err() {
                module.imports.pop();
            }
            result
        }
        ReplEntry::Definition(definition) => {
            module.definitions.push(definition);
            let result =
                compile_repl_entry_helper(&module, module_interfaces, &source, &mut errors);
            if result.is_err() {
                module.definitions.pop();
            }
            result
        }
        ReplEntry::Expression(expression) => {
            module.definitions.push(ast::Definition::Pattern(
                ast::Node {
                    value: ast::Pattern_::Hole,
                    start: 0,
                    end: 0,
                    line: 1,
                    column: 0,
                },
                expression,
            ));
            let result =
                compile_repl_entry_helper(&module, module_interfaces, &source, &mut errors);
            module.definitions.pop();
            result
        }
    }
    .map(|()| "ok.".to_string())
}

// Encapsulate the compiler logic regardless of repl entry type
fn compile_repl_entry_helper<'ast>(
    module: &'ast Module,
    module_interfaces: &mut HashMap<String, Rc<TypeEnv>>,
    source: &Source,
    errors: &mut Vec<infer::Error<'ast>>,
) -> Result<(), String> {
    let result = infer::infer(&module_interfaces, &module);
    match result {
        Ok(typ) => {
            module_interfaces.insert(module.name.to_string(), typ);
        }
        Err((_, mut module_errors)) => {
            errors.append(&mut module_errors);
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors
            .iter()
            .map(|e| e.to_string(&source))
            .collect::<Vec<String>>()
            .join("\n\n"))
    }
}
