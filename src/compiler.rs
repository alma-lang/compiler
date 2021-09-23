use crate::ast::{self, Module, ReplEntry};
use crate::infer;
use crate::javascript;
use crate::module_asts::ModuleAsts;
use crate::module_interfaces::ModuleInterfaces;
use crate::parser;
use crate::source::Source;
use crate::tokenizer;
use smol_str::SmolStr;
use std::fmt::Write;

pub fn compile(source: &Source) -> Result<String, String> {
    // Tokenize and parse the single file.
    // TODO: When there are multiple files, this should be extracted and also called from the loop
    // when resolving modules and file names
    let tokens = tokenizer::parse(source).map_err(|errors| {
        errors
            .iter()
            .map(|e| e.to_string(source))
            .collect::<Vec<String>>()
            .join("\n\n")
    })?; // TODO: Don't exit when there are multiple files, append errors and skip next stages

    let (module, submodules) =
        parser::parse(source, &tokens).map_err(|error| error.to_string(source))?; // TODO: Don't exit when there are multiple files, append errors and skip next stages

    let mut infer_queue: Vec<SmolStr> = vec![];
    let mut module_asts = ModuleAsts::new();
    let mut module_interfaces = ModuleInterfaces::new();
    let mut errors: Vec<String> = vec![];

    for module in submodules {
        // Add all submodules to queue
        infer_queue.push(module.name.full_name.clone());
        module_asts.insert(module.name.full_name.clone(), module);
    }
    // And put the top level one last to start with it
    infer_queue.push(module.name.full_name.clone());
    module_asts.insert(module.name.full_name.clone(), module);

    loop {
        match infer_queue.pop() {
            None => break,
            Some(module_name) => {
                // Already inferred, skip
                if module_interfaces.map().contains_key(&module_name) {
                    continue;
                }

                let module = module_asts.get(&module_name).unwrap();

                // Check dependencies and skip back to queue if there are any that need processing
                // before this module.
                {
                    let dependencies = module.dependencies();
                    let mut skip = false;
                    for dependency in dependencies {
                        let name = &dependency.full_name;
                        if !module_interfaces.map().contains_key(name) {
                            if skip == false {
                                // At least one dependency to be proccessed first,
                                // put this module first in the queue and then the deps
                                skip = true;
                                infer_queue.push(module_name.clone());
                            }
                            infer_queue.push(name.clone());
                        }
                    }
                    if skip {
                        continue;
                    }
                }

                // Process this module
                let typ = match infer::infer(&module_interfaces, module) {
                    Ok(typ) => typ,
                    Err((typ, module_errors)) => {
                        errors.append(
                            &mut module_errors
                                .iter()
                                .map(|e| e.to_string(source))
                                .collect::<Vec<String>>(),
                        );
                        typ
                    }
                };
                module_interfaces.insert(module_name, typ);
            }
        };
    }

    if !errors.is_empty() {
        return Err(errors.join("\n\n"));
    }

    let javascript_files = javascript::generate(&module_asts, &module_interfaces);

    let mut out = String::new();

    // Print types
    write!(&mut out, "{}", &module_interfaces).unwrap();

    // Print code
    out.push_str(&javascript::files_to_bundle(&javascript_files));

    Ok(out)
}

pub fn compile_repl_entry(
    module: &mut Module,
    module_interfaces: &mut ModuleInterfaces,
    source: &Source,
) -> Result<String, String> {
    let tokens = tokenizer::parse(source).map_err(|errors| {
        errors
            .iter()
            .map(|e| e.to_string(source))
            .collect::<Vec<String>>()
            .join("\n\n")
    })?;

    let entry = parser::parse_repl(source, &tokens).map_err(|error| error.to_string(source))?;

    let mut errors = vec![];

    // TODO: Annotate the AST with types. After inference, fetch what we stored in the module and
    // print the type of it
    match entry {
        ReplEntry::Import(import) => {
            module.imports.push(import);
            let result = compile_repl_entry_helper(module, module_interfaces, source, &mut errors);
            if result.is_err() {
                module.imports.pop();
            }
            result
        }
        ReplEntry::Definition(definition) => {
            module.definitions.push(definition);
            let result = compile_repl_entry_helper(module, module_interfaces, source, &mut errors);
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
            let result = compile_repl_entry_helper(module, module_interfaces, source, &mut errors);
            module.definitions.pop();
            result
        }
    }
    .map(|()| "ok.".to_string())
}

// Encapsulate the compiler logic regardless of repl entry type
fn compile_repl_entry_helper<'ast>(
    module: &'ast Module,
    module_interfaces: &mut ModuleInterfaces,
    source: &Source,
    errors: &mut Vec<infer::Error<'ast>>,
) -> Result<(), String> {
    let result = infer::infer(module_interfaces, module);
    match result {
        Ok(typ) => {
            module_interfaces.insert(module.name.full_name.clone(), typ);
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
            .map(|e| e.to_string(source))
            .collect::<Vec<String>>()
            .join("\n\n"))
    }
}
