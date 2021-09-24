use crate::ast::ModuleFullName;
use crate::compiler::errors;
use crate::compiler::types::{ModuleAsts, ModuleInterfaces, ModuleSources, Sources};
use crate::infer;
use crate::parser;
use crate::source::Source;
use crate::tokenizer;

pub fn process_sources(entry_sources: Vec<Source>) -> (Vec<String>, Sources) {
    let entry_files: Vec<String> = entry_sources.iter().map(|s| s.name().to_string()).collect();

    let mut sources = Sources::default();
    for source in entry_sources {
        sources.insert(source.name().to_string(), source);
    }

    (entry_files, sources)
}

pub fn parse_files<'sources>(
    entry_files: &Vec<String>,
    sources: &'sources Sources,
) -> Result<(Vec<ModuleFullName>, ModuleSources<'sources>, ModuleAsts), String> {
    let mut errors: Vec<String> = vec![];
    let mut module_sources = ModuleSources::default();
    let mut module_asts = ModuleAsts::default();
    let mut entry_modules = vec![];

    for file in entry_files {
        let source = sources.get(file).unwrap();

        let mut parse_queue: Vec<ModuleFullName> = vec![];

        let parse_result = parse_file(
            &source,
            &mut parse_queue,
            &mut module_asts,
            &mut module_sources,
        );

        match parse_result {
            Ok(modules) => {
                entry_modules.extend(modules);

                loop {
                    match parse_queue.pop() {
                        None => break,
                        Some(module_name) => {
                            if !module_asts.contains_key(&module_name) {
                                // Unparsed module.

                                // Resolve module to file path and parse it or error
                                // Maybe not error since infer will error with the import not
                                // found?
                                // TODO
                                // parse_file(&resolved_source, &mut parse_queue, &mut module_asts)?;
                            }
                        }
                    }
                }
            }
            Err(module_errors) => errors.extend(module_errors),
        }
    }

    if errors.is_empty() {
        Ok((entry_modules, module_sources, module_asts))
    } else {
        Err(errors::to_string(errors))
    }
}

fn parse_file<'source>(
    source: &'source Source,
    parse_queue: &mut Vec<ModuleFullName>,
    module_asts: &mut ModuleAsts,
    module_sources: &mut ModuleSources<'source>,
) -> Result<Vec<ModuleFullName>, Vec<String>> {
    let tokens = tokenizer::parse(source).map_err(|errors| {
        errors
            .iter()
            .map(|e| e.to_string(source))
            .collect::<Vec<String>>()
    })?;

    let mut module_names = vec![];

    let (module, submodules) =
        parser::parse(source, &tokens).map_err(|error| vec![error.to_string(source)])?;

    for module in submodules {
        // Add all submodules to queue
        parse_queue.push(module.name.full_name.clone());
        module_names.push(module.name.full_name.clone());
        module_sources.insert(module.name.full_name.clone(), source);

        // Queue dependencies
        let dependencies = module.dependencies();
        for dependency in dependencies {
            let name = &dependency.full_name;
            if !module_asts.contains_key(name) {
                parse_queue.push(name.clone());
            }
        }

        module_asts.insert(module.name.full_name.clone(), module);
    }
    // And put the top level one last to start with it
    parse_queue.push(module.name.full_name.clone());
    module_names.push(module.name.full_name.clone());
    module_sources.insert(module.name.full_name.clone(), source);
    // Queue dependencies
    let dependencies = module.dependencies();
    for dependency in dependencies {
        let name = &dependency.full_name;
        if !module_asts.contains_key(name) {
            parse_queue.push(name.clone());
        }
    }
    module_asts.insert(module.name.full_name.clone(), module);

    Ok(module_names)
}

fn check_cycles_in_path(
    module_name: &ModuleFullName,
    visited_path: &[ModuleFullName],
    errors: &mut Vec<String>,
) -> bool {
    let mut cycle = false;
    let mut cycle_names = visited_path;

    debug_assert!(
        &cycle_names[cycle_names.len() - 1] == module_name,
        "Last item in visited_path should always be this module's name"
    );

    for (i, visited) in visited_path.iter().rev().enumerate() {
        // Skip the last one, since it should always be this module's name
        if i > 0 && visited == module_name {
            cycle = true;
            cycle_names = &visited_path[visited_path.len() - (i + 1)..];
        }
    }

    if cycle {
        errors.push(format!(
            "Cycle detected between modules:\n\n    {}\n\n\
                            Alma's module system does not support \
                            cyclic dependencies.",
            cycle_names.join("\n        ↓\n    ")
        ));
    }

    cycle
}

pub fn infer(
    entry_modules: Vec<ModuleFullName>,
    module_sources: &ModuleSources,
    module_asts: &ModuleAsts,
) -> Result<ModuleInterfaces, String> {
    let mut errors = vec![];
    let mut module_interfaces = ModuleInterfaces::new();
    let mut infer_queue: Vec<ModuleFullName> = vec![];
    infer_queue.extend(entry_modules);

    loop {
        match infer_queue.pop() {
            None => break,
            Some(module_name) => {
                // If not already inferred
                if module_interfaces.map().contains_key(&module_name) {
                    continue;
                }
                let module = module_asts
                    .get(&module_name)
                    .unwrap_or_else(|| panic!("Couldn't find module {} in the ASTs", module_name));

                // Check dependencies and skip back to queue if there are any that need processing
                // before this module.
                {
                    let dependencies = module.dependencies();
                    let mut skip = false;
                    for dependency in dependencies {
                        let name = &dependency.full_name;
                        // If not inferred but parsed, ready for inference.
                        // If not parsed, it means it couldn't be found, and the module who
                        // imported it will report the error. Don't add it to the infer queue.
                        if !module_interfaces.map().contains_key(name)
                            && module_asts.contains_key(name)
                        {
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
                                .map(|e| e.to_string(module_sources.get(&module_name).unwrap()))
                                .collect::<Vec<String>>(),
                        );
                        typ
                    }
                };

                module_interfaces.insert(module_name.clone(), typ);
            }
        };
    }

    if errors.is_empty() {
        Ok(module_interfaces)
    } else {
        Err(errors::to_string(errors))
    }
}
