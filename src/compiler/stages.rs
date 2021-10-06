use crate::ast::ModuleFullName;
use crate::compiler::errors;
use crate::compiler::types::{ModuleAsts, ModuleInterfaces, ModuleSources, Sources};
use crate::infer;
use crate::parser;
use crate::source::Source;
use crate::strings::Strings;
use crate::tokenizer;
use crate::typ::PrimitiveTypes;
use fnv::FnvHashSet as HashSet;

pub fn process_sources(entry_sources: Vec<Source>) -> (Vec<String>, Sources) {
    let entry_files: Vec<String> = entry_sources.iter().map(|s| s.name().to_string()).collect();

    let mut sources = Sources::default();
    for source in entry_sources {
        sources.insert(source.name().to_string(), source);
    }

    (entry_files, sources)
}

pub fn parse_files<'sources>(
    entry_files: &[String],
    sources: &'sources Sources,
    strings: &mut Strings,
) -> Result<(Vec<ModuleFullName>, ModuleSources<'sources>, ModuleAsts), String> {
    let mut errors: Vec<String> = vec![];
    let mut module_sources = ModuleSources::default();
    let mut module_asts = ModuleAsts::default();
    let mut entry_modules = vec![];

    for file in entry_files {
        let source = sources.get(file).unwrap();

        let mut parse_queue: Vec<ModuleFullName> = vec![];

        let parse_result = parse_file(
            source,
            &mut parse_queue,
            &mut module_asts,
            &mut module_sources,
            strings,
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
    strings: &mut Strings,
) -> Result<Vec<ModuleFullName>, Vec<String>> {
    let tokens = tokenizer::parse(source).map_err(|errors| {
        errors
            .iter()
            .map(|e| e.to_string(source))
            .collect::<Vec<String>>()
    })?;

    let mut module_names = vec![];

    let (module, submodules) =
        parser::parse(source, &tokens, strings).map_err(|error| vec![error.to_string(source)])?;

    for module in submodules {
        // Add all submodules to queue
        parse_queue.push(module.name.full_name);
        module_names.push(module.name.full_name);
        module_sources.insert(module.name.full_name, source);

        // Queue dependencies
        let dependencies = module.dependencies();
        for dependency in dependencies {
            let name = &dependency.full_name;
            if !module_asts.contains_key(name) {
                parse_queue.push(*name);
            }
        }

        module_asts.insert(module.name.full_name, module);
    }
    // And put the top level one last to start with it
    parse_queue.push(module.name.full_name);
    module_names.push(module.name.full_name);
    module_sources.insert(module.name.full_name, source);
    // Queue dependencies
    let dependencies = module.dependencies();
    for dependency in dependencies {
        let name = &dependency.full_name;
        if !module_asts.contains_key(name) {
            parse_queue.push(*name);
        }
    }
    module_asts.insert(module.name.full_name, module);

    Ok(module_names)
}

pub fn check_cycles(
    entry_modules: &[ModuleFullName],
    module_asts: &ModuleAsts,
    strings: &Strings,
) -> Result<Vec<ModuleFullName>, String> {
    let mut sorted_modules = vec![];
    let mut errors: Vec<String> = vec![];
    let mut visited_modules: HashSet<ModuleFullName> = HashSet::default();
    let mut entry_points_queue: Vec<Vec<ModuleFullName>> = vec![];
    entry_points_queue.extend(entry_modules.iter().map(|m| vec![*m]));

    fn last_visited_is(name: &ModuleFullName, visited_path: &[ModuleFullName]) -> bool {
        visited_path.last().map(|m| m == name).unwrap_or(false)
    }

    // For each entry point module, check for cycles separately
    'check_from_module: loop {
        let mut visited_path: Vec<ModuleFullName> = Vec::new();

        match entry_points_queue.pop() {
            None => break,
            Some(mut modules_queue) => loop {
                // Check the modules to visit queue
                match modules_queue.pop() {
                    None => break,
                    Some(module_name) => {
                        let already_visited = visited_modules.contains(&module_name);

                        // If I haven't checked this module before at all in any entrypoint...
                        if !already_visited {
                            // Place it in the queue of modules to check it, in the visited modules
                            // path, and add its dependencies to check after it to the modules
                            // queue
                            let module = module_asts.get(&module_name).unwrap();
                            modules_queue.push(module_name);
                            visited_modules.insert(module_name);
                            visited_path.push(module_name);
                            sorted_modules.push(module_name);

                            if !module.imports.is_empty() {
                                for module in module.dependencies() {
                                    // Check for direct cycles before adding it to the queue since
                                    // this disrupts the popping of elements from visited_path when
                                    // the module directly references itself, because we check by
                                    // name. Instead report the error here if it exists and bail
                                    // out.
                                    if module.full_name != module_name {
                                        modules_queue.push(module.full_name);
                                    } else {
                                        let name = strings.resolve(module_name);
                                        errors.push(cycle_error(&[name, name]));
                                        break 'check_from_module;
                                    }
                                }
                            }
                        } else {
                            // If the module has already been visited then we check it for cycles
                            // in the current visited_path

                            // If the module was visited during this modules queue, it should be in
                            // the visited_path last, so we remove it for check_cycles_in_path.
                            // This one check was problematic with direct cycles like A imports A,
                            // but this is checked separately when adding dependencies above.
                            if last_visited_is(&module_name, &visited_path) {
                                visited_path.pop();
                            }

                            let found_cycle = check_cycles_in_path(
                                &module_name,
                                &visited_path,
                                &mut errors,
                                strings,
                            );

                            // If there is a cycle, don't bother checking anything else for this
                            // entry point module, try reporting cycles in other entry points.
                            if found_cycle {
                                break 'check_from_module;
                            }
                        }
                    }
                }
            },
        }
    }

    if errors.is_empty() {
        Ok(sorted_modules)
    } else {
        Err(errors::to_string(errors))
    }
}

fn check_cycles_in_path(
    module_name: &ModuleFullName,
    visited_path: &[ModuleFullName],
    errors: &mut Vec<String>,
    strings: &Strings,
) -> bool {
    let mut cycle = false;
    let mut cycle_names = visited_path;

    for (i, visited) in visited_path.iter().rev().enumerate() {
        if visited == module_name {
            cycle = true;
            cycle_names = &visited_path[visited_path.len() - (i + 1)..];
            break;
        }
    }

    if cycle {
        let mut cycle_names: Vec<&str> = cycle_names.iter().map(|m| strings.resolve(*m)).collect();
        cycle_names.push(strings.resolve(*module_name));

        errors.push(cycle_error(&cycle_names));
    }

    cycle
}

fn cycle_error(cycle_names: &[&str]) -> String {
    format!(
        "Cycle detected between modules:\n\n    {}\n\n\
                            Alma's module system does not support \
                            cyclic dependencies.",
        cycle_names.join("\n        ↓\n    ")
    )
}

pub fn infer(
    entry_modules: Vec<ModuleFullName>,
    module_sources: &ModuleSources,
    module_asts: &ModuleAsts,
    primitive_types: &PrimitiveTypes,
    strings: &mut Strings,
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
                let module = module_asts.get(&module_name).unwrap_or_else(|| {
                    panic!(
                        "Couldn't find module {} in the ASTs",
                        strings.resolve(module_name)
                    )
                });

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
                            if !skip {
                                // At least one dependency to be proccessed first,
                                // put this module first in the queue and then the deps
                                skip = true;
                                infer_queue.push(module_name);
                            }
                            infer_queue.push(*name);
                        }
                    }
                    if skip {
                        continue;
                    }
                }

                // Process this module
                let typ = match infer::infer(&module_interfaces, module, primitive_types, strings) {
                    Ok(typ) => typ,
                    Err((typ, module_errors)) => {
                        errors.append(
                            &mut module_errors
                                .iter()
                                .map(|e| {
                                    e.to_string(module_sources.get(&module_name).unwrap(), strings)
                                })
                                .collect::<Vec<String>>(),
                        );
                        typ
                    }
                };

                module_interfaces.insert(module_name, typ);
            }
        };
    }

    if errors.is_empty() {
        Ok(module_interfaces)
    } else {
        Err(errors::to_string(errors))
    }
}
