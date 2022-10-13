use crate::ast::ModuleFullName;
use crate::compiler::state::{ModuleIndex, State};
use crate::parser;
use crate::source::Source;
use crate::tokenizer;
use crate::{infer, source};
use fnv::FnvHashSet as HashSet;

pub fn process_sources(entry_sources: Vec<Source>, state: &mut State) -> Vec<source::Index> {
    let mut entry_files: Vec<source::Index> = Vec::new();

    for source in entry_sources {
        let idx = state.add_source(source);
        entry_files.push(idx);
    }

    entry_files
}

pub fn parse_files(
    entry_files: &[source::Index],
    state: &mut State,
) -> Result<Vec<ModuleIndex>, String> {
    let mut errors: Vec<String> = vec![];
    let mut entry_modules = vec![];

    for file in entry_files {
        let mut parse_queue: Vec<ModuleFullName> = vec![];

        let parse_result = parse_file(*file, &mut parse_queue, state);

        match parse_result {
            Ok(modules) => {
                entry_modules.extend(modules);

                loop {
                    match parse_queue.pop() {
                        None => break,
                        Some(module_name) => {
                            if !state.has_ast(module_name) {
                                // Unparsed module.

                                // Resolve module to file path and parse it or error
                                // Maybe not error since infer will error with the import not
                                // found?
                                // TODO
                                // parse_file(&resolved_source, &mut parse_queue, &mut modules)?;
                            }
                        }
                    }
                }
            }
            Err(module_errors) => errors.extend(module_errors),
        }
    }

    if errors.is_empty() {
        Ok(entry_modules)
    } else {
        Err(errors_to_string(errors))
    }
}

fn parse_file(
    file: source::Index,
    parse_queue: &mut Vec<ModuleFullName>,
    state: &mut State,
) -> Result<Vec<ModuleIndex>, Vec<String>> {
    let strings = &mut state.strings;
    let source = &state.sources[file];
    let tokens = &mut state.tokens[file];
    tokenizer::parse(source, strings, tokens).map_err(|errors| {
        errors
            .iter()
            .map(|e| e.to_string(source))
            .collect::<Vec<String>>()
    })?;

    let mut module_names = vec![];

    let (module_ast, submodules) = parser::parse(source, tokens, strings)
        .map_err(|error| vec![error.to_string(source, strings)])?;

    let module_name = module_ast.name.full_name;
    let module_index = state.add_module_ast(file, module_ast);

    for submodule_ast in submodules {
        let submodule_name = submodule_ast.name.full_name;
        let submodule_index = state.add_module_ast(file, submodule_ast);

        // Add all submodules to queue
        // TODO: Is it really necessary to add this to the parse_queue given it has already been
        // parsed?
        parse_queue.push(submodule_name);
        module_names.push(submodule_index);

        // Queue dependencies
        let dependencies = state.modules[submodule_index].dependencies();
        for dependency in dependencies {
            if !state.has_ast(dependency.full_name) {
                parse_queue.push(dependency.full_name);
            }
        }
    }
    // And put the top level one last to start with it
    // TODO: Is it really necessary to add this to the parse_queue given it has already been
    // parsed?
    parse_queue.push(module_name);
    module_names.push(module_index);
    // Queue dependencies
    let dependencies = state.modules[module_index].dependencies();
    for dependency in dependencies {
        if !state.has_ast(dependency.full_name) {
            parse_queue.push(dependency.full_name);
        }
    }

    Ok(module_names)
}

pub fn check_cycles(
    entry_modules: &[ModuleIndex],
    state: &State,
) -> Result<Vec<ModuleIndex>, String> {
    let mut sorted_modules = vec![];
    let mut errors: Vec<String> = vec![];
    let mut visited_modules: HashSet<ModuleIndex> = HashSet::default();
    let mut entry_points_queue: Vec<Vec<ModuleIndex>> = vec![];
    entry_points_queue.extend(entry_modules.iter().map(|m| vec![*m]));

    fn last_visited_is(idx: &ModuleIndex, visited_path: &[ModuleIndex]) -> bool {
        visited_path.last().map(|m| m == idx).unwrap_or(false)
    }

    // For each entry point module, check for cycles separately
    'check_from_module: loop {
        let mut visited_path: Vec<ModuleIndex> = Vec::new();

        match entry_points_queue.pop() {
            None => break,
            Some(mut modules_queue) => {
                loop {
                    // Check the modules to visit queue
                    match modules_queue.pop() {
                        None => break,
                        Some(module_idx) => {
                            let already_visited = visited_modules.contains(&module_idx);

                            // If I haven't checked this module before at all in any entrypoint...
                            if !already_visited {
                                // Place it in the queue of modules to check it, in the visited modules
                                // path, and add its dependencies to check after it to the modules
                                // queue
                                let module_ast = &state.modules[module_idx];
                                modules_queue.push(module_idx);
                                visited_modules.insert(module_idx);
                                visited_path.push(module_idx);
                                sorted_modules.push(module_idx);

                                if !module_ast.imports.is_empty() {
                                    for dependency_ast in module_ast.dependencies() {
                                        // Check for direct cycles before adding it to the queue since
                                        // this disrupts the popping of elements from visited_path when
                                        // the module directly references itself, because we check by
                                        // name. Instead report the error here if it exists and bail
                                        // out.
                                        if dependency_ast.full_name != module_ast.name.full_name {
                                            modules_queue.push(
                                                state
                                                    .get_module_idx_with_name(dependency_ast.full_name).unwrap_or_else(|| {
                                                    let module_name = state.strings.resolve(module_ast.name.full_name);
                                                    panic!("Couldn't find module {module_name} in the ASTs")
                                                }),
                                            );
                                        } else {
                                            let name =
                                                state.strings.resolve(module_ast.name.full_name);
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
                                if last_visited_is(&module_idx, &visited_path) {
                                    visited_path.pop();
                                }

                                let found_cycle = check_cycles_in_path(
                                    &module_idx,
                                    &visited_path,
                                    &mut errors,
                                    state,
                                );

                                // If there is a cycle, don't bother checking anything else for this
                                // entry point module, try reporting cycles in other entry points.
                                if found_cycle {
                                    break 'check_from_module;
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    if errors.is_empty() {
        Ok(sorted_modules)
    } else {
        Err(errors_to_string(errors))
    }
}

fn check_cycles_in_path(
    module_name: &ModuleIndex,
    visited_path: &[ModuleIndex],
    errors: &mut Vec<String>,
    state: &State,
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
        let mut cycle_names: Vec<&str> = cycle_names
            .iter()
            .map(|m| state.strings.resolve(state.modules[*m].name.full_name))
            .collect();
        cycle_names.push(
            state
                .strings
                .resolve(state.modules[*module_name].name.full_name),
        );

        errors.push(cycle_error(&cycle_names));
    }

    cycle
}

fn cycle_error(cycle_names: &[&str]) -> String {
    let cycle_graph = cycle_names.join("\n        ↓\n    ");
    format!(
        "Cycle detected between modules:\n\n    {cycle_graph}\n\n\
        Alma's module system does not support \
        cyclic dependencies."
    )
}

pub fn infer(entry_modules: Vec<ModuleIndex>, state: &mut State) -> Result<(), String> {
    let mut errors = vec![];
    let mut infer_queue: Vec<ModuleIndex> = vec![];
    infer_queue.extend(entry_modules);

    loop {
        match infer_queue.pop() {
            None => break,
            Some(module_idx) => {
                // If not already inferred
                if state.module_interfaces[module_idx].is_some() {
                    continue;
                }
                let module_ast = &state.modules[module_idx];

                // Check dependencies and skip back to queue if there are any that need processing
                // before this module.
                {
                    let dependencies = module_ast.dependencies();
                    let mut skip = false;
                    for dependency in dependencies {
                        let name = dependency.full_name;
                        // If not inferred but parsed, ready for inference.
                        // If not parsed, it means it couldn't be found, and the module who
                        // imported it will report the error. Don't add it to the infer queue.
                        if !state.has_types(name) && state.has_ast(name) {
                            if !skip {
                                // At least one dependency to be proccessed first,
                                // put this module first in the queue and then the deps
                                skip = true;
                                infer_queue.push(module_idx);
                            }
                            infer_queue.push(state.get_module_idx_with_name(name).unwrap_or_else(
                                || {
                                    let module_name = state.strings.resolve(name);
                                    panic!("Couldn't find module {module_name} in the ASTs")
                                },
                            ));
                        }
                    }
                    if skip {
                        continue;
                    }
                }

                // Process this module
                match infer::infer(state, module_idx) {
                    Ok(()) => (),
                    Err(module_errors) => {
                        errors.append(
                            &mut module_errors
                                .iter()
                                .map(|e| {
                                    let source_idx = state.module_to_source_idx[module_idx];
                                    e.to_string(
                                        &state.sources[source_idx],
                                        &state.strings,
                                        &state.tokens[source_idx],
                                        &state.types,
                                        &state.modules[module_idx].expressions.spans,
                                    )
                                })
                                .collect::<Vec<String>>(),
                        );
                    }
                }
            }
        };
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors_to_string(errors))
    }
}

pub fn errors_to_string(errors: Vec<String>) -> String {
    errors.join("\n\n----------------------------------------\n\n")
}
