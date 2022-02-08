use crate::ast;
use crate::compiler;
use crate::compiler::stages::process_sources;
use crate::compiler::types::ModuleInterfaces;
use crate::source::Source;
use crate::strings::Strings;
use crate::typ::Type;
use rustyline::{error::ReadlineError, Editor};
use std::process;

pub fn repl() {
    let mut strings = Strings::new();
    let primitive_types = Type::primitive_types(&mut strings);

    let module_name = ast::ModuleName::new(
        vec![ast::Node {
            value: ast::CapitalizedIdentifier_::new("Repl", &mut strings),
            start: 0,
            end: 0,
            line: 1,
            column: 0,
        }],
        &mut strings,
    )
    .unwrap();

    let mut module = ast::Module {
        name: module_name,
        exports: vec![],
        imports: vec![],
        definitions: vec![],
        type_definitions: vec![],
    };
    let mut module_interfaces = ModuleInterfaces::new();

    let mut rl = Editor::<()>::new();
    // TODO: figure out how to store this inthe global config dir
    // let _ = rl.load_history(repl_history_path);

    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());

                let file = Source::new_orphan(line);
                match compiler::compile_repl_entry(
                    &mut module,
                    &mut module_interfaces,
                    &file,
                    &mut strings,
                    &primitive_types,
                ) {
                    Ok(out) => println!("{out}"),
                    Err(errs) => eprintln!("{errs}"),
                };
                println!();
            }
            Err(ReadlineError::Interrupted) => {
                println!("Bye!");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("Bye!");
                break;
            }
            Err(err) => {
                eprintln!("Error: {err:?}");
                break;
            }
        }
    }

    // TODO: See above on rl.load_history
    // rl.append_history(repl_history_path).unwrap();
}

fn source_from_path(file_path: String) -> Source {
    match Source::new_file(file_path.clone()) {
        Ok(source) => source,
        Err(err) => {
            eprintln!("There was a problem with the file '{file_path}'");
            eprintln!("{err}");
            process::exit(1);
        }
    }
}

pub fn compile_files(files: Vec<String>) {
    let sources: Vec<Source> = files.into_iter().map(source_from_path).collect();
    let (entry_sources, sources) = process_sources(sources);

    match compiler::compile(&entry_sources, &sources) {
        Ok(out) => println!("{out}"),
        Err(errs) => {
            eprintln!("\n{errs}\n");
            process::exit(1);
        }
    };
}

pub fn bench(runs: u32, file_path: String) {
    let (entry_sources, sources) = process_sources(vec![source_from_path(file_path)]);

    for _ in 0..runs {
        match compiler::compile(&entry_sources, &sources) {
            Ok(out) => println!("{out}"),
            Err(errs) => {
                eprintln!("\n{errs}\n");
                process::exit(1);
            }
        };
    }
}
