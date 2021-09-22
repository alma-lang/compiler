use crate::ast;
use crate::compiler;
use crate::module_interfaces::ModuleInterfaces;
use crate::source::Source;
use rustyline::{error::ReadlineError, Editor};
use std::process;

pub fn prompt() {
    let module_name = ast::ModuleName::new(vec![ast::Node {
        value: ast::Identifier_::new("REPL"),
        start: 0,
        end: 0,
        line: 1,
        column: 0,
    }])
    .unwrap();

    let mut module = ast::Module {
        name: module_name,
        exports: vec![],
        imports: vec![],
        definitions: vec![],
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
                match compiler::compile_repl_entry(&mut module, &mut module_interfaces, &file) {
                    Ok(out) => println!("{}", out),
                    Err(errs) => println!("{}", errs),
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
                println!("Error: {:?}", err);
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
            eprintln!("There was a problem with the file '{}'", file_path);
            eprintln!("{}", err);
            process::exit(1);
        }
    }
}

pub fn compile_files(files: Vec<String>) {
    for file_path in files {
        let file = source_from_path(file_path);

        match compiler::compile(&file) {
            Ok(out) => println!("{}", out),
            Err(errs) => println!("{}", errs),
        };
    }
}

pub fn bench(runs: u32, file_path: String) {
    let file = source_from_path(file_path);

    for _ in 0..runs {
        match compiler::compile(&file) {
            Ok(out) => println!("{}", out),
            Err(errs) => println!("{}", errs),
        };
    }
}
