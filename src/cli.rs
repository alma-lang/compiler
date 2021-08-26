use crate::ast;
use crate::compiler;
use crate::source::Source;
use crate::type_env::TypeEnv;
use rustyline::{error::ReadlineError, Editor};
use std::collections::HashMap;
use std::fs;
use std::process;
use std::rc::Rc;

pub fn prompt() {
    let mut module = ast::Module {
        name: ast::ModuleName(vec![ast::Node {
            value: ast::Identifier_::new("REPL"),
            start: 0,
            end: 0,
            line: 1,
            column: 0,
        }]),
        exports: vec![],
        imports: vec![],
        definitions: vec![],
    };
    let mut module_interfaces: HashMap<String, Rc<TypeEnv>> = HashMap::new();

    let mut rl = Editor::<()>::new();
    // TODO: figure out how to store this inthe global config dir
    // let _ = rl.load_history(repl_history_path);

    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());

                let file = Source::new_orphan(&line);
                match compiler::compile_repl_entry(&mut module, &mut module_interfaces, &file) {
                    Ok(out) => println!("{}", out),
                    Err(errs) => println!("{}", errs),
                };
                println!("");
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

fn source_from_path(file_path: &str) -> String {
    fs::read_to_string(&file_path).unwrap_or_else(|err| {
        println!("Failed to read file {}", file_path);
        println!("  {}", err);
        process::exit(1);
    })
}

pub fn compile_files(files: Vec<String>) {
    for file_path in files {
        let contents = source_from_path(&file_path);
        let file = Source::new_file(file_path, &contents);

        match compiler::compile(&file) {
            Ok(out) => println!("{}", out),
            Err(errs) => println!("{}", errs),
        };
    }
}

pub fn bench(runs: u32, file_path: String) {
    let contents = source_from_path(&file_path);
    let file = Source::new_file(file_path, &contents);

    for _ in 0..runs {
        match compiler::compile(&file) {
            Ok(out) => println!("{}", out),
            Err(errs) => println!("{}", errs),
        };
    }
}
