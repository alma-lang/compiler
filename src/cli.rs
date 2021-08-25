use crate::ast;
use crate::compiler;
use crate::source::Source;
use crate::type_env::TypeEnv;
use std::collections::HashMap;
use std::fs;
use std::io::{self, Write};
use std::process;
use std::rc::Rc;

pub fn prompt() {
    let mut module = ast::Module {
        name: ast::Node {
            value: ast::Identifier_::new("REPL"),
            start: 0,
            end: 0,
            line: 1,
            column: 0,
        },
        exports: vec![],
        imports: vec![],
        definitions: vec![],
    };
    let mut module_interfaces: HashMap<String, Rc<TypeEnv>> = HashMap::new();

    loop {
        print!("> ");
        io::stdout().flush().expect("Flushing REPL prompt failed");

        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap_or_else(|err| {
            println!("Failed to read line");
            println!("  {}", err);
            process::exit(1);
        });
        println!("");

        let file = Source::new_orphan(&input);
        match compiler::compile_repl_entry(&mut module, &mut module_interfaces, &file) {
            Ok(out) => println!("{}", out),
            Err(errs) => println!("{}", errs),
        };
        println!("");
    }
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
