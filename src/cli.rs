use crate::compiler::compile;
use crate::source::Source;
use std::fs;
use std::io::{self, Write};
use std::process;

pub fn prompt() {
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
        match compile(&file) {
            Ok(out) => println!("{}", out),
            Err(errs) => println!("{}", errs),
        };
        println!("");
    }
}

pub fn file(file_path: String) {
    let contents = fs::read_to_string(&file_path).unwrap_or_else(|err| {
        println!("Failed to read file {}", file_path);
        println!("  {}", err);
        process::exit(1);
    });

    let file = Source::new_file(file_path, &contents);

    match compile(&file) {
        Ok(out) => println!("{}", out),
        Err(errs) => println!("{}", errs),
    };
}
