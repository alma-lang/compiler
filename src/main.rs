mod ast;
mod compiler;
mod parser;
mod source;
mod token;
mod tokenizer;

use crate::compiler::compile;

fn main() {
    use crate::source::Source;

    match compile(&Source::new_orphan("Hello, world!")) {
        Ok(out) => println!("OK\n\n{}", out),
        Err(errs) => println!("ERROR\n\n{}", errs),
    }
}
