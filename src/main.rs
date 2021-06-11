mod ast;
mod cli;
mod compiler;
mod infer;
mod parser;
mod source;
mod token;
mod tokenizer;
mod typ;
mod type_env;

use std::env;
use std::process;

fn main() {
    let argv = env::args().skip(1).collect::<Vec<String>>();
    let args: Vec<_> = argv.iter().map(|x| x.as_str()).collect();

    match args.as_slice() {
        ["repl"] => cli::prompt(),
        ["run", path] => cli::file(path.to_string()),
        _ => {
            println!(
                "
  repl                 Start the REPL
  run [file.alma]      Run [file.alma]
"
            );
            process::exit(0);
        }
    };
}
