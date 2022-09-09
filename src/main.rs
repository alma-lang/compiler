mod ast;
mod cli;
mod compiler;
mod index;
mod infer;
mod javascript;
mod parser;
mod source;
mod strings;
mod token;
mod tokenizer;
mod typ;
mod type_env;

use std::path::Path;

use clap::{value_t, App, AppSettings, Arg, SubCommand};

fn main() {
    let matches = App::new("Alma")
        .version("0.0")
        .about("\nThe Alma language CLI. More info at https://alma-lang.org")
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .setting(AppSettings::VersionlessSubcommands)
        // Subcommands
        .subcommand(SubCommand::with_name("repl").about("Start the interactive Alma REPL"))
        .subcommand(
            SubCommand::with_name("make")
                .about("Compile files")
                .arg(
                    Arg::with_name("FILE")
                        .required(true)
                        .multiple(true)
                        .help("File entry points to compile"),
                )
                .arg(
                    Arg::with_name("output")
                        .default_value("alma/out")
                        .long("output")
                        .short("o")
                        .help("Where to emit the compiled output"),
                ),
        )
        .subcommand(
            SubCommand::with_name("bench")
                .about("Benchmark compilation")
                .setting(AppSettings::Hidden)
                .arg(
                    Arg::with_name("FILE")
                        .required(true)
                        .index(1)
                        .help("File to compile for the benchmark"),
                )
                .arg(
                    Arg::with_name("runs")
                        .default_value("1000")
                        .long("runs")
                        .short("r")
                        .help("Number of times to compile the file after reading it"),
                ),
        )
        .get_matches();

    if matches.subcommand_matches("repl").is_some() {
        cli::repl();
    } else if let Some(matches) = matches.subcommand_matches("bench") {
        cli::bench(
            value_t!(matches, "runs", u32).unwrap_or_else(|e| e.exit()),
            matches.value_of("FILE").unwrap().to_owned(),
        );
    } else if let Some(matches) = matches.subcommand_matches("make") {
        let files = matches
            .values_of("FILE")
            .unwrap()
            .map(|f| f.to_owned())
            .collect();
        let output = matches.value_of("output").unwrap();
        cli::compile_files(files, Path::new(output));
    }
}
