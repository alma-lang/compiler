mod ast;
mod cli;
mod compiler;
mod infer;
mod javascript;
mod parser;
mod source;
mod token;
mod tokenizer;
mod typ;
mod type_env;

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
            SubCommand::with_name("make").about("Compile files").arg(
                Arg::with_name("FILE")
                    .required(true)
                    // Just one file for now
                    // .multiple(true)
                    .help("File entry points to compile"),
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

    if let Some(_) = matches.subcommand_matches("repl") {
        cli::prompt();
    } else if let Some(matches) = matches.subcommand_matches("bench") {
        cli::bench(
            value_t!(matches, "runs", u32).unwrap_or_else(|e| e.exit()),
            matches.value_of("FILE").unwrap().to_owned(),
        );
    } else if let Some(matches) = matches.subcommand_matches("make") {
        // TODO: Just one file for now
        // let files = matches.values_of_lossy("FILE").unwrap();
        let files = vec![matches.value_of_lossy("FILE").unwrap().into_owned()];
        cli::compile_files(files);
    }
}
