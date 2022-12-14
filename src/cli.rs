use crate::compiler;
use crate::compiler::stages::process_sources;
use crate::javascript::OutputFile;
use crate::source::Source;
use std::path::{Path, PathBuf};
// use rustyline::{error::ReadlineError, Editor};
use std::fs;
use std::process;

pub fn repl() {
    unimplemented!();
    /*
    let mut state = compiler::State::new();
    let primitive_types = Type::primitive_types(&mut state.strings);

    let module_name = ast::ModuleName::new(
        vec![ast::Node {
            value: {
                let sym = state.strings.get_or_intern("Repl");
                ast::CapitalizedIdentifier_::new(sym)
            },
            start: 0.into(),
            end: 0.into(),
        }],
        &mut state.strings,
    )
    .unwrap();

    let source_idx = state.add_source(Source::new_orphan("".to_owned()));
    state.add_module_ast(
        source_idx,
        ast::Module {
            name: module_name,
            exports: vec![],
            imports: vec![],
            definitions: vec![],
            type_definitions: vec![],
        },
    );

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
    */
}

pub fn compile_files(arg_files: Vec<String>, output: &Path) {
    let mut files = vec!["Alma.alma".to_owned()];
    files.extend(arg_files);
    let sources: Vec<Source> = files.iter().map(|f| Source::from_path(f)).collect();
    let mut state = compiler::State::new();
    let entry_sources = process_sources(sources, &mut state);

    match compiler::compile(&entry_sources, &mut state, output) {
        Ok(files) => {
            for (path, file) in files {
                let mut dir = PathBuf::from(&path);
                dir.pop();
                let dir_write_result = fs::create_dir_all(&dir);
                match file {
                    OutputFile::File(contents) => {
                        let write_result =
                            dir_write_result.and_then(|()| fs::write(&path, contents));
                        if let Err(e) = write_result {
                            eprintln!("\nWriting output file at '{path}' failed.\n");
                            eprintln!("\n{e}\n");
                            process::exit(1);
                        }
                    }
                    OutputFile::CopyFrom(from_path) => {
                        let copy_result =
                            dir_write_result.and_then(|()| fs::copy(&from_path, &path));
                        if let Err(e) = copy_result {
                            eprintln!("\nCopying file '{from_path}' to '{path}' failed.\n");
                            eprintln!("\n{e}\n");
                            process::exit(1);
                        }
                    }
                }
            }
            let output = output.to_str().unwrap();
            println!("Success. Files emitted in {output}");
        }
        Err(errs) => {
            eprintln!("\n{errs}\n");
            process::exit(1);
        }
    };
}

pub fn bench(runs: u32, file_path: String) {
    let mut state = compiler::State::new();
    let entry_sources = process_sources(
        vec![
            Source::from_path("Alma.alma"),
            Source::from_path(&file_path),
        ],
        &mut state,
    );

    for _ in 0..runs {
        match compiler::compile(&entry_sources, &mut state, Path::new("")) {
            Ok(_out) => println!("Ok."),
            Err(errs) => {
                eprintln!("\n{errs}\n");
                process::exit(1);
            }
        };
    }
}
