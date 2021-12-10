pub mod errors;
pub mod stages;
pub mod types;

use crate::ast::{self, Module, ReplEntry, TypedDefinition};
use crate::compiler::stages::{check_cycles, infer, parse_files};
use crate::compiler::types::{ModuleInterfaces, Sources};
use crate::infer;
use crate::javascript;
use crate::parser;
use crate::source::Source;
use crate::strings::Strings;
use crate::tokenizer;
use crate::typ::Type;
use crate::type_env::PolyTypeEnv;

pub fn compile(entry_sources: &[String], sources: &Sources) -> Result<String, String> {
    let mut strings = Strings::new();

    let (entry_modules, module_sources, module_asts) =
        parse_files(entry_sources, sources, &mut strings)?;

    let mut sorted_modules = check_cycles(&entry_modules, &module_asts, &strings)?;
    sorted_modules.reverse();

    let primitive_types = Type::primitive_types(&mut strings);

    let module_interfaces = infer(
        entry_modules,
        &module_sources,
        &module_asts,
        &primitive_types,
        &mut strings,
    )?;

    Ok(javascript::generate(
        &sorted_modules,
        &module_asts,
        &module_interfaces,
        &strings,
    ))
}

pub fn compile_repl_entry(
    module: &mut Module,
    module_interfaces: &mut ModuleInterfaces,
    source: &Source,
    strings: &mut Strings,
    primitive_types: &PolyTypeEnv,
) -> Result<String, String> {
    let tokens = tokenizer::parse(source).map_err(|errors| {
        errors
            .iter()
            .map(|e| e.to_string(source))
            .collect::<Vec<String>>()
            .join("\n\n")
    })?;

    let entry =
        parser::parse_repl(source, &tokens, strings).map_err(|error| error.to_string(source))?;

    let mut errors = vec![];

    // TODO: Annotate the AST with types. After inference, fetch what we stored in the module and
    // print the type of it
    match entry {
        ReplEntry::Import(import) => {
            module.imports.push(import);
            let result = compile_repl_entry_helper(
                module,
                module_interfaces,
                source,
                &mut errors,
                strings,
                primitive_types,
            );
            if result.is_err() {
                module.imports.pop();
            }
            result
        }
        ReplEntry::Definition(definition) => {
            module
                .definitions
                .push(TypedDefinition::Untyped(definition));
            let result = compile_repl_entry_helper(
                module,
                module_interfaces,
                source,
                &mut errors,
                strings,
                primitive_types,
            );
            if result.is_err() {
                module.definitions.pop();
            }
            result
        }
        ReplEntry::Expression(expression) => {
            module
                .definitions
                .push(TypedDefinition::Untyped(ast::Definition::Pattern(
                    ast::Node {
                        value: ast::Pattern_::Hole,
                        start: 0,
                        end: 0,
                        line: 1,
                        column: 0,
                    },
                    expression,
                )));
            let result = compile_repl_entry_helper(
                module,
                module_interfaces,
                source,
                &mut errors,
                strings,
                primitive_types,
            );
            module.definitions.pop();
            result
        }
    }
    .map(|()| "ok.".to_string())
}

// Encapsulate the compiler logic regardless of repl entry type
fn compile_repl_entry_helper<'ast>(
    module: &'ast Module,
    module_interfaces: &mut ModuleInterfaces,
    source: &Source,
    errors: &mut Vec<infer::Error<'ast>>,
    strings: &mut Strings,
    primitive_types: &PolyTypeEnv,
) -> Result<(), String> {
    let result = infer::infer(module_interfaces, module, primitive_types, strings);
    match result {
        Ok(typ) => {
            module_interfaces.insert(module.name.full_name, typ);
        }
        Err((_, mut module_errors)) => {
            errors.append(&mut module_errors);
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors
            .iter()
            .map(|e| e.to_string(source, strings))
            .collect::<Vec<String>>()
            .join("\n\n"))
    }
}

#[cfg(test)]
pub mod tests {
    use crate::compiler::stages::process_sources;
    use crate::source::Source;
    use insta::assert_snapshot;

    #[test]
    pub fn test_compile() {
        fn compile(source_codes: &[&'static str]) -> String {
            let (entry_sources, sources) = process_sources(
                source_codes
                    .iter()
                    .map(|s| Source::new_orphan(s.to_string()))
                    .collect(),
            );
            match super::compile(&entry_sources, &sources) {
                Ok(s) => s,
                Err(e) => e,
            }
        }

        // Normal modules

        assert_snapshot!(compile(&[r"
module Modules exposing (main)

import Modules.WeirdMath exposing (weirdAdd)

main = weirdAdd



module Modules.Constants exposing (five)

    five = 5



module Modules.WeirdMath exposing (weirdAdd)

    import Modules.Constants exposing (five)

    weirdAdd = \x y -> x * y + five
        "]));

        // Cycle modules

        assert_snapshot!(compile(&[r"
module CycleModule

import CycleModule.Test


module CycleModule.Test
    import CycleModule
        "]));

        assert_snapshot!(compile(&[r"
module CycleModule

import CycleModule
        "]));
    }
}
