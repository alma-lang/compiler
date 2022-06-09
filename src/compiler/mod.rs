pub mod errors;
pub mod stages;
pub mod state;
pub mod types;

use self::state::ModuleIndex;
pub use self::state::State;
use crate::ast::{self /*, ReplEntry, TypedDefinition*/};
use crate::compiler::stages::{check_cycles, infer, parse_files};
use crate::compiler::types::ModuleInterfaces;
use crate::javascript;
// use crate::parser;
use crate::source::Source;
use crate::strings::Strings;
// use crate::tokenizer;
use crate::typ::Type;
use crate::type_env::PolyTypeEnv;
use crate::{infer, source};

pub fn compile(entry_sources: &[source::Index], state: &mut State) -> Result<String, String> {
    let entry_modules = parse_files(entry_sources, state)?;

    let mut sorted_modules = check_cycles(&entry_modules, &state)?;
    sorted_modules.reverse();

    let primitive_types = Type::primitive_types(&mut state.strings);

    infer(entry_modules, state, &primitive_types)?;

    Ok(javascript::generate(&sorted_modules, &state))
}

pub fn compile_repl_entry(
    _module_idx: &ModuleIndex,
    _source: Source,
    _state: &mut State,
    _primitive_types: &PolyTypeEnv,
) -> Result<String, String> {
    unimplemented!();
    /*
    tokenizer::parse(source, strings, module).map_err(|errors| {
        errors
            .iter()
            .map(|e| e.to_string(source))
            .collect::<Vec<String>>()
            .join("\n\n")
    })?;

    let entry = parser::parse_repl(source, &module.tokens, strings)
        .map_err(|error| error.to_string(source, strings))?;

    let mut errors = vec![];

    // TODO: Annotate the AST with types. After inference, fetch what we stored in the module and
    // print the type of it
    match entry {
        ReplEntry::Import(import) => {
            module.ast_mut().imports.push(import);
            let result = compile_repl_entry_helper(
                &module,
                module_interfaces,
                source,
                &mut errors,
                strings,
                primitive_types,
            );
            if result.is_err() {
                module.ast_mut().imports.pop();
            }
            result
        }
        ReplEntry::Definition(definition) => {
            module
                .ast_mut()
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
                module.ast_mut().definitions.pop();
            }
            result
        }
        ReplEntry::Expression(expression) => {
            module
                .ast_mut()
                .definitions
                .push(TypedDefinition::Untyped(ast::Definition::Pattern(
                    ast::Node {
                        value: ast::Pattern_::Hole,
                        start: 0.into(),
                        end: 0.into(),
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
            module.ast_mut().definitions.pop();
            result
        }
    }
    .map(|()| "ok.".to_string())
    */
}

// Encapsulate the compiler logic regardless of repl entry type
fn compile_repl_entry_helper<'ast>(
    _module: &'ast ast::Module,
    _module_interfaces: &mut ModuleInterfaces,
    _source: &Source,
    _errors: &mut Vec<infer::Error>,
    _strings: &mut Strings,
    _primitive_types: &PolyTypeEnv,
) -> Result<(), String> {
    unimplemented!();
    /*
    let result = infer::infer(module_interfaces, module, primitive_types, strings);
    match result {
        Ok(typ) => {
            module_interfaces.insert(module.ast().name.full_name, typ);
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
            .map(|e| e.to_string(source, strings, module))
            .collect::<Vec<String>>()
            .join("\n\n"))
    }
    */
}

#[cfg(test)]
pub mod tests {
    use crate::compiler;
    use crate::compiler::stages::process_sources;
    use crate::source::Source;
    use insta::assert_snapshot;

    #[test]
    pub fn test_compile() {
        fn compile(source_codes: &[&'static str]) -> String {
            let mut state = compiler::State::new();
            let entry_sources = process_sources(
                source_codes
                    .iter()
                    .map(|s| Source::new_orphan(s.to_string()))
                    .collect(),
                &mut state,
            );
            match super::compile(&entry_sources, &mut state) {
                Ok(s) => s,
                Err(e) => e,
            }
        }

        // Normal modules

        assert_snapshot!(
            "Normal module dependency chain",
            compile(&[r"
module Modules exposing (main)

import Modules.WeirdMath exposing (weirdAdd)

main = weirdAdd



module Modules.Constants exposing (five)

    five = 5



module Modules.WeirdMath exposing (weirdAdd)

    import Modules.Constants exposing (five)

    weirdAdd = \x y -> x * y + five
        "])
        );

        // Cycle modules

        assert_snapshot!(
            "Direct dependency module cycle",
            compile(&[r"
module CycleModule

import CycleModule.Test


module CycleModule.Test
    import CycleModule
        "])
        );

        assert_snapshot!(
            "Direct module cycle",
            compile(&[r"
module CycleModule

import CycleModule
        "])
        );

        assert_snapshot!(
            "Type only top level definition",
            compile(&[r"
module ModuleWithTypeOnlyDefinitions exposing (main)

test : Float -> Float -> Float

main a = test a 5
        "])
        );
    }
}
