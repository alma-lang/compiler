pub mod stages;
pub mod state;

use std::path::Path;

pub use self::state::State;
use crate::compiler::stages::{check_cycles, infer, parse_files};
use crate::javascript::{self, OutputFile};
use crate::source;

pub fn compile(
    entry_sources: &[source::Index],
    state: &mut State,
    output: &Path,
) -> Result<Vec<(String, OutputFile)>, String> {
    let entry_modules = parse_files(entry_sources, state)?;

    let mut sorted_modules = check_cycles(&entry_modules, state)?;
    sorted_modules.reverse();

    infer(entry_modules, state)?;

    Ok(javascript::generate(&sorted_modules, state, output))
}

// pub fn compile_repl_entry(
//     _module_idx: &ModuleIndex,
//     _source: Source,
//     _state: &mut State,
//     _primitive_types: &PolyTypeEnv,
// ) -> Result<String, String> {
//     unimplemented!();
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
// }

// Encapsulate the compiler logic regardless of repl entry type
// fn compile_repl_entry_helper<'ast>(
//     _module: &'ast ast::Module,
//     _module_interfaces: &mut ModuleInterfaces,
//     _source: &Source,
//     _errors: &mut [infer::Error],
//     _strings: &mut Strings,
//     _primitive_types: &PolyTypeEnv,
// ) -> Result<(), String> {
//     unimplemented!();
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
// }

#[cfg(test)]
pub mod tests {
    use std::path::Path;

    use crate::compiler;
    use crate::compiler::stages::process_sources;
    use crate::javascript::OutputFile;
    use crate::source::Source;
    use insta::assert_snapshot;
    use std::fmt::Write;

    #[test]
    pub fn test_compile() {
        fn compile(source_codes: &[&'static str]) -> String {
            let mut state = compiler::State::new();
            let alma_source = Source::new_file("Alma.alma").unwrap();
            let mut sources = vec![alma_source];
            sources.extend(
                source_codes
                    .iter()
                    .map(|s| Source::new("Test.alma", s.to_string())),
            );
            let entry_sources = process_sources(sources, &mut state);
            match super::compile(&entry_sources, &mut state, Path::new("alma_out")) {
                Ok(s) => {
                    let mut out = String::new();
                    for (f, c) in s {
                        out.push_str("// ");
                        out.push_str(&f);
                        out.push_str("\n\n");
                        match c {
                            OutputFile::File(c) => out.push_str(&c),
                            OutputFile::CopyFrom(c) => write!(out, "// Copy from: {c}").unwrap(),
                        };
                        out.push_str("\n\n\n");
                    }
                    out
                }
                Err(e) => e,
            }
        }

        // Normal modules

        assert_snapshot!(
            "Normal module dependency chain",
            compile(&[r"
module Test exposing (main)

import Test.WeirdMath exposing (weirdAdd)

main = weirdAdd



module Test.Constants exposing (five)

    five = 5



module Test.WeirdMath exposing (weirdAdd)

    import Test.Constants exposing (five)

    weirdAdd = \x y -> x * y + five
        "])
        );

        // Cycle modules

        assert_snapshot!(
            "Direct dependency module cycle",
            compile(&[r"
module Test

import Test.TestInner


module Test.TestInner
    import Test
        "])
        );

        assert_snapshot!(
            "Direct module cycle",
            compile(&[r"
module Test

import Test
        "])
        );

        assert_snapshot!(
            "Type only top level definition",
            compile(&[r"
module Test exposing (main)

test : Float -> Float -> Float

main a = test a 5
        "])
        );

        assert_snapshot!(
            "External definitions",
            compile(&[r"
module Test exposing (main)

external test : Float -> Float -> Float

main a = test a 5

module Test.Inner exposing (another_test)
    external another_test : Float -> Float
        "])
        );

        assert_snapshot!(
            "Binary operators",
            compile(&[r"
module Test exposing (main)

test = 1 - 2 + 3 * 4 * 5

main = 8 + 2 == 10 or False and True

        "])
        );

        assert_snapshot!(
            "Type patterns and pattern matching",
            compile(&[r#"
module Test exposing (main, Option(Some, None))

type Option a = Some a | None

type Id = Id Float

type List a = Cons a (List a) | Nil

type Pair a b = Pair a b

main =
    let
        Id id = Id 1
        test = when Id 5 is
            Id id -> id
        test = when "test" is
            "banana" -> id
            "phone" -> id
        test = when 5 is
            5 -> id
            _ -> id
        test = when Some(Id(1)) is
            Some (Id id) -> id
        test = when 5 is
            5 as a -> a
            1 as b | 2 as b | b -> b
            _ -> id
        test = when Nil is
            Cons a Nil |
            Cons _ (Cons a Nil) |
            Cons _ (Cons _ (Cons a Nil)) -> a
        test = when Pair None (Some 3) is
            Pair (Some (Some (5 as a | 7 as a) | Some (1 as a))) (Some (3 as b | b)) ->
                True
    Some id

        "#])
        );
    }
}
