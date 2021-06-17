use crate::infer;
use crate::parser;
use crate::source::Source;
use crate::tokenizer;
use crate::type_env::TypeEnv;
use std::rc::Rc;

pub fn compile(input: &Source) -> Result<String, String> {
    let tokens = tokenizer::parse(&input).map_err(|errors| {
        errors
            .iter()
            .map(|e| e.to_string(&input))
            .collect::<Vec<String>>()
            .join("\n\n")
    })?;

    let modules = parser::parse(input, tokens).map_err(|error| error.to_string(&input))?;

    let types = modules.iter().fold(
        Ok(vec![]),
        |result: Result<Vec<Rc<TypeEnv>>, Vec<infer::Error>>, module| match (
            result,
            infer::infer(&module),
        ) {
            (Ok(mut types), Ok(typ)) => {
                types.push(typ);
                Ok(types)
            }
            (Err(mut errors), Err(mut module_errors)) => {
                errors.append(&mut module_errors);
                Err(errors)
            }
            (Err(errors), Ok(_)) => Err(errors),
            (Ok(_), Err(errors)) => Err(errors),
        },
    );

    Ok(match types {
        Ok(types) => types
            .iter()
            .zip(modules.iter())
            .map(|(t, m)| format!("{}\n\n{}\n", m.name.value, t))
            .collect::<Vec<String>>()
            .join("\n\n"),
        Err(errors) => errors
            .iter()
            .map(|e| e.to_string(&input))
            .collect::<Vec<String>>()
            .join("\n\n"),
    })
}
