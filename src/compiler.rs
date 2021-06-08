use crate::parser;
use crate::source::Source;
use crate::tokenizer;

pub fn compile(input: &Source) -> Result<String, String> {
    let tokens = tokenizer::parse(&input).map_err(|errors| {
        errors
            .iter()
            .map(|e| e.to_string(&input))
            .collect::<Vec<String>>()
            .join("\n\n")
    })?;

    let ast = parser::parse(input, tokens).map_err(|errors| {
        errors
            .iter()
            .map(|e| e.to_string(&input))
            .collect::<Vec<String>>()
            .join("\n\n")
    })?;

    Ok(format!("{:?}", ast))
}
