use crate::infer;
use crate::parser;
use crate::source::Source;
use crate::tokenizer;
use crate::type_env::TypeEnv;
use std::collections::HashMap;
use std::rc::Rc;

pub fn compile(source: &Source) -> Result<String, String> {
    let tokens = tokenizer::parse(&source).map_err(|errors| {
        errors
            .iter()
            .map(|e| e.to_string(&source))
            .collect::<Vec<String>>()
            .join("\n\n")
    })?;

    let modules = parser::parse(source, &tokens).map_err(|error| error.to_string(&source))?;

    let mut module_interfaces: HashMap<String, Rc<TypeEnv>> = HashMap::new();
    let mut errors = vec![];

    for module in &modules {
        match infer::infer(&module_interfaces, &module) {
            Ok(typ) => {
                module_interfaces.insert(module.name.value.name.clone(), typ);
            }
            Err((typ, mut module_errors)) => {
                module_interfaces.insert(module.name.value.name.clone(), typ);
                errors.append(&mut module_errors);
            }
        }
    }

    if errors.is_empty() {
        Ok(modules
            .iter()
            .map(|m| {
                format!(
                    "{}\n\n{}\n",
                    m.name.value,
                    module_interfaces
                        .get(&m.name.value.name)
                        .unwrap_or(&Rc::new(TypeEnv::new()))
                )
            })
            .collect::<Vec<String>>()
            .join("\n\n"))
    } else {
        Err(errors
            .iter()
            .map(|e| e.to_string(&source))
            .collect::<Vec<String>>()
            .join("\n\n"))
    }
}
