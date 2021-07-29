use crate::ast::Module;
use crate::type_env::TypeEnv;
use std::collections::HashMap;
use std::fmt::Write;
use std::rc::Rc;

pub struct File {
    pub name: String,
    pub contents: String,
}

pub fn generate(
    modules: &Vec<Module>,
    module_interfaces: &HashMap<String, Rc<TypeEnv>>,
) -> Vec<File> {
    let mut files = vec![];
    for module in modules {
        let file = generate_file(
            module,
            module_interfaces.get(&module.name.value.name).unwrap(),
        );
        files.push(file);
    }
    files
}

fn generate_file(module: &Module, interface: &TypeEnv) -> File {
    let mut code = String::new();

    generate_imports(&mut code, module);

    File {
        name: module.name.value.name.clone(),
        contents: code,
    }
}

fn generate_imports(code: &mut String, module: &Module) {
    for import in &module.imports {
        let import = &import.value;
        let alias = &import
            .alias
            .as_ref()
            .unwrap_or(&import.module_name)
            .value
            .name;

        write!(
            code,
            r#"import * as {} from "{}"\n"#,
            alias,
            import.module_name.value.name, // TODO: Wrong path
        )
        .unwrap();

        if !import.exposing.is_empty() {
            let mut identifiers = vec![];
            for exposing in &import.exposing {
                identifiers.append(&mut exposing.value.identifiers());
            }
            // TODO: put identifiers in the import statement
            write!(
                code,
                r#"import {{{}}} from "{}"\n"#,
                import.module_name.value.name, import.module_name.value.name,
            )
            .unwrap();
        }
    }
}
