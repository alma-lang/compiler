use crate::ast::expression::IdentifierName;
use crate::ast::ModuleFullName;
use crate::strings::Strings;
use crate::typ::Types;
use crate::type_env::{PolyTypeEnv, TypeEnv};
use fnv::FnvHashMap;
use std::fmt::Write;

pub type HashMap<K, V> = FnvHashMap<K, V>;

#[derive(Debug)]
pub struct ModuleInterface {
    pub types: TypeEnv,
    pub type_constructors: HashMap<IdentifierName, PolyTypeEnv>,
    pub definitions: PolyTypeEnv,
}

impl ModuleInterface {
    pub fn write_to_string(&self, out: &mut String, strings: &Strings, types: &Types) {
        for (_typ, constructors) in self.type_constructors.iter() {
            constructors.write_to_string(out, strings, types);
            out.push_str("\n\n");
        }

        self.definitions.write_to_string(out, strings, types);
    }
}

#[derive(Debug)]
pub struct ModuleInterfaces(HashMap<ModuleFullName, ModuleInterface>);

impl ModuleInterfaces {
    pub fn new() -> Self {
        let env = HashMap::default();

        Self(env)
    }

    pub fn get(&self, key: &ModuleFullName) -> Option<&ModuleInterface> {
        self.0.get(key)
    }

    pub fn insert(&mut self, key: ModuleFullName, value: ModuleInterface) {
        self.0.insert(key, value);
    }

    pub fn map(&self) -> &HashMap<ModuleFullName, ModuleInterface> {
        &self.0
    }

    pub fn write_to_string(&self, out: &mut String, strings: &Strings, types: &Types) {
        let mut entries: Vec<_> = self.0.iter().collect();
        // we need to sort the entries because they come out with different order and they mess up
        // tests
        entries.sort_by_key(|(k, _)| strings.resolve(**k));

        for (i, (name, interface)) in entries.iter().enumerate() {
            if i > 0 {
                out.push_str("\n\n\n");
            }
            let name = strings.resolve(**name);
            write!(out, "module {name}\n\n").unwrap();
            interface.write_to_string(out, strings, types);
        }
    }
}
