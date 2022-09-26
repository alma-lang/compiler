use crate::ast::expression::IdentifierName;
use crate::strings::Strings;
use crate::typ::Types;
use crate::type_env::{PolyTypeEnv, TypeEnv};
use fnv::FnvHashMap;

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
