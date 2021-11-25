use crate::ast::Module;
use crate::source::Source;
use crate::strings::{Strings, Symbol as StringSymbol};
use crate::type_env::{PolyTypeEnv, TypeEnv};
use fnv::FnvHashMap;
use std::fmt::Write;
use std::rc::Rc;

pub type HashMap<K, V> = FnvHashMap<K, V>;

pub type Sources = HashMap<String, Source>;

pub type ModuleSources<'source> = HashMap<StringSymbol, &'source Source>;

pub type ModuleAsts = HashMap<StringSymbol, Module>;

#[derive(Debug)]
pub struct ModuleInterface {
    pub types: Rc<TypeEnv>,
    pub type_constructors: HashMap<StringSymbol, Rc<PolyTypeEnv>>,
    pub definitions: Rc<PolyTypeEnv>,
}

impl ModuleInterface {
    pub fn to_string(&self, strings: &Strings) -> String {
        let mut out = String::new();

        for (_typ, constructors) in self.type_constructors.iter() {
            out.push_str(&constructors.to_string(strings));
            out.push_str("\n\n");
        }

        out.push_str(&self.definitions.to_string(strings));

        out
    }
}

#[derive(Debug)]
pub struct ModuleInterfaces(HashMap<StringSymbol, ModuleInterface>);

impl ModuleInterfaces {
    pub fn new() -> Self {
        let env = HashMap::default();

        Self(env)
    }

    pub fn get(&self, key: &StringSymbol) -> Option<&ModuleInterface> {
        self.0.get(key)
    }

    pub fn insert(&mut self, key: StringSymbol, value: ModuleInterface) {
        self.0.insert(key, value);
    }

    pub fn map(&self) -> &HashMap<StringSymbol, ModuleInterface> {
        &self.0
    }

    pub fn to_string(&self, strings: &Strings) -> String {
        let mut entries: Vec<_> = self.0.iter().collect();
        // we need to sort the entries because they come out with different order and they mess up
        // tests
        entries.sort_by_key(|(k, _)| strings.resolve(**k));

        let mut out = String::new();
        for (i, (name, interface)) in entries.iter().enumerate() {
            if i > 0 {
                out.push_str("\n\n\n");
            }
            write!(
                out,
                "module {}\n\n{}",
                strings.resolve(**name),
                interface.to_string(strings)
            )
            .unwrap();
        }

        out
    }
}
