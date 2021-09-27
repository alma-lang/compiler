use crate::ast::Module;
use crate::source::Source;
use crate::strings::{Strings, Symbol as StringSymbol};
use crate::type_env::TypeEnv;
use fnv::FnvHashMap as HashMap;
use std::fmt::Write;
use std::rc::Rc;

pub type Sources = HashMap<String, Source>;

pub type ModuleSources<'source> = HashMap<StringSymbol, &'source Source>;

pub type ModuleAsts = HashMap<StringSymbol, Module>;

pub struct ModuleInterfaces(HashMap<StringSymbol, Rc<TypeEnv>>);

impl ModuleInterfaces {
    pub fn new() -> Self {
        let env = HashMap::default();

        Self(env)
    }

    pub fn get(&self, key: &StringSymbol) -> Option<&Rc<TypeEnv>> {
        self.0.get(key)
    }

    pub fn insert(&mut self, key: StringSymbol, value: Rc<TypeEnv>) {
        self.0.insert(key, value);
    }

    pub fn map(&self) -> &HashMap<StringSymbol, Rc<TypeEnv>> {
        &self.0
    }

    pub fn to_string<'strings>(&self, strings: &Strings) -> String {
        let mut entries: Vec<_> = self.0.iter().collect();
        // we need to sort the entries because they come out with different order and they mess up
        // tests
        entries.sort_by_key(|(k, _)| *k);

        let mut out = String::new();
        for (name, type_env) in entries {
            write!(
                out,
                "module {}\n\n{}\n",
                strings.resolve(*name),
                type_env.to_string(strings)
            )
            .unwrap();
        }

        out
    }
}
