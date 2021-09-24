use crate::ast::Module;
use crate::source::Source;
use crate::type_env::TypeEnv;
use fnv::FnvHashMap as HashMap;
use smol_str::SmolStr;
use std::fmt;
use std::rc::Rc;

pub type Sources = HashMap<String, Source>;

pub type ModuleSources<'source> = HashMap<SmolStr, &'source Source>;

pub type ModuleAsts = HashMap<SmolStr, Module>;

pub struct ModuleInterfaces(HashMap<SmolStr, Rc<TypeEnv>>);

impl ModuleInterfaces {
    pub fn new() -> Self {
        let env = HashMap::default();

        Self(env)
    }

    pub fn get(&self, key: &SmolStr) -> Option<&Rc<TypeEnv>> {
        self.0.get(key)
    }

    pub fn insert(&mut self, key: SmolStr, value: Rc<TypeEnv>) {
        self.0.insert(key, value);
    }

    pub fn map(&self) -> &HashMap<SmolStr, Rc<TypeEnv>> {
        &self.0
    }
}

impl fmt::Display for ModuleInterfaces {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut entries: Vec<_> = self.0.iter().collect();
        // we need to sort the entries because they come out with different order and they mess up
        // tests
        entries.sort_by_key(|(k, _)| *k);

        for (name, type_env) in &entries {
            write!(f, "module {}\n\n{}\n", name, type_env)?;
        }

        Ok(())
    }
}
