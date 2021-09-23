use crate::ast::Module;
use fnv::FnvHashMap as HashMap;
use smol_str::SmolStr;
use std::fmt;
use std::rc::Rc;

#[derive(Debug)]
pub struct ModuleAsts(HashMap<SmolStr, Module>);

impl ModuleAsts {
    pub fn new() -> Self {
        let env = HashMap::default();

        Self(env)
    }

    pub fn get(&self, key: &SmolStr) -> Option<&Module> {
        self.0.get(key)
    }

    pub fn insert(&mut self, key: SmolStr, value: Module) {
        self.0.insert(key, value);
    }

    pub fn map(&self) -> &HashMap<SmolStr, Module> {
        &self.0
    }
}
