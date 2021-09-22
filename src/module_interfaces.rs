use crate::type_env::TypeEnv;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

pub struct ModuleInterfaces(HashMap<Rc<String>, Rc<TypeEnv>>);

impl ModuleInterfaces {
    pub fn new() -> Self {
        let env = HashMap::new();

        Self(env)
    }

    pub fn get(&self, key: &Rc<String>) -> Option<&Rc<TypeEnv>> {
        self.0.get(key)
    }

    pub fn insert(&mut self, key: Rc<String>, value: Rc<TypeEnv>) {
        self.0.insert(key, value);
    }

    pub fn map(&self) -> &HashMap<Rc<String>, Rc<TypeEnv>> {
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
            write!(f, "module {}\n{}\n\n", name, type_env)?;
        }

        Ok(())
    }
}
