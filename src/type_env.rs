use crate::typ::Type;
use im_rc::HashMap;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeEnv(HashMap<String, Rc<Type>>);

impl TypeEnv {
    pub fn get(&self, key: &str) -> Option<&Rc<Type>> {
        self.0.get(key)
    }

    pub fn insert(&mut self, key: String, value: Rc<Type>) {
        self.0.insert(key, value);
    }

    pub fn new() -> Self {
        let env = HashMap::new();

        Self(env)
    }

    pub fn map(&self) -> &HashMap<String, Rc<Type>> {
        &self.0
    }
}

impl fmt::Display for TypeEnv {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut entries: Vec<_> = self.0.iter().collect();
        // we need to sort the entries because they come out with different order and they mess up
        // tests
        entries.sort_by_key(|(k, _)| *k);

        for (name, typ) in &entries {
            write!(f, "{} : {}\n\n", name, typ)?;
        }

        Ok(())
    }
}
