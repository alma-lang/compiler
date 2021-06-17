use crate::typ::Type;
use im_rc::HashMap;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone)]
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
}

impl fmt::Display for TypeEnv {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = String::new();

        let mut entries: Vec<_> = self.0.iter().collect();
        // what do you want from me rust, here, have a clone
        entries.sort_by_key(|(k, _)| k.clone());

        for (name, typ) in &entries {
            s.push_str(&format!("{} : {}\n\n", name, typ));
        }

        write!(f, "{}", s)
    }
}
