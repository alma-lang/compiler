use crate::typ::Type;
use im_rc::HashMap;
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
