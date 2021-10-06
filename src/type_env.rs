use crate::strings::{Strings, Symbol as StringSymbol};
use crate::typ::Type;
use fnv::FnvBuildHasher;
use im_rc::HashMap;
use std::fmt::Write;
use std::rc::Rc;

type Map = HashMap<StringSymbol, Rc<Type>, FnvBuildHasher>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeEnv(Map);

impl TypeEnv {
    pub fn get(&self, key: &StringSymbol) -> Option<&Rc<Type>> {
        self.0.get(key)
    }

    pub fn insert(&mut self, key: StringSymbol, value: Rc<Type>) {
        self.0.insert(key, value);
    }

    pub fn new() -> Self {
        let env = HashMap::default();

        Self(env)
    }

    pub fn map(&self) -> &Map {
        &self.0
    }

    pub fn to_string(&self, strings: &Strings) -> String {
        let mut entries: Vec<_> = self.0.iter().collect();
        // we need to sort the entries because they come out with different order and they mess up
        // tests
        entries.sort_by_key(|(k, _)| strings.resolve(**k));

        let mut out = String::new();
        for (i, (name, typ)) in entries.iter().enumerate() {
            if i > 0 {
                out.push_str("\n\n");
            }
            write!(
                out,
                "{} : {}",
                strings.resolve(**name),
                typ.to_string(strings)
            )
            .unwrap();
        }

        out
    }
}
