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

    pub fn to_string<'strings>(&self, strings: &Strings) -> String {
        let mut entries: Vec<_> = self.0.iter().collect();
        // we need to sort the entries because they come out with different order and they mess up
        // tests
        entries.sort_by_key(|(k, _)| *k);

        let mut out = String::new();
        for (name, typ) in entries {
            write!(
                out,
                "{} : {}\n\n",
                strings.resolve(*name),
                typ.to_string(strings)
            )
            .unwrap();
        }

        out
    }
}
