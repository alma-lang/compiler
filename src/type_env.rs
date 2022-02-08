use crate::strings::{Strings, Symbol as StringSymbol};
use crate::typ::{PolyType, Type};
use fnv::FnvBuildHasher;
use im_rc::HashMap;
use std::fmt::Write;
use std::rc::Rc;

type TypeMap = HashMap<StringSymbol, Rc<Type>, FnvBuildHasher>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeEnv(TypeMap);

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

    pub fn map(&self) -> &TypeMap {
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
            let name = strings.resolve(**name);
            let typ = typ.to_string(strings);
            write!(out, "{name} : {typ}").unwrap();
        }

        out
    }
}

type PolyTypeMap = HashMap<StringSymbol, Rc<PolyType>, FnvBuildHasher>;

// TODO: Join this with TypeEnv and parametrize it, and use a trait for the
// to_string(strings: &Strings)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PolyTypeEnv(PolyTypeMap);

impl PolyTypeEnv {
    pub fn get(&self, key: &StringSymbol) -> Option<&Rc<PolyType>> {
        self.0.get(key)
    }

    pub fn insert(&mut self, key: StringSymbol, value: Rc<PolyType>) {
        self.0.insert(key, value);
    }

    pub fn new() -> Self {
        let env = HashMap::default();

        Self(env)
    }

    pub fn map(&self) -> &PolyTypeMap {
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
            let name = strings.resolve(**name);
            let typ = typ.to_string(strings);
            write!(out, "{name} : {typ}").unwrap();
        }

        out
    }
}
