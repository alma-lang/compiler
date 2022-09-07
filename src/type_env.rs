use crate::strings::{Strings, Symbol as StringSymbol};
use crate::typ::{self, PolyType, Types};
use fnv::FnvBuildHasher;
use im_rc::HashMap;
use std::fmt::Write;

type TypeMap = HashMap<StringSymbol, typ::Index, FnvBuildHasher>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeEnv(TypeMap);

impl TypeEnv {
    pub fn get(&self, key: &StringSymbol) -> Option<typ::Index> {
        self.0.get(key).copied()
    }

    pub fn insert(&mut self, key: StringSymbol, value: typ::Index) {
        self.0.insert(key, value);
    }

    pub fn new() -> Self {
        let env = HashMap::default();

        Self(env)
    }

    pub fn map(&self) -> &TypeMap {
        &self.0
    }

    pub fn write_to_string(&self, out: &mut String, strings: &Strings, types: &Types) {
        let mut entries: Vec<_> = self.0.iter().collect();
        // we need to sort the entries because they come out with different order and they mess up
        // tests
        entries.sort_by_key(|(k, _)| strings.resolve(**k));

        for (i, (name, typ)) in entries.iter().enumerate() {
            if i > 0 {
                out.push_str("\n\n");
            }
            let name = strings.resolve(**name);
            write!(out, "{name} : ").unwrap();
            types[**typ].write_to_string(out, strings, types);
        }
    }
}

type PolyTypeMap = HashMap<StringSymbol, PolyType, FnvBuildHasher>;

// TODO: Join this with TypeEnv and parametrize it, and use a trait for the
// to_string(strings: &Strings)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PolyTypeEnv(PolyTypeMap);

impl PolyTypeEnv {
    pub fn get(&self, key: &StringSymbol) -> Option<&PolyType> {
        self.0.get(key)
    }

    pub fn insert(&mut self, key: StringSymbol, value: PolyType) {
        self.0.insert(key, value);
    }

    pub fn new() -> Self {
        let env = HashMap::default();

        Self(env)
    }

    pub fn map(&self) -> &PolyTypeMap {
        &self.0
    }

    pub fn write_to_string(&self, out: &mut String, strings: &Strings, types: &Types) {
        let mut entries: Vec<_> = self.0.iter().collect();
        // we need to sort the entries because they come out with different order and they mess up
        // tests
        entries.sort_by_key(|(k, _)| strings.resolve(**k));

        for (i, (name, typ)) in entries.iter().enumerate() {
            if i > 0 {
                out.push_str("\n\n");
            }
            let name = strings.resolve(**name);
            write!(out, "{name} : ").unwrap();
            typ.write_to_string(out, strings, types);
        }
    }
}
