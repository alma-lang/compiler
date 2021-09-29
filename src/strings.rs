use fnv::FnvBuildHasher;
use string_interner::{DefaultBackend, DefaultSymbol, StringInterner, Symbol as _};

pub type Symbol = DefaultSymbol;

#[derive(Debug)]
pub struct Strings(StringInterner<Symbol, DefaultBackend<Symbol>, FnvBuildHasher>);

impl Strings {
    pub fn new() -> Self {
        // TODO: Test other hashers and capacity
        Strings(StringInterner::new())
    }

    pub fn _get<T>(&self, string: T) -> Option<Symbol>
    where
        T: AsRef<str>,
    {
        self.0.get(string)
    }

    pub fn get_or_intern<T>(&mut self, string: T) -> Symbol
    where
        T: AsRef<str>,
    {
        self.0.get_or_intern(string)
    }

    pub fn resolve(&self, symbol: Symbol) -> &str {
        self.0
            .resolve(symbol)
            .unwrap_or_else(|| panic!("Couldn't find string with symbol {}", symbol.to_usize()))
    }
}
