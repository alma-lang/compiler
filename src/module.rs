use crate::token::Token;

pub mod tokens {
    pub type Index = usize;
}

pub struct Module {
    pub tokens: Vec<Token>,
}

impl Module {
    pub fn new() -> Self {
        Self { tokens: Vec::new() }
    }
}
