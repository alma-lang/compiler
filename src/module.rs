use crate::ast;
use crate::token::Token;

pub mod tokens {
    pub type Index = usize;
}

pub struct Module {
    pub tokens: Vec<Token>,
    pub ast: Option<ast::Module>,
}

impl Module {
    pub fn new() -> Self {
        Self {
            tokens: Vec::new(),
            ast: None,
        }
    }
    pub fn ast(&self) -> &ast::Module {
        self.ast.as_ref().unwrap()
    }
    pub fn ast_mut(&mut self) -> &mut ast::Module {
        self.ast.as_mut().unwrap()
    }
}
