use crate::source::Source;

#[derive(PartialEq, Debug, Clone)]
pub struct Token {
    pub kind: Type,
    pub position: usize,
    pub end_position: usize,
    pub line: u32,
    pub column: u32,
    pub indent: u32,
}

impl Token {
    pub fn lexeme<'a>(&self, source: &'a Source) -> &'a str {
        match self.kind {
            Type::Eof => "[End of file]",
            _ => source
                .text_at(self.position..self.end_position)
                .expect("Couldn't extract lexeme from token"),
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Type {
    // SingleToken-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Colon,
    Slash,
    Backslash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Arrow,

    // Literals.
    Identifier,
    String_,
    Float,

    // Keywords.
    And,
    Or,
    Not,
    If,
    Then,
    Else,
    True,
    False,
    Let,
    In,
    As,
    Exposing,
    Module,
    Import,

    Comment,
    Eof,
}
