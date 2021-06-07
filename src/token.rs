#[derive(PartialEq, Debug)]
pub struct Token {
    pub kind: Type,
    pub lexeme: String,
    pub position: usize,
    pub line: u32,
    pub column: u32,
    pub indent: u32,
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
    String,
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
    Import,
    As,
    Exports,
    Module,

    Comment,
    Eof,
}
