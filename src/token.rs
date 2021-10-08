#[derive(PartialEq, Debug)]
pub struct Token<'lexeme> {
    pub kind: Type,
    pub position: usize,
    pub end_position: usize,
    pub lexeme: &'lexeme str,
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
    Underscore,
    Pipe,

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
    CapitalizedIdentifier,
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
    Type,

    Comment,
    Eof,
}
