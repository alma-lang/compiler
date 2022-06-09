use crate::strings::{self, Strings};
use derive_more::{Add, AddAssign, From, Into, Sub, SubAssign};
use typed_index_collections::TiVec;

#[derive(
    Debug, From, Into, Add, AddAssign, Sub, SubAssign, PartialEq, Eq, PartialOrd, Ord, Clone, Copy,
)]
pub struct Index(usize);

pub type Tokens = TiVec<Index, Token>;

#[derive(PartialEq, Debug, Copy, Clone)]
pub struct Token {
    pub kind: Type,
    pub indent: u32,
    pub start: usize,
    pub end: usize,
    pub line: u32,
    pub column: u32,
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
    Identifier(strings::Symbol),
    CapitalizedIdentifier(strings::Symbol),
    String_(strings::Symbol),
    Float(strings::Symbol),

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
impl Type {
    pub fn to_string<'strings>(&self, strings: &'strings Strings) -> &'strings str {
        use self::Type::*;
        match self {
            LeftParen => "(",
            RightParen => ")",
            LeftBrace => "{",
            RightBrace => "}",
            Comma => ",",
            Dot => ".",
            Minus => "-",
            Plus => "+",
            Semicolon => ";",
            Colon => ":",
            Slash => "/",
            Backslash => "\\",
            Star => "*",
            Underscore => "_",
            Pipe => "|",

            Bang => "!",
            BangEqual => "!=",
            Equal => "=",
            EqualEqual => "==",
            Greater => ">",
            GreaterEqual => ">=",
            Less => "<",
            LessEqual => "<=",
            Arrow => "->",

            Identifier(symbol) => strings.resolve(*symbol),
            CapitalizedIdentifier(symbol) => strings.resolve(*symbol),
            String_(symbol) => strings.resolve(*symbol),
            Float(symbol) => strings.resolve(*symbol),

            And => "and",
            Or => "or",
            Not => "not",
            If => "if",
            Then => "then",
            Else => "else",
            True => "True",
            False => "False",
            Let => "let",
            In => "in",
            As => "as",
            Exposing => "exposing",
            Module => "module",
            Import => "import",
            Type => "type",

            Comment => "[Comment]",
            Eof => "[End of file]",
        }
    }
}
