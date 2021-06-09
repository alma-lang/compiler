use crate::token::Token;

#[derive(PartialEq, Debug)]
pub struct Node<V> {
    pub start: usize,
    pub end: usize,
    pub line: u32,
    pub column: u32,
    pub value: V,
}

impl<V> Node<V> {
    pub fn new(value: V, first_token: &Token, last_token: &Token) -> Self {
        Node {
            value,
            start: first_token.position,
            end: last_token.position + last_token.lexeme.len(),
            line: first_token.line,
            column: first_token.column,
        }
    }
}

type Unary = Node<Unary_>;

#[derive(PartialEq, Debug)]
pub enum Unary_ {
    Not,
    Minus,
}

mod binop {
    use super::Node;
    use lazy_static::lazy_static;

    #[derive(PartialEq, Debug)]
    enum Type {
        Or,
        And,
        Equal,
        NotEqual,
        GreaterThan,
        GreaterEqualThan,
        LessThan,
        LessEqualThan,
        Addition,
        Substraction,
        Multiplication,
        Division,
    }

    #[derive(PartialEq, Debug)]
    enum Associativity {
        LTR,
        RTL,
    }

    pub type Binop = Node<Binop_>;

    #[derive(PartialEq, Debug)]
    pub struct Binop_ {
        typ: Type,
        precedence: u32,
        associativity: Associativity,
        fn_: String,
    }

    use Associativity::*;
    use Type::*;

    lazy_static! {
        pub static ref OR: Binop_ = Binop_ {
            typ: Or,
            precedence: 6,
            associativity: LTR,
            fn_: "(or)".to_owned(),
        };
        pub static ref AND: Binop_ = Binop_ {
            typ: And,
            precedence: 7,
            associativity: LTR,
            fn_: "(and)".to_owned(),
        };
        pub static ref EQUAL: Binop_ = Binop_ {
            typ: Equal,
            precedence: 11,
            associativity: LTR,
            fn_: "(==)".to_owned(),
        };
        pub static ref NOT_EQUAL: Binop_ = Binop_ {
            typ: NotEqual,
            precedence: 11,
            associativity: LTR,
            fn_: "(!=)".to_owned(),
        };
        pub static ref GREATER_THAN: Binop_ = Binop_ {
            typ: GreaterThan,
            precedence: 12,
            associativity: LTR,
            fn_: "(>)".to_owned(),
        };
        pub static ref GREATER_EQUAL_THAN: Binop_ = Binop_ {
            typ: GreaterEqualThan,
            precedence: 12,
            associativity: LTR,
            fn_: "(>=)".to_owned(),
        };
        pub static ref LESS_THAN: Binop_ = Binop_ {
            typ: LessThan,
            precedence: 12,
            associativity: LTR,
            fn_: "(<)".to_owned(),
        };
        pub static ref LESS_EQUAL_THAN: Binop_ = Binop_ {
            typ: LessEqualThan,
            precedence: 12,
            associativity: LTR,
            fn_: "(<=)".to_owned(),
        };
        pub static ref ADDITION: Binop_ = Binop_ {
            typ: Addition,
            precedence: 14,
            associativity: LTR,
            fn_: "(+)".to_owned(),
        };
        pub static ref SUBSTRACTION: Binop_ = Binop_ {
            typ: Substraction,
            precedence: 14,
            associativity: LTR,
            fn_: "(-)".to_owned(),
        };
        pub static ref MULTIPLICATION: Binop_ = Binop_ {
            typ: Multiplication,
            precedence: 15,
            associativity: LTR,
            fn_: "(*)".to_owned(),
        };
        pub static ref DIVISION: Binop_ = Binop_ {
            typ: Division,
            precedence: 15,
            associativity: LTR,
            fn_: "(/)".to_owned(),
        };
    }
}
use binop::Binop;

type Pattern = Node<Pattern_>;

#[derive(PartialEq, Debug)]
enum Pattern_ {
    Identifier(String),
}

pub type Expression = Node<Expression_>;

#[derive(PartialEq, Debug)]
pub enum Expression_ {
    Unit,
    Bool(bool),
    Float(f64),
    String_(String),
    Identifier(String),
    Unary(Unary, Box<Expression>),
    Binary(Box<Expression>, Binop, Box<Expression>),
    Lambda(Vec<Pattern>, Box<Expression>),
    FnCall(Box<Expression>, Vec<Expression>),
    Let(Pattern, Box<Expression>, Box<Expression>),
    If(Box<Expression>, Box<Expression>, Box<Expression>),
}
