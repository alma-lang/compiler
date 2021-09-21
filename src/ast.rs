use crate::token::Token;
use crate::typ::Type;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

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
            end: last_token.end_position,
            line: first_token.line,
            column: first_token.column,
        }
    }

    pub fn copy_with_value<T>(value: V, node: &Node<T>) -> Self {
        Node {
            value,
            start: node.start,
            end: node.end,
            line: node.line,
            column: node.column,
        }
    }
}

#[derive(Debug)]
pub enum ReplEntry {
    Import(Import),
    Definition(Definition),
    Expression(Expression),
}

#[derive(Debug, PartialEq)]
pub struct ModuleName(pub Vec<Identifier>);

impl ModuleName {
    pub fn end(&self) -> usize {
        self.0
            .last()
            .expect("Module names should never be empty")
            .end
    }
}

impl fmt::Display for ModuleName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            self.0
                .iter()
                .map(|i| i.value.name.clone())
                .collect::<Vec<String>>()
                .join(".")
        )
    }
}

#[derive(Debug, PartialEq)]
pub struct Module {
    pub name: ModuleName,
    pub exports: Vec<Export>,
    pub imports: Vec<Import>,
    pub definitions: Vec<Definition>,
}

pub type Import = Node<Import_>;
#[derive(Debug, PartialEq)]
pub struct Import_ {
    pub module_name: ModuleName,
    pub alias: Option<Identifier>,
    pub exposing: Vec<Export>,
}

pub type Export = Node<Export_>;
#[derive(Debug, PartialEq)]
pub enum Export_ {
    // Enum because there may be sub-exports like `exposing (Maybe(Just))` in the future
    Identifier(Identifier),
}

impl Export_ {
    pub fn identifiers(&self) -> Vec<&Identifier> {
        let mut identifiers = vec![];
        match self {
            Self::Identifier(identifier) => identifiers.push(identifier),
        }
        identifiers
    }
}

#[derive(PartialEq, Debug)]
pub enum Definition {
    Lambda(Identifier, Expression /* ExpressionType::Lambda */),
    Pattern(Pattern, Expression),
}

pub type Expression = Node<Expression_>;

#[derive(PartialEq, Debug)]
pub struct Expression_ {
    pub typ: RefCell<Option<Rc<Type>>>,
    pub expr: ExpressionType,
}

impl Expression_ {
    pub fn untyped(expr: ExpressionType) -> Self {
        Self {
            typ: RefCell::new(None),
            expr,
        }
    }

    pub fn set_type(&self, typ: Rc<Type>) {
        *self.typ.borrow_mut() = Some(typ);
    }
}

#[derive(PartialEq, Debug)]
pub enum ExpressionType {
    Unit,
    Bool(bool),
    Float(f64),
    String_(String),
    Identifier(Identifier),
    PropAccess(Box<Expression>, Identifier),
    PropAccessLambda(Identifier),
    Record(Vec<(Identifier, Expression)>),
    RecordUpdate(Box<Expression>, Vec<(Identifier, Expression)>),
    Unary(Unary, Box<Expression>),
    Binary(Box<Expression>, Binop, Box<[Expression; 2]>),
    Lambda(Vec<Pattern>, Box<Expression>),
    FnCall(Box<Expression>, Vec<Expression>),
    Let(Vec<Definition>, Box<Expression>),
    If(Box<Expression>, Box<Expression>, Box<Expression>),
}

type Unary = Node<Unary_>;

#[derive(PartialEq, Debug, Clone)]
pub enum Unary_ {
    Not,
    Minus,
}

pub mod binop {
    use super::{Identifier_, Node};
    use lazy_static::lazy_static;

    #[derive(PartialEq, Debug, Clone)]
    pub enum Type {
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

    #[derive(PartialEq, Debug, Clone)]
    pub enum Associativity {
        Ltr,
        // RTL,
    }

    pub type Binop = Node<Binop_>;

    #[derive(PartialEq, Debug, Clone)]
    pub struct Binop_ {
        typ: Type,
        pub precedence: u32,
        pub associativity: Associativity,
        pub fn_: Identifier_,
    }

    use Associativity::*;
    use Type::*;

    lazy_static! {
        pub static ref OR: Binop_ = Binop_ {
            typ: Or,
            precedence: 6,
            associativity: Ltr,
            fn_: Identifier_::new("__op__or"),
        };
        pub static ref AND: Binop_ = Binop_ {
            typ: And,
            precedence: 7,
            associativity: Ltr,
            fn_: Identifier_::new("__op__and"),
        };
        pub static ref EQUAL: Binop_ = Binop_ {
            typ: Equal,
            precedence: 11,
            associativity: Ltr,
            fn_: Identifier_::new("__op__eq"),
        };
        pub static ref NOT_EQUAL: Binop_ = Binop_ {
            typ: NotEqual,
            precedence: 11,
            associativity: Ltr,
            fn_: Identifier_::new("__op__ne"),
        };
        pub static ref GREATER_THAN: Binop_ = Binop_ {
            typ: GreaterThan,
            precedence: 12,
            associativity: Ltr,
            fn_: Identifier_::new("__op__gt"),
        };
        pub static ref GREATER_EQUAL_THAN: Binop_ = Binop_ {
            typ: GreaterEqualThan,
            precedence: 12,
            associativity: Ltr,
            fn_: Identifier_::new("__op__ge"),
        };
        pub static ref LESS_THAN: Binop_ = Binop_ {
            typ: LessThan,
            precedence: 12,
            associativity: Ltr,
            fn_: Identifier_::new("__op__lt"),
        };
        pub static ref LESS_EQUAL_THAN: Binop_ = Binop_ {
            typ: LessEqualThan,
            precedence: 12,
            associativity: Ltr,
            fn_: Identifier_::new("__op__le"),
        };
        pub static ref ADDITION: Binop_ = Binop_ {
            typ: Addition,
            precedence: 14,
            associativity: Ltr,
            fn_: Identifier_::new("__op__add"),
        };
        pub static ref SUBSTRACTION: Binop_ = Binop_ {
            typ: Substraction,
            precedence: 14,
            associativity: Ltr,
            fn_: Identifier_::new("__op__sub"),
        };
        pub static ref MULTIPLICATION: Binop_ = Binop_ {
            typ: Multiplication,
            precedence: 15,
            associativity: Ltr,
            fn_: Identifier_::new("__op__mult"),
        };
        pub static ref DIVISION: Binop_ = Binop_ {
            typ: Division,
            precedence: 15,
            associativity: Ltr,
            fn_: Identifier_::new("__op__div"),
        };
    }
}
use binop::Binop;

pub type Pattern = Node<Pattern_>;

#[derive(PartialEq, Debug)]
pub enum Pattern_ {
    Hole,
    Identifier(Identifier),
}

pub type Identifier = Node<Identifier_>;
#[derive(PartialEq, Debug, Clone)]
pub struct Identifier_ {
    pub name: String,
    pub case: IdentifierCase,
}

impl Identifier_ {
    pub fn new(name: &str) -> Self {
        use IdentifierCase::*;

        let case = name
            .chars()
            .next()
            .map(|c| if c.is_uppercase() { Pascal } else { Camel })
            .expect("Can't construct an empty identifier");

        Self {
            name: name.to_string(),
            case,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum IdentifierCase {
    Pascal,
    Camel,
}

impl fmt::Display for Identifier_ {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}
