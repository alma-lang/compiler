use crate::source::Source;
use crate::strings::{Strings, Symbol as StringSymbol};
use crate::token::Token;
use crate::typ;
use lazy_static::lazy_static;
use regex::Regex;
use std::cell::RefCell;
use std::rc::Rc;

// AST Node

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

// Repl

#[derive(Debug)]
pub enum ReplEntry {
    Import(Import),
    Definition(Definition),
    Expression(Expression),
}

// Modules

pub type ModuleFullName = StringSymbol;

#[derive(Debug, PartialEq)]
pub struct ModuleName {
    pub parts: Vec<CapitalizedIdentifier>,
    pub full_name: ModuleFullName,
}

impl ModuleName {
    pub fn new(
        parts: Vec<CapitalizedIdentifier>,
        strings: &mut Strings,
    ) -> Result<Self, (usize, Vec<CapitalizedIdentifier>)> {
        let str_parts = parts
            .iter()
            .map(|i| i.value.to_string(strings))
            .collect::<Vec<_>>();
        let full_name = str_parts.join(".");

        match ModuleName::valid_parts(&str_parts) {
            Ok(()) => {
                let full_name = strings.get_or_intern(full_name);
                Ok(Self { parts, full_name })
            }
            Err(i) => Err((i, parts)),
        }
    }

    pub fn valid_part(part: &str) -> bool {
        lazy_static! {
            static ref MODULE_IDENTIFIER_RE: Regex = Regex::new(r"^[A-Z][a-zA-Z0-9]*$").unwrap();
        }

        MODULE_IDENTIFIER_RE.is_match(part)
    }

    pub fn valid_parts(parts: &[&str]) -> Result<(), usize> {
        for (i, part) in parts.iter().enumerate() {
            if !ModuleName::valid_part(part) {
                return Err(i);
            }
        }
        Ok(())
    }

    pub fn valid_top_level_in_file(&self, source: &Source, strings: &Strings) -> bool {
        if let Some(file_name) = source.file_stem() {
            let last_module_segment = self
                .parts
                .last()
                .expect("Module name parts should never be empty");

            last_module_segment.value.to_string(strings) == file_name
        } else {
            true
        }
    }

    pub fn valid_in_parent_module(&self, parent: &ModuleName) -> bool {
        if parent.parts.len() != self.parts.len() - 1 {
            return false;
        }

        for (i, parent_name) in parent.parts.iter().enumerate() {
            let name = &self.parts[i];
            if name.value.name != parent_name.value.name {
                return false;
            }
        }

        true
    }

    pub fn end(&self) -> usize {
        self.parts
            .last()
            .expect("Module names should never be empty")
            .end
    }

    pub fn to_string<'strings>(&self, strings: &'strings Strings) -> &'strings str {
        strings.resolve(self.full_name)
    }
}

#[derive(Debug)]
pub struct Module {
    pub name: ModuleName,
    pub exports: Vec<Export>,
    pub imports: Vec<Import>,
    pub definitions: Vec<Definition>,
    pub type_definitions: Vec<types::TypeDefinition>,
}

impl Module {
    pub fn dependencies(&self) -> Vec<&ModuleName> {
        let mut deps = vec![];
        for import in &self.imports {
            deps.push(&import.value.module_name);
        }
        deps
    }
}

pub type Import = Node<Import_>;
#[derive(Debug, PartialEq)]
pub struct Import_ {
    pub module_name: ModuleName,
    pub alias: Option<CapitalizedIdentifier>,
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

#[derive(Debug)]
pub enum Definition {
    Lambda(Identifier, Expression /* ExpressionType::Lambda */),
    Pattern(Pattern, Expression),
}

// Types

pub mod types {
    use super::*;

    pub type TypeDefinition = Node<TypeDefinition_>;

    #[derive(Debug)]
    pub struct TypeDefinition_ {
        name: CapitalizedIdentifier,
        vars: Vec<Identifier>,
        typ: TypeDefinitionType,
    }
    impl TypeDefinition_ {
        pub fn new(
            name: CapitalizedIdentifier,
            vars: Vec<Identifier>,
            typ: TypeDefinitionType,
        ) -> Self {
            Self { name, vars, typ }
        }
    }

    #[derive(Debug)]
    pub enum TypeDefinitionType {
        Union(Vec<Constructor>),
        Record(RecordType),
    }

    pub type Constructor = Node<Constructor_>;
    #[derive(Debug)]
    pub struct Constructor_ {
        name: CapitalizedIdentifier,
        params: Vec<Type>,
    }
    impl Constructor_ {
        pub fn new(name: CapitalizedIdentifier, params: Vec<Type>) -> Self {
            Constructor_ { name, params }
        }
    }

    #[derive(Debug)]
    pub enum Type {
        App(Constructor),
        Var(Identifier),
        Record(RecordType),
    }

    #[derive(Debug)]
    pub enum RecordType {
        Record(Record),
        RecordExt(RecordExt),
    }

    pub type Record = Node<Record_>;
    #[derive(Debug)]
    pub struct Record_ {
        fields: Vec<(Identifier, Type)>,
    }
    impl Record_ {
        pub fn new(fields: Vec<(Identifier, Type)>) -> Self {
            Self { fields }
        }
    }

    pub type RecordExt = Node<RecordExt_>;
    #[derive(Debug)]
    pub struct RecordExt_ {
        extension: Identifier,
        fields: Vec<(Identifier, Type)>,
    }
    impl RecordExt_ {
        pub fn new(extension: Identifier, fields: Vec<(Identifier, Type)>) -> Self {
            Self { extension, fields }
        }
    }
}

// Expressions

pub type Expression = Node<Expression_>;

#[derive(Debug)]
pub struct Expression_ {
    pub typ: RefCell<Option<Rc<typ::Type>>>,
    pub expr: ExpressionType,
}

impl Expression_ {
    pub fn untyped(expr: ExpressionType) -> Self {
        Self {
            typ: RefCell::new(None),
            expr,
        }
    }

    pub fn set_type(&self, typ: Rc<typ::Type>) {
        *self.typ.borrow_mut() = Some(typ);
    }
}

#[derive(Debug)]
pub enum ExpressionType {
    Unit,
    Bool(bool),
    Float(f64),
    String_(StringSymbol),
    Identifier(Option<ModuleName>, AnyIdentifier),
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

// Operators

type Unary = Node<Unary_>;

#[derive(PartialEq, Debug, Clone)]
pub enum Unary_ {
    Not,
    Minus,
}

pub mod binop {
    use super::{Identifier_, Node};
    use crate::strings::Strings;

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

    impl Binop_ {
        pub fn or(strings: &mut Strings) -> Self {
            Binop_ {
                typ: Or,
                precedence: 6,
                associativity: Ltr,
                fn_: Identifier_::new("__op__or", strings),
            }
        }

        pub fn and(strings: &mut Strings) -> Self {
            Binop_ {
                typ: And,
                precedence: 7,
                associativity: Ltr,
                fn_: Identifier_::new("__op__and", strings),
            }
        }

        pub fn equal(strings: &mut Strings) -> Self {
            Binop_ {
                typ: Equal,
                precedence: 11,
                associativity: Ltr,
                fn_: Identifier_::new("__op__eq", strings),
            }
        }

        pub fn not_equal(strings: &mut Strings) -> Self {
            Binop_ {
                typ: NotEqual,
                precedence: 11,
                associativity: Ltr,
                fn_: Identifier_::new("__op__ne", strings),
            }
        }

        pub fn greater_than(strings: &mut Strings) -> Self {
            Binop_ {
                typ: GreaterThan,
                precedence: 12,
                associativity: Ltr,
                fn_: Identifier_::new("__op__gt", strings),
            }
        }

        pub fn greater_equal_than(strings: &mut Strings) -> Self {
            Binop_ {
                typ: GreaterEqualThan,
                precedence: 12,
                associativity: Ltr,
                fn_: Identifier_::new("__op__ge", strings),
            }
        }

        pub fn less_than(strings: &mut Strings) -> Self {
            Binop_ {
                typ: LessThan,
                precedence: 12,
                associativity: Ltr,
                fn_: Identifier_::new("__op__lt", strings),
            }
        }

        pub fn less_equal_than(strings: &mut Strings) -> Self {
            Binop_ {
                typ: LessEqualThan,
                precedence: 12,
                associativity: Ltr,
                fn_: Identifier_::new("__op__le", strings),
            }
        }

        pub fn addition(strings: &mut Strings) -> Self {
            Binop_ {
                typ: Addition,
                precedence: 14,
                associativity: Ltr,
                fn_: Identifier_::new("__op__add", strings),
            }
        }

        pub fn substraction(strings: &mut Strings) -> Self {
            Binop_ {
                typ: Substraction,
                precedence: 14,
                associativity: Ltr,
                fn_: Identifier_::new("__op__sub", strings),
            }
        }

        pub fn multiplication(strings: &mut Strings) -> Self {
            Binop_ {
                typ: Multiplication,
                precedence: 15,
                associativity: Ltr,
                fn_: Identifier_::new("__op__mult", strings),
            }
        }

        pub fn division(strings: &mut Strings) -> Self {
            Binop_ {
                typ: Division,
                precedence: 15,
                associativity: Ltr,
                fn_: Identifier_::new("__op__div", strings),
            }
        }
    }
}
use binop::Binop;

// Patterns

pub type Pattern = Node<Pattern_>;

#[derive(PartialEq, Debug)]
pub enum Pattern_ {
    Hole,
    Identifier(Identifier),
}

// Identifiers

type IdentifierName = StringSymbol;

pub type CapitalizedIdentifier = Node<CapitalizedIdentifier_>;
#[derive(PartialEq, Debug, Clone)]
pub struct CapitalizedIdentifier_ {
    pub name: IdentifierName,
}
impl CapitalizedIdentifier_ {
    pub fn new(name: &str, strings: &mut Strings) -> Self {
        let name_sym = strings.get_or_intern(name);
        Self { name: name_sym }
    }

    pub fn to_string<'strings>(&self, strings: &'strings Strings) -> &'strings str {
        strings.resolve(self.name)
    }
}

pub type Identifier = Node<Identifier_>;
#[derive(PartialEq, Debug, Clone)]
pub struct Identifier_ {
    pub name: IdentifierName,
}

impl Identifier_ {
    pub fn new(name: &str, strings: &mut Strings) -> Self {
        let name_sym = strings.get_or_intern(name);
        Self { name: name_sym }
    }

    pub fn to_string<'strings>(&self, strings: &'strings Strings) -> &'strings str {
        strings.resolve(self.name)
    }
}

#[derive(Debug)]
pub enum AnyIdentifier {
    Identifier(Identifier),
    CapitalizedIdentifier(CapitalizedIdentifier),
}
impl AnyIdentifier {
    pub fn name(&self) -> IdentifierName {
        use AnyIdentifier::*;
        match self {
            Identifier(i) => i.value.name,
            CapitalizedIdentifier(i) => i.value.name,
        }
    }

    pub fn node(&self) -> Node<()> {
        use AnyIdentifier::*;
        match self {
            Identifier(i) => Node::copy_with_value((), i),
            CapitalizedIdentifier(i) => Node::copy_with_value((), i),
        }
    }

    pub fn to_string<'strings>(&self, strings: &'strings Strings) -> &'strings str {
        use AnyIdentifier::*;
        match self {
            Identifier(i) => i.value.to_string(strings),
            CapitalizedIdentifier(i) => i.value.to_string(strings),
        }
    }
}
