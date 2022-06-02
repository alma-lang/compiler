use crate::index::Idx;
use crate::module;
use crate::source::Source;
use crate::strings::{Strings, Symbol as StringSymbol};
use crate::token::Token;
use crate::typ;
use lazy_static::lazy_static;
use regex::Regex;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(PartialEq, Debug)]
pub struct Node<V> {
    pub start: Idx<Token>,
    pub end: Idx<Token>,
    pub value: V,
}

impl<V> Node<V> {
    pub fn new(value: V, first_token: Idx<Token>, last_token: Idx<Token>) -> Self {
        Node {
            value,
            start: first_token,
            end: last_token,
        }
    }

    pub fn with_value<T>(&self, value: T) -> Node<T> {
        Node {
            value,
            start: self.start,
            end: self.end,
        }
    }

    pub fn unit(&self) -> Node<()> {
        self.with_value(())
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

    pub fn end(&self) -> Idx<Token> {
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
    pub definitions: Vec<TypedDefinition>,
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
    Identifier(Identifier),
    Type(CapitalizedIdentifier, Vec<CapitalizedIdentifier>),
}

#[derive(Debug)]
pub enum TypedDefinition {
    TypeSignature(TypeSignature),
    Typed(TypeSignature, Definition),
    Untyped(Definition),
}
impl TypedDefinition {
    pub fn definition(&self) -> Option<&Definition> {
        use TypedDefinition::*;
        match self {
            Typed(_, definition) | Untyped(definition) => Some(definition),
            TypeSignature(_) => None,
        }
    }

    pub fn typ(&self) -> Option<&TypeSignature> {
        use TypedDefinition::*;
        match self {
            Typed(typ, _) | TypeSignature(typ) => Some(typ),
            Untyped(_) => None,
        }
    }
}

#[derive(Debug)]
pub struct TypeSignature {
    pub name: Identifier,
    pub typ: types::Type,
}

#[derive(Debug)]
pub enum Definition {
    Lambda(Identifier, Lambda),
    Pattern(Pattern, Expression),
}

// Types

pub mod types {
    use super::*;

    pub type TypeDefinition = Node<TypeDefinition_>;

    #[derive(Debug)]
    pub struct TypeDefinition_ {
        pub name: CapitalizedIdentifier,
        pub vars: Vec<Identifier>,
        pub typ: TypeDefinitionType,
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
        pub name: CapitalizedIdentifier,
        pub params: Vec<Type>,
    }
    impl Constructor_ {
        pub fn new(name: CapitalizedIdentifier, params: Vec<Type>) -> Self {
            Constructor_ { name, params }
        }
    }

    #[derive(Debug)]
    pub enum Type {
        Fun(Vec<Type>, Box<Type>),
        App(Constructor),
        Var(Identifier),
        Record(RecordType),
    }
    impl Type {
        pub fn fill_vars<'typ>(&'typ self, vars: &mut Vec<&'typ Identifier>) {
            use Type::*;
            match self {
                Fun(params, ret) => {
                    for param in params {
                        param.fill_vars(vars);
                    }
                    ret.fill_vars(vars);
                }
                App(constructor) => {
                    for param in &constructor.value.params {
                        param.fill_vars(vars);
                    }
                }
                Var(identifier) => vars.push(identifier),
                Record(record) => record.fill_vars(vars),
            }
        }
        pub fn vars(&self) -> Vec<&Identifier> {
            let mut vars = Vec::new();
            self.fill_vars(&mut vars);
            vars
        }
    }

    #[derive(Debug)]
    pub enum RecordType {
        Record(Record),
        RecordExt(RecordExt),
    }
    impl RecordType {
        pub fn fill_vars<'typ>(&'typ self, vars: &mut Vec<&'typ Identifier>) {
            use RecordType::*;
            match self {
                Record(rec) => rec.value.fill_vars(vars),
                RecordExt(rec) => rec.value.fill_vars(vars),
            }
        }
    }

    pub type Record = Node<Record_>;
    #[derive(Debug)]
    pub struct Record_ {
        pub fields: Vec<(Identifier, Type)>,
    }
    impl Record_ {
        pub fn new(fields: Vec<(Identifier, Type)>) -> Self {
            Self { fields }
        }

        pub fn fill_vars<'typ>(&'typ self, vars: &mut Vec<&'typ Identifier>) {
            for (_, typ) in &self.fields {
                typ.fill_vars(vars);
            }
        }
    }

    pub type RecordExt = Node<RecordExt_>;
    #[derive(Debug)]
    pub struct RecordExt_ {
        pub extension: Identifier,
        pub fields: Vec<(Identifier, Type)>,
    }
    impl RecordExt_ {
        pub fn new(extension: Identifier, fields: Vec<(Identifier, Type)>) -> Self {
            Self { extension, fields }
        }

        pub fn fill_vars<'typ>(&'typ self, vars: &mut Vec<&'typ Identifier>) {
            vars.push(&self.extension);
            for (_, typ) in &self.fields {
                typ.fill_vars(vars);
            }
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
    Lambda(Lambda),
    FnCall(Box<Expression>, Vec<Expression>),
    Let(Vec<TypedDefinition>, Box<Expression>),
    If(Box<Expression>, Box<Expression>, Box<Expression>),
}

#[derive(Debug)]
pub struct Lambda {
    pub parameters: Vec<Pattern>,
    pub body: Box<Expression>,
}

// Operators

type Unary = Node<Unary_>;

#[derive(PartialEq, Debug, Clone)]
pub enum Unary_ {
    Not,
    Minus,
}

pub mod binop {
    use crate::strings::Strings;

    use super::{Identifier_, Node};

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
    }

    impl Binop_ {
        pub fn get_function_identifier(&self, strings: &mut Strings) -> Identifier_ {
            let name_sym = strings.get_or_intern(match self.typ {
                Or => "__op__or",
                And => "__op__and",
                Equal => "__op__eq",
                NotEqual => "__op__ne",
                GreaterThan => "__op__gt",
                GreaterEqualThan => "__op__ge",
                LessThan => "__op__lt",
                LessEqualThan => "__op__le",
                Addition => "__op__add",
                Substraction => "__op__sub",
                Multiplication => "__op__mult",
                Division => "__op__div",
            });
            Identifier_::new(name_sym)
        }
    }

    use Associativity::*;
    use Type::*;

    pub const OR: Binop_ = Binop_ {
        typ: Or,
        precedence: 6,
        associativity: Ltr,
    };

    pub const AND: Binop_ = Binop_ {
        typ: And,
        precedence: 7,
        associativity: Ltr,
    };

    pub const EQUAL: Binop_ = Binop_ {
        typ: Equal,
        precedence: 11,
        associativity: Ltr,
    };

    pub const NOT_EQUAL: Binop_ = Binop_ {
        typ: NotEqual,
        precedence: 11,
        associativity: Ltr,
    };

    pub const GREATER_THAN: Binop_ = Binop_ {
        typ: GreaterThan,
        precedence: 12,
        associativity: Ltr,
    };

    pub const GREATER_EQUAL_THAN: Binop_ = Binop_ {
        typ: GreaterEqualThan,
        precedence: 12,
        associativity: Ltr,
    };

    pub const LESS_THAN: Binop_ = Binop_ {
        typ: LessThan,
        precedence: 12,
        associativity: Ltr,
    };

    pub const LESS_EQUAL_THAN: Binop_ = Binop_ {
        typ: LessEqualThan,
        precedence: 12,
        associativity: Ltr,
    };

    pub const ADDITION: Binop_ = Binop_ {
        typ: Addition,
        precedence: 14,
        associativity: Ltr,
    };

    pub const SUBSTRACTION: Binop_ = Binop_ {
        typ: Substraction,
        precedence: 14,
        associativity: Ltr,
    };

    pub const MULTIPLICATION: Binop_ = Binop_ {
        typ: Multiplication,
        precedence: 15,
        associativity: Ltr,
    };

    pub const DIVISION: Binop_ = Binop_ {
        typ: Division,
        precedence: 15,
        associativity: Ltr,
    };
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
    pub fn new(name_sym: StringSymbol) -> Self {
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
    pub fn new(name_sym: StringSymbol) -> Self {
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
            Identifier(i) => i.with_value(()),
            CapitalizedIdentifier(i) => i.with_value(()),
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
