use crate::index;
use crate::source::Source;
use crate::strings::{Strings, Symbol as StringSymbol};
use crate::token;
use crate::typ;
use expression::*;
use lazy_static::lazy_static;
use regex::Regex;
use typed_index_collections::TiVec;

pub mod span {
    use super::*;

    #[derive(PartialEq, Debug, Copy, Clone)]
    pub struct Span {
        pub start: token::Index,
        pub end: token::Index,
    }

    pub type Index = index::Index<Span>;

    pub type Spans = TiVec<Index, Span>;
}

// Repl

#[derive(Debug)]
pub enum ReplEntry {
    Import(Import),
    Definition(Definition),
    Expression(expression::Index),
}

// Modules

pub type ModuleFullName = StringSymbol;

#[derive(Debug, PartialEq, Clone)]
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
            .map(|i| i.to_string(strings))
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

            last_module_segment.to_string(strings) == file_name
        } else {
            true
        }
    }

    pub fn valid_in_parent_module(&self, parent: &ModuleName) -> bool {
        if parent.parts.len() != self.parts.len() - 1 {
            return false;
        }

        for (i, parent_name) in parent.parts.iter().enumerate() {
            let part = &self.parts[i];
            if part.name != parent_name.name {
                return false;
            }
        }

        true
    }

    pub fn last_span(&self) -> span::Index {
        self.parts
            .last()
            .expect("Module names should never be empty")
            .span
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
    pub expressions: Expressions,
    pub expression_types: ExpressionTypes,
}

impl Module {
    pub fn dependencies(&self) -> Vec<&ModuleName> {
        let mut deps = vec![];
        for import in &self.imports {
            deps.push(&import.module_name);
        }
        deps
    }

    pub fn has_externals(&self) -> bool {
        self.definitions
            .iter()
            .any(|def| matches!(def, TypedDefinition::External(_)))
            || self
                .type_definitions
                .iter()
                .any(|t| matches!(t.typ, types::TypeDefinitionData::External { .. }))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Import {
    pub span: span::Index,
    pub module_name: ModuleName,
    pub alias: Option<CapitalizedIdentifier>,
    pub exposing: Vec<Export>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Export {
    pub span: span::Index,
    pub typ: ExportData,
}
#[derive(Debug, PartialEq, Clone)]
pub enum ExportData {
    Identifier(Identifier),
    Type {
        name: CapitalizedIdentifier,
        constructors: Vec<CapitalizedIdentifier>,
    },
}

#[derive(Debug, Clone)]
pub enum TypedDefinition {
    External(TypeSignature),
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
            External(_) => None,
        }
    }

    pub fn typ(&self) -> Option<&TypeSignature> {
        use TypedDefinition::*;
        match self {
            External(typ) | Typed(typ, _) | TypeSignature(typ) => Some(typ),
            Untyped(_) => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeSignature {
    pub name: Identifier,
    pub typ: types::Type,
}

#[derive(Debug, Clone)]
pub enum Definition {
    Lambda(Identifier, Lambda),
    Pattern(Pattern, expression::Index),
}

// Types

pub mod types {
    use super::*;

    #[derive(Debug)]
    pub struct TypeDefinition {
        pub span: span::Index,
        pub name: CapitalizedIdentifier,
        pub vars: Vec<Identifier>,
        pub typ: TypeDefinitionData,
    }

    #[derive(Debug)]
    pub enum TypeDefinitionData {
        External { constructors: Vec<Constructor> },
        Union { constructors: Vec<Constructor> },
        Record(RecordType),
        Empty,
    }

    #[derive(Debug, Clone)]
    pub struct Constructor {
        pub span: span::Index,
        pub name: CapitalizedIdentifier,
        pub params: Vec<Type>,
    }

    #[derive(Debug, Clone)]
    pub enum Type {
        Fun { params: Vec<Type>, ret: Box<Type> },
        App(Constructor),
        Var(Identifier),
        Record(RecordType),
    }
    impl Type {
        pub fn fill_vars<'typ>(&'typ self, vars: &mut Vec<&'typ Identifier>) {
            use Type::*;
            match self {
                Fun { params, ret } => {
                    for param in params {
                        param.fill_vars(vars);
                    }
                    ret.fill_vars(vars);
                }
                App(constructor) => {
                    for param in &constructor.params {
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

    #[derive(Debug, Clone)]
    pub enum RecordType {
        Record(Record),
        RecordExt(RecordExt),
    }
    impl RecordType {
        pub fn fill_vars<'typ>(&'typ self, vars: &mut Vec<&'typ Identifier>) {
            use RecordType::*;
            match self {
                Record(rec) => rec.fill_vars(vars),
                RecordExt(rec) => rec.fill_vars(vars),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct Record {
        pub span: span::Index,
        pub fields: Vec<(Identifier, Type)>,
    }
    impl Record {
        pub fn fill_vars<'typ>(&'typ self, vars: &mut Vec<&'typ Identifier>) {
            for (_, typ) in &self.fields {
                typ.fill_vars(vars);
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct RecordExt {
        pub span: span::Index,
        pub extension: Identifier,
        pub fields: Vec<(Identifier, Type)>,
    }
    impl RecordExt {
        pub fn fill_vars<'typ>(&'typ self, vars: &mut Vec<&'typ Identifier>) {
            vars.push(&self.extension);
            for (_, typ) in &self.fields {
                typ.fill_vars(vars);
            }
        }
    }
}

// Expressions

pub mod expression {
    use super::*;
    use crate::index;

    pub type Index = index::Index<Expression>;
    pub type Expressions = TiVec<Index, Expression>;
    pub type ExpressionTypes = TiVec<Index, Option<typ::Index>>;

    #[derive(Debug, Clone)]
    pub struct Expression {
        pub span: span::Index,
        pub expr: ExpressionData,
    }

    impl Expression {
        pub fn untyped(expr: ExpressionData, span: span::Index) -> Self {
            Self { expr, span }
        }
    }

    #[derive(Debug, Clone)]
    pub enum ExpressionData {
        Float(f64),
        String_(StringSymbol),
        Identifier {
            module: Option<ModuleName>,
            identifier: AnyIdentifier,
        },
        PropertyAccess {
            expression: expression::Index,
            property: Identifier,
        },
        PropertyAccessLambda {
            property: Identifier,
        },
        Record {
            fields: Vec<(Identifier, expression::Index)>,
        },
        RecordUpdate {
            record: expression::Index,
            fields: Vec<(Identifier, expression::Index)>,
        },
        Unary {
            op: Unary,
            expression: expression::Index,
        },
        Binary {
            expression: expression::Index,
            op: Binop,
            arguments: [expression::Index; 2],
        },
        Lambda(Lambda),
        FnCall {
            function: expression::Index,
            arguments: Vec<expression::Index>,
        },
        Let {
            definitions: Vec<TypedDefinition>,
            body: expression::Index,
        },
        If {
            condition: expression::Index,
            then: expression::Index,
            else_: expression::Index,
        },
    }

    #[derive(Debug, Clone)]
    pub struct Lambda {
        pub parameters: Vec<Pattern>,
        pub body: expression::Index,
    }

    // Operators

    #[derive(PartialEq, Debug, Clone)]
    pub struct Unary {
        pub span: span::Index,
        pub typ: UnaryData,
    }
    #[derive(PartialEq, Debug, Clone)]
    pub enum UnaryData {
        Not,
        Minus,
    }

    pub mod binop {
        use super::{span, Identifier};
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

        #[derive(PartialEq, Debug, Clone)]
        pub struct Binop {
            pub typ: Type,
            pub precedence: u32,
            pub associativity: Associativity,
        }

        impl Binop {
            pub fn get_function_identifier(
                &self,
                strings: &mut Strings,
                span: span::Index,
            ) -> Identifier {
                let name_sym = strings.get_or_intern(match self.typ {
                    Or => "or_",
                    And => "and_",
                    Equal => "eq",
                    NotEqual => "ne",
                    GreaterThan => "gt",
                    GreaterEqualThan => "ge",
                    LessThan => "lt",
                    LessEqualThan => "le",
                    Addition => "add",
                    Substraction => "sub",
                    Multiplication => "mult",
                    Division => "div",
                });
                Identifier {
                    name: name_sym,
                    span,
                }
            }
        }

        use Associativity::*;
        use Type::*;

        pub const OR: Binop = Binop {
            typ: Or,
            precedence: 6,
            associativity: Ltr,
        };

        pub const AND: Binop = Binop {
            typ: And,
            precedence: 7,
            associativity: Ltr,
        };

        pub const EQUAL: Binop = Binop {
            typ: Equal,
            precedence: 11,
            associativity: Ltr,
        };

        pub const NOT_EQUAL: Binop = Binop {
            typ: NotEqual,
            precedence: 11,
            associativity: Ltr,
        };

        pub const GREATER_THAN: Binop = Binop {
            typ: GreaterThan,
            precedence: 12,
            associativity: Ltr,
        };

        pub const GREATER_EQUAL_THAN: Binop = Binop {
            typ: GreaterEqualThan,
            precedence: 12,
            associativity: Ltr,
        };

        pub const LESS_THAN: Binop = Binop {
            typ: LessThan,
            precedence: 12,
            associativity: Ltr,
        };

        pub const LESS_EQUAL_THAN: Binop = Binop {
            typ: LessEqualThan,
            precedence: 12,
            associativity: Ltr,
        };

        pub const ADDITION: Binop = Binop {
            typ: Addition,
            precedence: 14,
            associativity: Ltr,
        };

        pub const SUBSTRACTION: Binop = Binop {
            typ: Substraction,
            precedence: 14,
            associativity: Ltr,
        };

        pub const MULTIPLICATION: Binop = Binop {
            typ: Multiplication,
            precedence: 15,
            associativity: Ltr,
        };

        pub const DIVISION: Binop = Binop {
            typ: Division,
            precedence: 15,
            associativity: Ltr,
        };
    }
    use binop::Binop;

    // Patterns

    #[derive(PartialEq, Debug, Clone)]
    pub struct Pattern {
        pub span: span::Index,
        pub typ: PatternData,
    }
    #[derive(PartialEq, Debug, Clone)]
    pub enum PatternData {
        Hole,
        Identifier(Identifier),
    }

    // Identifiers

    type IdentifierName = StringSymbol;

    #[derive(PartialEq, Debug, Clone)]
    pub struct CapitalizedIdentifier {
        pub name: IdentifierName,
        pub span: span::Index,
    }
    impl CapitalizedIdentifier {
        pub fn to_string<'strings>(&self, strings: &'strings Strings) -> &'strings str {
            strings.resolve(self.name)
        }
    }

    #[derive(PartialEq, Debug, Clone)]
    pub struct Identifier {
        pub name: IdentifierName,
        pub span: span::Index,
    }

    impl Identifier {
        pub fn to_string<'strings>(&self, strings: &'strings Strings) -> &'strings str {
            strings.resolve(self.name)
        }
    }

    #[derive(Debug, Clone)]
    pub enum AnyIdentifier {
        Identifier(Identifier),
        CapitalizedIdentifier(CapitalizedIdentifier),
    }
    impl AnyIdentifier {
        pub fn name(&self) -> IdentifierName {
            use AnyIdentifier::*;
            match self {
                Identifier(i) => i.name,
                CapitalizedIdentifier(i) => i.name,
            }
        }

        pub fn span(&self) -> span::Index {
            use AnyIdentifier::*;
            match self {
                Identifier(i) => i.span,
                CapitalizedIdentifier(i) => i.span,
            }
        }

        pub fn to_string<'strings>(&self, strings: &'strings Strings) -> &'strings str {
            use AnyIdentifier::*;
            match self {
                Identifier(i) => i.to_string(strings),
                CapitalizedIdentifier(i) => i.to_string(strings),
            }
        }
    }
}
