use crate::pattern_matching::CompiledPatternMatch;
use crate::source::Source;
use crate::strings::{Strings, Symbol as StringSymbol};
use crate::token;
use crate::typ;
use expression::*;
use fnv::FnvHashMap;
use lazy_static::lazy_static;
use regex::Regex;
use typed_index_collections::TiVec;

use self::span::Span;
#[cfg(test)]
use crate::token::Tokens;
#[cfg(test)]
use std::fmt::Write;

// Ast to_test_string context helper

#[cfg(test)]
pub struct TestToStringContext<'a> {
    pub strings: &'a Strings,
    pub tokens: &'a Tokens,
    pub expressions: &'a Expressions,
    pub indent: usize,
}
#[cfg(test)]
impl<'a> TestToStringContext<'a> {
    pub fn indented(&self) -> Self {
        Self {
            strings: self.strings,
            tokens: self.tokens,
            expressions: self.expressions,
            indent: self.indent + 4,
        }
    }
}

pub mod span {

    use super::*;

    #[derive(PartialEq, Eq, Debug, Copy, Clone)]
    pub struct Span {
        pub start: token::Index,
        pub end: token::Index,
    }
    impl Span {
        #[cfg(test)]
        pub fn to_test_string(&self, ctx: &TestToStringContext) -> String {
            format!(
                "{}:{}",
                ctx.tokens[self.start].start, ctx.tokens[self.end].end
            )
        }
    }
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

#[derive(Debug, PartialEq, Eq, Clone)]
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

    pub fn last_span(&self) -> Span {
        self.parts
            .last()
            .expect("Module names should never be empty")
            .span
    }

    pub fn to_string<'strings>(&self, strings: &'strings Strings) -> &'strings str {
        strings.resolve(self.full_name)
    }

    #[cfg(test)]
    pub fn to_test_string(&self, ctx: &TestToStringContext) -> String {
        format!("{name}", name = ctx.strings.resolve(self.full_name))
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
    pub compiled_pattern_matching_expressions: FnvHashMap<expression::Index, CompiledPatternMatch>,
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
    }

    #[cfg(test)]
    pub fn to_test_string(&self, ctx: &TestToStringContext) -> String {
        let mut out = String::new();
        writeln!(out, "module {name}", name = self.name.to_test_string(ctx)).unwrap();
        writeln!(out, "    exports:").unwrap();
        writeln!(
            out,
            "{}",
            &self
                .exports
                .iter()
                .map(|export| export.to_test_string(&ctx.indented().indented()))
                .collect::<Vec<String>>()
                .join("\n"),
        )
        .unwrap();
        writeln!(out, "    imports:").unwrap();
        writeln!(
            out,
            "{}",
            &self
                .imports
                .iter()
                .map(|import| import.to_test_string(&ctx.indented().indented()))
                .collect::<Vec<String>>()
                .join("\n"),
        )
        .unwrap();
        writeln!(out, "    type_definitions:").unwrap();
        writeln!(
            out,
            "{}",
            &self
                .type_definitions
                .iter()
                .map(|type_definition| type_definition.to_test_string(&ctx.indented().indented()))
                .collect::<Vec<String>>()
                .join("\n"),
        )
        .unwrap();
        writeln!(out, "    definitions:").unwrap();
        writeln!(
            out,
            "{}",
            &self
                .definitions
                .iter()
                .map(|definition| definition.to_test_string(&ctx.indented().indented()))
                .collect::<Vec<String>>()
                .join("\n"),
        )
        .unwrap();

        out
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Import {
    pub span: Span,
    pub module_name: ModuleName,
    pub alias: Option<CapitalizedIdentifier>,
    pub exposing: Vec<Export>,
}
impl Import {
    #[cfg(test)]
    pub fn to_test_string(&self, ctx: &TestToStringContext) -> String {
        format!(
            "{0:indent$}{name}{alias} {span}{nl}{exports}",
            "",
            indent = ctx.indent,
            span = self.span.to_test_string(ctx),
            name = self.module_name.to_test_string(ctx),
            alias = self
                .alias
                .as_ref()
                .map(|a| format!(" alias {}", a.to_test_string(ctx)))
                .unwrap_or("".to_owned()),
            nl = if self.exposing.is_empty() { "" } else { "\n" },
            exports = self
                .exposing
                .iter()
                .map(|e| e.to_test_string(&ctx.indented()))
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Export {
    pub span: Span,
    pub typ: ExportData,
}
impl Export {
    #[cfg(test)]
    pub fn to_test_string(&self, ctx: &TestToStringContext) -> String {
        format!(
            "{0:indent$}{export} {span}",
            "",
            indent = ctx.indent,
            export = self.typ.to_test_string(ctx),
            span = self.span.to_test_string(ctx)
        )
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ExportData {
    Identifier(Identifier),
    Type {
        name: CapitalizedIdentifier,
        constructors: Vec<CapitalizedIdentifier>,
    },
}
impl ExportData {
    #[cfg(test)]
    pub fn to_test_string(&self, ctx: &TestToStringContext) -> String {
        match self {
            ExportData::Identifier(id) => id.to_test_string(ctx),
            ExportData::Type { name, constructors } => format!(
                "{name}{ps}{ctors}{pe}",
                name = name.to_test_string(ctx),
                ps = if constructors.is_empty() { "" } else { " (" },
                pe = if constructors.is_empty() { "" } else { ")" },
                ctors = constructors
                    .iter()
                    .map(|c| c.to_test_string(ctx))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
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

    #[cfg(test)]
    pub fn to_test_string(&self, ctx: &TestToStringContext) -> String {
        let (typ, content) = {
            let ctx = &ctx.indented();
            match self {
                TypedDefinition::External(signature) => ("External", signature.to_test_string(ctx)),
                TypedDefinition::TypeSignature(signature) => {
                    ("TypeSignature", signature.to_test_string(ctx))
                }
                TypedDefinition::Typed(signature, definition) => (
                    "Typed",
                    format!(
                        "{}\n{}",
                        signature.to_test_string(ctx),
                        definition.to_test_string(ctx),
                    ),
                ),
                TypedDefinition::Untyped(definition) => ("Untyped", definition.to_test_string(ctx)),
            }
        };
        format!("{0:indent$}{typ}\n{content}", "", indent = ctx.indent,)
    }
}

#[derive(Debug, Clone)]
pub struct TypeSignature {
    pub name: Identifier,
    pub typ: types::Type,
}
impl TypeSignature {
    #[cfg(test)]
    pub fn to_test_string(&self, ctx: &TestToStringContext) -> String {
        format!(
            "{0:indent$}{name}:\n{typ}",
            "",
            indent = ctx.indent,
            name = self.name.to_test_string(ctx),
            typ = self.typ.to_test_string(&ctx.indented()),
        )
    }
}

#[derive(Debug, Clone)]
pub enum Definition {
    Lambda(Identifier, Lambda),
    Pattern(Pattern, expression::Index),
}
impl Definition {
    #[cfg(test)]
    pub fn to_test_string(&self, ctx: &TestToStringContext) -> String {
        match self {
            Definition::Lambda(identifier, lambda) => format!(
                "{0:indent$}Lambda {name}\n{lambda}",
                "",
                indent = ctx.indent,
                name = identifier.to_test_string(ctx),
                lambda = lambda.to_test_string(&ctx.indented()),
            ),
            Definition::Pattern(pattern, expression_idx) => format!(
                "{0:indent$}pattern:\n{pattern}\n{0:indent$}expr:\n{expression}",
                "",
                indent = ctx.indent,
                pattern = pattern.to_test_string(&ctx.indented()),
                expression = ctx.expressions.values[*expression_idx]
                    .to_test_string(*expression_idx, &ctx.indented())
            ),
        }
    }
}

// Types

pub mod types {
    use super::*;

    #[derive(Debug)]
    pub struct TypeDefinition {
        pub span: Span,
        pub name: CapitalizedIdentifier,
        pub vars: Vec<Identifier>,
        pub typ: TypeDefinitionData,
    }
    impl TypeDefinition {
        #[cfg(test)]
        pub fn to_test_string(&self, ctx: &TestToStringContext) -> String {
            let typ = self.typ.to_test_string(&ctx.indented());
            format!(
                "{0:indent$}{name}{ps}{vars}{pe}{nl}{typ}",
                "",
                indent = ctx.indent,
                name = self.name.to_test_string(ctx),
                ps = if self.vars.is_empty() { "" } else { " (" },
                pe = if self.vars.is_empty() { "" } else { ")" },
                vars = self
                    .vars
                    .iter()
                    .map(|var| var.to_test_string(ctx))
                    .collect::<Vec<String>>()
                    .join(", "),
                nl = if typ.is_empty() { "" } else { "\n" }
            )
        }
    }

    #[derive(Debug)]
    pub enum TypeDefinitionData {
        Union { constructors: Vec<Constructor> },
        Record(RecordType),
        Empty,
    }
    impl TypeDefinitionData {
        #[cfg(test)]
        pub fn to_test_string(&self, ctx: &TestToStringContext) -> String {
            match self {
                TypeDefinitionData::Union { constructors } => {
                    if constructors.is_empty() {
                        "".to_owned()
                    } else {
                        constructors
                            .iter()
                            .map(|constructor| constructor.to_test_string(ctx))
                            .collect::<Vec<String>>()
                            .join("\n")
                    }
                }
                TypeDefinitionData::Record(record) => record.to_test_string(ctx),
                TypeDefinitionData::Empty => "".to_owned(),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct Constructor {
        pub span: Span,
        pub name: CapitalizedIdentifier,
        pub params: Vec<Type>,
    }
    impl Constructor {
        #[cfg(test)]
        pub fn to_test_string(&self, ctx: &TestToStringContext) -> String {
            format!(
                "{0:indent$}{name} {span}{nl}{params}",
                "",
                indent = ctx.indent,
                span = self.span.to_test_string(ctx),
                name = self.name.to_test_string(ctx),
                params = self
                    .params
                    .iter()
                    .map(|p| p.to_test_string(&ctx.indented()))
                    .collect::<Vec<String>>()
                    .join("\n"),
                nl = if self.params.is_empty() { "" } else { "\n" }
            )
        }
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

        #[cfg(test)]
        pub fn to_test_string(&self, ctx: &TestToStringContext) -> String {
            match self {
                Type::Fun { params, ret } => format!(
                    "{0:indent$}Fn\n{0:indent2$}params:\n{params}\n{ret}",
                    "",
                    indent = ctx.indent,
                    indent2 = ctx.indented().indent,
                    params = params
                        .iter()
                        .map(|p| p.to_test_string(&ctx.indented().indented()))
                        .collect::<Vec<String>>()
                        .join("\n"),
                    ret = ret.to_test_string(&ctx.indented()),
                ),
                Type::App(constructor) => constructor.to_test_string(ctx),
                Type::Var(identifier) => format!(
                    "{0:indent$}{identifier}",
                    "",
                    indent = ctx.indent,
                    identifier = identifier.to_test_string(ctx),
                ),
                Type::Record(record) => record.to_test_string(ctx),
            }
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

        #[cfg(test)]
        pub fn to_test_string(&self, ctx: &TestToStringContext) -> String {
            match self {
                RecordType::Record(record) => record.to_test_string(ctx),
                RecordType::RecordExt(record) => record.to_test_string(ctx),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct Record {
        pub span: Span,
        pub fields: Vec<(Identifier, Type)>,
    }
    impl Record {
        pub fn fill_vars<'typ>(&'typ self, vars: &mut Vec<&'typ Identifier>) {
            for (_, typ) in &self.fields {
                typ.fill_vars(vars);
            }
        }

        #[cfg(test)]
        pub fn to_test_string(&self, ctx: &TestToStringContext) -> String {
            format!(
                "{0:indent$}{{ {span}{nl}{fields}{nl}{0:closing_indent$}}}",
                "",
                indent = ctx.indent,
                span = self.span.to_test_string(ctx),
                nl = if self.fields.is_empty() { "" } else { "\n" },
                closing_indent = if self.fields.is_empty() {
                    1
                } else {
                    ctx.indent
                },
                fields = self
                    .fields
                    .iter()
                    .map(|(id, typ)| format!(
                        "{0:indent$}{id} :\n{typ}",
                        "",
                        indent = ctx.indented().indent,
                        id = id.to_test_string(ctx),
                        typ = typ.to_test_string(&ctx.indented().indented())
                    ))
                    .collect::<Vec<String>>()
                    .join("\n"),
            )
        }
    }

    #[derive(Debug, Clone)]
    pub struct RecordExt {
        pub span: Span,
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

        #[cfg(test)]
        pub fn to_test_string(&self, ctx: &TestToStringContext) -> String {
            format!(
                "{0:indent$}{{ {span}\n{0:indent2$}{extension} |{nl}{fields}{nl}{0:closing_indent$}}}",
                "",
                indent = ctx.indent,
                indent2 = ctx.indented().indent,
                span = self.span.to_test_string(ctx),
                extension = self.extension.to_test_string(ctx),
                nl = if self.fields.is_empty() { "" } else { "\n" },
                closing_indent = if self.fields.is_empty() {
                    1
                } else {
                    ctx.indent
                },
                fields = self
                    .fields
                    .iter()
                    .map(|(id, typ)| format!(
                        "{0:indent$}{id} :\n{typ}",
                        "",
                        indent = ctx.indented().indent,
                        id = id.to_test_string(ctx),
                        typ = typ.to_test_string(&ctx.indented().indented())
                    ))
                    .collect::<Vec<String>>()
                    .join("\n"),
            )
        }
    }
}

// Expressions

pub mod expression {
    use super::{span::Span, *};
    use crate::index;

    pub type Index = index::Index<Expression>;
    pub type ExpressionValues = TiVec<Index, Expression>;
    pub type ExpressionTypes = TiVec<Index, Option<typ::Index>>;
    pub type ExpressionSpans = TiVec<Index, Span>;

    #[derive(Debug)]
    pub struct Expressions {
        pub values: ExpressionValues,
        pub types: ExpressionTypes,
        pub spans: ExpressionSpans,
    }
    impl Expressions {
        pub fn new() -> Self {
            Self {
                values: ExpressionValues::new(),
                types: ExpressionTypes::new(),
                spans: ExpressionSpans::new(),
            }
        }

        pub fn empty(&mut self) -> Index {
            let idx = self.values.push_and_get_key(Expression::Float(0.0));
            self.types.push(None);
            self.spans.push(Span {
                start: 0.into(),
                end: 0.into(),
            });
            idx
        }

        pub fn untyped(&mut self, idx: Index, expression: Expression, span: Span) -> Index {
            self.values[idx] = expression;
            self.spans[idx] = span;
            idx
        }
    }

    #[derive(Debug, Clone)]
    pub enum Expression {
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
        PatternMatching {
            conditions: Vec<expression::Index>,
            branches: Vec<PatternMatchingBranch>,
        },
    }
    impl Expression {
        #[cfg(test)]
        pub fn to_test_string(&self, idx: expression::Index, ctx: &TestToStringContext) -> String {
            let span = ctx.expressions.spans[idx].to_test_string(ctx);
            let expr = match self {
                Expression::Float(num) => format!("Float {num} {span}"),
                Expression::String_(str_sym) => {
                    format!("String \"{}\" {span}", ctx.strings.resolve(*str_sym))
                }
                Expression::Identifier { module, identifier } => {
                    format!(
                        "Identifier {module}{id} {span}",
                        module = module
                            .as_ref()
                            .map(|m| format!("{}. ", m.to_test_string(ctx)))
                            .unwrap_or("".to_owned()),
                        id = identifier.to_test_string(ctx)
                    )
                }
                Expression::PropertyAccess {
                    expression,
                    property,
                } => format!(
                    "PropertyAccess {span}\n\
                    {0:indent$}expression:\n{expression}\n\
                    {0:indent$}property: {property}",
                    "",
                    indent = ctx.indented().indent,
                    expression = ctx.expressions.values[*expression]
                        .to_test_string(*expression, &ctx.indented().indented()),
                    property = property.to_test_string(ctx)
                ),
                Expression::PropertyAccessLambda { property } => format!(
                    "PropertyAccessLambda {span} {property}",
                    property = property.to_test_string(ctx)
                ),
                Expression::Record { fields } => format!(
                    "{{ {span}{nl}\
                    {fields}{nl}\
                    {0:closing_indent$}}}",
                    "",
                    nl = if fields.is_empty() { "" } else { "\n" },
                    closing_indent = if fields.is_empty() { 1 } else { ctx.indent },
                    fields = fields
                        .iter()
                        .map(|(id, expr)| format!(
                            "{0:indent$}{id} :\n{expr}",
                            "",
                            indent = ctx.indented().indent,
                            id = id.to_test_string(ctx),
                            expr = ctx.expressions.values[*expr]
                                .to_test_string(*expr, &ctx.indented().indented())
                        ))
                        .collect::<Vec<String>>()
                        .join("\n"),
                ),
                Expression::RecordUpdate { record, fields } => format!(
                    "{{ {span}\n\
                    {0:indent2$}record:\n\
                    {record}{nl}{sep}{nl}\
                    {fields}{nl}\
                    {0:closing_indent$}}}",
                    "",
                    indent2 = ctx.indented().indent,
                    record = ctx.expressions.values[*record]
                        .to_test_string(*record, &ctx.indented().indented()),
                    nl = if fields.is_empty() { "" } else { "\n" },
                    sep = if fields.is_empty() {
                        "".to_owned()
                    } else {
                        format!("{0:indent$}|", "", indent = ctx.indented().indent)
                    },
                    closing_indent = if fields.is_empty() { 1 } else { ctx.indent },
                    fields = fields
                        .iter()
                        .map(|(id, expr)| format!(
                            "{0:indent$}{id} :\n{expr}",
                            "",
                            indent = ctx.indented().indented().indent,
                            id = id.to_test_string(ctx),
                            expr = ctx.expressions.values[*expr]
                                .to_test_string(*expr, &ctx.indented().indented().indented())
                        ))
                        .collect::<Vec<String>>()
                        .join("\n"),
                ),
                Expression::Unary { op, expression } => format!(
                    "Unary {span} {op}\n{expression}",
                    expression = ctx.expressions.values[*expression]
                        .to_test_string(*expression, &ctx.indented()),
                    op = op.to_test_string(ctx)
                ),
                Expression::Binary { op, arguments, .. } => format!(
                    "Binary {span} {op}\n\
                    {0:indent$}lhs:\n\
                    {lhs}\n\
                    {0:indent$}rhs:\n\
                    {rhs}",
                    "",
                    indent = ctx.indented().indent,
                    lhs = ctx.expressions.values[arguments[0]]
                        .to_test_string(arguments[0], &ctx.indented().indented()),
                    rhs = ctx.expressions.values[arguments[1]]
                        .to_test_string(arguments[1], &ctx.indented().indented()),
                    op = op.to_test_string()
                ),
                Expression::Lambda(lambda) => format!(
                    "Lambda {span}\n{lambda}",
                    lambda = lambda.to_test_string(&ctx.indented())
                ),
                Expression::FnCall {
                    function,
                    arguments,
                } => format!(
                    "FnCall {span}\n\
                    {0:indent$}fn:\n\
                    {fun}\n\
                    {0:indent$}args:\n\
                    {args}",
                    "",
                    indent = ctx.indented().indent,
                    fun = ctx.expressions.values[*function]
                        .to_test_string(*function, &ctx.indented().indented()),
                    args = arguments
                        .iter()
                        .map(|a| ctx.expressions.values[*a]
                            .to_test_string(*a, &ctx.indented().indented()))
                        .collect::<Vec<String>>()
                        .join("\n"),
                ),
                Expression::Let { definitions, body } => format!(
                    "Let {span}\n\
                    {0:indent$}definitions:\n\
                    {defs}\n\
                    {body}",
                    "",
                    indent = ctx.indented().indent,
                    body = ctx.expressions.values[*body].to_test_string(*body, &ctx.indented()),
                    defs = definitions
                        .iter()
                        .map(|d| d.to_test_string(&ctx.indented().indented()))
                        .collect::<Vec<String>>()
                        .join("\n"),
                ),
                Expression::If {
                    condition,
                    then,
                    else_,
                } => format!(
                    "If {span}\n\
                    {0:indent$}condition:\n\
                    {condition}\n\
                    {0:indent$}then:\n\
                    {then}\n\
                    {0:indent$}else:\n\
                    {else_}",
                    "",
                    indent = ctx.indented().indent,
                    condition = ctx.expressions.values[*condition]
                        .to_test_string(*condition, &ctx.indented().indented()),
                    then = ctx.expressions.values[*then]
                        .to_test_string(*then, &ctx.indented().indented()),
                    else_ = ctx.expressions.values[*else_]
                        .to_test_string(*else_, &ctx.indented().indented()),
                ),
                Expression::PatternMatching {
                    conditions: expressions,
                    branches,
                } => format!(
                    "PatternMatching {span}\n\
                    {0:indent$}conditions:\n\
                    {condition}\n\
                    {branches}",
                    "",
                    indent = ctx.indented().indent,
                    condition = expressions
                        .iter()
                        .map(|expression| ctx.expressions.values[*expression]
                            .to_test_string(*expression, &ctx.indented().indented()))
                        .collect::<Vec<String>>()
                        .join("\n"),
                    branches = branches
                        .iter()
                        .map(|b| b.to_test_string(&ctx.indented()))
                        .collect::<Vec<String>>()
                        .join("\n"),
                ),
            };
            format!("{0:indent$}{expr}", "", indent = ctx.indent)
        }
    }

    #[derive(Debug, Clone)]
    pub struct Lambda {
        pub parameters: Vec<Pattern>,
        pub body: expression::Index,
    }
    impl Lambda {
        #[cfg(test)]
        pub fn to_test_string(&self, ctx: &TestToStringContext) -> String {
            format!(
                "{0:indent$}params:\n{parameters}\n{body}",
                "",
                indent = ctx.indent,
                parameters = self
                    .parameters
                    .iter()
                    .map(|p| p.to_test_string(&ctx.indented()))
                    .collect::<Vec<String>>()
                    .join("\n"),
                body = ctx.expressions.values[self.body].to_test_string(self.body, ctx),
            )
        }
    }

    #[derive(Debug, Clone)]
    pub struct PatternMatchingBranch {
        pub span: Span,
        pub patterns: Vec<Pattern>,
        pub condition: Option<expression::Index>,
        pub body: expression::Index,
    }
    impl PatternMatchingBranch {
        #[cfg(test)]
        pub fn to_test_string(&self, ctx: &TestToStringContext) -> String {
            format!(
                "{0:indent$}PatternMatchingBranch {span}\n\
                {0:indent2$}patterns:\n\
                {patterns}\n\
                {condition}\
                {0:indent2$}body:\n\
                {body}",
                "",
                indent = ctx.indent,
                indent2 = ctx.indented().indent,
                patterns = self
                    .patterns
                    .iter()
                    .map(|pattern| pattern.to_test_string(&ctx.indented().indented()))
                    .collect::<Vec<String>>()
                    .join("\n"),
                condition = self
                    .condition
                    .map(|c| format!(
                        "{0:indent$}condition:\n{condition}\n",
                        "",
                        indent = ctx.indented().indent,
                        condition =
                            ctx.expressions.values[c].to_test_string(c, &ctx.indented().indented())
                    ))
                    .unwrap_or("".to_owned()),
                body = ctx.expressions.values[self.body]
                    .to_test_string(self.body, &ctx.indented().indented()),
                span = self.span.to_test_string(ctx)
            )
        }
    }

    // Operators

    #[derive(PartialEq, Eq, Debug, Clone)]
    pub struct Unary {
        pub span: Span,
        pub typ: UnaryData,
    }
    impl Unary {
        #[cfg(test)]
        pub fn to_test_string(&self, ctx: &TestToStringContext) -> String {
            format!(
                "{:?} {span}",
                self.typ,
                span = self.span.to_test_string(ctx)
            )
        }
    }

    #[derive(PartialEq, Eq, Debug, Clone)]
    pub enum UnaryData {
        Not,
        Minus,
    }

    pub mod binop {
        use super::Identifier;
        use crate::{ast::span::Span, strings::Strings};

        #[derive(PartialEq, Eq, Debug, Clone)]
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

        #[derive(PartialEq, Eq, Debug, Clone)]
        pub enum Associativity {
            Ltr,
            // RTL,
        }

        #[derive(PartialEq, Eq, Debug, Clone)]
        pub struct Binop {
            pub typ: Type,
            pub precedence: u32,
            pub associativity: Associativity,
        }

        impl Binop {
            pub fn get_function_identifier(&self, strings: &mut Strings, span: Span) -> Identifier {
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

            #[cfg(test)]
            pub fn to_test_string(&self) -> String {
                format!("{:?}", self.typ,)
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
    use fnv::FnvHashSet;

    // Patterns

    #[derive(PartialEq, Eq, Debug, Clone)]
    pub struct Pattern {
        pub span: Span,
        pub data: PatternData,
    }
    impl Pattern {
        #[cfg(test)]
        pub fn to_test_string(&self, ctx: &TestToStringContext) -> String {
            self.data.to_test_string(self.span.to_test_string(ctx), ctx)
        }
    }

    #[derive(PartialEq, Eq, Debug, Clone)]
    pub enum PatternData {
        Hole,
        Identifier(Identifier),
        String_(StringSymbol),
        Float(StringSymbol),
        Type {
            module: Option<ModuleName>,
            constructor: CapitalizedIdentifier,
            params: Vec<Pattern>,
        },
        Record(Vec<(Identifier, Pattern)>),
        Named {
            pattern: Box<Pattern>,
            name: Identifier,
        },
        Or(Vec<Pattern>),
    }
    impl PatternData {
        pub fn is_useless_in_bindings(&self) -> bool {
            use PatternData::*;
            match self {
                String_(_) | Float(_) => true,
                Hole | Identifier(_) | Type { .. } | Named { .. } => false,
                Or(patterns) => patterns.iter().any(|p| p.data.is_useless_in_bindings()),
                Record(fields) => fields.iter().all(|(_, p)| p.data.is_useless_in_bindings()),
            }
        }

        pub fn get_bindings(&self, names: &mut FnvHashSet<IdentifierName>) {
            use PatternData::*;
            match self {
                String_(_) | Float(_) | Hole => (),
                Identifier(ident) => {
                    names.insert(ident.name);
                }
                Type { params, .. } => params.iter().for_each(|p| p.data.get_bindings(names)),
                Named { name, pattern } => {
                    names.insert(name.name);
                    pattern.data.get_bindings(names);
                }
                Or(patterns) => patterns.iter().for_each(|p| p.data.get_bindings(names)),
                Record(fields) => fields.iter().for_each(|(_, p)| p.data.get_bindings(names)),
            }
        }

        #[cfg(test)]
        pub fn to_test_string(&self, span: String, ctx: &TestToStringContext) -> String {
            use PatternData::*;

            match self {
                Hole => format!("{0:indent$}_ {span}", "", indent = ctx.indent),
                Identifier(identifier) => format!(
                    "{0:indent$}Identifier {identifier} {span}",
                    "",
                    indent = ctx.indent,
                    identifier = identifier.to_test_string(ctx)
                ),
                String_(str_sym) => format!(
                    "{0:indent$}String \"{str}\" {span}",
                    "",
                    indent = ctx.indent,
                    str = ctx.strings.resolve(*str_sym)
                ),
                Float(num) => {
                    format!(
                        "{0:indent$}Float {num} {span}",
                        "",
                        indent = ctx.indent,
                        num = ctx.strings.resolve(*num)
                    )
                }
                Type {
                    module,
                    constructor,
                    params,
                } => format!(
                    "{0:indent$}{module}{constructor} {span}{nl}{params}",
                    "",
                    indent = ctx.indent,
                    module = module
                        .as_ref()
                        .map(|m| format!("{}.", m.to_test_string(ctx)))
                        .unwrap_or("".to_owned()),
                    constructor = constructor.to_test_string(ctx),
                    nl = if params.is_empty() { "" } else { "\n" },
                    params = params
                        .iter()
                        .map(|p| p.to_test_string(&ctx.indented()))
                        .collect::<Vec<String>>()
                        .join("\n")
                ),
                Named { pattern, name } => format!(
                    "{0:indent$}Named: {name} {span}\n{pattern}",
                    "",
                    indent = ctx.indent,
                    name = name.to_test_string(ctx),
                    pattern = pattern.to_test_string(&ctx.indented()),
                ),
                Or(patterns) => format!(
                    "{0:indent$}Or {span}\n{patterns}",
                    "",
                    indent = ctx.indent,
                    patterns = patterns
                        .iter()
                        .map(|p| p.to_test_string(&ctx.indented()))
                        .collect::<Vec<String>>()
                        .join("\n")
                ),
                Record(fields) => format!(
                    "{0:indent$}{{ {span}{nl}{fields}{nl}{0:closing_indent$}}}",
                    "",
                    indent = ctx.indent,
                    nl = if fields.is_empty() { "" } else { "\n" },
                    closing_indent = if fields.is_empty() { 1 } else { ctx.indent },
                    fields = fields
                        .iter()
                        .map(|(id, p)| format!(
                            "{0:indent$}{id} :\n{p}",
                            "",
                            indent = ctx.indented().indent,
                            id = id.to_test_string(ctx),
                            p = p.to_test_string(&ctx.indented().indented())
                        ))
                        .collect::<Vec<String>>()
                        .join("\n"),
                ),
            }
        }
    }

    // Identifiers

    pub type IdentifierName = StringSymbol;

    #[derive(PartialEq, Eq, Debug, Clone)]
    pub struct CapitalizedIdentifier {
        pub name: IdentifierName,
        pub span: Span,
    }
    impl CapitalizedIdentifier {
        pub fn to_string<'strings>(&self, strings: &'strings Strings) -> &'strings str {
            strings.resolve(self.name)
        }

        #[cfg(test)]
        pub fn to_test_string(&self, ctx: &TestToStringContext) -> String {
            format!(
                "{} {}",
                ctx.strings.resolve(self.name),
                self.span.to_test_string(ctx)
            )
        }
    }

    #[derive(PartialEq, Eq, Debug, Copy, Clone)]
    pub struct Identifier {
        pub name: IdentifierName,
        pub span: Span,
    }

    impl Identifier {
        pub fn to_string(self, strings: &Strings) -> &str {
            strings.resolve(self.name)
        }

        #[cfg(test)]
        pub fn to_test_string(&self, ctx: &TestToStringContext) -> String {
            format!(
                "{} {}",
                ctx.strings.resolve(self.name),
                self.span.to_test_string(ctx)
            )
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

        pub fn span(&self) -> Span {
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

        #[cfg(test)]
        pub fn to_test_string(&self, ctx: &TestToStringContext) -> String {
            use AnyIdentifier::*;
            match self {
                Identifier(i) => i.to_test_string(ctx),
                CapitalizedIdentifier(i) => i.to_test_string(ctx),
            }
        }
    }
}
