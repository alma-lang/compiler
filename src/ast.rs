use crate::index;
use crate::source::Source;
use crate::strings::{Strings, Symbol as StringSymbol};
use crate::token;
use crate::typ;
use expression::*;
use fnv::FnvHashMap;
use lazy_static::lazy_static;
use regex::Regex;
use typed_index_collections::TiVec;

#[cfg(test)]
use self::span::Spans;
#[cfg(test)]
use crate::token::Tokens;
#[cfg(test)]
use std::fmt::Write;

// Ast to_test_string context helper

#[cfg(test)]
pub struct TestToStringContext<'a> {
    pub strings: &'a Strings,
    pub tokens: &'a Tokens,
    pub spans: &'a Spans,
    pub expressions: &'a Expressions,
    pub indent: usize,
}
#[cfg(test)]
impl<'a> TestToStringContext<'a> {
    pub fn indented(&self) -> Self {
        Self {
            strings: self.strings,
            tokens: self.tokens,
            spans: self.spans,
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

    pub fn last_span(&self) -> span::Index {
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
    pub expression_types: ExpressionTypes,
    pub compiled_pattern_matching_expressions:
        FnvHashMap<expression::Index, pattern_matching::CompiledPatternMatch>,
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
    pub span: span::Index,
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
            span = ctx.spans[self.span].to_test_string(ctx),
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
    pub span: span::Index,
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
            span = ctx.spans[self.span].to_test_string(ctx)
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
                expression = ctx.expressions[*expression_idx].to_test_string(&ctx.indented())
            ),
        }
    }
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
        pub span: span::Index,
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
                span = ctx.spans[self.span].to_test_string(ctx),
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
        pub span: span::Index,
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
                span = ctx.spans[self.span].to_test_string(ctx),
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

        #[cfg(test)]
        pub fn to_test_string(&self, ctx: &TestToStringContext) -> String {
            format!(
                "{0:indent$}{{ {span}\n{0:indent2$}{extension} |{nl}{fields}{nl}{0:closing_indent$}}}",
                "",
                indent = ctx.indent,
                indent2 = ctx.indented().indent,
                span = ctx.spans[self.span].to_test_string(ctx),
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

        #[cfg(test)]
        pub fn to_test_string(&self, ctx: &TestToStringContext) -> String {
            self.expr
                .to_test_string(ctx.spans[self.span].to_test_string(ctx), ctx)
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
        PatternMatching {
            conditions: Vec<expression::Index>,
            branches: Vec<PatternMatchingBranch>,
        },
    }
    impl ExpressionData {
        #[cfg(test)]
        pub fn to_test_string(&self, span: String, ctx: &TestToStringContext) -> String {
            let expr = match self {
                ExpressionData::Float(num) => format!("Float {num} {span}"),
                ExpressionData::String_(str_sym) => {
                    format!("String \"{}\" {span}", ctx.strings.resolve(*str_sym))
                }
                ExpressionData::Identifier { module, identifier } => {
                    format!(
                        "Identifier {module}{id} {span}",
                        module = module
                            .as_ref()
                            .map(|m| format!("{}. ", m.to_test_string(ctx)))
                            .unwrap_or("".to_owned()),
                        id = identifier.to_test_string(ctx)
                    )
                }
                ExpressionData::PropertyAccess {
                    expression,
                    property,
                } => format!(
                    "PropertyAccess {span}\n\
                    {0:indent$}expression:\n{expression}\n\
                    {0:indent$}property: {property}",
                    "",
                    indent = ctx.indented().indent,
                    expression =
                        ctx.expressions[*expression].to_test_string(&ctx.indented().indented()),
                    property = property.to_test_string(ctx)
                ),
                ExpressionData::PropertyAccessLambda { property } => format!(
                    "PropertyAccessLambda {span} {property}",
                    property = property.to_test_string(ctx)
                ),
                ExpressionData::Record { fields } => format!(
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
                            expr =
                                ctx.expressions[*expr].to_test_string(&ctx.indented().indented())
                        ))
                        .collect::<Vec<String>>()
                        .join("\n"),
                ),
                ExpressionData::RecordUpdate { record, fields } => format!(
                    "{{ {span}\n\
                    {0:indent2$}record:\n\
                    {record}{nl}{sep}{nl}\
                    {fields}{nl}\
                    {0:closing_indent$}}}",
                    "",
                    indent2 = ctx.indented().indent,
                    record = ctx.expressions[*record].to_test_string(&ctx.indented().indented()),
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
                            expr = ctx.expressions[*expr]
                                .to_test_string(&ctx.indented().indented().indented())
                        ))
                        .collect::<Vec<String>>()
                        .join("\n"),
                ),
                ExpressionData::Unary { op, expression } => format!(
                    "Unary {span} {op}\n{expression}",
                    expression = ctx.expressions[*expression].to_test_string(&ctx.indented()),
                    op = op.to_test_string(ctx)
                ),
                ExpressionData::Binary { op, arguments, .. } => format!(
                    "Binary {span} {op}\n\
                    {0:indent$}lhs:\n\
                    {lhs}\n\
                    {0:indent$}rhs:\n\
                    {rhs}",
                    "",
                    indent = ctx.indented().indent,
                    lhs = ctx.expressions[arguments[0]].to_test_string(&ctx.indented().indented()),
                    rhs = ctx.expressions[arguments[1]].to_test_string(&ctx.indented().indented()),
                    op = op.to_test_string()
                ),
                ExpressionData::Lambda(lambda) => format!(
                    "Lambda {span}\n{lambda}",
                    lambda = lambda.to_test_string(&ctx.indented())
                ),
                ExpressionData::FnCall {
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
                    fun = ctx.expressions[*function].to_test_string(&ctx.indented().indented()),
                    args = arguments
                        .iter()
                        .map(|a| ctx.expressions[*a].to_test_string(&ctx.indented().indented()))
                        .collect::<Vec<String>>()
                        .join("\n"),
                ),
                ExpressionData::Let { definitions, body } => format!(
                    "Let {span}\n\
                    {0:indent$}definitions:\n\
                    {defs}\n\
                    {body}",
                    "",
                    indent = ctx.indented().indent,
                    body = ctx.expressions[*body].to_test_string(&ctx.indented()),
                    defs = definitions
                        .iter()
                        .map(|d| d.to_test_string(&ctx.indented().indented()))
                        .collect::<Vec<String>>()
                        .join("\n"),
                ),
                ExpressionData::If {
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
                    condition =
                        ctx.expressions[*condition].to_test_string(&ctx.indented().indented()),
                    then = ctx.expressions[*then].to_test_string(&ctx.indented().indented()),
                    else_ = ctx.expressions[*else_].to_test_string(&ctx.indented().indented()),
                ),
                ExpressionData::PatternMatching {
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
                        .map(|expression| ctx.expressions[*expression]
                            .to_test_string(&ctx.indented().indented()))
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
                body = ctx.expressions[self.body].to_test_string(ctx),
            )
        }
    }

    #[derive(Debug, Clone)]
    pub struct PatternMatchingBranch {
        pub span: span::Index,
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
                        condition = ctx.expressions[c].to_test_string(&ctx.indented().indented())
                    ))
                    .unwrap_or("".to_owned()),
                body = ctx.expressions[self.body].to_test_string(&ctx.indented().indented()),
                span = ctx.spans[self.span].to_test_string(ctx)
            )
        }
    }

    // Operators

    #[derive(PartialEq, Eq, Debug, Clone)]
    pub struct Unary {
        pub span: span::Index,
        pub typ: UnaryData,
    }
    impl Unary {
        #[cfg(test)]
        pub fn to_test_string(&self, ctx: &TestToStringContext) -> String {
            format!(
                "{:?} {span}",
                self.typ,
                span = ctx.spans[self.span].to_test_string(ctx)
            )
        }
    }

    #[derive(PartialEq, Eq, Debug, Clone)]
    pub enum UnaryData {
        Not,
        Minus,
    }

    pub mod binop {
        use super::{span, Identifier};
        use crate::strings::Strings;

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
        pub span: span::Index,
        pub data: PatternData,
    }
    impl Pattern {
        #[cfg(test)]
        pub fn to_test_string(&self, ctx: &TestToStringContext) -> String {
            self.data
                .to_test_string(ctx.spans[self.span].to_test_string(ctx), ctx)
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
        pub span: span::Index,
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
                ctx.spans[self.span].to_test_string(ctx)
            )
        }
    }

    #[derive(PartialEq, Eq, Debug, Copy, Clone)]
    pub struct Identifier {
        pub name: IdentifierName,
        pub span: span::Index,
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
                ctx.spans[self.span].to_test_string(ctx)
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

pub mod pattern_matching {
    use super::expression::{self, IdentifierName, Pattern, PatternData};
    use crate::{
        compiler,
        strings::Strings,
        typ::{self, Type, Types},
    };
    use fnv::{FnvHashMap, FnvHashSet};

    #[derive(Debug)]
    pub struct CompiledPatternMatch {
        pub tree: Decision,
        pub diagnostics: Diagnostics,
    }

    impl CompiledPatternMatch {
        /// Returns a list of patterns not covered by the match expression.
        pub fn missing_patterns(&self, compiler: &compiler::State) -> Vec<String> {
            let mut names = FnvHashSet::default();
            let mut steps = Vec::new();

            self.add_missing_patterns(&self.tree, &mut steps, &mut names, compiler);

            let mut missing: Vec<String> = names.into_iter().collect();

            // Sorting isn't necessary, but it makes it a bit easier to write tests.
            missing.sort();
            missing
        }

        fn add_missing_patterns(
            &self,
            node: &Decision,
            missing_patterns: &mut Vec<MissingPattern>,
            missing: &mut FnvHashSet<String>,
            compiler: &compiler::State,
        ) {
            match node {
                Decision::Success(_) => {}
                Decision::Failure => {
                    let mut mapping = FnvHashMap::default();

                    // At this point the missing_patterns stack looks something like this:
                    // `[missing_pattern, missing_pattern + arguments, missing_pattern, ...]`. To construct a pattern
                    // name from this stack, we first map all variables to their
                    // missing_pattern indexes. This is needed because when a missing_pattern defines
                    // arguments, the missing_patterns for those arguments don't necessarily
                    // appear in order in the missing_pattern stack.
                    //
                    // This mapping is then used when (recursively) generating a
                    // pattern name.
                    //
                    // This approach could probably be done more efficiently, so if
                    // you're reading this and happen to know of a way, please
                    // submit a merge request :)
                    for (index, step) in missing_patterns.iter().enumerate() {
                        mapping.insert(&step.variable, index);
                    }

                    let name = missing_patterns
                        .first()
                        .map(|missing_pattern| {
                            missing_pattern.pattern_name(
                                missing_patterns,
                                &mapping,
                                &compiler.strings,
                            )
                        })
                        .unwrap_or_else(|| "_".to_string());

                    missing.insert(name);
                }
                Decision::Guard(_, _, fallback) => {
                    self.add_missing_patterns(&*fallback, missing_patterns, missing, compiler);
                }
                Decision::Switch(var, cases, fallback) => {
                    for case in cases {
                        match &case.condition {
                            // Constructor::True => {
                            //     let name = "true".to_string();

                            //     missing_patterns.push(MissingPattern::new(*var, name, Vec::new()));
                            // }
                            // Constructor::False => {
                            //     let name = "false".to_string();

                            //     missing_patterns.push(MissingPattern::new(*var, name, Vec::new()));
                            // }
                            // Constructor::Int(_) | Constructor::Range(_, _) => {
                            //     let name = "_".to_string();

                            //     missing_patterns.push(MissingPattern::new(*var, name, Vec::new()));
                            // }
                            // Constructor::Pair(_, _) => {
                            //     let args = case.arguments.clone();

                            //     missing_patterns.push(MissingPattern::new(
                            //         *var,
                            //         String::new(),
                            //         args,
                            //     ));
                            // }
                            DecisionCaseCondition::Constructor(name, _idx) => {
                                let args = case.arguments.clone();
                                missing_patterns.push(MissingPattern::new(*var, *name, args));
                            }
                        }

                        self.add_missing_patterns(&case.body, missing_patterns, missing, compiler);
                        missing_patterns.pop();
                    }

                    if let Some(node) = fallback {
                        self.add_missing_patterns(&*node, missing_patterns, missing, compiler);
                    }
                }
            }
        }
    }

    /// Information about a single constructor/value (aka missing_pattern) being tested, used
    /// to build a list of names of missing patterns.
    #[derive(Debug)]
    struct MissingPattern {
        variable: PatternVariable,
        name: IdentifierName,
        arguments: Vec<PatternVariable>,
    }

    impl MissingPattern {
        fn new(
            variable: PatternVariable,
            name: IdentifierName,
            arguments: Vec<PatternVariable>,
        ) -> Self {
            Self {
                variable,
                name,
                arguments,
            }
        }

        fn pattern_name(
            &self,
            terms: &[MissingPattern],
            mapping: &FnvHashMap<&PatternVariable, usize>,
            strings: &Strings,
        ) -> String {
            let name = strings.resolve(self.name);
            if self.arguments.is_empty() {
                name.to_owned()
            } else {
                let args = self
                    .arguments
                    .iter()
                    .map(|arg| {
                        mapping
                            .get(arg)
                            .map(|&idx| terms[idx].pattern_name(terms, mapping, strings))
                            .unwrap_or_else(|| "_".to_string())
                    })
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("{}({})", name, args)
            }
        }
    }

    #[derive(Debug)]
    pub enum Decision {
        Success(Body),
        Failure,
        Guard(expression::Index, Body, Box<Decision>),
        Switch(
            PatternVariable,
            Vec<DecisionSwitchCase>,
            Option<Box<Decision>>,
        ),
    }

    #[derive(Debug)]
    pub struct DecisionSwitchCase {
        condition: DecisionCaseCondition,
        arguments: Vec<PatternVariable>,
        body: Decision,
    }

    impl DecisionSwitchCase {
        fn new(
            condition: DecisionCaseCondition,
            arguments: Vec<PatternVariable>,
            body: Decision,
        ) -> Self {
            Self {
                condition,
                arguments,
                body,
            }
        }
    }

    #[derive(Debug)]
    pub enum DecisionCaseCondition {
        Constructor(IdentifierName, usize),
    }

    #[derive(Debug)]
    pub struct Diagnostics {
        incomplete_pattern_match: bool,
        reachable_branches: Vec<expression::Index>,
    }

    #[derive(PartialEq, Hash, Eq, Debug, Copy, Clone)]
    pub struct PatternVariable {
        id: usize,
        type_id: typ::Index,
    }

    #[derive(Clone, Debug)]
    pub struct Body {
        bindings: Vec<(IdentifierName, PatternVariable)>,
        expression: expression::Index,
    }

    #[derive(Clone)]
    pub struct Row {
        columns: Vec<Column>,
        guard: Option<expression::Index>,
        body: Body,
    }

    impl Row {
        fn new(columns: Vec<Column>, guard: Option<expression::Index>, body: Body) -> Self {
            Self {
                columns,
                guard,
                body,
            }
        }

        fn remove_column(&mut self, variable: &PatternVariable) -> Option<Column> {
            self.columns
                .iter()
                .position(|c| &c.variable == variable)
                .map(|idx| self.columns.remove(idx))
        }
    }

    #[derive(Clone)]
    pub struct Column {
        variable: PatternVariable,
        pattern: Pattern,
    }

    impl Column {
        fn new(variable: PatternVariable, pattern: Pattern) -> Self {
            Self { variable, pattern }
        }
    }

    pub struct Compiler<'a> {
        variable_id: usize,
        types: &'a Types,
        compiler_state: &'a compiler::State,
        diagnostics: Diagnostics,
    }

    impl<'a> Compiler<'a> {
        pub fn new(types: &'a Types, compiler_state: &'a compiler::State) -> Self {
            Self {
                variable_id: 0,
                types,
                compiler_state,
                diagnostics: Diagnostics {
                    incomplete_pattern_match: false,
                    reachable_branches: Vec::new(),
                },
            }
        }

        pub fn compile(
            mut self,
            types: Vec<typ::Index>,
            branches: Vec<(Vec<Pattern>, Body)>,
        ) -> CompiledPatternMatch {
            let mut rows = Vec::new();
            for (patterns, body) in branches {
                let mut columns = vec![];
                for (typ, pattern) in types.iter().zip(patterns) {
                    let var = self.new_variable(*typ);
                    columns.push(Column::new(var, pattern));
                }
                rows.push(Row::new(columns, None, body));
            }

            CompiledPatternMatch {
                tree: self.compile_rows(rows),
                diagnostics: self.diagnostics,
            }
        }

        fn compile_rows(&mut self, rows: Vec<Row>) -> Decision {
            if rows.is_empty() {
                self.diagnostics.incomplete_pattern_match = true;

                return Decision::Failure;
            }

            let mut rows = rows
                .into_iter()
                .map(|row| self.move_variable_patterns(row))
                .collect::<Vec<_>>();

            // There may be multiple rows, but if the first one has no patterns
            // those extra rows are redundant, as a row without columns/patterns
            // always matches.
            if rows.first().map_or(false, |c| c.columns.is_empty()) {
                let row = rows.remove(0);

                self.diagnostics
                    .reachable_branches
                    .push(row.body.expression);

                return self.get_success_or_guard(row, rows);
            }

            let branch_var = self.branch_variable(&rows[0], &rows);

            match self.variable_type(branch_var) {
                Type::Named { module, name, .. } => {
                    let cases = self
                        .compiler_state
                        .module_interfaces
                        .get(
                            *self
                                .compiler_state
                                .module_name_to_module_idx
                                .get(module)
                                .unwrap(),
                        )
                        .unwrap()
                        .as_ref()
                        .unwrap()
                        .type_constructors
                        .get(name)
                        .unwrap()
                        .map()
                        .iter()
                        .enumerate()
                        .map(|(_idx, (name, poly_type))| {
                            (
                                *name,
                                match &self.types[poly_type.typ] {
                                    Type::Fn { params, .. } => Some(self.new_variables(params)),
                                    Type::Named { .. } => None,
                                    _ => panic!("Expected a constructor type to be a function"),
                                },
                                Vec::new(),
                            )
                        })
                        .collect();

                    Decision::Switch(
                        branch_var,
                        self.compile_constructor_cases(rows, branch_var, cases),
                        None,
                    )
                }
                Type::Var(_) => todo!(),
                // Can get here with named patterns and identifier patterns
                Type::Fn { params: _, ret: _ } => todo!(),
                Type::Record { fields: _ } => todo!(),
                Type::RecordExt {
                    fields: _,
                    base_record: _,
                } => todo!(),
                Type::Alias {
                    module: _,
                    name: _,
                    params: _,
                    destination: _,
                } => todo!(),
            }
        }

        /*
        /// Compiles the cases and fallback cases for integer and range patterns.
        ///
        /// Integers have an infinite number of constructors, so we specialise the
        /// compilation of integer and range patterns.
        fn compile_int_cases(
            &mut self,
            rows: Vec<Row>,
            branch_var: PatternVariable,
        ) -> (Vec<DecisionSwitchCase>, Box<Decision>) {
            let mut raw_cases: Vec<(Constructor, Vec<PatternVariable>, Vec<Row>)> = Vec::new();
            let mut fallback_rows = Vec::new();
            let mut tested: FnvHashMap<(i64, i64), usize> = FnvHashMap::default();

            for mut row in rows {
                if let Some(col) = row.remove_column(&branch_var) {
                    for (pat, row) in flatten_or(col.pattern, row) {
                        let (key, cons) = match pat {
                            Pattern::Int(val) => ((val, val), Constructor::Int(val)),
                            Pattern::Range(start, stop) => {
                                ((start, stop), Constructor::Range(start, stop))
                            }
                            _ => unreachable!(),
                        };

                        if let Some(index) = tested.get(&key) {
                            raw_cases[*index].2.push(row);
                            continue;
                        }

                        tested.insert(key, raw_cases.len());
                        raw_cases.push((cons, Vec::new(), vec![row]));
                    }
                } else {
                    fallback_rows.push(row);
                }
            }

            for (_, _, rows) in &mut raw_cases {
                rows.append(&mut fallback_rows.clone());
            }

            let cases = raw_cases
                .into_iter()
                .map(|(cons, vars, rows)| {
                    DecisionSwitchCase::new(cons, vars, self.compile_rows(rows))
                })
                .collect();

            (cases, Box::new(self.compile_rows(fallback_rows)))
        }
        */

        /// Compiles the cases and sub cases for the constructor located at the
        /// column of the branching variable.
        ///
        /// What exactly this method does may be a bit hard to understand from the
        /// code, as there's simply quite a bit going on. Roughly speaking, it does
        /// the following:
        ///
        /// 1. It takes the column we're branching on (based on the branching
        ///    variable) and removes it from every row.
        /// 2. We add additional columns to this row, if the constructor takes any
        ///    arguments (which we'll handle in a nested match).
        /// 3. We turn the resulting list of rows into a list of cases, then compile
        ///    those into decision (sub) trees.
        ///
        /// If a row didn't include the branching variable, we simply copy that row
        /// into the list of rows for every constructor to test.
        ///
        /// For this to work, the `cases` variable must be prepared such that it has
        /// a triple for every constructor we need to handle. For an ADT with 10
        /// constructors, that means 10 triples. This is needed so this method can
        /// assign the correct sub matches to these constructors.
        ///
        /// Types with infinite constructors (e.g. int and string) are handled
        /// separately; they don't need most of this work anyway.
        fn compile_constructor_cases(
            &mut self,
            rows: Vec<Row>,
            branch_var: PatternVariable,
            mut cases: Vec<(IdentifierName, Option<Vec<PatternVariable>>, Vec<Row>)>,
        ) -> Vec<DecisionSwitchCase> {
            for mut row in rows {
                if let Some(col) = row.remove_column(&branch_var) {
                    for (pat, row) in flatten_or(col.pattern, row) {
                        if let PatternData::Type {
                            constructor,
                            params,
                            ..
                        } = &pat.data
                        {
                            let idx = cases
                                .iter()
                                .position(|(c, _, _)| *c == constructor.name)
                                .unwrap();
                            let mut cols = row.columns;

                            if let Some(type_params) = &cases[idx].1 {
                                for (var, pat) in type_params.iter().zip(params.iter()) {
                                    cols.push(Column::new(*var, pat.clone()));
                                }
                            }

                            cases[idx].2.push(Row::new(cols, row.guard, row.body));
                        }
                    }
                } else {
                    for (_, _, rows) in &mut cases {
                        rows.push(row.clone());
                    }
                }
            }

            cases
                .into_iter()
                .enumerate()
                .map(|(idx, (constructor, vars, rows))| {
                    DecisionSwitchCase::new(
                        DecisionCaseCondition::Constructor(constructor, idx),
                        vars.unwrap_or(Vec::new()),
                        self.compile_rows(rows),
                    )
                })
                .collect()
        }

        fn get_success_or_guard(&mut self, row: Row, rows: Vec<Row>) -> Decision {
            return if let Some(guard) = row.guard {
                Decision::Guard(guard, row.body, Box::new(self.compile_rows(rows)))
            } else {
                Decision::Success(row.body)
            };
        }

        /// Moves variable-only patterns/tests into the right-hand side/body of a
        /// case.
        ///
        /// This turns cases like this:
        ///
        ///     case foo -> print(foo)
        ///
        /// Into this:
        ///
        ///     case -> {
        ///       let foo = it
        ///       print(foo)
        ///     }
        ///
        /// Where `it` is a variable holding the value `case foo` is compared
        /// against, and the case/row has no patterns (i.e. always matches).
        fn move_variable_patterns(&self, row: Row) -> Row {
            let mut bindings = row.body.bindings;

            for col in &row.columns {
                if let PatternData::Named { name, .. } | PatternData::Identifier(name) =
                    &col.pattern.data
                {
                    bindings.push((name.name, col.variable));
                }
            }

            let columns = row
                .columns
                .into_iter()
                .filter(|col| !matches!(col.pattern.data, PatternData::Identifier(_)))
                .collect();

            Row {
                columns,
                guard: row.guard,
                body: Body {
                    bindings,
                    expression: row.body.expression,
                },
            }
        }

        fn branch_variable(&self, row: &Row, rows: &[Row]) -> PatternVariable {
            let mut counts = FnvHashMap::default();

            for row in rows {
                for col in &row.columns {
                    *counts.entry(&col.variable).or_insert(0_usize) += 1
                }
            }

            row.columns
                .iter()
                .map(|col| col.variable)
                .max_by_key(|var| counts[var])
                .unwrap()
        }

        fn new_variable(&mut self, type_id: typ::Index) -> PatternVariable {
            let var = PatternVariable {
                id: self.variable_id,
                type_id,
            };

            self.variable_id += 1;
            var
        }

        fn new_variables(&mut self, type_ids: &[typ::Index]) -> Vec<PatternVariable> {
            type_ids.iter().map(|t| self.new_variable(*t)).collect()
        }

        fn variable_type(&self, id: PatternVariable) -> &Type {
            &self.types[id.type_id]
        }
    }

    fn flatten_or(pattern: Pattern, row: Row) -> Vec<(Pattern, Row)> {
        if let PatternData::Or(args) = pattern.data {
            args.into_iter().map(|p| (p, row.clone())).collect()
        } else {
            vec![(pattern, row)]
        }
    }
}
