use crate::ast::{
    binop::*,
    types::{self, Type, TypeDefinition},
    CapitalizedIdentifier, Expression,
    ExpressionType::{self as ET, Float, If, Let, String_, *},
    Expression_ as E, Identifier, Import, Lambda, Module, Node, Pattern, Pattern_,
    Unary_::{Minus, Not},
    *,
};
use crate::module;
use crate::source::Source;
use crate::strings::Strings;
use crate::token::{
    self, Token,
    Type::{self as TT, *},
};

/* Grammar draft (●○):
    ● file                 → module EOF
    ● module               → "module" module_name exposing? imports? module_definitions?
    ● module_name          → CAPITALIZED_IDENTIFIER ( "." CAPITALIZED_IDENTIFIER )*
    ● exposing             → "exposing" "(" export ( "," export )* ")"
    ● export               → IDENTIFIER
                           | CAPITALIZED_IDENTIFIER ( "(" CAPITALIZED_IDENTIFIER ( "," CAPITALIZED_IDENTIFIER )* ")" )?
    ● imports              → ( import )*
    ● import               → "import" IDENTIFIER ( "as" IDENTIFIER )? exposing?

    ● module_definitions   → ( type_def | module | typed_binding )+

    // Type definitions
    ● type_identifier      → CAPITALIZED_IDENTIFIER
    ● type_var             → IDENTIFIER
    ● type_def             → "type" type_identifier ( type_var )* "=" type_record | type_union_branches
    ● type_union_branches  → type_constructor ( "|" type_constructor )*
    ● type_constructor     → type_identifier ( type_param )*
    ● type_param           → type_parens | type_identifier | type_var | type_record
    ● type_parens          → "(" type_function ")"
    ● type_record          → "{" ( type_var "|" )? ( type_record_field ( "," type_record_field )* )? "}"
    ● type_record_field    → IDENTIFIER ":" type_function
    ● type_function        → type ( ( "->" type )* "->" type )?
    ● type                 → type_constructor | type_var | type_record | type_parens

    ● type_signature       → identifier ":" type_function

    // Expressions
    ● let                  → "let" ( typed_binding )+ ( "in" )? expression
    ● typed_binding        → type_signature binding
                           | binding
    ● binding              → ( identifier ( params )? | pattern ) "=" expression
    ● lambda               → "\" params "->" expression
    ● pattern              → parsed from Ast.Pattern
    ● if                   → "if" binary "then" expression "else" expression
    // Operators
    ● binary               → unary ( binop unary )*
    ● binop                → // parsed from Ast.Binop operator list
    ● unary                → ( "not" | "-" )? call
    // Other primitives
    ● call                 → prop_access ( prop_access )*
    ● prop_access          → primary properties
    ● properties           → ( "." ( IDENTIFIER ) )*
    ● primary              → NUMBER | STRING | "false" | "true"
                           | ( module_name "." )? ( IDENTIFIER | CAPITALIZED_IDENTIFIER )
                           | "." IDENTIFIER
                           | record
                           | "(" ( expression )? ")"

    // Records
    ● record               → "{" ( expression "|" )? ( field ( "," field )* )? "}"
    ● field                → IDENTIFIER ":" expression
*/

#[derive(Debug)]
enum ErrorType {
    // Top level
    InvalidReplEntry,
    InvalidEndOfInput,

    // Module definition
    MissingModuleName,
    InvalidModuleNameSegment,
    MissingTopLevelModule,
    ModuleAndFileNameMismatch(ModuleName),
    SubmoduleAndParentModuleNameMismatch(ModuleName),
    InvalidModuleDefinitionLhs,
    InvalidModuleDefinition,

    // Exports
    NotEnoughModuleExports,
    InvalidModuleExportsDelimiter,
    InvalidModuleExportsSeparatorOrLastDelimiter,
    InvalidModuleExport,
    InvalidModuleExportConstructor,
    MissingModuleExportConstructors,
    InvalidModuleExportConstructorsSeparatorOrLastDelimiter,

    // Imports
    InvalidImportModuleName,
    InvalidImportModuleAlias,

    // Types
    InvalidTypeDefinitionName,
    InvalidTypeDefinitionTypeVarsOrEqualSeparator,
    InvalidUnionTypeDefinitionConstructor,
    MissingUnionTypeDefinitionConstructors,
    InvalidRecordTypeFieldTypeOrLastRecordDelimiter(/* First delimiter of the record */ Token),
    InvalidRecordTypeFieldKeyOrExtensibleRecordVariable,
    InvalidRecordTypeFieldSeparatorOrExtensibleRecordSeparator,
    MissingRecordTypeFields,
    InvalidRecordTypeFieldKey,
    InvalidRecordTypeFieldType,
    InvalidRecordTypeFieldSeparator,
    InvalidParenthesizedTypeDelimiter(/* First delimiter */ Token),
    InvalidFunctionParameterType,
    InvalidParenthesizedTypeType,
    InvalidTypeSignatureType,

    // Expressions
    MissingLetBindings,
    InvalidLetBodyIndent,
    InvalidLetBodyExpression,
    InvalidLetBindingParametersOrEqualSeparator(Identifier_),
    InvalidLetBindingSeparator,
    InvalidLetBindingRhs,
    InvalidIfCondition,
    InvalidIfThen,
    InvalidThenBranch,
    InvalidIfElse,
    InvalidIfElseBranch,
    MissingLambdaParamaters,
    InvalidLambdaArrow,
    InvalidLambdaBody,
    InvalidBinopRhs(/* Binop token */ Token),
    InvalidUnaryRhs(/* Unary op token */ Token),
    InvalidPropertyAccessSeparator,
    InvalidPropertyAccessIdentifier,
    InvalidFloat(Token),
    InvalidRecordFieldSeparatorOrLastDelimiter,
    InvalidRecordFieldsOrExtensibleRecordExpression,
    InvalidExtensibleRecordFieldSeparatorOrLastDelimiter,
    InvalidParenthesizedExpression,
    InvalidParenthesizedExpressionLastDelimiter(/* First delimiter */ Token),
    InvalidPropertyAccessLambdaWhitespace,
    InvalidPropertyAccessLambdaIdentifier,
    MissingRecordFields,
    InvalidRecordFieldKey,
    InvalidRecordFieldKeyValueSeparator,
    InvalidRecordFieldValue,
}

use ErrorType::*;

impl ErrorType {
    pub fn get_code_pointer(&self) -> Option<Token> {
        match self {
            InvalidRecordTypeFieldTypeOrLastRecordDelimiter(first_brace) => Some(*first_brace),
            InvalidParenthesizedTypeDelimiter(first_paren) => Some(*first_paren),
            _ => None,
        }
    }

    pub fn to_string<'tokens, 'strings>(
        &self,
        error: &Error,
        strings: &'strings Strings,
    ) -> String {
        let get_lexeme = |token: &Token| token.kind.to_string(strings);
        let expected_but_found = |message: &str| {
            let lexeme = get_lexeme(&error.token);
            format!("{message}, but instead found: `{lexeme}`")
        };

        match self {
            ModuleAndFileNameMismatch(name) => format!(
                "The module name '{name}' differs from the name of the file.\n\n\
                Module names need to match the folder and file names from the \
                file system",
                name = &name.to_string(strings)
            ),

            SubmoduleAndParentModuleNameMismatch(name) => format!(
                "The sub-module name '{name}' differs from the name of the parent \
                module.\n\nModule names need to match their parent module path \
                names and specify their name at the end.\n\nLike this:\n\
                \n    module Admin\
                \n        module Admin.User\
                \n            module Admin.User.Id\
                \n\n",
                name = &name.to_string(strings)
            ),

            MissingTopLevelModule => {
                expected_but_found("Expected `module FileName` at the start of the file")
            }

            InvalidReplEntry => expected_but_found(
                "Expected an import, a top level definition, \
                or an expression",
            ),

            InvalidEndOfInput => expected_but_found("Expected the end of input"),

            MissingModuleName => expected_but_found("Expected the module name"),

            InvalidModuleNameSegment => format!(
                "Invalid module name '{lexeme}'.\n\n\
                Module names have to be `PascalCase` \
                and also not have extraneous characters, \
                because they need to match the file name \
                in the file system.",
                lexeme = get_lexeme(&error.token)
            ),

            NotEnoughModuleExports => expected_but_found(
                "Parsing the module exports expected at least \
                one definition or type to export",
            ),

            InvalidModuleExportsSeparatorOrLastDelimiter => expected_but_found(
                "Parsing the module exports expected a comma \
                separated list of exports inside parenthesis",
            ),

            InvalidModuleExportsDelimiter => expected_but_found(
                "Parsing the module exports expected a comma \
                separated list of exports inside parenthesis",
            ),

            InvalidModuleExport => {
                expected_but_found("Expected a function or type name from the module")
            }

            InvalidModuleExportConstructor => {
                expected_but_found("Expected a `PascalCase` name for a constructor")
            }

            InvalidModuleExportConstructorsSeparatorOrLastDelimiter => expected_but_found(
                "Parsing the type constructors expected a comma \
                    separated list of constructor names inside parenthesis",
            ),

            MissingModuleExportConstructors => expected_but_found(
                "Parsing the type constructors expected at least \
                one inside the parens",
            ),

            InvalidImportModuleName => {
                expected_but_found("Expected an identifier of the module to import")
            }

            InvalidImportModuleAlias => {
                expected_but_found("Expected an identifier for the alias of the module")
            }

            InvalidTypeDefinitionName => {
                expected_but_found("Expected a `PascalCase` name for the type")
            }

            InvalidTypeDefinitionTypeVarsOrEqualSeparator => expected_but_found(
                "Expected type variable names like `a` or a `=` sign \
                between the name and the type definition",
            ),

            InvalidUnionTypeDefinitionConstructor => expected_but_found(
                "Expected a constructor for the type like `type User = User Int`",
            ),

            MissingUnionTypeDefinitionConstructors => expected_but_found(
                "Expected at least one constructor \
                for the type like `type User = User Int`",
            ),

            InvalidRecordTypeFieldTypeOrLastRecordDelimiter(_first_delimiter) => {
                expected_but_found(
                    // TODO: This error message can be better and also be triggered when there is
                    // an issue with the , separator while adding more record fields
                    "Expected `}` to close a record literal",
                )
            }

            InvalidRecordTypeFieldKeyOrExtensibleRecordVariable => expected_but_found(
                "Expected a record literal `{ x : Int, y : Int }`\
                    or an extensible record `{ a | x : Int, y : Int }`",
            ),

            InvalidRecordTypeFieldSeparatorOrExtensibleRecordSeparator => {
                // TODO: This error message can trigger also when there is an issue with the
                // : separator between fields, so it isn't only about extensible records. Should be
                // made a bit more expressive.
                expected_but_found(
                    "Expected `|` between the type variable and \
                    the fields of the record (like this \
                    `{ a | field : Type }`)",
                )
            }

            MissingRecordTypeFields => expected_but_found(
                // TODO: This error message should talk about having at least one field with value,
                // it is too generic and can be improved
                "Expected a record field (like this `{ field : Type }`)",
            ),

            InvalidRecordTypeFieldKey => expected_but_found(
                "Expected a camelCase identifier for \
                the field name in the record",
            ),

            InvalidRecordTypeFieldType => {
                expected_but_found("Expected a type for the field in the record")
            }

            InvalidRecordTypeFieldSeparator => expected_but_found(
                "Expected a camelCase identifier for \
                the field name in the record",
            ),

            InvalidParenthesizedTypeDelimiter(_first_paren) => expected_but_found(
                // TODO: This error message can be better and show the whole parenthesized
                // expression
                "Expected `)` after parenthesized type",
            ),

            InvalidFunctionParameterType =>
            // Improve this message, very generic
            {
                expected_but_found("Expected a type")
            }

            InvalidParenthesizedTypeType => {
                expected_but_found("Expected a type inside the parenthesis")
            }

            InvalidTypeSignatureType => {
                expected_but_found("Expected a type for the type signature")
            }

            InvalidModuleDefinitionLhs => expected_but_found(
                "Expected the left side of a definition like `n = 5` \
                or `add x y = x + y` or a type definition like \
                `type User = LoggedIn | Anon`",
            ),

            InvalidModuleDefinition => expected_but_found(
                "Expected a definition like `n = 5` \
                or `add x y = x + y` or a type definition like \
                `type User = LoggedIn | Anon`",
            ),

            MissingLetBindings => expected_but_found(
                // TODO: This error message can be improved, may show up when there were no
                // bindings in the same line or indented from the let, which means they could be
                // badly indentend below. At the very least it can be made more useful
                "Expected a pattern for the left side of the let expression",
            ),

            InvalidLetBodyIndent => expected_but_found(
                "Expected the let definition to be followed by another \
                expression in the next line and same indentation",
            ),

            InvalidLetBodyExpression => {
                expected_but_found("Expected an expression for the body of the let bindings")
            }

            InvalidLetBindingParametersOrEqualSeparator(name) => expected_but_found(&format!(
                "Expected an `=` sign or list of parameters for the definition of `{name}`",
                name = name.to_string(strings)
            )),

            InvalidLetBindingSeparator => expected_but_found(
                "Expected an = and an expression \
                for the right side of the definition",
            ),

            InvalidLetBindingRhs => {
                expected_but_found("Expected an expression for the right side of the definition")
            }

            InvalidIfCondition => expected_but_found(
                "Expected an expression for the condition \
                in the if expression (eg: if True then 1 else 2)",
            ),

            InvalidIfThen => expected_but_found(
                "Expected the keyword `then` and \
                an expression to parse the if expression",
            ),

            InvalidThenBranch => expected_but_found(
                "Expected an expression for the first\
                branch of the if (eg: if True then \"Hi\" else \"Ho\")",
            ),

            // TODO: Improve error message
            InvalidIfElse => expected_but_found("Expected the `else` branch of the if expression"),

            InvalidIfElseBranch => expected_but_found(
                "Expected an expression for the else\
                branch of the if (eg: if True then \"Hi\" else \"Ho\")",
            ),

            MissingLambdaParamaters => expected_but_found("Expected a list of parameters"),

            InvalidLambdaArrow => expected_but_found(
                "Expected a `->` arrow after the list of parameters for the function",
            ),

            InvalidLambdaBody => {
                expected_but_found("Expected an expression for the body of the function")
            }

            InvalidBinopRhs(op) => expected_but_found(&format!(
                "Expected an expression after the binary operator `{lexeme}`",
                lexeme = get_lexeme(op)
            )),

            InvalidUnaryRhs(op) => expected_but_found(&format!(
                "Expected an expression after the unary operator `{lexeme}`",
                lexeme = get_lexeme(op)
            )),

            InvalidPropertyAccessSeparator => "A property access must have the dot \
                and identifier together, \
                like this `a.b.c`"
                .to_owned(),

            InvalidPropertyAccessIdentifier => "Expected an identifier after a \
                dot for the property access"
                .to_owned(),

            InvalidFloat(token) => format!(
                "Failed to parse number token `{lexeme}`",
                lexeme = get_lexeme(token)
            ),

            InvalidRecordFieldSeparatorOrLastDelimiter => {
                expected_but_found("Expected `}` to close a record literal")
            }

            InvalidRecordFieldsOrExtensibleRecordExpression => expected_but_found(
                "Expected a record literal `{ x = 1, y = 2 }`\
                or a record update `{ record | x = 5, y = 4 }`",
            ),

            InvalidExtensibleRecordFieldSeparatorOrLastDelimiter =>
            // TODO: This error message can be better
            {
                expected_but_found("Expected `}` to close a record update expression")
            }

            InvalidParenthesizedExpression => {
                expected_but_found("Expected an expression inside the parenthesis")
            }

            InvalidParenthesizedExpressionLastDelimiter(_first_paren) =>
            // TODO: This error message can be better
            {
                expected_but_found("Expected `)` after parenthesized expression")
            }

            InvalidPropertyAccessLambdaWhitespace => "A property access must have the dot \
                and identifier together, like this `.name`"
                .to_owned(),

            InvalidPropertyAccessLambdaIdentifier => "Expected an identifier after a \
                dot for the property access"
                .to_owned(),

            MissingRecordFields => {
                expected_but_found("Expected a record field (like this `{ field = 5 }`)")
            }

            InvalidRecordFieldKey => {
                expected_but_found("Expected an identifier for the name of the field in the record")
            }

            InvalidRecordFieldKeyValueSeparator => expected_but_found(
                "Expected a `:` separating the name of \
                the field and the value in the record",
            ),

            InvalidRecordFieldValue => expected_but_found(
                "Expected an expression for the value \
                of the field in the record",
            ),
        }
    }
}

#[derive(Debug)]
pub struct Error {
    kind: ErrorType,
    pub token: Token,
}

impl Error {
    fn new(token: Token, kind: ErrorType) -> Self {
        Self { token, kind }
    }

    pub fn to_string<'source, 'strings, 'tokens>(
        &self,
        source: &'source Source,
        strings: &'strings Strings,
    ) -> String {
        let message = {
            let message = self.kind.to_string(self, strings);
            let point_at_token = self.kind.get_code_pointer().unwrap_or(self.token);

            let lines_report = source
                .lines_report_at_position_with_pointer(
                    point_at_token.start,
                    Some(point_at_token.end),
                    point_at_token.line,
                )
                .unwrap();
            format!("{message}\n\n{lines_report}")
        };
        let source_name = source.name();
        let line = self.token.line;
        let column = self.token.column;
        format!("{source_name}:{line}:{column}\n\n{message}")
    }
}

type ParseResult<A> = Result<A, Error>;

#[derive(Debug)]
struct State<'source, 'strings, 'tokens> {
    strings: &'strings mut Strings,
    source: &'source Source,
    tokens: &'tokens [Token],
    current: usize,
}

impl<'source, 'strings, 'tokens> State<'source, 'strings, 'tokens> {
    fn file(&mut self) -> ParseResult<(Module, Vec<Module>)> {
        let module = self.module(None)?;
        match module {
            Some(mut modules) => self.eof((modules.pop().unwrap(), modules)),
            None => Err(self.error(MissingTopLevelModule)),
        }
    }

    fn repl_entry(&mut self) -> ParseResult<ReplEntry> {
        let result = match self.import()? {
            Some(import) => ReplEntry::Import(import),
            None => match self.binding()? {
                Some(definition) => ReplEntry::Definition(definition),
                None => ReplEntry::Expression(
                    self.required(Self::expression, |self_| self_.error(InvalidReplEntry))?,
                ),
            },
        };

        self.eof(result)
    }

    pub fn eof<T>(&mut self, result: T) -> ParseResult<T> {
        let (_, eof_token) = self.get_token();
        match eof_token.kind {
            TT::Eof => Ok(result),
            _ => Err(self.error(InvalidEndOfInput)),
        }
    }

    fn module(&mut self, parent_module: Option<&ModuleName>) -> ParseResult<Option<Vec<Module>>> {
        let top_level = matches!(parent_module, None);

        if let Some((_, module_token)) = self.match_token(TT::Module) {
            let name = self.module_name(|| MissingModuleName)?;

            if top_level && !name.valid_top_level_in_file(self.source, self.strings) {
                return Err(Error::new(
                    *module_token,
                    ErrorType::ModuleAndFileNameMismatch(name),
                ));
            } else if parent_module
                .map(|m| !name.valid_in_parent_module(m))
                .unwrap_or(false)
            {
                return Err(Error::new(
                    *module_token,
                    ErrorType::SubmoduleAndParentModuleNameMismatch(name),
                ));
            }

            let exports = self.exposing()?;

            let imports = self.many(Self::import)?;

            let (mut modules, definitions, type_definitions) =
                self.module_definitions(top_level, &name, module_token, vec![], vec![], vec![])?;

            modules.push(Module {
                name,
                exports,
                imports,
                definitions,
                type_definitions,
            });

            Ok(Some(modules))
        } else {
            Ok(None)
        }
    }

    fn exposing(&mut self) -> ParseResult<Vec<Export>> {
        if self.match_token(TT::Exposing).is_none() {
            return Ok(vec![]);
        }

        let exports = self.one_or_many_delimited(
            TT::Comma,
            (TT::LeftParen, TT::RightParen),
            Self::export,
            |self_| self_.error(NotEnoughModuleExports),
            |self_| self_.error(InvalidModuleExportsSeparatorOrLastDelimiter),
        )?;

        if let Some(exports) = exports {
            Ok(exports)
        } else {
            Err(self.error(InvalidModuleExportsDelimiter))
        }
    }

    fn export(&mut self) -> ParseResult<Export> {
        if let Some(export) = self.identifier() {
            // Make intermediate node to please borrow checker
            let node = &export.with_value(());
            Ok(node.with_value(Export_::Identifier(export)))
        } else if let Some(export) = self.export_type()? {
            Ok(export)
        } else {
            Err(self.error(InvalidModuleExport))
        }
    }
    fn export_type(&mut self) -> ParseResult<Option<Export>> {
        if let Some(export) = self.capitalized_identifier() {
            let constructors = self.one_or_many_delimited(
                TT::Comma,
                (TT::LeftParen, TT::RightParen),
                |self_| {
                    self_.required(
                        |self_| Ok(self_.capitalized_identifier()),
                        |self_| self_.error(InvalidModuleExportConstructor),
                    )
                },
                |self_| self_.error(MissingModuleExportConstructors),
                |self_| self_.error(InvalidModuleExportConstructorsSeparatorOrLastDelimiter),
            )?;

            let constructors = constructors.unwrap_or_else(|| vec![]);

            let start = export.start;
            Ok(Some(Node {
                value: Export_::Type(export, constructors),
                start,
                end: self.prev_token_index(),
            }))
        } else {
            Ok(None)
        }
    }

    fn import(&mut self) -> ParseResult<Option<Import>> {
        if let Some((import_token_index, _)) = self.match_token(TT::Import) {
            let module_name = self.module_name(|| InvalidImportModuleName)?;

            let alias = if self.match_token(TT::As).is_some() {
                Some(self.required(
                    |self_| Ok(self_.capitalized_identifier()),
                    |self_| self_.error(InvalidImportModuleAlias),
                )?)
            } else {
                None
            };

            let exposing = self.exposing()?;

            let end = if !exposing.is_empty() {
                self.prev_token_index()
            } else if let Some(alias) = &alias {
                alias.end
            } else {
                module_name.end()
            };

            Ok(Some(Node {
                value: Import_ {
                    module_name,
                    alias,
                    exposing,
                },
                start: import_token_index,
                end,
            }))
        } else {
            Ok(None)
        }
    }

    fn type_def(&mut self) -> ParseResult<Option<TypeDefinition>> {
        let (type_token_index, _) = self.get_token();

        if self.match_token(TT::Type).is_none() {
            return Ok(None);
        }

        let name = self.required(
            |self_| Ok(self_.capitalized_identifier()),
            |self_| self_.error(InvalidTypeDefinitionName),
        )?;

        let vars = self.many(|self_| match self_.identifier() {
            Some(variable) => Ok(Some(variable)),
            None => {
                let (_, token) = self_.get_token();
                if matches!(token.kind, Equal) {
                    self_.advance();
                    Ok(None)
                } else {
                    Err(self_.error(InvalidTypeDefinitionTypeVarsOrEqualSeparator))
                }
            }
        })?;

        let type_definition = if let Some(record) = self.type_record()? {
            types::TypeDefinitionType::Record(record)
        } else {
            let branches = self.type_union_branches()?;
            types::TypeDefinitionType::Union(branches)
        };

        let (end_index, _) = self.prev_token();
        Ok(Some(Node {
            value: types::TypeDefinition_::new(name, vars, type_definition),
            start: type_token_index,
            end: end_index,
        }))
    }

    fn type_union_branches(&mut self) -> ParseResult<Vec<types::Constructor>> {
        // Optionally eat a first pipe for multiline branches
        self.match_token(TT::Pipe);

        let branches = self.one_or_many_sep(
            |self_| Ok(self_.match_token(TT::Pipe)),
            |self_| {
                self_.required(Self::type_constructor, |self_| {
                    self_.error(InvalidUnionTypeDefinitionConstructor)
                })
            },
            |self_| self_.error(MissingUnionTypeDefinitionConstructors),
        )?;

        Ok(branches)
    }

    fn type_constructor(&mut self) -> ParseResult<Option<types::Constructor>> {
        let (_, constructor_token) = self.get_token();
        if let Some(name) = self.capitalized_identifier() {
            let params = self.many(|self_| {
                if !self_.is_token_in_same_line_or_nested_indent_from(constructor_token) {
                    Ok(None)
                } else if let Some(param) = self_.type_param()? {
                    Ok(Some(param))
                } else {
                    Ok(None)
                }
            })?;

            let start = name.start;
            let (end, _) = self.prev_token();
            Ok(Some(Node {
                value: types::Constructor_::new(name, params),
                start,
                end,
            }))
        } else {
            Ok(None)
        }
    }

    fn type_record(&mut self) -> ParseResult<Option<types::RecordType>> {
        let (left_brace_token_index, left_brace_token) = self.get_token();
        if self.match_token(LeftBrace).is_none() {
            return Ok(None);
        }

        let (next_token_index, next_token) = self.get_token();
        let second_next_token = self.peek_next_token();

        match (next_token.kind, second_next_token.kind) {
            // Unit record
            (RightBrace, _) => {
                self.advance();
                Ok(Some(types::RecordType::Record(Node::new(
                    types::Record_::new(vec![]),
                    left_brace_token_index,
                    next_token_index,
                ))))
            }

            // Record literal
            (TT::Identifier(_), Colon) => {
                let fields = self.type_record_fields()?;

                if let Some((last_token_index, _)) = self.match_token(RightBrace) {
                    Ok(Some(types::RecordType::Record(Node::new(
                        types::Record_::new(fields),
                        left_brace_token_index,
                        last_token_index,
                    ))))
                } else {
                    Err(self.error(InvalidRecordTypeFieldTypeOrLastRecordDelimiter(
                        *left_brace_token,
                    )))
                }
            }

            _ => {
                let extension = if let Some(identifier) = self.identifier() {
                    identifier
                } else {
                    return Err(self.error(InvalidRecordTypeFieldKeyOrExtensibleRecordVariable));
                };

                if self.match_token(TT::Pipe).is_none() {
                    return Err(
                        self.error(InvalidRecordTypeFieldSeparatorOrExtensibleRecordSeparator)
                    );
                }

                let fields = self.type_record_fields()?;
                let (last_token_index, _) = self.get_token();

                if self.match_token(RightBrace).is_none() {
                    return Err(self.error(InvalidRecordTypeFieldTypeOrLastRecordDelimiter(
                        *left_brace_token,
                    )));
                }

                Ok(Some(types::RecordType::RecordExt(Node::new(
                    types::RecordExt_::new(extension, fields),
                    left_brace_token_index,
                    last_token_index,
                ))))
            }
        }
    }

    fn type_record_fields(&mut self) -> ParseResult<Vec<(Identifier, Type)>> {
        self.one_or_many_sep(
            |self_| Ok(self_.match_token(TT::Comma)),
            Self::type_record_field,
            |self_| self_.error(MissingRecordTypeFields),
        )
    }

    fn type_record_field(&mut self) -> ParseResult<(Identifier, Type)> {
        let identifier = self.required(
            |self_| Ok(self_.identifier()),
            |self_| self_.error(InvalidRecordTypeFieldKey),
        )?;

        if self.match_token(TT::Colon).is_none() {
            return Err(self.error(InvalidRecordTypeFieldSeparator));
        }

        let typ = self.type_function(|| InvalidRecordTypeFieldType)?;
        Ok((identifier, typ))
    }

    fn type_param(&mut self) -> ParseResult<Option<Type>> {
        if let Some(type_) = self.type_parens()? {
            Ok(Some(type_))
        } else if let Some(ident) = self.capitalized_identifier() {
            let (start, end) = (ident.start, ident.end);
            Ok(Some(Type::App(Node {
                value: types::Constructor_::new(ident, vec![]),
                start,
                end,
            })))
        } else if let Some(ident) = self.identifier() {
            Ok(Some(Type::Var(ident)))
        } else if let Some(record) = self.type_record()? {
            Ok(Some(Type::Record(record)))
        } else {
            Ok(None)
        }
    }

    fn type_parens(&mut self) -> ParseResult<Option<Type>> {
        let (_, token) = self.get_token();
        if self.match_token(LeftParen).is_none() {
            return Ok(None);
        }

        let type_ = self.type_function(|| InvalidParenthesizedTypeType)?;

        if self.match_token(RightParen).is_some() {
            Ok(Some(type_))
        } else {
            Err(self.error(InvalidParenthesizedTypeDelimiter(*token)))
        }
    }

    fn type_function(&mut self, on_type_parse_error: fn() -> ErrorType) -> ParseResult<Type> {
        let mut params = self.one_or_many_sep(
            // Using a comma for the parameters can be an issue because function types can be used
            // in other comma separated elements like record fields, so this type_function would
            // eat the comma that was intended for the record field.
            //     type Test = { a : x, y -> z }
            //     type Test = { a : x, y : z }
            //     type Test = { a : (x, y -> z) }
            // So for now only arrows as parameter separators
            |self_| Ok(self_.match_token(TT::Arrow)),
            |self_| {
                self_.required(Self::type_, |self_| {
                    self_.error(InvalidFunctionParameterType)
                })
            },
            |self_| self_.error(on_type_parse_error()),
        )?;

        if params.len() == 1 {
            Ok(params.swap_remove(0))
        } else {
            let ret = params.pop().unwrap();
            Ok(Type::Fun(params, Box::new(ret)))
        }
    }

    fn type_(&mut self) -> ParseResult<Option<Type>> {
        if let Some(ident) = self.identifier() {
            Ok(Some(Type::Var(ident)))
        } else if let Some(type_) = self.type_constructor()? {
            Ok(Some(Type::App(type_)))
        } else if let Some(record) = self.type_record()? {
            Ok(Some(Type::Record(record)))
        } else if let Some(typ) = self.type_parens()? {
            Ok(Some(typ))
        } else {
            Ok(None)
        }
    }

    fn module_definitions(
        &mut self,
        top_level: bool,
        module_name: &ModuleName,
        module_token: &Token,
        mut modules: Vec<Module>,
        mut definitions: Vec<TypedDefinition>,
        mut type_definitions: Vec<TypeDefinition>,
    ) -> ParseResult<(Vec<Module>, Vec<TypedDefinition>, Vec<TypeDefinition>)> {
        let current_token_has_valid_indent =
            self.current_token_has_valid_indent_for_module_definitions(top_level, module_token);

        if current_token_has_valid_indent {
            if let Some(mut nested_modules) = self.module(Some(module_name))? {
                modules.append(&mut nested_modules);
                self.module_definitions(
                    top_level,
                    module_name,
                    module_token,
                    modules,
                    definitions,
                    type_definitions,
                )
            } else if let Some(definition) = self.typed_binding()? {
                definitions.push(definition);
                self.module_definitions(
                    top_level,
                    module_name,
                    module_token,
                    modules,
                    definitions,
                    type_definitions,
                )
            } else if let Some(type_def) = self.type_def()? {
                type_definitions.push(type_def);
                self.module_definitions(
                    top_level,
                    module_name,
                    module_token,
                    modules,
                    definitions,
                    type_definitions,
                )
            } else if self.match_token(TT::Eof).is_some() {
                Ok((modules, definitions, type_definitions))
            } else {
                Err(self.error(InvalidModuleDefinitionLhs))
            }
        } else if self.match_token(TT::Eof).is_some()
            || self.current_token_outside_indent_for_module_definitions(top_level, module_token)
        {
            Ok((modules, definitions, type_definitions))
        } else {
            Err(self.error(InvalidModuleDefinition))
        }
    }

    fn current_token_has_valid_indent_for_module_definitions(
        &self,
        top_level: bool,
        module_token: &Token,
    ) -> bool {
        if top_level {
            self.is_token_in_same_indent_and_column_as(module_token)
        } else {
            self.is_token_start_of_line_and_after_line_and_nested_indent_from(module_token)
        }
    }

    fn current_token_outside_indent_for_module_definitions(
        &self,
        top_level: bool,
        module_token: &Token,
    ) -> bool {
        if top_level {
            false
        } else {
            self.is_token_equal_or_less_indented_than(module_token)
        }
    }

    pub fn expression(&mut self) -> ParseResult<Option<Expression>> {
        if let Some(let_) = self.let_()? {
            Ok(Some(let_))
        } else if let Some(if_) = self.if_()? {
            Ok(Some(if_))
        } else if let Some(lambda) = self.lambda()? {
            Ok(Some(lambda))
        } else {
            self.binary()
        }
    }

    fn let_(&mut self) -> ParseResult<Option<Expression>> {
        let (let_token_index, let_token) = self.get_token();

        if self.match_token(TT::Let).is_none() {
            return Ok(None);
        }

        let bindings = self.one_or_many(
            |self_| {
                if self_.is_token_in_same_line_or_nested_indent_from(let_token) {
                    self_.typed_binding()
                } else {
                    Ok(None)
                }
            },
            |self_| self_.error(MissingLetBindings),
        )?;

        if self.match_token(TT::In).is_none()
            && !self.is_token_after_line_and_same_indent_as(let_token)
        {
            return Err(self.error(InvalidLetBodyIndent));
        }

        let body = self.required(Self::expression, |self_| {
            self_.error(InvalidLetBodyExpression)
        })?;

        let end = body.end;
        Ok(Some(Node {
            value: E::untyped(Let(bindings, Box::new(body))),
            start: let_token_index,
            end,
        }))
    }

    fn typed_binding(&mut self) -> ParseResult<Option<TypedDefinition>> {
        let (_, name_token) = self.get_token();
        match (name_token.kind, self.peek_next_token().kind) {
            (TT::Identifier(_), TT::Colon) => {
                let name = self.identifier().unwrap();

                // Skip over the colon
                self.advance();

                let typ = self.type_function(|| InvalidTypeSignatureType)?;
                let signature = TypeSignature { name, typ };

                // If the definition is not directly next to the signature, bail out to avoid
                // swallowing an unrelated definition
                let (_, next_name_token) = self.get_token();
                if next_name_token.kind != name_token.kind {
                    return Ok(Some(TypedDefinition::TypeSignature(signature)));
                }

                match self.binding()? {
                    Some(binding) => {
                        let valid_name = match &binding {
                            Definition::Pattern(
                                Node {
                                    value: Pattern_::Identifier(identifier),
                                    ..
                                },
                                _,
                            )
                            | Definition::Lambda(identifier, _)
                                if identifier.value == signature.name.value =>
                            {
                                true
                            }
                            _ => false,
                        };
                        if valid_name {
                            Ok(Some(TypedDefinition::Typed(signature, binding)))
                        } else {
                            panic!("Internal parser error: Should not have gotten into a binding if the name is different from the one in a type definition");
                        }
                    }
                    None => Ok(None),
                }
            }
            _ => Ok(self.binding()?.map(TypedDefinition::Untyped)),
        }
    }

    fn binding(&mut self) -> ParseResult<Option<Definition>> {
        let token_index = self.get_token_index();

        let pattern = self.pattern()?;

        let definition = match pattern {
            Some(Node {
                value: Pattern_::Identifier(identifier),
                ..
            }) => {
                // Peek to see if it is just an identifier and =, and return a pattern
                let (_, equal_token) = self.get_token();
                if equal_token.kind == Equal {
                    let expr = self.binding_rhs()?;
                    Some(Definition::Pattern(
                        Node::new(Pattern_::Identifier(identifier), token_index, token_index),
                        expr,
                    ))
                } else {
                    // Otherwise this is a lambda lhs, identifier + params
                    let params = self.one_or_many(Self::pattern, |self_| {
                        self_.error(InvalidLetBindingParametersOrEqualSeparator(
                            identifier.value.clone(),
                        ))
                    })?;

                    let expr = self.binding_rhs()?;

                    Some(Definition::Lambda(
                        identifier,
                        Lambda {
                            parameters: params,
                            body: Box::new(expr),
                        },
                    ))
                }
            }
            Some(pattern) => Some(Definition::Pattern(pattern, self.binding_rhs()?)),
            None => None,
        };

        Ok(definition)
    }
    fn binding_rhs(&mut self) -> ParseResult<Expression> {
        if self.match_token(TT::Equal).is_none() {
            return Err(self.error(InvalidLetBindingSeparator));
        }

        self.required(Self::expression, |self_| self_.error(InvalidLetBindingRhs))
    }

    fn if_(&mut self) -> ParseResult<Option<Expression>> {
        let token_index = self.get_token_index();

        if self.match_token(TT::If).is_none() {
            return Ok(None);
        }

        let condition = self.required(Self::binary, |self_| self_.error(InvalidIfCondition))?;

        if self.match_token(TT::Then).is_none() {
            return Err(self.error(InvalidIfThen));
        }

        let then = self.required(Self::expression, |self_| self_.error(InvalidThenBranch))?;

        if self.match_token(TT::Else).is_none() {
            return Err(self.error(InvalidIfElse));
        }

        let else_ = self.required(Self::expression, |self_| self_.error(InvalidIfElseBranch))?;

        let end = else_.end;
        Ok(Some(Node {
            value: E::untyped(If(Box::new(condition), Box::new(then), Box::new(else_))),
            start: token_index,
            end,
        }))
    }

    fn lambda(&mut self) -> ParseResult<Option<Expression>> {
        let token_index = self.get_token_index();

        if self.match_token(Backslash).is_none() {
            return Ok(None);
        }

        let params =
            self.one_or_many(Self::pattern, |self_| self_.error(MissingLambdaParamaters))?;

        if self.match_token(Arrow).is_none() {
            return Err(self.error(InvalidLambdaArrow));
        }

        let body = self.required(Self::expression, |self_| self_.error(InvalidLambdaBody))?;

        let end = body.end;
        Ok(Some(Node {
            value: E::untyped(ET::Lambda(Lambda {
                parameters: params,
                body: Box::new(body),
            })),
            start: token_index,
            end,
        }))
    }

    fn pattern(&mut self) -> ParseResult<Option<Pattern>> {
        let token_index = self.get_token_index();

        if self.match_token(TT::Underscore).is_some() {
            Ok(Some(Node::new(Pattern_::Hole, token_index, token_index)))
        } else if let Some(identifier) = self.identifier() {
            Ok(Some(Node::new(
                Pattern_::Identifier(identifier),
                token_index,
                token_index,
            )))
        } else {
            Ok(None)
        }
    }

    fn binary(&mut self) -> ParseResult<Option<Expression>> {
        if let Some(expr) = self.unary()? {
            self.many(Self::binary_step).map(|mut binops| {
                // Make the binops options to be able to take them later
                let mut binops: Vec<Option<(Binop, Expression)>> =
                    binops.drain(..).map(Some).collect();

                Some(organize_binops(
                    self.strings,
                    expr,
                    &mut binops,
                    &mut (0),
                    0,
                ))
            })
        } else {
            Ok(None)
        }
    }
    fn binary_step(&mut self) -> ParseResult<Option<(Binop, Expression)>> {
        let (token_index, token) = self.get_token();

        let op = match token.kind {
            Slash => Some(binop::DIVISION),
            Star => Some(binop::MULTIPLICATION),
            Plus => Some(binop::ADDITION),
            TT::Minus => Some(binop::SUBSTRACTION),
            BangEqual => Some(binop::NOT_EQUAL),
            EqualEqual => Some(binop::EQUAL),
            Greater => Some(binop::GREATER_THAN),
            GreaterEqual => Some(binop::GREATER_EQUAL_THAN),
            Less => Some(binop::LESS_THAN),
            LessEqual => Some(binop::LESS_EQUAL_THAN),
            And => Some(binop::AND),
            Or => Some(binop::OR),
            _ => None,
        };

        if let Some(op) = op {
            self.advance();

            let op_node = Node::new(op, token_index, token_index);

            let right = self.required(Self::unary, |self_| self_.error(InvalidBinopRhs(*token)))?;

            Ok(Some((op_node, right)))
        } else {
            Ok(None)
        }
    }

    fn unary(&mut self) -> ParseResult<Option<Expression>> {
        let (token_index, token) = self.get_token();

        let u = match token.kind {
            TT::Not => {
                self.advance();
                Some(Not)
            }
            TT::Minus => {
                self.advance();
                Some(Minus)
            }
            _ => None,
        };

        match (u, self.call()?) {
            (Some(u), Some(expr)) => {
                let op = Node::new(u, token_index, token_index);
                let (start, end) = (op.start, expr.end);
                Ok(Some(Node {
                    value: E::untyped(Unary(op, Box::new(expr))),
                    start,
                    end,
                }))
            }
            (None, Some(expr)) => Ok(Some(expr)),
            (Some(_), None) => Err(self.error(InvalidUnaryRhs(*token))),
            (None, None) => Ok(None),
        }
    }

    fn call(&mut self) -> ParseResult<Option<Expression>> {
        let (_, token) = self.get_token();

        if let Some(expr) = self.prop_access()? {
            let args = self.many(|self_| self_.argument(token))?;

            if args.is_empty() {
                Ok(Some(expr))
            } else {
                let last_arg = &args[args.len() - 1];

                let (start, end) = (expr.start, last_arg.end);
                Ok(Some(Node {
                    value: E::untyped(FnCall(Box::new(expr), args)),
                    start,
                    end,
                }))
            }
        } else {
            Ok(None)
        }
    }

    fn argument(&mut self, first_token: &Token) -> ParseResult<Option<Expression>> {
        if self.is_token_in_same_line_or_nested_indent_from(first_token) {
            match self.prop_access()? {
                Some(arg) => Ok(Some(arg)),
                // We tried to get an argument, but there was no match, or it was not well indented
                None => Ok(None),
            }
        } else {
            Ok(None)
        }
    }

    fn prop_access(&mut self) -> ParseResult<Option<Expression>> {
        if let Some(expr) = self.primary()? {
            self.properties(expr).map(Some)
        } else {
            Ok(None)
        }
    }
    fn properties(&mut self, expr: Expression) -> ParseResult<Expression> {
        let mut expr = expr;
        while let Some(identifier) = self.property(self.tokens[expr.end].end)? {
            expr = {
                let start = expr.start;
                let end = identifier.end;
                Node {
                    value: E::untyped(PropAccess(Box::new(expr), identifier)),
                    start,
                    end,
                }
            };
        }

        Ok(expr)
    }
    fn property(&mut self, prev_end: usize) -> ParseResult<Option<Identifier>> {
        let (_, dot_token) = self.get_token();
        match dot_token.kind {
            // Dot token without whitespace between the prev token is a record access
            Dot if dot_token.start == prev_end => {
                self.advance();

                let (identifier_token_index, identifier_token) = self.get_token();
                match identifier_token.kind {
                    // Dot token without whitespace between it and the identifier is a prop access
                    TT::Identifier(name) if identifier_token.start == dot_token.end => {
                        self.advance();
                        Ok(Some(Node::new(
                            Identifier_::new(name),
                            identifier_token_index,
                            identifier_token_index,
                        )))
                    }

                    TT::Identifier(_) => {
                        Err(Error::new(*dot_token, InvalidPropertyAccessSeparator))
                    }

                    _ => Err(Error::new(*dot_token, InvalidPropertyAccessIdentifier)),
                }
            }
            _ => Ok(None),
        }
    }

    fn primary(&mut self) -> ParseResult<Option<Expression>> {
        let (token_index, token) = self.get_token();

        match token.kind {
            False => {
                self.advance();
                Ok(Some(Node::new(
                    E::untyped(Bool(false)),
                    token_index,
                    token_index,
                )))
            }
            True => {
                self.advance();
                Ok(Some(Node::new(
                    E::untyped(Bool(true)),
                    token_index,
                    token_index,
                )))
            }

            TT::Float(lexeme) => {
                let n = self
                    .strings
                    .resolve(lexeme)
                    .parse::<f64>()
                    .map_err(|_| Error::new(*token, InvalidFloat(*token)))?;

                self.advance();

                Ok(Some(Node::new(
                    E::untyped(Float(n)),
                    token_index,
                    token_index,
                )))
            }

            TT::Identifier(lexeme) => {
                self.advance();

                let identifier = Node::new(Identifier_::new(lexeme), token_index, token_index);

                Ok(Some(Node::new(
                    E::untyped(ET::Identifier(None, AnyIdentifier::Identifier(identifier))),
                    token_index,
                    token_index,
                )))
            }

            TT::CapitalizedIdentifier(_) => {
                // TODO: This is slightly wrong, since it will validate the last module segment as
                // a module name, even though it can be just a CapitalizedIdentifier which can have
                // more characters.
                // Maybe what should happen is that I should restrain capitalized identifiers to
                // have the same kinds of characters as the module name part.
                let mut module = self.module_name(|| InvalidModuleNameSegment)?;

                let (module, identifier) = if module.parts.len() == 1 {
                    // Is there a normal identifier afterwards? If so it is a Module.ident, if not,
                    // the module's only part is a capitalized identifier.
                    let module_end_position = self.tokens[module.end()].end;
                    if let Some(ident) = self.property(module_end_position)? {
                        (Some(module), AnyIdentifier::Identifier(ident))
                    } else {
                        let first = module.parts.swap_remove(0);
                        (None, AnyIdentifier::CapitalizedIdentifier(first))
                    }
                } else {
                    // Is there a normal identifier afterwards? If so the whole module is fine, if
                    // not, the module's last part is a capitalized identifier.
                    let module_end_position = self.tokens[module.end()].end;
                    if let Some(ident) = self.property(module_end_position)? {
                        (Some(module), AnyIdentifier::Identifier(ident))
                    } else {
                        let ident = module.parts.pop().unwrap();
                        let module = ModuleName::new(module.parts, self.strings).expect("Internal parser error: Module should be valid as it is being built from a previously built module");
                        (Some(module), AnyIdentifier::CapitalizedIdentifier(ident))
                    }
                };

                let (start, end) = if let Some(module) = &module {
                    let first = &module.parts[0];
                    (first.start, identifier.node().end)
                } else {
                    let node = identifier.node();
                    (node.start, node.end)
                };
                Ok(Some(Node {
                    value: E::untyped(ET::Identifier(module, identifier)),
                    start,
                    end,
                }))
            }

            TT::String_(string) => {
                self.advance();
                Ok(Some(Node::new(
                    E::untyped(String_(string)),
                    token_index,
                    token_index,
                )))
            }

            LeftBrace => {
                self.advance();
                let (next_token_index, next_token) = self.get_token();
                let second_next_token = self.peek_next_token();

                match (next_token.kind, second_next_token.kind) {
                    // Unit record
                    (RightBrace, _) => {
                        self.advance();

                        Ok(Some(Node::new(
                            E::untyped(Record(vec![])),
                            token_index,
                            next_token_index,
                        )))
                    }

                    // Record literal
                    (TT::Identifier(_), Colon) | (TT::Identifier(_), Equal) => {
                        let fields = self.record_fields()?;

                        if let Some((right_brace_token_index, _)) = self.match_token(RightBrace) {
                            Ok(Some(Node::new(
                                E::untyped(Record(fields)),
                                token_index,
                                right_brace_token_index,
                            )))
                        } else {
                            Err(self.error(InvalidRecordFieldSeparatorOrLastDelimiter))
                        }
                    }

                    // Record update
                    _ => {
                        let record = self.required(Self::expression, |self_| {
                            self_.error(InvalidRecordFieldsOrExtensibleRecordExpression)
                        })?;

                        if self.match_token(Pipe).is_none() {
                            return Err(self.error(InvalidRecordFieldsOrExtensibleRecordExpression));
                        }

                        let fields = self.record_fields()?;

                        if let Some((right_brace_token_index, _)) = self.match_token(RightBrace) {
                            Ok(Some(Node::new(
                                E::untyped(RecordUpdate(Box::new(record), fields)),
                                token_index,
                                right_brace_token_index,
                            )))
                        } else {
                            Err(self.error(InvalidExtensibleRecordFieldSeparatorOrLastDelimiter))
                        }
                    }
                }
            }

            LeftParen => {
                self.advance();

                // Unit expression
                if let Some((right_paren_token_index, _)) = self.match_token(RightParen) {
                    return Ok(Some(Node::new(
                        E::untyped(Unit),
                        token_index,
                        right_paren_token_index,
                    )));
                }

                // Parenthesized expression
                let expr = self.required(Self::expression, |self_| {
                    self_.error(InvalidParenthesizedExpression)
                })?;

                if self.match_token(RightParen).is_some() {
                    Ok(Some(expr))
                } else {
                    Err(self.error(InvalidParenthesizedExpressionLastDelimiter(*token)))
                }
            }

            Dot => {
                self.advance();
                let (identifier_token_index, identifier_token) = self.get_token();

                match identifier_token.kind {
                    // Dot token without whitespace between it and the identifier is a lambda w/ prop access
                    TT::Identifier(lexeme) if identifier_token.start == token.end => {
                        self.advance();

                        let name_identifier = Node::new(
                            Identifier_::new(lexeme),
                            identifier_token_index,
                            identifier_token_index,
                        );

                        Ok(Some(Node::new(
                            E::untyped(PropAccessLambda(name_identifier)),
                            token_index,
                            identifier_token_index,
                        )))
                    }

                    // Dot with whitespace and an identifier afterwards
                    TT::Identifier(_) => {
                        Err(Error::new(*token, InvalidPropertyAccessLambdaWhitespace))
                    }

                    _ => Err(Error::new(*token, InvalidPropertyAccessLambdaIdentifier)),
                }
            }

            _ => Ok(None),
        }
    }

    fn record_fields(&mut self) -> ParseResult<Vec<(Identifier, Expression)>> {
        self.one_or_many_sep(
            |self_| Ok(self_.match_token(TT::Comma)),
            Self::record_field,
            |self_| self_.error(MissingRecordFields),
        )
    }

    fn record_field(&mut self) -> ParseResult<(Identifier, Expression)> {
        let identifier = self.required(
            |self_| Ok(self_.identifier()),
            |self_| self_.error(InvalidRecordFieldKey),
        )?;

        let (_, separator) = self.get_token();
        if !matches!(separator.kind, Colon | Equal) {
            return Err(self.error(InvalidRecordFieldKeyValueSeparator));
        }
        self.advance();

        let expr = self.required(Self::expression, |self_| {
            self_.error(InvalidRecordFieldValue)
        })?;

        Ok((identifier, expr))
    }

    fn module_name(&mut self, on_name_parse_error: fn() -> ErrorType) -> ParseResult<ModuleName> {
        let start = self.current;
        let identifiers = self.one_or_many_sep(
            |self_| {
                let (_, dot_token) = self_.get_token();
                let possibly_module_identifier = self_.peek_next_token();
                match (dot_token.kind, possibly_module_identifier.kind) {
                    (TT::Dot, TT::CapitalizedIdentifier(_)) => {
                        self_.advance();
                        Ok(Some(dot_token))
                    }
                    _ => Ok(None),
                }
            },
            |self_| {
                self_.required(
                    |self_| Ok(self_.capitalized_identifier()),
                    |self_| self_.error(InvalidModuleNameSegment),
                )
            },
            |self_| self_.error(on_name_parse_error()),
        )?;

        debug_assert!(!identifiers.is_empty());
        let end = self.current;
        self.build_module_name(start, &self.tokens[start..end], identifiers)
    }
    fn build_module_name(
        &mut self,
        start: usize,
        tokens: &'tokens [Token],
        names: Vec<CapitalizedIdentifier>,
    ) -> ParseResult<ModuleName> {
        ModuleName::new(names, self.strings).map_err(|(i, names)| {
            let name = &names[i];
            let token_relative_index = tokens
                .iter()
                .position(|t| t.start == self.tokens[name.start].start)
                .unwrap();
            Error::new(
                self.tokens[start + token_relative_index],
                InvalidModuleNameSegment,
            )
        })
    }

    fn identifier(&mut self) -> Option<Identifier> {
        if let (
            identifier_token_index,
            Token {
                kind: TT::Identifier(lexeme),
                ..
            },
        ) = self.get_token()
        {
            self.advance();
            let name_identifier = Identifier_::new(*lexeme);
            Some(Node::new(
                name_identifier,
                identifier_token_index,
                identifier_token_index,
            ))
        } else {
            None
        }
    }

    fn capitalized_identifier(&mut self) -> Option<CapitalizedIdentifier> {
        if let (
            identifier_token_index,
            Token {
                kind: TT::CapitalizedIdentifier(lexeme),
                ..
            },
        ) = self.get_token()
        {
            self.advance();
            let name_identifier = CapitalizedIdentifier_::new(*lexeme);
            Some(Node::new(
                name_identifier,
                identifier_token_index,
                identifier_token_index,
            ))
        } else {
            None
        }
    }

    // Utilities

    fn prev_token(&self) -> (module::tokens::Index, &'tokens Token) {
        self.prev_non_comment_token(self.current)
    }
    fn prev_token_index(&self) -> module::tokens::Index {
        let (idx, _) = self.prev_non_comment_token(self.current);
        idx
    }

    fn get_token(&self) -> (module::tokens::Index, &'tokens Token) {
        self.next_non_comment_token(self.current)
    }
    fn get_token_index(&self) -> module::tokens::Index {
        let (idx, _) = self.next_non_comment_token(self.current);
        idx
    }

    fn peek_next_token(&self) -> &'tokens Token {
        let (first, token) = self.next_non_comment_token(self.current);
        match token.kind {
            Eof => token,
            _ => {
                let (_, token) = self.next_non_comment_token(first + 1);
                token
            }
        }
    }

    fn advance(&mut self) {
        let (i, token) = self.next_non_comment_token(self.current);
        match token.kind {
            Eof => (),
            _ => self.current = i + 1,
        };
    }

    fn next_non_comment_token(&self, start: usize) -> (usize, &'tokens Token) {
        let mut i = start;
        let mut token;
        loop {
            token = self
                .tokens
                .get(i)
                .expect("Internal parser error: Out of bounds access to tokens array");
            match token.kind {
                Eof => break,
                Comment => {
                    i += 1;
                    continue;
                }
                _ => {
                    break;
                }
            };
        }
        (i, token)
    }

    fn prev_non_comment_token(&self, start: usize) -> (usize, &'tokens Token) {
        let mut i = start - 1;
        let mut token;
        loop {
            token = self
                .tokens
                .get(i)
                .expect("Internal parser error: Out of bounds access to tokens array");
            match token.kind {
                Eof => break,
                Comment => {
                    i -= 1;
                    continue;
                }
                _ => {
                    break;
                }
            };
        }
        (i, token)
    }

    fn is_token_in_same_indent_and_column_as(&self, parent_token: &Token) -> bool {
        let (_, token) = self.get_token();
        parent_token.line < token.line
            && token.indent == parent_token.indent
            && token.column == parent_token.column
    }

    fn is_token_after_line_and_same_indent_as(&self, parent_token: &Token) -> bool {
        let (_, token) = self.get_token();
        parent_token.line < token.line && token.indent == parent_token.indent
    }

    fn is_token_in_same_line_or_nested_indent_from(&self, parent_token: &Token) -> bool {
        let (_, token) = self.get_token();
        parent_token.line == token.line
            || (parent_token.line < token.line && token.indent > parent_token.indent)
    }

    fn is_token_start_of_line_and_after_line_and_nested_indent_from(
        &self,
        parent_token: &Token,
    ) -> bool {
        let (_, token) = self.get_token();
        parent_token.line < token.line
            && token.indent > parent_token.indent
            && token.column == token.indent
    }

    fn is_token_equal_or_less_indented_than(&self, parent_token: &Token) -> bool {
        let (_, token) = self.get_token();
        parent_token.line < token.line && token.indent <= parent_token.indent
    }

    fn one_or_many_delimited<Parser, ParserResult, E1, E2>(
        &mut self,
        separator: token::Type,
        delimiter: (token::Type, token::Type),
        parse: Parser,
        on_first_not_found: E1,
        on_missing_separator_or_last_delimiter: E2,
    ) -> ParseResult<Option<Vec<ParserResult>>>
    where
        Parser: Fn(&mut Self) -> ParseResult<ParserResult>,
        E1: Fn(&mut Self) -> Error,
        E2: Fn(&mut Self) -> Error,
    {
        let (first_delimiter, last_delimiter) = delimiter;

        let (_, token) = self.get_token();
        if token.kind != first_delimiter {
            return Ok(None);
        }
        self.advance();

        let mut items = vec![];
        let mut i = 0;

        loop {
            let (_, token) = self.get_token();
            if token.kind == last_delimiter {
                if i == 0 {
                    return Err(on_first_not_found(self));
                } else {
                    self.advance();
                    break;
                }
            }

            if i > 0 {
                if token.kind == separator {
                    self.advance();
                } else {
                    return Err(on_missing_separator_or_last_delimiter(self));
                }
            }

            let item = parse(self)?;
            items.push(item);
            i += 1;
        }

        Ok(Some(items))
    }

    fn one_or_many<Parser, ParserResult, E>(
        &mut self,
        parse: Parser,
        on_first_not_found: E,
    ) -> ParseResult<Vec<ParserResult>>
    where
        Parser: Fn(&mut Self) -> ParseResult<Option<ParserResult>>,
        E: Fn(&mut Self) -> Error,
    {
        let items = self.many(parse)?;
        if items.is_empty() {
            Err(on_first_not_found(self))
        } else {
            Ok(items)
        }
    }

    fn one_or_many_sep<Parser, ParserResult, Delimiter, DelimiterParser, E>(
        &mut self,
        delimiter: DelimiterParser,
        parse: Parser,
        on_first_not_found: E,
    ) -> ParseResult<Vec<ParserResult>>
    where
        Parser: Fn(&mut Self) -> ParseResult<ParserResult>,
        DelimiterParser: Fn(&mut Self) -> ParseResult<Option<Delimiter>>,
        E: Fn(&mut Self) -> Error,
    {
        let mut i = 0;
        let mut items = vec![];
        loop {
            if i > 0 {
                let del = delimiter(self)?;
                if del.is_none() {
                    break;
                }
            }
            let item = parse(self)?;
            items.push(item);
            i += 1;
        }

        if items.is_empty() {
            Err(on_first_not_found(self))
        } else {
            Ok(items)
        }
    }

    fn many<Parser, ParserResult>(&mut self, parse: Parser) -> ParseResult<Vec<ParserResult>>
    where
        Parser: Fn(&mut Self) -> ParseResult<Option<ParserResult>>,
    {
        let mut items = vec![];
        while let Some(item) = parse(self)? {
            items.push(item);
        }
        Ok(items)
    }

    fn required<Parser, E, ParserResult>(
        &mut self,
        parse: Parser,
        on_none: E,
    ) -> ParseResult<ParserResult>
    where
        Parser: Fn(&mut Self) -> ParseResult<Option<ParserResult>>,
        E: Fn(&mut Self) -> Error,
    {
        if let Some(result) = parse(self)? {
            Ok(result)
        } else {
            Err(on_none(self))
        }
    }

    fn match_token(&mut self, typ: token::Type) -> Option<(module::tokens::Index, &'tokens Token)> {
        let (token_index, token) = self.get_token();
        if std::mem::discriminant(&token.kind) == std::mem::discriminant(&typ) {
            self.advance();
            Some((token_index, token))
        } else {
            None
        }
    }

    fn error(&self, error_type: ErrorType) -> Error {
        let (_, token) = self.get_token();
        Error::new(*token, error_type)
    }
}

fn organize_binops(
    strings: &mut Strings,
    left: Expression,
    binops: &mut Vec<Option<(Binop, Expression)>>,
    current: &mut usize,
    min_precedence: u32,
) -> Expression {
    // https://eli.thegreenplace.net/2012/08/02/parsing-expressions-by-precedence-climbing
    // better than the wikipedia article for precedence climibing

    let mut left = left;

    loop {
        let next = binops.get_mut(*current);

        match next {
            Some(op_and_expr) => {
                let keep_parsing = matches!(op_and_expr, Some((op, _rhs)) if op.value.precedence >= min_precedence);

                if keep_parsing {
                    // Take ownership of the op and rhs
                    let (op, rhs) = op_and_expr.take().unwrap();

                    *current += 1;

                    let next_min_precedence = op.value.precedence
                        + if op.value.associativity == Associativity::Ltr {
                            1
                        } else {
                            0
                        };

                    let right = organize_binops(strings, rhs, binops, current, next_min_precedence);

                    let (start, end) = (left.start, right.end);
                    left = Node {
                        value: E::untyped(Binary(
                            Box::new(op.with_value(E::untyped(ET::Identifier(
                                None,
                                AnyIdentifier::Identifier(
                                    op.with_value(op.value.get_function_identifier(strings)),
                                ),
                            )))),
                            op,
                            Box::new([left, right]),
                        )),
                        start,
                        end,
                    }
                } else {
                    break;
                }
            }
            None => break,
        }
    }

    left
}

pub fn parse<'source, 'strings, 'tokens>(
    source: &'source Source,
    tokens: &'tokens [Token],
    strings: &'strings mut Strings,
) -> ParseResult<(Module, Vec<Module>)> {
    let mut parser = State {
        strings,
        source,
        tokens,
        current: 0,
    };

    parser.file()
}

pub fn parse_repl<'source, 'strings, 'tokens>(
    source: &'source Source,
    tokens: &'tokens [Token],
    strings: &'strings mut Strings,
) -> ParseResult<ReplEntry> {
    let mut parser = State {
        strings,
        source,
        tokens,
        current: 0,
    };

    parser.repl_entry()
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::tokenizer;

    pub fn parse_expression<'source, 'strings, 'tokens>(
        source: &'source Source,
        tokens: &'tokens [Token],
        strings: &'strings mut Strings,
    ) -> ParseResult<Box<Expression>> {
        let mut parser = State {
            strings,
            source,
            tokens,
            current: 0,
        };

        let result = parser.required(State::expression, |self_| {
            self_.error(InvalidModuleDefinitionLhs)
        })?;
        parser.eof(Box::new(result))
    }

    mod test_expression {
        use super::*;
        use insta::assert_snapshot;

        #[test]
        fn test_bool() {
            assert_snapshot!(parse("True"));
            assert_snapshot!(parse("False"));
        }

        #[test]
        fn test_unit() {
            assert_snapshot!(parse("()"));
        }

        #[test]
        fn test_int() {
            assert_snapshot!(parse("123"));
        }

        #[test]
        fn test_float() {
            assert_snapshot!(parse("123.2"));
        }

        #[test]
        fn test_camel_case_identifier() {
            assert_snapshot!(parse("variableOne"));
        }

        #[test]
        fn test_snake_case_identifier() {
            assert_snapshot!(parse("variable_one"));
        }

        #[test]
        fn test_special_chars_identifier() {
            assert_snapshot!(parse("espaÆàʥñÑol"));
        }

        #[test]
        fn test_emoji_string() {
            assert_snapshot!(parse("\"😄\""));
        }

        #[test]
        fn test_new_line_string() {
            assert_snapshot!(parse("\"\n\""));
        }

        #[test]
        fn test_empty_string() {
            assert_snapshot!(parse("\"\""));
        }

        #[test]
        fn test_parenthesis() {
            assert_snapshot!(parse("(\"\")"));
            assert_snapshot!(parse("(((1)))"));
            assert_snapshot!(parse("(((1))"));
            assert_snapshot!(parse("(((1))))"));
            assert_snapshot!(parse(
                "(
  ((1))
)",
            ));
        }

        #[test]
        fn test_function_call() {
            assert_snapshot!(parse("fun arg"));
            assert_snapshot!(parse("fun\n arg"));
            assert_snapshot!(parse("  fun\n    arg"));
            assert_snapshot!(parse("fun\narg"));
            assert_snapshot!(parse(
                "
fun arg1
  arg2 arg3
  arg4",
            ));
            assert_snapshot!(parse("hello ()"));
        }

        #[test]
        fn test_unary_operators() {
            assert_snapshot!(parse("not False"));
            assert_snapshot!(parse("- 5"));
            assert_snapshot!(parse("incr (-5)"));
        }

        #[test]
        fn test_binary_operators() {
            assert_snapshot!(parse("1 - 5"));
            assert_snapshot!(parse("1 - -5"));
            assert_snapshot!(parse("1 + 2 / 3"));
            assert_snapshot!(parse("1 == 2 / 3"));
        }

        #[test]
        fn test_lambda() {
            assert_snapshot!(parse("\\a -> a"));
            assert_snapshot!(parse("\\a -> \\b -> a"));
            assert_snapshot!(parse("\\a b -> a"));
        }

        #[test]
        fn test_if() {
            assert_snapshot!(parse("if True then 1 else 2"));
            assert_snapshot!(parse(
                "
if True then
  1

else
  2",
            ));
            assert_snapshot!(parse("if True then incr 1 else 2"));
            assert_snapshot!(parse("if True then if False then 1 else 3 else 2"));
            assert_snapshot!(parse("if True { 1 } else 2"));
            assert_snapshot!(parse("if True then 1"));
        }

        #[test]
        fn test_let() {
            assert_snapshot!(
                "single line body same indent",
                parse(
                    "\
let x = 1
x"
                )
            );
            assert_snapshot!(
                "single line indented body",
                parse(
                    "\
let x = a
  x"
                )
            );
            assert_snapshot!(
                "single line indented binding value same line body",
                parse(
                    "\
let x = a
  x
x"
                )
            );
        }

        #[test]
        fn test_let_in() {
            assert_snapshot!("let in", parse("let x = a x in x"));
            assert_snapshot!(
                "let in multiline",
                parse(
                    "\
let x = a
  x
in
x"
                )
            );
            assert_snapshot!(
                "let in multiline bad indent in binding value",
                parse(
                    "\
let
  x = a
  x
in
x"
                )
            );
            assert_snapshot!(
                "let in multiline 2",
                parse(
                    "\
let
  x = a
    x
in
x"
                )
            );
            assert_snapshot!(
                "let in, many bindings indented",
                parse(
                    "\
let
  x = a
    x
 b = 5
in
x"
                )
            );
        }

        #[test]
        fn test_let_pascalcase_identifier() {
            assert_snapshot!(parse(
                "\
let
  IAmNotCamelCase = 1
in
IAmNotCamelCase
"
            ));
        }

        #[test]
        fn test_hole_pattern() {
            assert_snapshot!(parse("let _ = a x in x"));
        }

        #[test]
        fn test_lambda_syntax_sugar() {
            assert_snapshot!(parse(
                "\
let
  incr n = n + 1
in
incr 5
"
            ));
            assert_snapshot!(parse(
                "\
let add x y = x + y
add 5"
            ));
        }

        #[test]
        fn test_record() {
            assert_snapshot!(parse("{}"));
            assert_snapshot!(parse("let a = { in 5"));
            assert_snapshot!(parse("{ 1 : 5 }"));
            assert_snapshot!(parse("{ x : 5 }"));
            assert_snapshot!(parse("{ Sneaky : 5 }"));
            assert_snapshot!(parse("{ , x : 5 }"));
            assert_snapshot!(parse("{ x : 5 , }"));
            assert_snapshot!(parse("{ x : 5 , y : 10 }"));
            assert_snapshot!(parse("{ x = 5 }"));
            assert_snapshot!(parse("{ x = { x = 5 } }"));
        }

        #[test]
        fn test_property_access() {
            assert_snapshot!(parse("a.b"));
            assert_snapshot!(parse("a. b"));
            assert_snapshot!(parse("a.b.c.d"));
        }

        #[test]
        fn test_shorthand_property_access_lambda() {
            assert_snapshot!(parse(".b"));
            assert_snapshot!(parse(". b"));
            assert_snapshot!(parse("a .b"));
            assert_snapshot!(parse(".a b"));
        }

        #[test]
        fn test_record_update() {
            assert_snapshot!(parse("{ 5 | x = 1 }"));
            assert_snapshot!(parse("{ 5 : x = 1 }"));
            assert_snapshot!(parse("{ 5 | x = 1, y = 3 }"));
            assert_snapshot!(
                "arbitrary expression in the record slot",
                parse("{ if True then {} else {} | x = 1, y = 3 }")
            );
        }

        #[test]
        fn test_identifiers_with_module_access() {
            assert_snapshot!(parse("A"));
            assert_snapshot!(parse("A.B"));
            assert_snapshot!(parse("A.B.C"));
            assert_snapshot!(parse("A.b.c"));
            assert_snapshot!(parse("A.B.c"));
            assert_snapshot!(parse("A.B.C.d"));
            assert_snapshot!(parse("function record.access"));
            assert_snapshot!(parse("A.b"));
        }

        fn parse(code: &str) -> String {
            let source = Source::new_orphan(code.to_string());
            let mut strings = Strings::new();
            let mut module = module::Module::new();
            tokenizer::parse(&source, &mut strings, &mut module).unwrap();
            let result = match &parse_expression(&source, &module.tokens, &mut strings) {
                Ok(ast) => format!("{ast:#?}"),
                Err(error) => {
                    let error_str = error.to_string(&source, &strings);
                    format!("{error_str}\n\n{error:#?}")
                }
            };
            format!("Input:\n\n{code}\n\nResult:\n\n{result}")
        }
    }

    mod test_module_parser {
        use super::*;
        use insta::assert_snapshot;

        #[test]
        fn test_module_not_an_expression() {
            assert_snapshot!(parse("True"));
        }

        #[test]
        fn test_empty_module() {
            assert_snapshot!(parse("module Test"));
        }

        #[test]
        fn test_top_level_def() {
            assert_snapshot!(parse(
                "\
module Test

a = 1"
            ));
            assert_snapshot!(parse(
                "\
module Test

a = 1

b = True"
            ));
        }

        #[test]
        fn test_parsing_errors() {
            assert_snapshot!(parse(
                "\
module Test

a = 1 + 2) + 3

b = * add
  5"
            ));
            assert_snapshot!(parse(
                "\
module Test

a = 1 + 2 + 3

b = * add
  5"
            ));
        }

        #[test]
        fn test_submodule() {
            assert_snapshot!(parse(
                "\
module Parent

module Parent.Test"
            ));
            assert_snapshot!(parse(
                "\
module Parent

module Parent.Test

  a = 1"
            ));
            assert_snapshot!(parse(
                "\
module Parent

module Parent.Test

  a = 1

a = 1
"
            ));
            assert_snapshot!(
                "parsing errors",
                parse(
                    "\
module Parent

module Parent.Test

  a = 1 + 2) + 3

  b = * add
    5

a = 1 + 2) + 3

b = * add
  5"
                )
            );
        }

        #[test]
        fn test_submodules_nested() {
            assert_snapshot!(parse(
                "\
module Parent

module Parent.Test1
  a = 1

  module Parent.Test1.Test1Test
    b = 1

module Parent.Test2
    c = 5
    ",
            ));
        }

        #[test]
        fn test_exposing() {
            assert_snapshot!(parse(
                "\
module Test exposing (a)

a = 1

b = True"
            ));
            assert_snapshot!(parse(
                "\
module Test exposing (a, b)

a = 1

b = True"
            ));
            assert_snapshot!(parse(
                "\
module Test exposing a, b

a = 1

b = True"
            ));
            assert_snapshot!(parse(
                "\
module Test exposing (a b)

a = 1

b = True"
            ));
            assert_snapshot!(parse(
                "\
module Test exposing (a, b

a = 1

b = True"
            ));
        }

        #[test]
        fn test_import() {
            assert_snapshot!(parse(
                "\
module Test

import Banana"
            ));
            assert_snapshot!(parse(
                "\
module Test

import Banana as B"
            ));
            assert_snapshot!(parse(
                "\
module Test

import Banana exposing (phone)"
            ));
            assert_snapshot!(parse(
                "\
module Test

import Banana as B

import Phone exposing (raffi)

import Apple as A exposing (orange)
",
            ));
        }

        #[test]
        fn test_module_name() {
            assert_snapshot!(parse("module i_am_not_PascalCase"));
            assert_snapshot!(parse(
                "\
module Test

module i_am_not_PascalCase
    test = 1
"
            ));
            assert_snapshot!(parse(
                "\
module Test

module Test.Test2

    module i_am_not_PascalCase
        test = 1
"
            ));
        }

        #[test]
        fn test_definition_pascalcase_identifier() {
            assert_snapshot!(parse(
                "\
module Test

IAmNotCamelCase = 1
"
            ));
        }

        #[test]
        fn test_lambda_syntax_sugar() {
            assert_snapshot!(parse(
                "\
module Test

incr n = n + 1
"
            ));
        }

        #[test]
        fn test_module_name_validation() {
            assert_snapshot!(parse("module Te_st"));
            assert_snapshot!(parse("module Test.Te_st"));
            assert_snapshot!(parse("module Test.Te_st.Test"));
            assert_snapshot!(parse(
                "
module Test.Something

module Test.Banana
"
            ));
        }

        #[test]
        fn test_type_definition() {
            assert_snapshot!(
                "no definition",
                parse(
                    "
module Test

type Banana
"
                )
            );
            assert_snapshot!(
                "wrong definition",
                parse(
                    "
module Test

type Banana : asdf
"
                )
            );
        }

        #[test]
        fn test_union_types() {
            assert_snapshot!(
                "single branch",
                parse(
                    "
module Test

type Fruit = Banana
"
                )
            );
            assert_snapshot!(
                "multiple branch",
                parse(
                    "
module Test

type Fruit = Banana | Apple
"
                )
            );
            assert_snapshot!(
                "wrong branch separator",
                parse(
                    "
module Test

type Fruit = Banana / Apple
"
                )
            );
            assert_snapshot!(
                "branch with params",
                parse(
                    "
module Test

type Fruit = Banana a
"
                )
            );
            assert_snapshot!(
                "with type variable parameter",
                parse(
                    "
module Test

type Fruit a = Banana a
"
                )
            );
            assert_snapshot!(
                "with type variable parameter many branches",
                parse(
                    "
module Test

type Fruit a = Banana a | Phone
"
                )
            );
            assert_snapshot!(
                "multiline w/ equal",
                parse(
                    "
module Test

type Fruit a
    = Banana a
    | Phone
"
                )
            );
            assert_snapshot!(
                "multiline w/ vbar",
                parse(
                    "
module Test

type Fruit a =
    | Banana a
    | Phone
"
                )
            );
        }

        #[test]
        fn test_record() {
            assert_snapshot!(
                "empty",
                parse(
                    "
module Test

type Fruit = {}
"
                )
            );
            assert_snapshot!(
                "unclosed record",
                parse(
                    "
module Test

type Fruit = {
"
                )
            );
            assert_snapshot!(
                "missing value on field",
                parse(
                    "
module Test

type Fruit = { name }
"
                )
            );
            assert_snapshot!(
                "single field",
                parse(
                    "
module Test

type Fruit = { name : String }
"
                )
            );
            assert_snapshot!(
                "many fields",
                parse(
                    "
module Test

type Fruit = { name : String , banana: Phone }
"
                )
            );
        }

        #[test]
        fn test_extensible_record() {
            assert_snapshot!(parse(
                "
module Test

type Fruit a = { a | name : String , banana: Phone }
"
            ));
        }

        #[test]
        fn test_constructor_parameters() {
            assert_snapshot!(parse(
                "
module Test

type Fruit a = Banana a Phone a
"
            ));
            assert_snapshot!(parse(
                "
module Test

type Fruit a = Banana a (Phone a)
"
            ));
        }

        #[test]
        fn test_record_field_value_with_params() {
            assert_snapshot!(parse(
                "
module Test

type Fruit a = { test : Banana a (Phone a) }
"
            ));
            assert_snapshot!(parse(
                "
module Test

type Fruit a = { test : (Banana a (Phone a)) }
"
            ));
        }

        #[test]
        fn test_types_and_definitions() {
            assert_snapshot!(parse(
                "\
module Test exposing (main)

type Fruit = Banana

main = Banana
"
            ));
        }

        #[test]
        fn test_exposing_types() {
            assert_snapshot!(parse(
                "\
module Test exposing (Fruit(Banana))

type Fruit = Banana
"
            ));
            assert_snapshot!(parse(
                "\
module Test exposing (main)

import Test.Fruits exposing (Fruit(Banana))

main = 1

module Test.Fruits exposing (Fruit(Banana))
    type Fruit = Banana
"
            ));
        }

        #[test]
        fn test_single_line_comments() {
            assert_snapshot!(parse(
                "\
-- Comment
module Test
    -- Comment
    exposing (
    -- Comment
    main)

-- Comment
import Test.Fruits exposing (Fruit(Banana))

-- Comment
main =
-- Comment
    1

-- Comment
module Test.Fruits
    -- Comment
    exposing (Fruit(Banana))
    -- Comment
    type Fruit =
        -- Comment
        Banana
"
            ));
        }

        #[test]
        fn test_function_types() {
            assert_snapshot!(parse(
                "\
module Test exposing (Fruit)

type Fruit a b = Fruit (a -> b)
"
            ));
            assert_snapshot!(parse(
                "\
module Test exposing (Fruit)

type Fruit a b = { f : a -> b }
"
            ));
            assert_snapshot!(parse(
                "\
module Test exposing (Fruit)

type Fruit a b = { f : (a -> b) }
"
            ));
            assert_snapshot!(parse(
                "\
module Test exposing (Fruit)

type Fruit a b = Fruit (a -> b -> c)
"
            ));
        }

        #[test]
        fn test_type_only_definitions() {
            assert_snapshot!(parse(
                "\
module Test exposing (main)

main : Fruit
"
            ));
            assert_snapshot!(parse(
                "\
module Test exposing (main)

main : Fruit

test = 5
"
            ));
            assert_snapshot!(parse(
                "\
module Test exposing (main)

main : Fruit

signature : Fruit

test = 5
"
            ));
        }

        #[test]
        fn test_typed_definitions() {
            assert_snapshot!(parse(
                "\
module Test exposing (main)

main : Fruit
main = Fruit

test = 5
"
            ));
        }

        #[test]
        fn test_signatures() {
            assert_snapshot!(parse(
                "\
module Test exposing (main)

main : Fruit a b

signature : Fruit b

test = 5
"
            ));
            assert_snapshot!(parse(
                "\
module Test exposing (main)

main : Fruit -> Fruit
"
            ));
            assert_snapshot!(parse(
                "\
module Test exposing (main)

main : Fruit -> Fruit a

test = 5
"
            ));
            assert_snapshot!(parse(
                "\
module Test exposing (main)

main : Fruit a b -> Fruit a b
main = Fruit

test = 5
"
            ));
            assert_snapshot!(parse(
                "\
module Test exposing (main)

main : Fruit a -> Fruit b -> Fruit c

signature : Fruit d -> Fruit e -> Fruit f

test = 5
"
            ));
        }

        #[test]
        fn test_signatures_in_let_bindings() {
            assert_snapshot!(parse(
                "\
module Test exposing (main)

main =
    let
        test : Fruit a -> Fruit b -> Fruit c

        test2 : Fruit a -> Fruit b -> Fruit c
        test2 a b = c

        test3 a b = c

    test
"
            ));
        }

        fn parse(code: &str) -> String {
            let source = Source::new_orphan(code.to_string());
            let mut strings = Strings::new();
            let mut module = module::Module::new();
            tokenizer::parse(&source, &mut strings, &mut module).unwrap();
            let result = match &super::parse(&source, &module.tokens, &mut strings) {
                Ok(ast) => format!("{ast:#?}"),
                Err(error) => {
                    let error_str = error.to_string(&source, &strings);
                    format!("{error_str}\n\n{error:#?}")
                }
            };
            format!("Input:\n\n{code}\n\nResult:\n\n{result}")
        }
    }
}
