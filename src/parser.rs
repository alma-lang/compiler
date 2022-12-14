use crate::ast::expression::{
    binop, AnyIdentifier, Expressions, PatternData, PatternMatchingBranch, Unary,
};
use crate::ast::{
    expression::{
        self,
        binop::*,
        CapitalizedIdentifier, Expression as E, Identifier, Lambda, Pattern,
        UnaryData::{Minus, Not},
    },
    span::Span,
    types::{self, Type, TypeDefinition},
    Import, Module, ModuleName, ReplEntry,
};
use crate::ast::{Definition, Export, ExportData, ModuleFullName, TypeSignature, TypedDefinition};
use crate::source::Source;
use crate::strings::Strings;
use crate::token::{
    self, Token, Tokens,
    Type::{self as TT, *},
};
use fnv::FnvHashMap;
use typed_index_collections::TiSlice;

/* Grammar draft (●○):
    ● file                     → module EOF
    ● module                   → "module" module_name exposing? imports? module_definitions?
    ● module_name              → CAPITALIZED_IDENTIFIER ( "." CAPITALIZED_IDENTIFIER )*
    ● exposing                 → "exposing" "(" export ( "," export )* ")"
    ● export                   → IDENTIFIER
                               | CAPITALIZED_IDENTIFIER ( "(" CAPITALIZED_IDENTIFIER ( "," CAPITALIZED_IDENTIFIER )* ")" )?
    ● imports                  → ( import )*
    ● import                   → "import" IDENTIFIER ( "as" IDENTIFIER )? exposing?

    ● module_definitions       → ( type_def | module | top_level_binding )+

    // Type definitions
    ● type_identifier          → CAPITALIZED_IDENTIFIER
    ● type_var                 → IDENTIFIER
    ● type_def                 → type_def_union | type_def_alias
    ● type_def_name            → type_identifier ( type_var )*
    ● type_def_union           → "external"? "type" type_def_name "=" type_union_branches
    ● type_def_alias           → "alias" type_def_name "=" type_record
    ● type_union_branches      → type_constructor ( "|" type_constructor )*
    ● type_constructor         → type_identifier ( type_param )*
    ● type_param               → type_parens | type_identifier | type_var | type_record
    ● type_parens              → "(" type_function ")"
    ● type_record              → "{" ( type_var "|" )? ( type_record_field ( "," type_record_field )* )? "}"
    ● type_record_field        → IDENTIFIER ":" type_function
    ● type_function            → type ( ( "->" type )* "->" type )?
    ● type                     → type_constructor | type_var | type_record | type_parens

    ● type_signature           → IDENTIFIER ":" type_function

    // Expressions
    ● expression               → let | if | lambda | pattern_matching | binary
    ● let                      → "let" ( typed_binding )+ ( "in" )? expression
    ● top_level_binding        → "external" type_signature
                               | typed_binding
    ● typed_binding            → type_signature binding
                               | binding
    ● binding                  → ( IDENTIFIER ( params )? | pattern_or ) "=" expression
    ● lambda                   → "\" params "->" expression
    ● pattern_or               → pattern_named ( "|" pattern_named )*
    ● pattern_named            → pattern ( "as" IDENTIFIER )?
    ● pattern                  → "_"
                               | IDENTIFIER
                               | NUMBER
                               | STRING
                               | ( module_name "." )? type_identifier ( pattern )*
                               | "(" pattern_or ")"
                               | "{" ( pattern_record_field ( "," pattern_record_field )* )? "}"
    ● pattern_record_field     → IDENTIFIER ( ":" pattern_or )?
    ● if                       → "if" binary "then" expression "else" expression
    ● pattern_matching         → "when" expression "is" ( pattern_matching_branch )+
    ● pattern_matching_branch  → pattern_or ( "if" expression ) "->" expression
    // Operators
    ● binary                   → unary ( binop unary )*
    ● binop                    → // parsed from Ast.Binop operator list
    ● unary                    → ( "not" | "-" )? call
    // Other primitives
    ● call                     → property_access ( property_access )*
    ● property_access          → primary properties
    ● properties               → ( "." ( IDENTIFIER ) )*
    ● primary                  → NUMBER | STRING
                               | ( module_name "." )? ( IDENTIFIER | CAPITALIZED_IDENTIFIER )
                               | "." IDENTIFIER
                               | record
                               | "(" ( expression )? ")"

    // Records
    ● record                   → "{" ( expression "|" )? ( field ( "," field )* )? "}"
    ● field                    → IDENTIFIER ( ":" expression )?
*/

#[derive(Debug, Copy, Clone)]
enum ErrorType {
    // Top level
    InvalidReplEntry,
    InvalidEndOfInput,

    // Module definition
    MissingModuleName,
    InvalidModuleNameSegment,
    MissingTopLevelModule,
    ModuleAndFileNameMismatch {
        module: ModuleFullName,
    },
    SubmoduleAndParentModuleNameMismatch {
        submodule: ModuleFullName,
    },
    InvalidModuleDefinitionLhs,
    InvalidModuleDefinition,
    InvalidExternalDefinition,

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
    InvalidAliasDefinitionType,
    InvalidRecordTypeFieldTypeOrLastRecordDelimiter {
        first_delimiter: Token,
    },
    InvalidRecordTypeFieldKeyOrExtensibleRecordVariable,
    InvalidRecordTypeFieldSeparatorOrExtensibleRecordSeparator,
    MissingRecordTypeFields,
    InvalidRecordTypeFieldKey,
    InvalidRecordTypeFieldType,
    InvalidRecordTypeFieldSeparator,
    InvalidParenthesizedTypeDelimiter {
        first_delimiter: Token,
    },
    InvalidFunctionParameterType,
    InvalidParenthesizedTypeType,
    InvalidTypeSignatureType,

    // Expressions
    MissingLetBindings,
    InvalidLetBodyIndent,
    InvalidLetBodyExpression,
    InvalidLetBindingParametersOrEqualSeparator {
        definition_identifier: Identifier,
    },
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
    InvalidPatternMatchingExpression,
    InvalidPatternMatchingIsKeyword,
    MissingPatternMatchingBranch,
    InvalidPatternMatchingBranchPattern,
    InvalidPatternMatchingBranchConditionExpression,
    InvalidPatternMatchingBranchSeparator,
    InvalidPatternMatchingBranchExpression,
    InvalidPatternMatchingBranchPatternAmountMismatch {
        num_patterns: usize,
        num_expressions: usize,
    },
    InvalidBinopRhs {
        op: Token,
    },
    InvalidUnaryRhs {
        op: Token,
    },
    InvalidPropertyAccessSeparator,
    InvalidPropertyAccessIdentifier,
    InvalidFloat(Token),
    InvalidRecordFieldSeparatorOrLastDelimiter,
    InvalidRecordFieldsOrExtensibleRecordExpression,
    InvalidExtensibleRecordFieldSeparatorOrLastDelimiter,
    InvalidParenthesizedExpression,
    InvalidParenthesizedExpressionLastDelimiter {
        first_delimiter: Token,
    },
    InvalidPropertyAccessLambdaWhitespace,
    InvalidPropertyAccessLambdaIdentifier,
    MissingRecordFields,
    InvalidRecordFieldKey,
    InvalidRecordFieldValue,

    // Patterns
    InvalidParenthesizedPattern,
    InvalidParenthesizedPatternLastDelimiter {
        first_delimiter: Token,
    },
    InvalidNamedPatternIdentifier,
    InvalidPatternInOrPattern,
    InvalidRecordPatternFieldValue,
    UselessPattern,
}

use ErrorType::*;

impl ErrorType {
    pub fn get_code_pointer(&self) -> Option<Token> {
        match self {
            InvalidRecordTypeFieldTypeOrLastRecordDelimiter { first_delimiter } => {
                Some(*first_delimiter)
            }
            InvalidParenthesizedTypeDelimiter { first_delimiter } => Some(*first_delimiter),
            InvalidParenthesizedExpressionLastDelimiter { first_delimiter } => {
                Some(*first_delimiter)
            }
            InvalidParenthesizedPatternLastDelimiter { first_delimiter } => Some(*first_delimiter),
            _ => None,
        }
    }

    pub fn to_string<'a>(&self, error: &Error, strings: &'a Strings) -> String {
        let get_lexeme = |token: &Token| token.kind.to_string(strings);
        let expected_but_found = |message: &str| {
            let lexeme = get_lexeme(&error.token);
            format!("{message}, but instead found: `{lexeme}`")
        };

        match self {
            // TODO: Store the file name in the error too, to improve the error message
            ModuleAndFileNameMismatch { module: name } => format!(
                "The module name '{name}' differs from the name of the file.\n\n\
                Module names need to match the folder and file names from the \
                file system",
                name = strings.resolve(*name)
            ),

            // TODO: Store the parent module name in the error too, to improve the error message
            SubmoduleAndParentModuleNameMismatch { submodule: name } => format!(
                "The sub-module name '{name}' differs from the name of the parent \
                module.\n\nModule names need to match their parent module path \
                names and specify their name at the end.\n\nLike this:\n\
                \n    module Admin\
                \n        module Admin.User\
                \n            module Admin.User.Id\
                \n\n",
                name = strings.resolve(*name)
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
                "Expected type variable names like `a`, or an `=` sign \
                between the name and the type definition, or just \
                something else below this type definition",
            ),

            InvalidUnionTypeDefinitionConstructor => expected_but_found(
                "Expected a constructor for the type like `type User = User Int`",
            ),

            MissingUnionTypeDefinitionConstructors => expected_but_found(
                "Expected at least one constructor \
                for the type like `type User = User Int`",
            ),

            InvalidAliasDefinitionType => expected_but_found(
                "Expected a record type for the type alias \
                like `alias User = { name : String }`",
            ),

            InvalidRecordTypeFieldTypeOrLastRecordDelimiter { first_delimiter: _ } => {
                expected_but_found(
                    // TODO: This error message can be better and also be triggered when there is
                    // an issue with the , separator while adding more record fields
                    "Expected `}` to close a record literal",
                )
            }

            InvalidRecordTypeFieldKeyOrExtensibleRecordVariable => expected_but_found(
                "Expected a record literal `{ x : Int, y : Int }` \
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

            InvalidParenthesizedTypeDelimiter { first_delimiter: _ } => expected_but_found(
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

            InvalidExternalDefinition => "External definitions can only be used with type \
                signature only definitions. Like this:\n\n    \
                external add5 : Float -> Float"
                .to_owned(),

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

            InvalidLetBindingParametersOrEqualSeparator {
                definition_identifier,
            } => expected_but_found(&format!(
                "Expected an `=` sign or list of parameters for the definition of `{name}`",
                name = definition_identifier.to_string(strings)
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

            InvalidPatternMatchingExpression => {
                expected_but_found("Expected an expression to match on")
            }

            InvalidPatternMatchingIsKeyword => expected_but_found(
                "Expected an `is` keyword after the expression \
                and before the pattern matching branches",
            ),

            MissingPatternMatchingBranch => {
                expected_but_found("Expected at least a pattern matching branch")
            }

            InvalidPatternMatchingBranchPattern => expected_but_found("Expected a pattern"),

            InvalidPatternMatchingBranchConditionExpression => expected_but_found(
                "Expected an expression for the condition \
                of the if in the pattern matching branch",
            ),

            InvalidPatternMatchingBranchSeparator => expected_but_found(
                "Expected an `->` separating the pattern \
                of the expression in the pattern matching branch",
            ),

            InvalidPatternMatchingBranchExpression => expected_but_found(
                "Expected an expression after the arrow for the \
                pattern matching branch",
            ),
            InvalidPatternMatchingBranchPatternAmountMismatch {
                num_patterns,
                num_expressions,
            } => {
                let patterns_plural = if *num_patterns != 1 { "s" } else { "" };
                let expressions_plural = if *num_expressions != 1 { "s" } else { "" };
                format!(
                    "This branch has {num_patterns} pattern{patterns_plural} \
                    but the `when` has \
                    {num_expressions} expression{expressions_plural}. \
                    Pattern matching branches need to have as many comma \
                    separated patterns as expressions in the `when` above."
                )
            }

            InvalidBinopRhs { op } => expected_but_found(&format!(
                "Expected an expression after the binary operator `{lexeme}`",
                lexeme = get_lexeme(op)
            )),

            InvalidUnaryRhs { op } => expected_but_found(&format!(
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
                expected_but_found("Expected `}` to close a record")
            }

            InvalidRecordFieldsOrExtensibleRecordExpression => expected_but_found(
                "Expected a record literal `{ x: 1, y: 2 }` \
                or a record update `{ record | x: 5, y: 4 }`",
            ),

            InvalidExtensibleRecordFieldSeparatorOrLastDelimiter =>
            // TODO: This error message can be better
            {
                expected_but_found("Expected `}` to close a record update expression")
            }

            InvalidParenthesizedExpression => {
                expected_but_found("Expected an expression inside the parenthesis")
            }

            InvalidParenthesizedExpressionLastDelimiter { first_delimiter: _ } =>
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
                expected_but_found("Expected a record field (like this `{ field: 5 }`)")
            }

            InvalidRecordFieldKey => {
                expected_but_found("Expected an identifier for the name of the field in the record")
            }

            InvalidRecordFieldValue => expected_but_found(
                "Expected an expression for the value \
                of the field in the record",
            ),

            InvalidParenthesizedPattern => {
                expected_but_found("Expected an expression inside the parenthesis")
            }

            InvalidParenthesizedPatternLastDelimiter { first_delimiter: _ } =>
            // TODO: This error message can be better
            {
                expected_but_found("Expected `)` after parenthesized pattern")
            }

            InvalidNamedPatternIdentifier => {
                expected_but_found("Expected an identifier for the name of the pattern")
            }

            InvalidPatternInOrPattern => {
                expected_but_found("Expected a valid pattern after the or `|`")
            }

            InvalidRecordPatternFieldValue => {
                expected_but_found("Expected a valid pattern for the record field value")
            }

            UselessPattern => "This incomplete pattern can't be exhaustive here.".to_owned(),
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

    pub fn to_string<'a>(&self, source: &'a Source, strings: &'a Strings) -> String {
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
        let source_name = source.path_str();
        let line = self.token.line;
        let column = self.token.column;
        format!("{source_name}:{line}:{column}\n\n{message}")
    }
}

type ParseResult<A> = Result<A, Error>;

#[derive(Debug)]
struct State<'a> {
    strings: &'a mut Strings,
    source: &'a Source,
    tokens: &'a Tokens,
    current: token::Index,
    expressions: &'a mut Expressions,
}

impl<'a> State<'a> {
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
                None => {
                    let expression =
                        self.required(Self::expression, |self_| self_.error(InvalidReplEntry))?;
                    ReplEntry::Expression(expression)
                }
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
                    ErrorType::ModuleAndFileNameMismatch {
                        module: name.full_name,
                    },
                ));
            } else if parent_module
                .map(|m| !name.valid_in_parent_module(m))
                .unwrap_or(false)
            {
                return Err(Error::new(
                    *module_token,
                    ErrorType::SubmoduleAndParentModuleNameMismatch {
                        submodule: name.full_name,
                    },
                ));
            }

            // We are going to parse a module, so save up the current expressions vec in
            // a temporary var and we will restore it after we are done with the module. This is
            // done because each module has its own expressions vec.
            let mut expressions = Expressions::new();
            std::mem::swap(&mut expressions, self.expressions);

            let exports = self.exposing()?;

            let mut imports = self.get_implicit_imports(&name.full_name);
            imports.extend(self.many(Self::import)?);

            let (mut modules, definitions, type_definitions) =
                self.module_definitions(top_level, &name, module_token, vec![], vec![], vec![])?;

            // Restore the previous expressions vec from whatever parent module we were parsing.
            // `expressions` will now be this submodule's expressions we just parsed.
            std::mem::swap(&mut expressions, self.expressions);

            let module = Module {
                name,
                exports,
                imports,
                definitions,
                type_definitions,
                expressions,
                compiled_pattern_matching_expressions: FnvHashMap::default(),
            };

            modules.push(module);

            Ok(Some(modules))
        } else {
            Ok(None)
        }
    }

    fn get_implicit_imports(&mut self, module_name: &ModuleFullName) -> Vec<Import> {
        let mut imports = vec![];
        let name = self.strings.get_or_intern("Alma");
        let span = Span {
            start: 0.into(),
            end: 0.into(),
        };

        if *module_name != name {
            imports.push(Import {
                module_name: ModuleName::new(
                    vec![CapitalizedIdentifier { name, span }],
                    self.strings,
                )
                .unwrap(),
                alias: None,
                exposing: vec![
                    Export {
                        span,
                        typ: ExportData::Type {
                            name: CapitalizedIdentifier {
                                name: self.strings.get_or_intern("Float"),
                                span,
                            },
                            constructors: vec![],
                        },
                    },
                    Export {
                        span,
                        typ: ExportData::Type {
                            name: CapitalizedIdentifier {
                                name: self.strings.get_or_intern("String"),
                                span,
                            },
                            constructors: vec![],
                        },
                    },
                    Export {
                        span,
                        typ: ExportData::Type {
                            name: CapitalizedIdentifier {
                                name: self.strings.get_or_intern("Bool"),
                                span,
                            },
                            constructors: vec![
                                CapitalizedIdentifier {
                                    name: self.strings.get_or_intern("True"),
                                    span,
                                },
                                CapitalizedIdentifier {
                                    name: self.strings.get_or_intern("False"),
                                    span,
                                },
                            ],
                        },
                    },
                ],
                span,
            });
        }

        imports
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
            Ok(Export {
                span: export.span,
                typ: ExportData::Identifier(export),
            })
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

            let constructors = constructors.unwrap_or_default();

            let start = export.span.start;
            let end = self.prev_token_index();
            Ok(Some(Export {
                typ: ExportData::Type {
                    name: export,
                    constructors,
                },
                span: Span { start, end },
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
                alias.span.end
            } else {
                module_name.last_span().end
            };

            Ok(Some(Import {
                module_name,
                alias,
                exposing,
                span: Span {
                    start: import_token_index,
                    end,
                },
            }))
        } else {
            Ok(None)
        }
    }

    fn type_def(&mut self) -> ParseResult<Option<TypeDefinition>> {
        match self.type_def_union() {
            Ok(None) => self.type_def_alias(),
            result => result,
        }
    }

    fn type_def_name(
        &mut self,
        type_def_first_token: &Token,
    ) -> ParseResult<(CapitalizedIdentifier, Vec<Identifier>)> {
        let name = self.required(
            |self_| Ok(self_.capitalized_identifier()),
            |self_| self_.error(InvalidTypeDefinitionName),
        )?;

        let vars = self.many(|self_| {
            if self_.is_token_in_same_line_or_nested_indent_from(type_def_first_token) {
                match self_.identifier() {
                    Some(variable) => Ok(Some(variable)),
                    None => Ok(None),
                }
            } else {
                Ok(None)
            }
        })?;

        Ok((name, vars))
    }

    fn type_def_union(&mut self) -> ParseResult<Option<TypeDefinition>> {
        let (first_token_index, first_token) = self.get_token();

        if self.match_token(TT::Type).is_none() {
            return Ok(None);
        }

        let (name, vars) = self.type_def_name(first_token)?;

        let (_, token) = self.get_token();

        let type_definition = if matches!(token.kind, Equal) {
            self.advance();

            let branches = self.type_union_branches()?;
            types::TypeDefinitionData::Union {
                constructors: branches,
            }
        } else if self.is_token_equal_or_less_indented_than(first_token) {
            types::TypeDefinitionData::Empty
        } else {
            return Err(self.error(InvalidTypeDefinitionTypeVarsOrEqualSeparator));
        };

        let (end_index, _) = self.prev_token();
        Ok(Some(types::TypeDefinition {
            name,
            vars,
            typ: type_definition,
            span: Span {
                start: first_token_index,
                end: end_index,
            },
        }))
    }

    fn type_def_alias(&mut self) -> ParseResult<Option<TypeDefinition>> {
        let (alias_token_index, alias_token) = self.get_token();

        if self.match_token(TT::Alias).is_none() {
            return Ok(None);
        }

        let (name, vars) = self.type_def_name(alias_token)?;

        let (_, token) = self.get_token();

        let type_definition = if matches!(token.kind, Equal) {
            self.advance();

            if let Some(record) = self.type_record()? {
                types::TypeDefinitionData::Record(record)
            } else {
                return Err(self.error(InvalidTypeDefinitionTypeVarsOrEqualSeparator));
            }
        } else if self.is_token_equal_or_less_indented_than(alias_token) {
            types::TypeDefinitionData::Empty
        } else {
            return Err(self.error(InvalidTypeDefinitionTypeVarsOrEqualSeparator));
        };

        let (end_index, _) = self.prev_token();
        Ok(Some(types::TypeDefinition {
            name,
            vars,
            typ: type_definition,
            span: Span {
                start: alias_token_index,
                end: end_index,
            },
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

            let start = name.span.start;
            let (end, _) = self.prev_token();
            Ok(Some(types::Constructor {
                name,
                params,
                span: Span { start, end },
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
                Ok(Some(types::RecordType::Record(types::Record {
                    fields: vec![],
                    span: Span {
                        start: left_brace_token_index,
                        end: next_token_index,
                    },
                })))
            }

            // Record literal
            (TT::Identifier(_), Colon) => {
                let fields = self.type_record_fields()?;

                if let Some((last_token_index, _)) = self.match_token(RightBrace) {
                    Ok(Some(types::RecordType::Record(types::Record {
                        fields,
                        span: Span {
                            start: left_brace_token_index,
                            end: last_token_index,
                        },
                    })))
                } else {
                    Err(self.error(InvalidRecordTypeFieldTypeOrLastRecordDelimiter {
                        first_delimiter: *left_brace_token,
                    }))
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
                    return Err(self.error(InvalidRecordTypeFieldTypeOrLastRecordDelimiter {
                        first_delimiter: *left_brace_token,
                    }));
                }

                Ok(Some(types::RecordType::RecordExt(types::RecordExt {
                    extension,
                    fields,
                    span: Span {
                        start: left_brace_token_index,
                        end: last_token_index,
                    },
                })))
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
            let span = ident.span;
            Ok(Some(Type::App(types::Constructor {
                name: ident,
                params: vec![],
                span,
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
            Err(self.error(InvalidParenthesizedTypeDelimiter {
                first_delimiter: *token,
            }))
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
            Ok(Type::Fun {
                params,
                ret: Box::new(ret),
            })
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
            } else if let Some(definition) = self.top_level_typed_binding()? {
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

    pub fn expression(&mut self) -> ParseResult<Option<expression::Index>> {
        if let Some(let_) = self.let_()? {
            Ok(Some(let_))
        } else if let Some(if_) = self.if_()? {
            Ok(Some(if_))
        } else if let Some(lambda) = self.lambda()? {
            Ok(Some(lambda))
        } else if let Some(pattern_matching) = self.pattern_matching()? {
            Ok(Some(pattern_matching))
        } else {
            self.binary()
        }
    }

    fn let_(&mut self) -> ParseResult<Option<expression::Index>> {
        let (let_token_index, let_token) = self.get_token();

        if self.match_token(TT::Let).is_none() {
            return Ok(None);
        }

        let idx = self.expressions.empty();

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

        let end = self.expressions.spans[body].end;
        Ok(Some(self.expressions.untyped(
            idx,
            E::Let {
                definitions: bindings,
                body,
            },
            Span {
                start: let_token_index,
                end,
            },
        )))
    }

    fn top_level_typed_binding(&mut self) -> ParseResult<Option<TypedDefinition>> {
        let (_, first_token) = self.get_token();
        let second_token = self.peek_next_token();

        let is_external = match (&first_token.kind, &second_token.kind) {
            // Commit, and error out if the external binding wasn't a type signature below
            (TT::External, TT::Identifier(_)) => {
                self.advance();
                true
            }
            // Don't commit if the next token is not an identifier, may be picked up later by type
            // definitions
            (TT::External, _) => return Ok(None),
            (_, _) => false,
        };

        let definition = self.typed_binding()?;
        if is_external {
            match definition {
                Some(TypedDefinition::TypeSignature(typ)) => {
                    Ok(Some(TypedDefinition::External(typ)))
                }
                Some(_) | None => Err(Error::new(*first_token, InvalidExternalDefinition)),
            }
        } else {
            Ok(definition)
        }
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
                                Pattern {
                                    data: PatternData::Identifier(identifier),
                                    ..
                                },
                                _,
                            )
                            | Definition::Lambda(identifier, _)
                                if identifier.name == signature.name.name =>
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
        let pattern = self.pattern_or()?;

        if let Some(pattern) = &pattern {
            self.error_if_useless_binding_pattern(pattern)?;
        }

        let definition = match pattern {
            Some(Pattern {
                data: PatternData::Identifier(identifier),
                ..
            }) => {
                // Peek to see if it is just an identifier and =, and return a pattern
                let (_, equal_token) = self.get_token();
                if equal_token.kind == Equal {
                    let expr = self.binding_rhs()?;
                    let span = identifier.span;
                    Some(Definition::Pattern(
                        Pattern {
                            data: PatternData::Identifier(identifier),
                            span,
                        },
                        expr,
                    ))
                } else {
                    // Otherwise this is a lambda lhs, identifier + params
                    let params = self.one_or_many(Self::pattern, |self_| {
                        self_.error(InvalidLetBindingParametersOrEqualSeparator {
                            definition_identifier: identifier,
                        })
                    })?;

                    for param in &params {
                        self.error_if_useless_binding_pattern(param)?;
                    }

                    let expr = self.binding_rhs()?;

                    Some(Definition::Lambda(
                        identifier,
                        Lambda {
                            parameters: params,
                            body: expr,
                        },
                    ))
                }
            }
            Some(pattern) => {
                let binding_rhs = self.binding_rhs()?;
                Some(Definition::Pattern(pattern, binding_rhs))
            }
            None => None,
        };

        Ok(definition)
    }
    fn binding_rhs(&mut self) -> ParseResult<expression::Index> {
        if self.match_token(TT::Equal).is_none() {
            return Err(self.error(InvalidLetBindingSeparator));
        }

        self.required(Self::expression, |self_| self_.error(InvalidLetBindingRhs))
    }

    fn if_(&mut self) -> ParseResult<Option<expression::Index>> {
        let token_index = self.get_token_index();

        if self.match_token(TT::If).is_none() {
            return Ok(None);
        }

        let idx = self.expressions.empty();

        let condition = self.required(Self::binary, |self_| self_.error(InvalidIfCondition))?;

        if self.match_token(TT::Then).is_none() {
            return Err(self.error(InvalidIfThen));
        }

        let then = self.required(Self::expression, |self_| self_.error(InvalidThenBranch))?;

        if self.match_token(TT::Else).is_none() {
            return Err(self.error(InvalidIfElse));
        }

        let else_ = self.required(Self::expression, |self_| self_.error(InvalidIfElseBranch))?;

        let end = self.expressions.spans[else_].end;
        Ok(Some(self.expressions.untyped(
            idx,
            E::If {
                condition,
                then,
                else_,
            },
            Span {
                start: token_index,
                end,
            },
        )))
    }

    fn lambda(&mut self) -> ParseResult<Option<expression::Index>> {
        let token_index = self.get_token_index();

        if self.match_token(Backslash).is_none() {
            return Ok(None);
        }

        let idx = self.expressions.empty();

        let params =
            self.one_or_many(Self::pattern, |self_| self_.error(MissingLambdaParamaters))?;

        for param in &params {
            self.error_if_useless_binding_pattern(param)?;
        }

        if self.match_token(Arrow).is_none() {
            return Err(self.error(InvalidLambdaArrow));
        }

        let body = self.required(Self::expression, |self_| self_.error(InvalidLambdaBody))?;

        let end = self.expressions.spans[body].end;
        Ok(Some(self.expressions.untyped(
            idx,
            E::Lambda(Lambda {
                parameters: params,
                body,
            }),
            Span {
                start: token_index,
                end,
            },
        )))
    }

    fn pattern_matching(&mut self) -> ParseResult<Option<expression::Index>> {
        let (when_token_index, when_token) = self.get_token();

        if self.match_token(TT::When).is_none() {
            return Ok(None);
        }

        let idx = self.expressions.empty();

        let expressions: Vec<_> = self
            .one_or_many_sep(
                |self_| Ok(self_.match_token(TT::Comma)),
                |self_| {
                    self_.required(Self::expression, |self_| {
                        self_.error(InvalidPatternMatchingExpression)
                    })
                },
                |self_| self_.error(InvalidPatternMatchingExpression),
            )?
            .into_iter()
            .map(|e| e)
            .collect();

        if self.match_token(TT::Is).is_none() {
            return Err(self.error(InvalidPatternMatchingIsKeyword));
        }

        let branches = self.one_or_many(
            |self_| {
                if self_.is_token_in_same_line_or_nested_indent_from(when_token) {
                    self_.pattern_matching_branch()
                } else {
                    Ok(None)
                }
            },
            |self_| self_.error(MissingPatternMatchingBranch),
        )?;

        // Verify we have only as many branch patterns as expressions
        let num_expressions = expressions.len();
        for branch in &branches {
            if branch.patterns.len() != num_expressions {
                return Err(Error::new(
                    self.tokens[branch.span.start],
                    InvalidPatternMatchingBranchPatternAmountMismatch {
                        num_expressions,
                        num_patterns: branch.patterns.len(),
                    },
                ));
            }
        }

        let end = branches.last().unwrap().span.end;
        Ok(Some(self.expressions.untyped(
            idx,
            E::PatternMatching {
                conditions: expressions,
                branches,
            },
            Span {
                start: when_token_index,
                end,
            },
        )))
    }

    fn pattern_matching_branch(&mut self) -> ParseResult<Option<PatternMatchingBranch>> {
        let token_index = self.get_token_index();

        let patterns = self.many_sep(
            |self_| Ok(self_.match_token(TT::Comma)),
            Self::pattern_or,
            |self_| self_.error(InvalidPatternMatchingBranchPattern),
        )?;
        if patterns.is_empty() {
            return Ok(None);
        };

        let condition = if self.match_token(TT::If).is_some() {
            Some(self.required(Self::expression, |self_| {
                self_.error(InvalidPatternMatchingBranchConditionExpression)
            })?)
        } else {
            None
        };

        if self.match_token(TT::Arrow).is_none() {
            return Err(self.error(InvalidPatternMatchingBranchSeparator));
        }

        let body = self.required(Self::expression, |self_| {
            self_.error(InvalidPatternMatchingBranchExpression)
        })?;

        let end = self.expressions.spans[body].end;

        Ok(Some(PatternMatchingBranch {
            span: Span {
                start: token_index,
                end,
            },
            patterns,
            condition: condition.map(|c| c),
            body,
        }))
    }

    fn error_if_useless_binding_pattern(&mut self, pattern: &Pattern) -> ParseResult<()> {
        if pattern.data.is_useless_in_bindings() {
            Err(Error::new(self.tokens[pattern.span.start], UselessPattern))
        } else {
            Ok(())
        }
    }

    fn pattern_or(&mut self) -> ParseResult<Option<Pattern>> {
        let token_index = self.get_token_index();
        let mut patterns = self.many_sep(
            |self_| Ok(self_.match_token(TT::Pipe)),
            Self::pattern_named,
            |self_| self_.error(InvalidPatternInOrPattern),
        )?;
        if patterns.is_empty() {
            Ok(None)
        } else if patterns.len() == 1 {
            Ok(Some(patterns.pop().unwrap()))
        } else {
            let end_token_index = patterns.last().unwrap().span.end;
            Ok(Some(Pattern {
                data: PatternData::Or(patterns),
                span: Span {
                    start: token_index,
                    end: end_token_index,
                },
            }))
        }
    }

    fn pattern_named(&mut self) -> ParseResult<Option<Pattern>> {
        let token_index = self.get_token_index();
        if let Some(pattern) = self.pattern()? {
            if self.match_token(TT::As).is_some() {
                if let Some(identifier) = self.identifier() {
                    let end_token_index = identifier.span.end;
                    Ok(Some(Pattern {
                        data: PatternData::Named {
                            pattern: Box::new(pattern),
                            name: identifier,
                        },
                        span: Span {
                            start: token_index,
                            end: end_token_index,
                        },
                    }))
                } else {
                    Err(self.error(InvalidNamedPatternIdentifier))
                }
            } else {
                Ok(Some(pattern))
            }
        } else {
            Ok(None)
        }
    }

    fn pattern(&mut self) -> ParseResult<Option<Pattern>> {
        let (token_index, token) = self.get_token();

        if self.match_token(TT::LeftParen).is_some() {
            if let Some(pattern) = self.pattern_or()? {
                if self.match_token(RightParen).is_some() {
                    Ok(Some(pattern))
                } else {
                    Err(self.error(InvalidParenthesizedPatternLastDelimiter {
                        first_delimiter: *token,
                    }))
                }
            } else {
                Err(self.error(InvalidParenthesizedPattern))
            }
        } else if self.match_token(TT::Underscore).is_some() {
            Ok(Some(Pattern {
                data: PatternData::Hole,
                span: Span {
                    start: token_index,
                    end: token_index,
                },
            }))
        } else if let Some(identifier) = self.identifier() {
            Ok(Some(Pattern {
                data: PatternData::Identifier(identifier),
                span: Span {
                    start: token_index,
                    end: token_index,
                },
            }))
        } else if let TT::String_(s) = token.kind {
            self.advance();
            Ok(Some(Pattern {
                data: PatternData::String_(s),
                span: Span {
                    start: token_index,
                    end: token_index,
                },
            }))
        } else if let TT::Float(n) = token.kind {
            self.strings
                .resolve(n)
                .parse::<f64>()
                .map_err(|_| Error::new(*token, InvalidFloat(*token)))?;

            self.advance();
            Ok(Some(Pattern {
                data: PatternData::Float(n),
                span: Span {
                    start: token_index,
                    end: token_index,
                },
            }))
        } else if self.match_token(TT::LeftBrace).is_some() {
            let fields = self.record_fields(
                Self::pattern_or,
                |_, identifier| {
                    let span = identifier.span;
                    Pattern {
                        span,
                        data: PatternData::Identifier(identifier),
                    }
                },
                ErrorType::InvalidRecordPatternFieldValue,
            )?;

            if let Some((right_brace_token_index, _)) = self.match_token(RightBrace) {
                Ok(Some(Pattern {
                    span: Span {
                        start: token_index,
                        end: right_brace_token_index,
                    },
                    data: PatternData::Record(fields),
                }))
            } else {
                Err(self.error(InvalidRecordFieldSeparatorOrLastDelimiter))
            }
        } else if let Some(pattern) = self.type_pattern()? {
            Ok(Some(pattern))
        } else {
            Ok(None)
        }
    }

    fn type_pattern(&mut self) -> ParseResult<Option<Pattern>> {
        let (token_index, token) = self.get_token();
        if matches!(token.kind, TT::CapitalizedIdentifier(_)) {
            let (module, constructor) = self.module_qualified_capitalized_identifier()?;
            let params = self.many(Self::pattern)?;

            let end = params
                .last()
                .map(|p| p.span.end)
                .unwrap_or_else(|| constructor.span.end);
            Ok(Some(Pattern {
                data: PatternData::Type {
                    module,
                    constructor,
                    params,
                },
                span: Span {
                    start: token_index,
                    end,
                },
            }))
        } else {
            Ok(None)
        }
    }

    fn binary(&mut self) -> ParseResult<Option<expression::Index>> {
        if let Some(expr) = self.unary()? {
            self.many(Self::binary_step).map(|mut binops| {
                // Make the binops options to be able to take them later
                let mut binops = binops.drain(..).map(Some).collect();

                Some(self.organize_binops(expr, &mut binops, &mut (0), 0))
            })
        } else {
            Ok(None)
        }
    }
    fn binary_step(&mut self) -> ParseResult<Option<(Span, Binop, expression::Index)>> {
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

            let span = Span {
                start: token_index,
                end: token_index,
            };

            let right = self.required(Self::unary, |self_| {
                self_.error(InvalidBinopRhs { op: *token })
            })?;

            Ok(Some((span, op, right)))
        } else {
            Ok(None)
        }
    }
    fn organize_binops(
        &mut self,
        left: expression::Index,
        binops: &mut Vec<Option<(Span, Binop, expression::Index)>>,
        current: &mut usize,
        min_precedence: u32,
    ) -> expression::Index {
        // https://eli.thegreenplace.net/2012/08/02/parsing-expressions-by-precedence-climbing
        // better than the wikipedia article for precedence climibing

        let mut left = left;

        loop {
            let next = binops.get_mut(*current);

            match next {
                Some(op_and_expr) => {
                    let keep_parsing = matches!(op_and_expr, Some((_op_span, op, _rhs)) if op.precedence >= min_precedence);

                    if keep_parsing {
                        // Take ownership of the op and rhs
                        let (op_span, op, rhs) = op_and_expr.take().unwrap();

                        *current += 1;

                        let next_min_precedence = op.precedence
                            + if op.associativity == Associativity::Ltr {
                                1
                            } else {
                                0
                            };

                        let right = self.organize_binops(rhs, binops, current, next_min_precedence);

                        let (start, end) = (
                            self.expressions.spans[left].start,
                            self.expressions.spans[right].end,
                        );

                        let span = Span {
                            start: 0.into(),
                            end: 0.into(),
                        };
                        let idx = self.expressions.empty();
                        let binary_expr = self.expressions.untyped(
                            // TODO: This will end up in reverse order in the expressions vec. Should
                            // probably be in tree traversal order instead
                            idx,
                            E::Identifier {
                                module: Some(
                                    // TODO: Creating this every time we have a binary expression
                                    // is awful. These could be pre-stored somewhere and just
                                    // cloned. Or ideally this fake expression we create here to
                                    // help with the type checking later is no longer necessary and
                                    // we can get rid of it and make it up as needed in the
                                    // inference pass.
                                    ModuleName::new(
                                        vec![CapitalizedIdentifier {
                                            span,
                                            name: self.strings.get_or_intern("Alma"),
                                        }],
                                        self.strings,
                                    )
                                    .unwrap(),
                                ),
                                identifier: AnyIdentifier::Identifier(
                                    op.get_function_identifier(self.strings, op_span),
                                ),
                            },
                            op_span,
                        );
                        let idx = self.expressions.empty();
                        left = self.expressions.untyped(
                            // TODO: This will end up in reverse order in the expressions vec. Should
                            // probably be in tree traversal order instead
                            idx,
                            E::Binary {
                                expression: binary_expr,
                                op,
                                arguments: ([left, right]),
                            },
                            Span { start, end },
                        )
                    } else {
                        break;
                    }
                }
                None => break,
            }
        }

        left
    }

    fn unary(&mut self) -> ParseResult<Option<expression::Index>> {
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
        let u = u.map(|u| (u, self.expressions.empty()));

        match (u, self.call()?) {
            (Some((op, idx)), Some(expr)) => {
                let end = self.expressions.spans[expr].end;
                let span = Span {
                    start: token_index,
                    end: token_index,
                };
                Ok(Some(self.expressions.untyped(
                    idx,
                    E::Unary {
                        op: Unary { typ: op, span },
                        expression: expr,
                    },
                    Span {
                        start: token_index,
                        end,
                    },
                )))
            }
            (None, Some(expr)) => Ok(Some(expr)),
            (Some(_), None) => Err(self.error(InvalidUnaryRhs { op: *token })),
            (None, None) => Ok(None),
        }
    }

    fn call(&mut self) -> ParseResult<Option<expression::Index>> {
        let (_, token) = self.get_token();
        let idx = self.expressions.empty();

        if let Some(expr) = self.property_access()? {
            let args = self.many(|self_| self_.argument(token))?;

            if args.is_empty() {
                Ok(Some(expr))
            } else {
                let last_arg = &args[args.len() - 1];

                let (start, end) = (
                    self.expressions.spans[expr].start,
                    self.expressions.spans[*last_arg].end,
                );
                Ok(Some(self.expressions.untyped(
                    idx,
                    E::FnCall {
                        function: expr,
                        arguments: args.into_iter().map(|a| a).collect(),
                    },
                    Span { start, end },
                )))
            }
        } else {
            Ok(None)
        }
    }

    fn argument(&mut self, first_token: &Token) -> ParseResult<Option<expression::Index>> {
        if self.is_token_in_same_line_or_nested_indent_from(first_token) {
            match self.property_access()? {
                Some(arg) => Ok(Some(arg)),
                // We tried to get an argument, but there was no match, or it was not well indented
                None => Ok(None),
            }
        } else {
            Ok(None)
        }
    }

    fn property_access(&mut self) -> ParseResult<Option<expression::Index>> {
        if let Some(expr) = self.primary()? {
            self.properties(expr).map(Some)
        } else {
            Ok(None)
        }
    }
    fn properties(&mut self, expr: expression::Index) -> ParseResult<expression::Index> {
        let mut properties = Vec::new();
        let start = self.expressions.spans[expr].start;
        let mut end = self.expressions.spans[expr].end;
        while let Some(identifier) = self.property(self.tokens[end].end)? {
            end = identifier.span.end;
            properties.push((identifier, Span { start, end }));
        }

        if !properties.is_empty() {
            let mut idxs = Vec::new();
            idxs.resize_with(properties.len(), || self.expressions.empty());

            let expr = properties.into_iter().rev().zip(idxs).rev().fold(
                expr,
                |expr, ((identifier, span), idx)| {
                    self.expressions.untyped(
                        idx,
                        E::PropertyAccess {
                            expression: expr,
                            property: identifier,
                        },
                        span,
                    )
                },
            );
            Ok(expr)
        } else {
            Ok(expr)
        }
    }
    fn property(&mut self, prev_end_token_position: usize) -> ParseResult<Option<Identifier>> {
        let (_, dot_token) = self.get_token();
        match dot_token.kind {
            // Dot token without whitespace between the prev token is a record access
            Dot if dot_token.start == prev_end_token_position => {
                self.advance();

                let (identifier_token_index, identifier_token) = self.get_token();
                match identifier_token.kind {
                    // Dot token without whitespace between it and the identifier is a prop access
                    TT::Identifier(name) if identifier_token.start == dot_token.end => {
                        self.advance();
                        Ok(Some(Identifier {
                            name,
                            span: Span {
                                start: identifier_token_index,
                                end: identifier_token_index,
                            },
                        }))
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

    fn primary(&mut self) -> ParseResult<Option<expression::Index>> {
        let (token_index, token) = self.get_token();

        match token.kind {
            TT::Float(lexeme) => {
                let n = self
                    .strings
                    .resolve(lexeme)
                    .parse::<f64>()
                    .map_err(|_| Error::new(*token, InvalidFloat(*token)))?;

                self.advance();

                let idx = self.expressions.empty();
                Ok(Some(self.expressions.untyped(
                    idx,
                    E::Float(n),
                    Span {
                        start: token_index,
                        end: token_index,
                    },
                )))
            }

            TT::Identifier(lexeme) => {
                self.advance();

                let span = Span {
                    start: token_index,
                    end: token_index,
                };
                let identifier = Identifier { name: lexeme, span };

                let idx = self.expressions.empty();
                Ok(Some(self.expressions.untyped(
                    idx,
                    E::Identifier {
                        module: None,
                        identifier: AnyIdentifier::Identifier(identifier),
                    },
                    Span {
                        start: token_index,
                        end: token_index,
                    },
                )))
            }

            TT::CapitalizedIdentifier(_) => {
                let (module, identifier) = self.module_qualified_any_identifier()?;

                let (start, end) = if let Some(module) = &module {
                    let first = &module.parts[0];
                    (first.span.start, identifier.span().end)
                } else {
                    let span = identifier.span();
                    (span.start, span.end)
                };
                let idx = self.expressions.empty();
                Ok(Some(self.expressions.untyped(
                    idx,
                    E::Identifier { module, identifier },
                    Span { start, end },
                )))
            }

            TT::String_(string) => {
                self.advance();
                let idx = self.expressions.empty();
                Ok(Some(self.expressions.untyped(
                    idx,
                    E::String_(string),
                    Span {
                        start: token_index,
                        end: token_index,
                    },
                )))
            }

            LeftBrace => {
                self.advance();
                let (next_token_index, next_token) = self.get_token();
                let second_next_token = self.peek_next_token();

                let idx = self.expressions.empty();

                match (next_token.kind, second_next_token.kind) {
                    // Unit record
                    (RightBrace, _) => {
                        self.advance();

                        Ok(Some(self.expressions.untyped(
                            idx,
                            E::Record { fields: vec![] },
                            Span{ start: token_index, end: next_token_index },
                        )))
                    }

                    // Record literal:
                    // Record field with value
                    (TT::Identifier(_), Colon)
                    | (TT::Identifier(_), Equal)
                    // Record field punning
                    | (TT::Identifier(_), Comma)
                    | (TT::Identifier(_), RightBrace) => {
                        let fields = self.record_fields(
                            Self::expression,
                            |self_, identifier| {
                                let span = identifier.span;
                                let idx = self_.expressions.empty();
                                self_.expressions.untyped(
                                    idx,
                                    E::Identifier {
                                        module: None,
                                        identifier: AnyIdentifier::Identifier(identifier),
                                    },
                                    span,
                                )
                            },
                            ErrorType::InvalidRecordFieldValue,
                        )?;

                        if let Some((right_brace_token_index, _)) = self.match_token(RightBrace) {
                            Ok(Some(self.expressions.untyped(
                                idx,
                                E::Record { fields },
                                Span{ start: token_index, end: right_brace_token_index },
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

                        let fields = self.record_fields(
                            Self::expression,
                            |self_, identifier| {
                                let span = identifier.span;
                                let idx = self_.expressions.empty();
                                self_.expressions.untyped(
                                    idx,
                                    E::Identifier {
                                        module: None,
                                        identifier: AnyIdentifier::Identifier(identifier),
                                    },
                                    span,
                                )
                            },
                            ErrorType::InvalidRecordFieldValue,
                        )?;

                        if let Some((right_brace_token_index, _)) = self.match_token(RightBrace) {
                            Ok(Some(self.expressions.untyped(
                                idx,
                                E::RecordUpdate {
                                    record,
                                    fields,
                                },
                                Span{ start: token_index, end: right_brace_token_index },
                            )))
                        } else {
                            Err(self.error(InvalidExtensibleRecordFieldSeparatorOrLastDelimiter))
                        }
                    }
                }
            }

            LeftParen => {
                self.advance();

                // Parenthesized expression
                let expr = self.required(Self::expression, |self_| {
                    self_.error(InvalidParenthesizedExpression)
                })?;

                if self.match_token(RightParen).is_some() {
                    Ok(Some(expr))
                } else {
                    Err(self.error(InvalidParenthesizedExpressionLastDelimiter {
                        first_delimiter: *token,
                    }))
                }
            }

            Dot => {
                self.advance();
                let (identifier_token_index, identifier_token) = self.get_token();

                match identifier_token.kind {
                    // Dot token without whitespace between it and the identifier is a lambda w/ prop access
                    TT::Identifier(lexeme) if identifier_token.start == token.end => {
                        self.advance();

                        let name_identifier = Identifier {
                            name: lexeme,
                            span: Span {
                                start: identifier_token_index,
                                end: identifier_token_index,
                            },
                        };

                        let idx = self.expressions.empty();
                        Ok(Some(self.expressions.untyped(
                            idx,
                            E::PropertyAccessLambda {
                                property: name_identifier,
                            },
                            Span {
                                start: token_index,
                                end: identifier_token_index,
                            },
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

    fn record_fields<Parser, Pun, T>(
        &mut self,
        field_value_parser: Parser,
        get_value_for_field_pun: Pun,
        invalid_field_value_error: ErrorType,
    ) -> ParseResult<Vec<(Identifier, T)>>
    where
        Parser: Fn(&mut Self) -> ParseResult<Option<T>> + Copy,
        Pun: Fn(&mut Self, Identifier) -> T + Copy,
    {
        self.one_or_many_sep(
            |self_| Ok(self_.match_token(TT::Comma)),
            |self_| {
                let (name, value) = self_.record_field(
                    field_value_parser,
                    get_value_for_field_pun,
                    invalid_field_value_error,
                )?;
                Ok((name, value))
            },
            |self_| self_.error(MissingRecordFields),
        )
    }

    fn record_field<Parser, Pun, T>(
        &mut self,
        field_value_parser: Parser,
        get_value_for_field_pun: Pun,
        invalid_field_value_error: ErrorType,
    ) -> ParseResult<(Identifier, T)>
    where
        Parser: Fn(&mut Self) -> ParseResult<Option<T>>,
        Pun: Fn(&mut Self, Identifier) -> T,
    {
        let identifier = self.required(
            |self_| Ok(self_.identifier()),
            |self_| self_.error(InvalidRecordFieldKey),
        )?;

        let (_, separator) = self.get_token();
        if !matches!(separator.kind, Colon | Equal) {
            let value = get_value_for_field_pun(self, identifier);
            return Ok((identifier, value));
        }
        self.advance();

        let expr = self.required(field_value_parser, |self_| {
            self_.error(invalid_field_value_error)
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
        start: token::Index,
        tokens: &'a TiSlice<token::Index, Token>,
        names: Vec<CapitalizedIdentifier>,
    ) -> ParseResult<ModuleName> {
        ModuleName::new(names, self.strings).map_err(|(i, names)| {
            let name = &names[i];
            let token_relative_index = tokens
                .iter()
                .position(|t| t.start == self.tokens[name.span.start].start)
                .unwrap();
            Error::new(
                self.tokens[start + token_relative_index.into()],
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
            let name_identifier = Identifier {
                name: *lexeme,
                span: Span {
                    start: identifier_token_index,
                    end: identifier_token_index,
                },
            };
            Some(name_identifier)
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
            let name_identifier = CapitalizedIdentifier {
                name: *lexeme,
                span: Span {
                    start: identifier_token_index,
                    end: identifier_token_index,
                },
            };
            Some(name_identifier)
        } else {
            None
        }
    }

    fn module_qualified_any_identifier(
        &mut self,
    ) -> ParseResult<(Option<ModuleName>, AnyIdentifier)> {
        // TODO: This is slightly wrong, since it will validate the last module segment as
        // a module name, even though it can be just a CapitalizedIdentifier which can have
        // more characters.
        // Maybe what should happen is that I should restrain capitalized identifiers to
        // have the same kinds of characters as the module name part.
        let mut module = self.module_name(|| InvalidModuleNameSegment)?;

        if module.parts.len() == 1 {
            // Is there a normal identifier afterwards? If so it is a Module.ident, if not,
            // the module's only part is a capitalized identifier.
            let module_end_position = self.tokens[module.last_span().end].end;
            if let Some(ident) = self.property(module_end_position)? {
                Ok((Some(module), AnyIdentifier::Identifier(ident)))
            } else {
                let first = module.parts.swap_remove(0);
                Ok((None, AnyIdentifier::CapitalizedIdentifier(first)))
            }
        } else {
            // Is there a normal identifier afterwards? If so the whole module is fine, if
            // not, the module's last part is a capitalized identifier.
            let module_end_position = self.tokens[module.last_span().end].end;
            if let Some(ident) = self.property(module_end_position)? {
                Ok((Some(module), AnyIdentifier::Identifier(ident)))
            } else {
                let ident = module.parts.pop().unwrap();
                let module = ModuleName::new(module.parts, self.strings).expect("Internal parser error: Module should be valid as it is being built from a previously built module");
                Ok((Some(module), AnyIdentifier::CapitalizedIdentifier(ident)))
            }
        }
    }

    fn module_qualified_capitalized_identifier(
        &mut self,
    ) -> ParseResult<(Option<ModuleName>, CapitalizedIdentifier)> {
        // TODO: This is slightly wrong, since it will validate the last module segment as
        // a module name, even though it can be just a CapitalizedIdentifier which can have
        // more characters.
        // Maybe what should happen is that I should restrain capitalized identifiers to
        // have the same kinds of characters as the module name part.
        let mut module = self.module_name(|| InvalidModuleNameSegment)?;

        if module.parts.len() == 1 {
            let first = module.parts.swap_remove(0);
            Ok((None, first))
        } else {
            let ident = module.parts.pop().unwrap();
            let module = ModuleName::new(module.parts, self.strings).expect("Internal parser error: Module should be valid as it is being built from a previously built module");
            Ok((Some(module), ident))
        }
    }

    // Utilities

    fn prev_token(&self) -> (token::Index, &'a Token) {
        self.prev_non_comment_token(self.current)
    }
    fn prev_token_index(&self) -> token::Index {
        let (idx, _) = self.prev_non_comment_token(self.current);
        idx
    }

    fn get_token(&self) -> (token::Index, &'a Token) {
        self.next_non_comment_token(self.current)
    }
    fn get_token_index(&self) -> token::Index {
        let (idx, _) = self.next_non_comment_token(self.current);
        idx
    }

    fn peek_next_token(&self) -> &'a Token {
        let (first, token) = self.next_non_comment_token(self.current);
        match token.kind {
            Eof => token,
            _ => {
                let (_, token) = self.next_non_comment_token(first + 1.into());
                token
            }
        }
    }

    fn advance(&mut self) {
        let (i, token) = self.next_non_comment_token(self.current);
        match token.kind {
            Eof => (),
            _ => self.current = i + 1.into(),
        };
    }

    fn next_non_comment_token(&self, start: token::Index) -> (token::Index, &'a Token) {
        let mut i = start;
        let mut token;
        loop {
            token = &self.tokens[i];
            match token.kind {
                Eof => break,
                Comment => {
                    i += 1.into();
                    continue;
                }
                _ => {
                    break;
                }
            };
        }
        (i, token)
    }

    fn prev_non_comment_token(&self, start: token::Index) -> (token::Index, &'a Token) {
        let mut i = start - 1.into();
        let mut token;
        loop {
            token = self
                .tokens
                .get(i)
                .expect("Internal parser error: Out of bounds access to tokens array");
            match token.kind {
                Eof => break,
                Comment => {
                    i -= 1.into();
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

    fn many_sep<Parser, ParserResult, Delimiter, DelimiterParser, E>(
        &mut self,
        delimiter: DelimiterParser,
        parse: Parser,
        on_parse_non_commit_after_first: E,
    ) -> ParseResult<Vec<ParserResult>>
    where
        Parser: Fn(&mut Self) -> ParseResult<Option<ParserResult>>,
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
            if let Some(item) = parse(self)? {
                items.push(item);
            } else {
                // Parser didn't commit. If it is the first item, then fine, but otherwise need to
                // error out since we already ate a delimiter.
                if i == 0 {
                    break;
                } else {
                    return Err(on_parse_non_commit_after_first(self));
                }
            }
            i += 1;
        }

        Ok(items)
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

    fn match_token(&mut self, typ: token::Type) -> Option<(token::Index, &'a Token)> {
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

pub fn parse<'a>(
    source: &'a Source,
    tokens: &'a Tokens,
    strings: &'a mut Strings,
) -> ParseResult<(Module, Vec<Module>)> {
    let mut expressions = Expressions::new();
    let mut parser = State {
        strings,
        source,
        tokens,
        current: 0.into(),
        expressions: &mut expressions,
    };

    parser.file()
}

pub fn parse_repl<'a>(
    source: &'a Source,
    tokens: &'a Tokens,
    strings: &'a mut Strings,
    expressions: &'a mut Expressions,
) -> ParseResult<ReplEntry> {
    let mut parser = State {
        strings,
        source,
        tokens,
        current: 0.into(),
        expressions,
    };

    parser.repl_entry()
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::tokenizer;

    pub fn parse_expression<'a>(
        source: &'a Source,
        tokens: &'a Tokens,
        strings: &'a mut Strings,
        expressions: &'a mut Expressions,
    ) -> ParseResult<expression::Index> {
        let mut parser = State {
            strings,
            source,
            tokens,
            current: 0.into(),
            expressions,
        };

        let result = parser.required(State::expression, |self_| {
            self_.error(InvalidModuleDefinitionLhs)
        })?;
        parser.eof(result)
    }

    mod test_expression {
        use crate::ast::TestToStringContext;

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
            assert_snapshot!(parse("hello {}"));
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
            assert_snapshot!(parse("{ x }"));
            assert_snapshot!(parse("{ x, y }"));
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
            assert_snapshot!(parse("{ r | x }"));
            assert_snapshot!(parse("{ r | x, y }"));
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

        #[test]
        fn test_pattern_matching_if_condition() {
            assert_snapshot!(parse(
                "\
when 5 is
    x if x > 3 -> 5
"
            ));
        }

        #[test]
        fn test_pattern_matching_multiple_values() {
            assert_snapshot!(parse(
                "\
when 5, is
    x if x > 3 -> 5
"
            ));
            assert_snapshot!(parse(
                "\
when 5, 5 is
    x -> 5
"
            ));
            assert_snapshot!(parse(
                "\
when 5, 5 is
    x, y -> x + y
"
            ));
            assert_snapshot!(parse(
                "\
when 5, 5 is
    x,  -> x + y
"
            ));
        }

        #[test]
        fn test_pattern_matching_record_pattern() {
            assert_snapshot!(parse(
                "\
when 5 is
    { hello } -> hello
    { world: banana } -> banana
"
            ));
            assert_snapshot!(parse(
                "\
when 5 is
    { world: , } -> banana
"
            ));
        }

        fn parse(code: &str) -> String {
            let source = Source::new("Test.alma", code.to_string());
            let mut strings = Strings::new();
            let mut tokens = Tokens::new();
            let mut expressions = Expressions::new();
            tokenizer::parse(&source, &mut strings, &mut tokens).unwrap();
            let result = match &parse_expression(&source, &tokens, &mut strings, &mut expressions) {
                Ok(ast) => {
                    let ctx = TestToStringContext {
                        indent: 0,
                        tokens: &tokens,
                        expressions: &expressions,
                        strings: &strings,
                    };
                    let ast = expressions.values[*ast].to_test_string(*ast, &ctx);
                    format!("{ast}")
                }
                Err(error) => {
                    let error_str = error.to_string(&source, &strings);
                    format!("{error_str}\n\n{error:#?}")
                }
            };
            format!("Input:\n\n{code}\n\nResult:\n\n{result}")
        }
    }

    mod test_module_parser {
        use crate::ast::TestToStringContext;

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
module Test

module Test.TestInner"
            ));
            assert_snapshot!(parse(
                "\
module Test

module Test.TestInner

  a = 1"
            ));
            assert_snapshot!(parse(
                "\
module Test

module Test.TestInner

  a = 1

a = 1
"
            ));
            assert_snapshot!(
                "parsing errors",
                parse(
                    "\
module Test

module Test.TestInner

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
module Test

module Test.Test1
  a = 1

  module Test.Test1.Test1Test
    b = 1

module Test.Test2
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
module Test

module Testo.Banana
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
                "no definition, followed by a type var",
                parse(
                    "
module Test

type Banana a b
"
                )
            );
            assert_snapshot!(
                "no definition, followed by a look-alike type var that is in the next line",
                parse(
                    "
module Test

type Banana a b

hello : Float
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

alias Fruit = {}
"
                )
            );
            assert_snapshot!(
                "unclosed record",
                parse(
                    "
module Test

alias Fruit = {
"
                )
            );
            assert_snapshot!(
                "missing value on field",
                parse(
                    "
module Test

alias Fruit = { name }
"
                )
            );
            assert_snapshot!(
                "single field",
                parse(
                    "
module Test

alias Fruit = { name : String }
"
                )
            );
            assert_snapshot!(
                "many fields",
                parse(
                    "
module Test

alias Fruit = { name : String , banana: Phone }
"
                )
            );
        }

        #[test]
        fn test_extensible_record() {
            assert_snapshot!(parse(
                "
module Test

alias Fruit a = { a | name : String , banana: Phone }
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

alias Fruit a = { test : Banana a (Phone a) }
"
            ));
            assert_snapshot!(parse(
                "
module Test

alias Fruit a = { test : (Banana a (Phone a)) }
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

alias Fruit a b = { f : a -> b }
"
            ));
            assert_snapshot!(parse(
                "\
module Test exposing (Fruit)

alias Fruit a b = { f : (a -> b) }
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
        fn test_external_definitions() {
            assert_snapshot!(parse(
                "\
module Test exposing (main)

external test : Bool

external test2 : Bool -> Bool
"
            ));
            assert_snapshot!(parse(
                "\
module Test exposing (main)

external test : Bool
test = True
"
            ));
            assert_snapshot!(parse(
                "\
module Test exposing (main)

external test2 : Bool -> Bool
test2 a = not a
"
            ));
            assert_snapshot!(parse(
                "\
module Test exposing (main)

external test2 a = not a
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

        #[test]
        fn test_parenthesized_pattern() {
            assert_snapshot!(parse(
                "\
module Test

(hello) = world

(_) = world
"
            ));
        }

        #[test]
        fn test_string_pattern() {
            assert_snapshot!(parse(
                "\
module Test

\"\" = world

\"hello\" = world
"
            ));
        }

        #[test]
        fn test_float_pattern() {
            assert_snapshot!(parse(
                "\
module Test

5 = world

3 = world
"
            ));
        }

        #[test]
        fn test_type_pattern() {
            assert_snapshot!(parse(
                "\
module Test

Banana = world

Phone hello = world
"
            ));
        }

        #[test]
        fn test_type_pattern_qualified() {
            assert_snapshot!(parse(
                "\
module Test

Banana.Phone = world

Banana.Phone asdf = world
"
            ));
        }

        #[test]
        fn test_pattern_matching() {
            assert_snapshot!(parse(
                "\
module Test

test = when 5 is
    1 -> True
    _ -> False
"
            ));
        }

        #[test]
        fn test_pattern_matching_errors() {
            assert_snapshot!(parse(
                "\
module Test

test = when and is
    1 -> True
    _ -> False
"
            ));

            assert_snapshot!(parse(
                "\
module Test

test = when 5 match
    1 -> True
    _ -> False
"
            ));

            assert_snapshot!(parse(
                "\
module Test

test = when 5 is

lol = 5
"
            ));

            assert_snapshot!(parse(
                "\
module Test

test = when 5 is
    1 => True
    _ => False
"
            ));

            assert_snapshot!(parse(
                "\
module Test

test = when 5 is
    1 -> ,
    _ -> False
"
            ));
        }

        #[test]
        fn test_pattern_matching_string_patern() {
            assert_snapshot!(parse(
                "\
module Test

test = when test is
    \"test\" -> True
"
            ));
        }

        #[test]
        fn test_pattern_matching_number_patern() {
            assert_snapshot!(parse(
                "\
module Test

test = when test is
    5 -> True
"
            ));
        }

        #[test]
        fn test_pattern_matching_type_patern() {
            assert_snapshot!(parse(
                "\
module Test

test = when test is
    Banana -> True
    Banana asdf -> True
    Banana.Banana asdf -> True
"
            ));
        }

        #[test]
        fn test_pattern_matching_named_pattern() {
            assert_snapshot!(parse(
                "\
module Test

Dimensions widht height as dimensions = Dimensions 500 400
"
            ));
            assert_snapshot!(parse(
                "\
module Test

test = when test is
    Banana as hello -> True
    Banana asdf as hello-> True
    Banana (asdf as hello) -> True
"
            ));
            assert_snapshot!(parse(
                "\
module Test

test = when test is
    _ as hello -> True
    hi as hello -> True
    \"hello\" as hello -> True
    5 as hello -> True
"
            ));
        }

        #[test]
        fn test_pattern_matching_or_pattern() {
            assert_snapshot!(parse(
                "\
module Test

Dimensions widht height as dimensions | dimensions = Dimensions 500 400
"
            ));

            assert_snapshot!(parse(
                "\
module Test

test = when test is
    Banana as hello | hello -> True
"
            ));
            assert_snapshot!(parse(
                "\
module Test

test = when test is
    Banana asdf as hello | hello -> True
"
            ));
            assert_snapshot!(parse(
                "\
module Test

test = when test is
    Banana (asdf as hello) | hello -> True
"
            ));
            assert_snapshot!(parse(
                "\
module Test

test = when test is
    Banana | hi as hello  -> True
"
            ));
            assert_snapshot!(parse(
                "\
module Test

test = when test is
    Banana asdf | hi as hello  -> True
"
            ));
            assert_snapshot!(parse(
                "\
module Test

test = when test is
    Banana (asdf as hello | hi)  -> True
"
            ));

            assert_snapshot!(parse(
                "\
module Test

test = when test is
    _ | smth -> True
"
            ));
            assert_snapshot!(parse(
                "\
module Test

test = when test is
    hi | smth -> True
"
            ));
            assert_snapshot!(parse(
                "\
module Test

test = when test is
    \"hello\" | smth -> True
"
            ));
            assert_snapshot!(parse(
                "\
module Test

test = when test is
    5 | smth -> True
"
            ));

            assert_snapshot!(parse(
                "\
module Test

test = when test is
    Some a | b -> True
    Some (a | b) -> True
"
            ));
            assert_snapshot!(parse(
                "\
module Test

test a as b = 5
"
            ));
            assert_snapshot!(parse(
                "\
module Test

test (a as b) | hello = 5
"
            ));
            assert_snapshot!(parse(
                "\
module Test

test ((a as b) | hello) = 5
"
            ));
        }

        fn parse(code: &str) -> String {
            let source = Source::new("Test.alma", code.to_string());
            let mut strings = Strings::new();
            let mut tokens = Tokens::new();
            tokenizer::parse(&source, &mut strings, &mut tokens).unwrap();
            let result = match &super::parse(&source, &tokens, &mut strings) {
                Ok((module, submodules)) => {
                    format!(
                        "{module}\n\n{submodules}",
                        module = {
                            let ctx = TestToStringContext {
                                indent: 0,
                                tokens: &tokens,
                                expressions: &module.expressions,
                                strings: &strings,
                            };
                            module.to_test_string(&ctx)
                        },
                        submodules = submodules
                            .iter()
                            .map(|ast| {
                                let ctx = TestToStringContext {
                                    indent: 0,
                                    tokens: &tokens,
                                    expressions: &ast.expressions,
                                    strings: &strings,
                                };
                                ast.to_test_string(&ctx)
                            })
                            .collect::<Vec<String>>()
                            .join("\n\n")
                    )
                }
                Err(error) => {
                    let error_str = error.to_string(&source, &strings);
                    format!("{error_str}\n\n{error:#?}")
                }
            };
            format!("Input:\n\n{code}\n\nResult:\n\n{result}")
        }
    }
}
