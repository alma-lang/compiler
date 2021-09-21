/* Grammar draft (●○):
    ● file           → module EOF
    ● module         → "module" IDENTIFIER exposing? imports? definitions?
    ● exposing       → "exposing" "(" IDENTIFIER ( "," IDENTIFIER )* ")"
    ● imports        → import ( import )*
    ● import         → "import" IDENTIFIER ( "as" IDENTIFIER )? exposing?
    ● definitions    → ( module | binding )+
    ● expression     → let | lambda | if | binary
    ● let            → "let" MAYBE_INDENT binding+ MAYBE_INDENT "in"? expression
    ● binding        → ( identifier params? | pattern ) "=" expression
    ● lambda         → "\" params? "->" expression
    ● params         → pattern ( pattern )*
    ● pattern        → parsed from Ast.Pattern
    ● if             → "if" binary "then" expression "else" expression
    // Operators
    ● binary         → unary ( binop unary )*
    ● binop          → // parsed from Ast.Binop operator list
    ● unary          → ( "not" | "-" )? call
    // Other primitives
    ● call           → prop_access ( prop_access )*
    ● prop_access    → primary ( "." IDENTIFIER )*
    ● primary        → NUMBER | STRING | "false" | "true"
                     | IDENTIFIER
                     | "." IDENTIFIER
                     | record
                     | "(" expression? ")"
    ● record         → "{" ( expression "|" )? ( field ("," field)* )? "}"
    ● field          → IDENTIFIER ":" expression
*/

use crate::ast::{
    binop::*,
    Expression,
    ExpressionType::{self as ET, Float, If, Let, String_, *},
    Expression_ as E, Identifier, Import, Module, Node, Pattern, Pattern_,
    Unary_::{Minus, Not},
    *,
};
use crate::source::Source;
use crate::token::{
    Token,
    Type::{self as TT, *},
};

#[derive(PartialEq, Debug)]
pub struct Error<'source, 'tokens> {
    message: String,
    token: &'tokens Token<'source>,
}

impl<'source, 'tokens> Error<'source, 'tokens> {
    fn new(
        source: &'source Source,
        token: &'tokens Token<'source>,
        point_at_token: Option<&'tokens Token>,
        message: &str,
    ) -> Self {
        let (position, end, line_number) = match point_at_token {
            Some(t) => (t.position, t.end_position, t.line),
            None => (token.position, token.end_position, token.line),
        };

        let message = format!(
            "{}:{}: {}\n\n{}",
            token.line,
            token.column,
            message,
            source
                .lines_report_at_position_with_pointer(position, Some(end), line_number,)
                .unwrap()
        );

        Self { message, token }
    }

    fn expected_but_found(
        source: &'source Source,
        token: &'tokens Token<'source>,
        point_at_token: Option<&'tokens Token>,
        message: &str,
    ) -> Self {
        Self::new(
            source,
            token,
            point_at_token,
            &format!("{}, but instead found: '{}'", message, token.lexeme),
        )
    }

    pub fn to_string(&self, source: &'source Source) -> String {
        format!(
            "{}:{}:{}\n\n{}",
            source.name(),
            self.token.line,
            self.token.column,
            self.message
        )
    }
}

type ParseResult<'source, 'tokens, A> = Result<A, Error<'source, 'tokens>>;

#[derive(Debug)]
struct State<'source, 'tokens> {
    source: &'source Source<'source>,
    tokens: &'tokens Vec<Token<'source>>,
    current: usize,
}

impl<'source, 'tokens> State<'source, 'tokens> {
    fn file(&mut self) -> ParseResult<'source, 'tokens, Vec<Module>> {
        let module = self.module(true)?;
        match module {
            Some(modules) => self.eof(modules),
            None => Err(Error::expected_but_found(
                self.source,
                self.get_token(),
                None,
                "Expected `module FileName` at the start of the file",
            )),
        }
    }

    fn repl_entry(&mut self) -> ParseResult<'source, 'tokens, ReplEntry> {
        let result = match self.import()? {
            Some(import) => ReplEntry::Import(import),
            None => match self.binding()? {
                Some(definition) => ReplEntry::Definition(definition),
                None => ReplEntry::Expression(self.required_expression(Some(
                    "Expected an import, a top level definition, \
                    or an expression",
                ))?),
            },
        };

        self.eof(result)
    }

    pub fn eof<T>(&mut self, result: T) -> ParseResult<'source, 'tokens, T> {
        let eof_token = self.get_token();
        match eof_token.kind {
            TT::Eof => Ok(result),
            _ => Err(Error::expected_but_found(
                self.source,
                eof_token,
                None,
                "Expected the end of input",
            )),
        }
    }

    fn module(&mut self, top_level: bool) -> ParseResult<'source, 'tokens, Option<Vec<Module>>> {
        let module_token = self.get_token();
        match module_token.kind {
            TT::Module => {
                self.advance();

                match self.module_identifier()? {
                    Some(name) => {
                        let (_, exports) = self.exposing()?;

                        let imports = self.imports()?;

                        let (mut modules, definitions) =
                            self.module_definitions(top_level, module_token, vec![], vec![])?;

                        modules.push(Module {
                            name,
                            exports,
                            imports,
                            definitions,
                        });

                        Ok(Some(modules))
                    }

                    None => Err(Error::expected_but_found(
                        self.source,
                        self.get_token(),
                        None,
                        "Expected the module name",
                    )),
                }
            }

            _ => Ok(None),
        }
    }

    fn exposing(&mut self) -> ParseResult<'source, 'tokens, (usize, Vec<Export>)> {
        // ○ exposing       → "exposing" "(" IDENTIFIER ( "," IDENTIFIER )* ")"
        match self.get_token().kind {
            TT::Exposing => {
                self.advance();

                match self.get_token().kind {
                    TT::LeftParen => {
                        self.advance();

                        let export = self.export()?;

                        let exports = self.exposing_rest(vec![export])?;

                        match self.get_token().kind {
                            TT::RightParen => {
                                let end = self.get_token().end_position;
                                self.advance();

                                Ok((end, exports))
                            }
                            _ => Err(Error::expected_but_found(
                                self.source,
                                self.get_token(),
                                None,
                                "Parsing the module exports expected a comma \
                                separated list of exports inside parenthesis",
                            )),
                        }
                    }
                    _ => Err(Error::expected_but_found(
                        self.source,
                        self.get_token(),
                        None,
                        "Parsing the module exports expected a comma \
                        separated list of exports inside parenthesis",
                    )),
                }
            }
            _ => Ok((self.get_token().end_position, vec![])),
        }
    }
    fn exposing_rest(
        &mut self,
        mut exports: Vec<Export>,
    ) -> ParseResult<'source, 'tokens, Vec<Export>> {
        match self.get_token().kind {
            TT::Comma => {
                self.advance();

                let export = self.export()?;
                exports.push(export);

                self.exposing_rest(exports)
            }
            _ => Ok(exports),
        }
    }

    fn export(&mut self) -> ParseResult<'source, 'tokens, Export> {
        match self.binding_identifier()? {
            Some(export) => {
                // Make intermediate node to please borrow checker
                let node = Node::copy_with_value((), &export);
                Ok(Node::copy_with_value(Export_::Identifier(export), &node))
            }
            _ => Err(Error::expected_but_found(
                self.source,
                self.get_token(),
                None,
                "Expected an identifier from the module to expose",
            )),
        }
    }

    fn imports(&mut self) -> ParseResult<'source, 'tokens, Vec<Import>> {
        let mut imports = vec![];

        while let Some(import) = self.import()? {
            imports.push(import);
        }

        Ok(imports)
    }

    fn import(&mut self) -> ParseResult<'source, 'tokens, Option<Import>> {
        let import_token = self.get_token();
        match import_token.kind {
            TT::Import => {
                self.advance();

                match self.module_identifier()? {
                    Some(module_name) => {
                        let alias = match self.get_token().kind {
                            TT::As => {
                                self.advance();

                                match self.module_identifier_part()? {
                                    Some(alias) => Ok(Some(alias)),
                                    _ => Err(Error::expected_but_found(
                                        self.source,
                                        self.get_token(),
                                        None,
                                        "Expected an identifier for the alias of the module",
                                    )),
                                }
                            }

                            _ => Ok(None),
                        }?;

                        let (exposing_end, exposing) = self.exposing()?;

                        let end = if !exposing.is_empty() {
                            exposing_end
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
                            start: import_token.position,
                            end,
                            line: import_token.line,
                            column: import_token.column,
                        }))
                    }
                    _ => Err(Error::expected_but_found(
                        self.source,
                        self.get_token(),
                        None,
                        "Expected an identifier of the module to import",
                    )),
                }
            }

            _ => Ok(None),
        }
    }

    fn module_definitions(
        &mut self,
        top_level: bool,
        module_token: &Token,
        mut modules: Vec<Module>,
        mut definitions: Vec<Definition>,
    ) -> ParseResult<'source, 'tokens, (Vec<Module>, Vec<Definition>)> {
        let current_token_has_valid_indent =
            self.current_token_has_valid_indent_for_module_definitions(top_level, module_token);

        if current_token_has_valid_indent {
            match self.module(false)? {
                Some(mut nested_modules) => {
                    modules.append(&mut nested_modules);
                    self.module_definitions(top_level, module_token, modules, definitions)
                }

                None => match self.binding()? {
                    Some(definition) => {
                        definitions.push(definition);
                        self.module_definitions(top_level, module_token, modules, definitions)
                    }

                    None => match self.get_token().kind {
                        TT::Eof => Ok((modules, definitions)),
                        _ => Err(Error::expected_but_found(
                            self.source,
                            self.get_token(),
                            None,
                            "Expected the left side of a definition like `n = 5` \
                             or `add x y = x + y`",
                        )),
                    },
                },
            }
        } else {
            match self.get_token().kind {
                TT::Eof => Ok((modules, definitions)),
                _ => {
                    if self.current_token_outside_indent_for_module_definitions(
                        top_level,
                        module_token,
                    ) {
                        Ok((modules, definitions))
                    } else {
                        Err(Error::expected_but_found(
                            self.source,
                            self.get_token(),
                            None,
                            "Expected a definition like `n = 5` \
                                        or `add x y = x + y`",
                        ))
                    }
                }
            }
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

    pub fn required_expression(
        &mut self,
        msg: Option<&'static str>,
    ) -> ParseResult<'source, 'tokens, Expression> {
        match self.expression()? {
            Some(expr) => Ok(expr),
            None => {
                let default_msg = "Expected an expression (a number, string, a let binding, \
                           function call, an identifier, etc.)";
                Err(Error::expected_but_found(
                    self.source,
                    self.get_token(),
                    None,
                    msg.unwrap_or(default_msg),
                ))
            }
        }
    }

    pub fn expression(&mut self) -> ParseResult<'source, 'tokens, Option<Expression>> {
        match self.let_()? {
            Some(let_) => Ok(Some(let_)),
            None => match self.if_()? {
                Some(if_) => Ok(Some(if_)),
                None => match self.lambda()? {
                    Some(lambda) => Ok(Some(lambda)),
                    None => self.binary(),
                },
            },
        }
    }

    fn let_(&mut self) -> ParseResult<'source, 'tokens, Option<Expression>> {
        let let_token = self.get_token();

        match let_token.kind {
            TT::Let => {
                self.advance();

                let bindings = self.let_bindings(let_token, vec![])?;

                if !bindings.is_empty() {
                    let is_in_keyword = if let TT::In = self.get_token().kind {
                        self.advance();
                        true
                    } else {
                        false
                    };
                    if is_in_keyword || self.is_token_after_line_and_same_indent_as(let_token) {
                        let body = self.required_expression(Some(
                            "Expected an expression for the body of the let bindings",
                        ))?;

                        let line = let_token.line;
                        let column = let_token.column;
                        let start = let_token.position;
                        let end = body.end;
                        Ok(Some(Node {
                            value: E::untyped(Let(bindings, Box::new(body))),
                            line,
                            column,
                            start,
                            end,
                        }))
                    } else {
                        Err(Error::expected_but_found(
                            self.source,
                            self.get_token(),
                            None,
                            "Expected the let definition \
                                to be followed by another \
                                expression in the next line \
                                and same indentation",
                        ))
                    }
                } else {
                    Err(Error::expected_but_found(
                        self.source,
                        let_token,
                        None,
                        "Expected a pattern for the left side of the let expression",
                    ))
                }
            }

            _ => Ok(None),
        }
    }
    fn let_bindings(
        &mut self,
        let_token: &Token,
        mut bindings: Vec<Definition>,
    ) -> ParseResult<'source, 'tokens, Vec<Definition>> {
        if self.is_token_in_same_line_or_nested_indent_from(let_token) {
            match self.binding()? {
                Some(binding) => {
                    bindings.push(binding);
                    self.let_bindings(let_token, bindings)
                }
                None => Ok(bindings),
            }
        } else {
            Ok(bindings)
        }
    }

    fn binding(&mut self) -> ParseResult<'source, 'tokens, Option<Definition>> {
        let token = self.get_token();

        let pattern = self.pattern()?;

        let definition = match pattern {
            Some(Node {
                value: Pattern_::Identifier(identifier),
                ..
            }) => {
                let equal_token = self.get_token();
                match equal_token.kind {
                    // Peek to see if it is just an identifier and =, and return a pattern
                    Equal => {
                        let expr = self.binding_rhs()?;
                        Some(Definition::Pattern(
                            Node::new(Pattern_::Identifier(identifier), token, token),
                            expr,
                        ))
                    }
                    // Otherwise this is a lambda lhs, identifier + params
                    _ => {
                        let params = self.params()?;
                        let expr = self.binding_rhs()?;
                        let line = token.line;
                        let column = token.column;
                        let start = token.position;
                        let end = expr.end;
                        Some(Definition::Lambda(
                            identifier,
                            Node {
                                value: E::untyped(ET::Lambda(params, Box::new(expr))),
                                line,
                                column,
                                start,
                                end,
                            },
                        ))
                    }
                }
            }
            Some(pattern) => Some(Definition::Pattern(pattern, self.binding_rhs()?)),
            None => None,
        };

        Ok(definition)
    }
    fn binding_rhs(&mut self) -> ParseResult<'source, 'tokens, Expression> {
        let equal_token = self.get_token();
        match equal_token.kind {
            Equal => {
                self.advance();

                self.required_expression(Some(
                    "Expected an expression for the right side of the definition",
                ))
            }

            _ => Err(Error::expected_but_found(
                self.source,
                equal_token,
                None,
                "Expected an = and an expression \
                for the right side of the definition",
            )),
        }
    }

    fn if_(&mut self) -> ParseResult<'source, 'tokens, Option<Expression>> {
        let token = self.get_token();

        match token.kind {
            TT::If => {
                self.advance();

                match self.binary()? {
                    Some(condition) => {
                        let then_token = self.get_token();
                        match then_token.kind {
                            Then => {
                                self.advance();
                                let then = self.required_expression(Some(
                                    "Expected an expression for the first\
                                    branch of the if (eg: if True then \"Hi\" else \"Ho\")",
                                ))?;

                                let else_token = self.get_token();
                                match else_token.kind {
                                    Else => {
                                        self.advance();

                                        let else_ = self.required_expression(Some(
                                            "Expected an expression for the else\
                                            branch of the if (eg: if True then \"Hi\" else \"Ho\")",
                                        ))?;

                                        let line = token.line;
                                        let column = token.column;
                                        let start = token.position;
                                        let end = else_.end;

                                        Ok(Some(Node {
                                            value: E::untyped(If(
                                                Box::new(condition),
                                                Box::new(then),
                                                Box::new(else_),
                                            )),
                                            line,
                                            column,
                                            start,
                                            end,
                                        }))
                                    }

                                    _ => Err(Error::expected_but_found(
                                        self.source,
                                        else_token,
                                        None,
                                        "Expected the `else` branch of the if expression",
                                    )),
                                }
                            }

                            _ => Err(Error::expected_but_found(
                                self.source,
                                then_token,
                                None,
                                "Expected the keyword `then` and \
                                an expression to parse the if expression",
                            )),
                        }
                    }

                    None => Err(Error::expected_but_found(
                        self.source,
                        self.get_token(),
                        None,
                        "Expected an expression for the condition \
                        in the if expression (eg: if True then 1 else 2)",
                    )),
                }
            }

            _ => Ok(None),
        }
    }

    fn lambda(&mut self) -> ParseResult<'source, 'tokens, Option<Expression>> {
        let token = self.get_token();

        match token.kind {
            Backslash => {
                self.advance();

                let params = self.params()?;
                if params.is_empty() {
                    Err(Error::expected_but_found(
                        self.source,
                        self.get_token(),
                        None,
                        "Expected a list of parameters",
                    ))
                } else {
                    let arrow = self.get_token();
                    match arrow.kind {
                        Arrow => {
                            self.advance();

                            let body = self.required_expression(Some(
                                "Expected an expression for the body of the function",
                            ))?;
                            let line = token.line;
                            let column = token.column;
                            let start = token.position;
                            let end = body.end;

                            Ok(Some(Node {
                                value: E::untyped(Lambda(params, Box::new(body))),
                                line,
                                column,
                                start,
                                end,
                            }))
                        }

                        _ => Err(Error::expected_but_found(
                            self.source,
                            token,
                            Some(arrow),
                            "Expected a -> after the list of parameters for the function",
                        )),
                    }
                }
            }
            _ => Ok(None),
        }
    }

    fn params(&mut self) -> ParseResult<'source, 'tokens, Vec<Pattern>> {
        let pattern = self.pattern()?;

        match pattern {
            Some(pattern) => {
                let mut params = vec![pattern];

                loop {
                    let pattern = self.pattern()?;

                    match pattern {
                        Some(pattern) => {
                            params.push(pattern);
                        }
                        None => break,
                    }
                }

                Ok(params)
            }

            None => Ok(vec![]),
        }
    }

    fn pattern(&mut self) -> ParseResult<'source, 'tokens, Option<Pattern>> {
        let token = self.get_token();

        match token.kind {
            TT::Underscore => {
                self.advance();
                Ok(Some(Node::new(Pattern_::Hole, token, token)))
            }
            TT::Identifier => match self.binding_identifier()? {
                Some(identifier) => Ok(Some(Node::new(
                    Pattern_::Identifier(identifier),
                    token,
                    token,
                ))),
                _ => Ok(None),
            },
            _ => Ok(None),
        }
    }

    fn binary(&mut self) -> ParseResult<'source, 'tokens, Option<Expression>> {
        match self.unary()? {
            Some(expr) => {
                self.binary_step().map(|mut binops| {
                    // Make the binops options to be able to take them later
                    let mut binops: Vec<Option<(Binop, Expression)>> =
                        binops.drain(..).map(Some).collect();

                    Some(organize_binops(expr, &mut binops, &mut (0), 0))
                })
            }
            None => Ok(None),
        }
    }
    fn binary_step(&mut self) -> ParseResult<'source, 'tokens, Vec<(Binop, Expression)>> {
        let mut binops = vec![];

        loop {
            let token = self.get_token();

            let op = match token.kind {
                Slash => Some(DIVISION.clone()),
                Star => Some(MULTIPLICATION.clone()),
                Plus => Some(ADDITION.clone()),
                TT::Minus => Some(SUBSTRACTION.clone()),
                BangEqual => Some(NOT_EQUAL.clone()),
                EqualEqual => Some(EQUAL.clone()),
                Greater => Some(GREATER_THAN.clone()),
                GreaterEqual => Some(GREATER_EQUAL_THAN.clone()),
                Less => Some(LESS_THAN.clone()),
                LessEqual => Some(LESS_EQUAL_THAN.clone()),
                And => Some(AND.clone()),
                Or => Some(OR.clone()),
                _ => None,
            };

            match op {
                Some(op) => {
                    self.advance();

                    let op_node = Node::new(op, token, token);
                    match self.unary()? {
                        Some(right) => {
                            binops.push((op_node, right));
                        }
                        None => {
                            return Err(Error::expected_but_found(
                                self.source,
                                self.get_token(),
                                None,
                                &format!(
                                    "Expected an expression after the binary operator '{}'",
                                    &token.lexeme
                                ),
                            ));
                        }
                    }
                }

                None => break,
            };
        }

        Ok(binops)
    }

    fn unary(&mut self) -> ParseResult<'source, 'tokens, Option<Expression>> {
        let token = self.get_token();

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
                let op = Node::new(u, token, token);
                let line = op.line;
                let column = op.column;
                let start = op.start;
                let end = expr.end;
                Ok(Some(Node {
                    value: E::untyped(Unary(op, Box::new(expr))),
                    line,
                    column,
                    start,
                    end,
                }))
            }
            (None, Some(expr)) => Ok(Some(expr)),
            (Some(_), None) => {
                let msg = format!(
                    "Expected an expression after the unary operator '{}'",
                    &token.lexeme
                );
                Err(Error::expected_but_found(
                    self.source,
                    self.get_token(),
                    None,
                    &msg,
                ))
            }
            (None, None) => Ok(None),
        }
    }

    fn call(&mut self) -> ParseResult<'source, 'tokens, Option<Expression>> {
        let token = self.get_token();

        match self.prop_access()? {
            Some(expr) => {
                let args = self.arguments(token, vec![])?;
                if args.is_empty() {
                    Ok(Some(expr))
                } else {
                    let last_arg = &args[args.len() - 1];

                    let line = expr.line;
                    let column = expr.column;
                    let start = expr.start;
                    let end = last_arg.end;
                    Ok(Some(Node {
                        value: E::untyped(FnCall(Box::new(expr), args)),
                        line,
                        column,
                        start,
                        end,
                    }))
                }
            }
            None => Ok(None),
        }
    }

    fn arguments(
        &mut self,
        first_token: &Token,
        mut args: Vec<Expression>,
    ) -> ParseResult<'source, 'tokens, Vec<Expression>> {
        if self.is_token_in_same_line_or_nested_indent_from(first_token) {
            self.primary().and_then(|arg| {
                match arg {
                    // We tried to get an argument, but there was no match, or it was not well indented
                    None => Ok(args),

                    Some(arg) => {
                        args.push(arg);
                        self.arguments(first_token, args)
                    }
                }
            })
        } else {
            Ok(args)
        }
    }

    fn prop_access(&mut self) -> ParseResult<'source, 'tokens, Option<Expression>> {
        self.primary().and_then(|expr| match expr {
            Some(expr) => {
                let mut expr = expr;

                loop {
                    let dot_token = self.get_token();
                    match dot_token.kind {
                        // Dot token without whitespace between the prev token is a record access
                        Dot if dot_token.position == expr.end => {
                            self.advance();

                            let identifier_token = self.get_token();
                            match identifier_token.kind {
                                // Dot token without whitespace between it and the identifier is a prop access
                                TT::Identifier
                                    if identifier_token.position == dot_token.end_position =>
                                {
                                    self.advance();

                                    let name_identifier = Node::new(
                                        Identifier_::new(identifier_token.lexeme),
                                        identifier_token,
                                        identifier_token,
                                    );
                                    expr = {
                                        let start = expr.start;
                                        let end = name_identifier.end;
                                        let line = expr.line;
                                        let column = expr.column;
                                        Node {
                                            value: E::untyped(PropAccess(
                                                Box::new(expr),
                                                name_identifier,
                                            )),
                                            start,
                                            end,
                                            line,
                                            column,
                                        }
                                    };
                                }

                                TT::Identifier => {
                                    return Err(Error::new(
                                        self.source,
                                        dot_token,
                                        None,
                                        "A property access must have the dot \
                                        and identifier together, \
                                        like this `a.b.c`",
                                    ));
                                }
                                _ => {
                                    return Err(Error::new(
                                        self.source,
                                        dot_token,
                                        None,
                                        "Expected an identifier after a \
                                        dot for the property access",
                                    ));
                                }
                            }
                        }
                        _ => break,
                    }
                }

                Ok(Some(expr))
            }
            None => Ok(None),
        })
    }

    fn primary(&mut self) -> ParseResult<'source, 'tokens, Option<Expression>> {
        let token = self.get_token();

        let result = match token.kind {
            False => {
                self.advance();
                Ok(Some(Node::new(E::untyped(Bool(false)), token, token)))
            }
            True => {
                self.advance();
                Ok(Some(Node::new(E::untyped(Bool(true)), token, token)))
            }

            TT::Float => {
                let lexeme = token.lexeme;
                let n = lexeme.parse::<f64>().map_err(|_| {
                    Error::new(
                        self.source,
                        token,
                        None,
                        &format!("Failed to parse number token '{}'", lexeme),
                    )
                })?;

                self.advance();

                Ok(Some(Node::new(E::untyped(Float(n)), token, token)))
            }

            TT::Identifier => {
                self.advance();

                Ok(Some(Node::new(
                    E::untyped(ET::Identifier(Node::new(
                        Identifier_::new(token.lexeme),
                        token,
                        token,
                    ))),
                    token,
                    token,
                )))
            }

            TT::String_ => {
                self.advance();

                let lexeme = token.lexeme;
                let value = lexeme[1..(lexeme.len() - 1)].to_string();
                Ok(Some(Node::new(E::untyped(String_(value)), token, token)))
            }

            LeftBrace => {
                self.advance();
                let next_token = self.get_token();
                let second_next_token = self.peek_next_token();

                match (next_token.kind, second_next_token.kind) {
                    // Unit record
                    (RightBrace, _) => {
                        self.advance();
                        Ok(Some(Node::new(
                            E::untyped(Record(vec![])),
                            token,
                            next_token,
                        )))
                    }

                    // Record literal
                    (TT::Identifier, Colon) | (TT::Identifier, Equal) => {
                        let fields = self.record_fields()?;
                        let last_token = self.get_token();

                        match last_token.kind {
                            RightBrace => {
                                self.advance();

                                Ok(Some(Node::new(
                                    E::untyped(Record(fields)),
                                    token,
                                    last_token,
                                )))
                            }
                            _ => Err(Error::expected_but_found(
                                self.source,
                                last_token,
                                Some(token),
                                "Expected '}' to close a record literal",
                            )),
                        }
                    }

                    _ => {
                        let record = self.required_expression(Some(
                            "Expected a record literal `{ x = 1, y = 2 }`\
                            or a record update `{ record | x = 5, y = 4 }`",
                        ))?;

                        let pipe_token = self.get_token();
                        match pipe_token.kind {
                            Pipe => {
                                self.advance();

                                let fields = self.record_fields()?;
                                let last_token = self.get_token();

                                match last_token.kind {
                                    RightBrace => {
                                        self.advance();

                                        Ok(Some(Node::new(
                                            E::untyped(RecordUpdate(Box::new(record), fields)),
                                            token,
                                            last_token,
                                        )))
                                    }
                                    _ => Err(Error::expected_but_found(
                                        self.source,
                                        last_token,
                                        Some(token),
                                        "Expected '}' to close a record update expression",
                                    )),
                                }
                            }
                            _ => Err(Error::expected_but_found(
                                self.source,
                                pipe_token,
                                None,
                                "Expected '|' between the record and \
                                the fields to update (like this \
                                `{ record | field = 5 }`)",
                            )),
                        }
                    }
                }
            }

            LeftParen => {
                self.advance();
                let next_token = self.get_token();

                match next_token.kind {
                    RightParen => {
                        self.advance();
                        Ok(Some(Node::new(E::untyped(Unit), token, next_token)))
                    }

                    _ => {
                        let expr = self.required_expression(Some(
                            "Expected an expression inside the parenthesis",
                        ))?;
                        let last_token = self.get_token();

                        match last_token.kind {
                            RightParen => {
                                self.advance();
                                Ok(Some(expr))
                            }
                            _ => Err(Error::expected_but_found(
                                self.source,
                                last_token,
                                Some(token),
                                "Expected ')' after parenthesized expression",
                            )),
                        }
                    }
                }
            }

            Dot => {
                self.advance();
                let identifier_token = self.get_token();

                match identifier_token.kind {
                    // Dot token without whitespace between it and the identifier is a lambda w/ prop access
                    TT::Identifier if identifier_token.position == token.end_position => {
                        self.advance();

                        let name_identifier = Node::new(
                            Identifier_::new(identifier_token.lexeme),
                            identifier_token,
                            identifier_token,
                        );
                        Ok(Some(Node::new(
                            E::untyped(PropAccessLambda(name_identifier)),
                            token,
                            identifier_token,
                        )))
                    }

                    // Dot with whitespace and an identifier afterwards
                    TT::Identifier => {
                        return Err(Error::new(
                            self.source,
                            token,
                            None,
                            "A property access must have the dot \
                                and identifier together, \
                                        like this `.name`",
                        ));
                    }

                    _ => {
                        return Err(Error::new(
                            self.source,
                            token,
                            None,
                            "Expected an identifier after a \
                                dot for the property access",
                        ));
                    }
                }
            }

            _ => Ok(None),
        };

        result
    }

    fn record_fields(&mut self) -> ParseResult<'source, 'tokens, Vec<(Identifier, Expression)>> {
        let mut fields = vec![];

        loop {
            let field = self.record_field()?;
            fields.push(field);

            let comma_token = self.get_token();
            if let TT::Comma = comma_token.kind {
                self.advance();
            } else {
                break;
            }
        }

        Ok(fields)
    }

    fn record_field(&mut self) -> ParseResult<'source, 'tokens, (Identifier, Expression)> {
        match self.binding_identifier()? {
            Some(identifier) => match self.get_token().kind {
                Colon | Equal => {
                    self.advance();

                    Ok((
                        identifier,
                        self.required_expression(Some(
                            "Expected an expression for the value \
                            of the field in the record",
                        ))?,
                    ))
                }
                _ => Err(Error::expected_but_found(
                    self.source,
                    self.get_token(),
                    None,
                    "Expected a ':' separating the name of \
                    the field and the value in the record",
                )),
            },
            _ => Err(Error::expected_but_found(
                self.source,
                self.get_token(),
                None,
                "Expected an identifier for the name of the field in the record",
            )),
        }
    }

    fn module_identifier_part(&mut self) -> ParseResult<'source, 'tokens, Option<Identifier>> {
        self.identifier(
            IdentifierCase::Pascal,
            "Expected a `PascalCase` module name",
        )
    }

    fn module_identifier(&mut self) -> ParseResult<'source, 'tokens, Option<ModuleName>> {
        match self.module_identifier_part()? {
            Some(name) => self
                .module_identifier_rest(vec![name])
                .map(Some),
            None => Ok(None),
        }
    }
    fn module_identifier_rest(
        &mut self,
        mut names: Vec<Identifier>,
    ) -> ParseResult<'source, 'tokens, ModuleName> {
        let dot_token = self.get_token();
        match dot_token.kind {
            TT::Dot => {
                self.advance();

                match self.module_identifier_part()? {
                    Some(name) => {
                        names.push(name);
                        self.module_identifier_rest(names)
                    }
                    None => Ok(ModuleName(names)),
                }
            }
            _ => Ok(ModuleName(names)),
        }
    }

    fn binding_identifier(&mut self) -> ParseResult<'source, 'tokens, Option<Identifier>> {
        self.identifier(IdentifierCase::Camel, "Expected a `camelCase` identifier")
    }

    fn identifier(
        &mut self,
        expect_case: IdentifierCase,
        invalid_case_err: &'static str,
    ) -> ParseResult<'source, 'tokens, Option<Identifier>> {
        let identifier_token = self.get_token();
        match identifier_token.kind {
            TT::Identifier => {
                self.advance();

                let name_identifier = Identifier_::new(identifier_token.lexeme);
                if &name_identifier.case == &expect_case {
                    let name = Node::new(name_identifier, identifier_token, identifier_token);
                    Ok(Some(name))
                } else {
                    Err(Error::expected_but_found(
                        self.source,
                        identifier_token,
                        None,
                        invalid_case_err,
                    ))
                }
            }
            _ => Ok(None),
        }
    }

    // Utilities

    fn get_token(&self) -> &'tokens Token<'source> {
        self.tokens
            .get(self.current)
            .expect("Out of bounds access to tokens array")
    }

    fn peek_next_token(&self) -> &'tokens Token<'source> {
        let token = self.get_token();
        match token.kind {
            Eof => token,
            _ => self
                .tokens
                .get(self.current + 1)
                .expect("Out of bounds access to tokens array"),
        }
    }

    fn advance(&mut self) {
        let token = self.get_token();
        match token.kind {
            Eof => (),
            _ => self.current += 1,
        };
    }

    fn is_token_in_same_indent_and_column_as(&self, parent_token: &Token) -> bool {
        let token = self.get_token();
        parent_token.line < token.line
            && token.indent == parent_token.indent
            && token.column == parent_token.column
    }

    fn is_token_after_line_and_same_indent_as(&self, parent_token: &Token) -> bool {
        let token = self.get_token();
        parent_token.line < token.line && token.indent == parent_token.indent
    }

    fn is_token_in_same_line_or_nested_indent_from(&self, parent_token: &Token) -> bool {
        let token = self.get_token();
        parent_token.line == token.line
            || (parent_token.line < token.line && token.indent > parent_token.indent)
    }

    fn is_token_start_of_line_and_after_line_and_nested_indent_from(
        &self,
        parent_token: &Token,
    ) -> bool {
        let token = self.get_token();
        parent_token.line < token.line
            && token.indent > parent_token.indent
            && token.column == token.indent
    }

    fn is_token_equal_or_less_indented_than(&self, parent_token: &Token) -> bool {
        let token = self.get_token();
        parent_token.line < token.line && token.indent <= parent_token.indent
    }
}

fn organize_binops(
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
                let keep_parsing = match &op_and_expr {
                    Some((op, _rhs)) if op.value.precedence >= min_precedence => true,
                    _ => false,
                };

                if keep_parsing {
                    // Take ownership of the op and rhs
                    let (op, rhs) = op_and_expr.take().unwrap();

                    *current += 1;

                    let next_min_precedence = op.value.precedence
                        + if op.value.associativity == Associativity::LTR {
                            1
                        } else {
                            0
                        };

                    let right = organize_binops(rhs, binops, current, next_min_precedence);

                    let line = left.line;
                    let column = left.column;
                    let start = left.start;
                    let end = right.end;

                    left = Node {
                        value: E::untyped(Binary(
                            Box::new(Node::copy_with_value(
                                E::untyped(ET::Identifier(Node::copy_with_value(
                                    op.value.fn_.clone(),
                                    &op,
                                ))),
                                &op,
                            )),
                            op,
                            Box::new([left, right]),
                        )),
                        line,
                        column,
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

pub fn parse<'source, 'tokens>(
    source: &'source Source,
    tokens: &'tokens Vec<Token<'source>>,
) -> ParseResult<'source, 'tokens, Vec<Module>> {
    let mut parser = State {
        source,
        tokens,
        current: 0,
    };

    parser.file()
}

pub fn parse_repl<'source, 'tokens>(
    source: &'source Source,
    tokens: &'tokens Vec<Token<'source>>,
) -> ParseResult<'source, 'tokens, ReplEntry> {
    let mut parser = State {
        source,
        tokens,
        current: 0,
    };

    parser.repl_entry()
}

pub fn parse_expression<'source, 'tokens>(
    source: &'source Source,
    tokens: &'tokens Vec<Token<'source>>,
) -> ParseResult<'source, 'tokens, Box<Expression>> {
    let mut parser = State {
        source,
        tokens,
        current: 0,
    };

    let result = parser.required_expression(None)?;
    parser.eof(Box::new(result))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokenizer;
    use insta::assert_snapshot;

    #[test]
    fn test_expression_parser() {
        assert_snapshot!(parse("True"));

        assert_snapshot!(parse("False"));

        assert_snapshot!(parse("()"));

        assert_snapshot!(parse("123"));

        assert_snapshot!(parse("123.2"));

        assert_snapshot!(parse("variableOne"));

        assert_snapshot!(parse("variable_one"));

        assert_snapshot!(parse("espaÆàʥñÑol"));

        assert_snapshot!(parse("\"😄\""));

        assert_snapshot!(parse("\"\n\""));

        assert_snapshot!(parse("\"\""));

        assert_snapshot!(parse("(\"\")"));

        assert_snapshot!(parse("(((1)))"));

        assert_snapshot!(parse("(((1))"));

        assert_snapshot!(parse("(((1))))"));

        assert_snapshot!(parse(
            "(
  ((1))
)",
        ));

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

        assert_snapshot!(parse("not False"));

        assert_snapshot!(parse("- 5"));

        assert_snapshot!(parse("incr (-5)"));

        assert_snapshot!(parse("1 - 5"));

        assert_snapshot!(parse("1 - -5"));

        assert_snapshot!(parse("1 + 2 / 3"));

        assert_snapshot!(parse("1 == 2 / 3"));

        assert_snapshot!(parse("\\a -> a"));

        assert_snapshot!(parse("\\a -> \\b -> a"));

        assert_snapshot!(parse("\\a b -> a"));

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

        assert_snapshot!(parse("let x = 1\nx"));

        assert_snapshot!(parse("let x = a\n  x"));

        assert_snapshot!(parse("let x = a\n  x\nx"));

        assert_snapshot!(parse("let x = a x in x"));

        assert_snapshot!(parse("let x = a\n  x\nin\nx"));

        assert_snapshot!(parse("let\n  x = a\n  x\nin\nx"));

        assert_snapshot!(parse("let\n  x = a\n    x\nin\nx"));

        assert_snapshot!(parse("let\n  x = a\n    x\n b = 5\nin\nx"));

        assert_snapshot!(parse(
            "\
let
  IAmNotCamelCase = 1
in
IAmNotCamelCase
"
        ));

        assert_snapshot!(parse("let _ = a x in x"));

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

        assert_snapshot!(parse("a.b"));

        assert_snapshot!(parse("a. b"));

        assert_snapshot!(parse("a.b.c.d"));

        assert_snapshot!(parse(".b"));

        assert_snapshot!(parse(". b"));

        assert_snapshot!(parse("a .b"));

        assert_snapshot!(parse(".a b"));

        assert_snapshot!(parse("{ 5 | x = 1 }"));

        assert_snapshot!(parse("{ 5 : x = 1 }"));

        assert_snapshot!(parse("{ 5 | x = 1, y = 3 }"));

        assert_snapshot!(parse("{ if True then {} else {} | x = 1, y = 3 }"));

        fn parse(code: &str) -> String {
            let source = Source::new_orphan(code);
            let tokens = tokenizer::parse(&source).unwrap();
            let result = parse_expression(&source, &tokens);
            format!(
                "Input:\n\n{}\n\nResult:\n\n{}",
                code,
                match &result {
                    Ok(ast) => format!("{:#?}", ast),
                    Err(e) => format!("{}\n\n{:#?}", e.message.clone(), e),
                }
            )
        }
    }

    #[test]
    fn test_module_parser() {
        assert_snapshot!(parse("True"));

        assert_snapshot!(parse("module Test"));

        assert_snapshot!(parse("module Test\n\na = 1"));

        assert_snapshot!(parse("module Test\n\na = 1\n\nb = True"));

        assert_snapshot!(parse("module Test\n\na = 1 + 2) + 3\n\nb = * add\n  5"));

        assert_snapshot!(parse("module Test\n\na = 1 + 2 + 3\n\nb = * add\n  5"));

        assert_snapshot!(parse("module Parent\n\nmodule Test"));

        assert_snapshot!(parse("module Parent\n\nmodule Test\n\n  a = 1"));

        assert_snapshot!(parse("module Parent\n\nmodule Test\n\n  a = 1\n\na = 1\n"));

        assert_snapshot!(parse({
            "module Parent\n\nmodule Test\n\n  a = 1 + 2) + 3\n\n  b = * add\n    5\n\na = 1 + 2) + 3\n\nb = * add\n  5"
        },));

        assert_snapshot!(parse(
            "module Parent

module Test1
  a = 1

  module Test1Test
    b = 1

module Test2
    c = 5
    ",
        ));

        assert_snapshot!(parse("module Test exposing (a)\n\na = 1\n\nb = True"));

        assert_snapshot!(parse("module Test exposing (a, b)\n\na = 1\n\nb = True"));

        assert_snapshot!(parse("module Test exposing a, b\n\na = 1\n\nb = True"));

        assert_snapshot!(parse("module Test exposing (a b)\n\na = 1\n\nb = True"));

        assert_snapshot!(parse("module Test exposing (a, b\n\na = 1\n\nb = True"));

        assert_snapshot!(parse("module Test\n\nimport Banana"));

        assert_snapshot!(parse("module Test\n\nimport Banana as B"));

        assert_snapshot!(parse("module Test\n\nimport Banana exposing (phone)"));

        assert_snapshot!(parse(
            "module Test

import Banana as B

import Phone exposing (raffi)

import Apple as A exposing (orange)
",
        ));

        assert_snapshot!(parse("module i_am_not_PascalCase"));

        assert_snapshot!(parse(
            "module Test

module i_am_not_PascalCase
    test = 1
"
        ));

        assert_snapshot!(parse(
            "module Test

module Test2

    module i_am_not_PascalCase
        test = 1
"
        ));

        assert_snapshot!(parse(
            "module Test

IAmNotCamelCase = 1
"
        ));

        assert_snapshot!(parse(
            "module Test

incr n = n + 1
"
        ));

        fn parse(code: &str) -> String {
            let source = Source::new_orphan(code);
            let tokens = tokenizer::parse(&source).unwrap();
            let result = super::parse(&source, &tokens);
            format!(
                "Input:\n\n{}\n\nResult:\n\n{}",
                code,
                match &result {
                    Ok(ast) => format!("{:#?}", ast),
                    Err(e) => format!("{}\n\n{:#?}", e.message.clone(), e),
                }
            )
        }
    }
}
