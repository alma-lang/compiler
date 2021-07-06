/* Grammar draft (●○):
    ● file           → module EOF
    ● module         → "module" IDENTIFIER exposing? imports? definitions?
    ● exposing       → "exposing" "(" IDENTIFIER ( "," IDENTIFIER )* ")"
    ● imports        → import ( import )*
    ● import         → "import" IDENTIFIER ( "as" IDENTIFIER )? exposing?
    ● definitions    → ( module | binding )+
    ● expression     → let | lambda | if | binary
    ● let            → "let" MAYBE_INDENT binding+ MAYBE_INDENT "in"? expression
    ● binding        → binding "=" expression
    ● lambda         → "\" params? "->" expression
    ● params         → "()" | pattern ( pattern )*
    ● pattern        → parsed from Ast.Pattern
    ● if             → "if" binary "then" expression "else" expression
    // Operators
    ● binary         → binary ( binop binary )*
    ● binop          → // parsed from Ast.Binop operator list
    ● unary          → ( "not" | "-" )? call
    // Other primitives
    ● call           → primary ( primary )*
    ● primary        → NUMBER | STRING | IDENTIFIER | "false" | "true"
                     | "(" expression ")"
*/

use crate::ast::{
    binop::*,
    Expression,
    Expression_::{Float, Identifier, If, Let, String_, *},
    Import, Module, Node, Pattern, Pattern_,
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

    fn repl_entry(&mut self) -> ParseResult<'source, 'tokens, Expression> {
        let expression = self.expression()?;
        self.eof(expression)
    }

    fn eof<T>(&mut self, result: T) -> ParseResult<'source, 'tokens, T> {
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

                let identifier_token = self.get_token();
                match identifier_token.kind {
                    TT::Identifier => {
                        self.advance();

                        let name_identifier = Identifier_::new(identifier_token.lexeme);
                        if let IdentifierCase::Pascal = &name_identifier.case {
                            let name =
                                Node::new(name_identifier, identifier_token, identifier_token);

                            let (_, exports) = self.exposing()?;

                            let imports = self.imports()?;

                            let (mut modules, definitions) =
                                self.module_definitions(top_level, &module_token, vec![], vec![])?;

                            modules.push(Module {
                                name,
                                exports,
                                imports,
                                definitions,
                            });

                            Ok(Some(modules))
                        } else {
                            Err(Error::expected_but_found(
                                self.source,
                                identifier_token,
                                None,
                                "The module name should be PascalCase",
                            ))
                        }
                    }

                    _ => Err(Error::expected_but_found(
                        self.source,
                        identifier_token,
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
                                &self.get_token(),
                                None,
                                "Parsing the module exports expected a comma \
                                separated list of exports inside parenthesis",
                            )),
                        }
                    }
                    _ => Err(Error::expected_but_found(
                        self.source,
                        &self.get_token(),
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
        let identifier_token = self.get_token();
        match identifier_token.kind {
            TT::Identifier => {
                self.advance();

                Ok(Node::new(
                    Export_(identifier_token.lexeme.to_string()),
                    &identifier_token,
                    &identifier_token,
                ))
            }
            _ => Err(Error::expected_but_found(
                self.source,
                &identifier_token,
                None,
                "Expected an identifier from the module to expose",
            )),
        }
    }

    // ○ imports        → import ( import )*
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

                let identifier_token = self.get_token();
                match identifier_token.kind {
                    TT::Identifier => {
                        self.advance();

                        let alias = match self.get_token().kind {
                            TT::As => {
                                self.advance();

                                let alias_token = self.get_token();
                                match alias_token.kind {
                                    TT::Identifier => {
                                        self.advance();

                                        Ok(Some(Node::new(
                                            Identifier_::new(alias_token.lexeme),
                                            &alias_token,
                                            &alias_token,
                                        )))
                                    }
                                    _ => Err(Error::expected_but_found(
                                        self.source,
                                        &identifier_token,
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
                            identifier_token.end_position
                        };

                        Ok(Some(Node {
                            value: Import_ {
                                module_name: Node::new(
                                    Identifier_::new(identifier_token.lexeme),
                                    &identifier_token,
                                    &identifier_token,
                                ),
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
                        &identifier_token,
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
                    self.module_definitions(top_level, &module_token, modules, definitions)
                }

                None => match self.binding()? {
                    Some(definition) => {
                        definitions.push(definition);
                        self.module_definitions(top_level, &module_token, modules, definitions)
                    }

                    None => match self.get_token().kind {
                        TT::Eof => Ok((modules, definitions)),
                        _ => Err(Error::expected_but_found(
                            self.source,
                            &self.get_token(),
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
                            &self.get_token(),
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

    fn expression(&mut self) -> ParseResult<'source, 'tokens, Expression> {
        match self.let_()? {
            Some(let_) => Ok(let_),
            None => match self.if_()? {
                Some(if_) => Ok(if_),
                None => match self.lambda()? {
                    Some(lambda) => Ok(lambda),
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

                let bindings = self.let_bindings(&let_token, vec![])?;

                if bindings.len() > 0 {
                    let is_in_keyword = if let TT::In = self.get_token().kind {
                        self.advance();
                        true
                    } else {
                        false
                    };
                    if is_in_keyword || self.is_token_after_line_and_same_indent_as(&let_token) {
                        let body = self.expression()?;

                        let line = let_token.line;
                        let column = let_token.column;
                        let start = let_token.position;
                        let end = body.end;
                        Ok(Some(Node {
                            value: Let(bindings, Box::new(body)),
                            line,
                            column,
                            start,
                            end,
                        }))
                    } else {
                        Err(Error::expected_but_found(
                            self.source,
                            &self.get_token(),
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
                        &let_token,
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
        let pattern = self.pattern()?;

        match pattern {
            Some(pattern) => {
                let equal_token = self.get_token();
                match equal_token.kind {
                    Equal => {
                        self.advance();

                        let value = self.expression()?;
                        Ok(Some(Definition { pattern, value }))
                    }

                    _ => Err(Error::expected_but_found(
                        self.source,
                        &equal_token,
                        None,
                        "Expected an = and an expression \
                                for the right side of let expression",
                    )),
                }
            }
            None => Ok(None),
        }
    }

    fn if_(&mut self) -> ParseResult<'source, 'tokens, Option<Expression>> {
        let token = self.get_token();

        match token.kind {
            TT::If => {
                self.advance();

                let condition = self.binary()?;

                let then_token = self.get_token();
                match then_token.kind {
                    Then => {
                        self.advance();
                        let then = self.expression()?;

                        let else_token = self.get_token();
                        match else_token.kind {
                            Else => {
                                self.advance();

                                let else_ = self.expression()?;

                                let line = token.line;
                                let column = token.column;
                                let start = token.position;
                                let end = else_.end;

                                Ok(Some(Node {
                                    value: If(Box::new(condition), Box::new(then), Box::new(else_)),
                                    line,
                                    column,
                                    start,
                                    end,
                                }))
                            }

                            _ => Err(Error::expected_but_found(
                                self.source,
                                &else_token,
                                None,
                                "Expected the `else` branch of the if expression",
                            )),
                        }
                    }

                    _ => Err(Error::expected_but_found(
                        self.source,
                        &then_token,
                        None,
                        "Expected the keyword `then` and an expression to parse the if expression",
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

                let arrow = self.get_token();
                match arrow.kind {
                    Arrow => {
                        self.advance();

                        let body = self.expression()?;
                        let line = token.line;
                        let column = token.column;
                        let start = token.position;
                        let end = body.end;

                        Ok(Some(Node {
                            value: Lambda(params, Box::new(body)),
                            line,
                            column,
                            start,
                            end,
                        }))
                    }

                    _ => Err(Error::expected_but_found(
                        self.source,
                        &token,
                        Some(arrow),
                        "Expected a -> after the list of parameters for the function",
                    )),
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

            None => Err(Error::expected_but_found(
                &self.source,
                &self.get_token(),
                None,
                "Expected a list of parameters",
            )),
        }
    }

    fn pattern(&mut self) -> ParseResult<'source, 'tokens, Option<Pattern>> {
        let token = self.get_token();

        match token.kind {
            TT::Identifier => {
                self.advance();
                Ok(Some(Node::new(
                    Pattern_::Identifier(Node::new(Identifier_::new(token.lexeme), &token, &token)),
                    &token,
                    &token,
                )))
            }
            _ => Ok(None),
        }
        /* For when we need to error out for more complex patterns:
        Err(
          Error::expected_but_found(
            self.input,
            token,
            "Expected a pattern (an identifier, destructuring a data structure, etc)",
          ),
        ) */
    }

    fn binary(&mut self) -> ParseResult<'source, 'tokens, Expression> {
        let expr = self.unary()?;

        self.binary_step().map(|mut binops| {
            // Make the binops options to be able to take them later
            let mut binops: Vec<Option<(Binop, Expression)>> =
                binops.drain(..).map(|b| Some(b)).collect();

            organize_binops(expr, &mut binops, &mut (0), 0)
        })
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

                    let op_node = Node::new(op, &token, &token);
                    let right = self.unary()?;
                    binops.push((op_node, right));
                }

                None => break,
            };
        }

        Ok(binops)
    }

    fn unary(&mut self) -> ParseResult<'source, 'tokens, Expression> {
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

        self.call().map(|expr| match u {
            Some(u) => {
                let op = Node::new(u, &token, &token);
                let line = op.line;
                let column = op.column;
                let start = op.start;
                let end = expr.end;
                Node {
                    value: Unary(op, Box::new(expr)),
                    line,
                    column,
                    start,
                    end,
                }
            }
            None => expr,
        })
    }

    fn call(&mut self) -> ParseResult<'source, 'tokens, Expression> {
        let token = self.get_token();

        self.primary().and_then(|expr| match expr {
            Some(expr) => self.arguments(&token, vec![]).map(|args| {
                if args.len() == 0 {
                    expr
                } else {
                    let last_arg = &args[args.len() - 1];

                    let line = expr.line;
                    let column = expr.column;
                    let start = expr.start;
                    let end = last_arg.end;
                    Node {
                        value: FnCall(Box::new(expr), args),
                        line,
                        column,
                        start,
                        end,
                    }
                }
            }),
            None => {
                let msg = "Expected an expression (a number, string, a let binding, \
                           function call, an identifier, etc.)";
                Err(Error::expected_but_found(self.source, &token, None, msg))
            }
        })
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

    fn primary(&mut self) -> ParseResult<'source, 'tokens, Option<Expression>> {
        let token = self.get_token();

        let result = match token.kind {
            False => Ok(Some(Node::new(Bool(false), &token, &token))),
            True => Ok(Some(Node::new(Bool(true), &token, &token))),

            TT::Float => {
                let lexeme = token.lexeme;
                let n = lexeme.parse::<f64>().map_err(|_| {
                    Error::new(
                        &self.source,
                        &token,
                        None,
                        &format!("Failed to parse number token '{}'", lexeme),
                    )
                })?;

                Ok(Some(Node::new(Float(n), &token, &token)))
            }

            TT::Identifier => Ok(Some(Node::new(
                Identifier(Node::new(Identifier_::new(token.lexeme), &token, &token)),
                &token,
                &token,
            ))),

            TT::String_ => {
                let lexeme = token.lexeme;
                let value = lexeme[1..(lexeme.len() - 1)].to_string();
                Ok(Some(Node::new(String_(value), &token, &token)))
            }

            LeftParen => {
                self.advance();

                let next_token = self.get_token();

                (match next_token.kind {
                    RightParen => Ok(Node::new(Unit, &token, next_token)),

                    _ => self.expression().and_then(|expr| {
                        let last_token = self.get_token();

                        match last_token.kind {
                            RightParen => Ok(expr),
                            _ => Err(Error::expected_but_found(
                                self.source,
                                &last_token,
                                Some(&token),
                                "Expected ')' after parenthesized expression",
                            )),
                        }
                    }),
                })
                .map(|ast| Some(ast))
            }

            _ => Ok(None),
        };

        // None of the branches advance after the last successful token, so we do it
        // here to avoid repetition
        match result {
            Ok(Some(_)) => self.advance(),
            _ => (),
        };

        result
    }

    // Utilities

    fn get_token(&self) -> &'tokens Token<'source> {
        self.tokens
            .get(self.current)
            .expect("Out of bounds access to tokens array")
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
                        value: Binary(
                            Box::new(Node::with_value_from_node(
                                Identifier(Node::with_value_from_node(op.value.fn_.clone(), &op)),
                                &op,
                            )),
                            op,
                            Box::new([left, right]),
                        ),
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
) -> ParseResult<'source, 'tokens, Box<Expression>> {
    let mut parser = State {
        source,
        tokens,
        current: 0,
    };

    parser.repl_entry().map(|e| Box::new(e))
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

        fn parse(code: &str) -> String {
            let source = Source::new_orphan(&code);
            let tokens = tokenizer::parse(&source).unwrap();
            let result = parse_repl(&source, &tokens);
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

import Apple as A exposing (orange)",
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

        fn parse(code: &str) -> String {
            let source = Source::new_orphan(&code);
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
