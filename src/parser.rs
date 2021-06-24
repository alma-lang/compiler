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

use std::rc::Rc;

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
    fn file(&mut self) -> ParseResult<'source, 'tokens, Vec<Rc<Module>>> {
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

    fn module(
        &mut self,
        top_level: bool,
    ) -> ParseResult<'source, 'tokens, Option<Vec<Rc<Module>>>> {
        let module_token = self.get_token();
        match module_token.kind {
            TT::Module => {
                self.advance();

                let identifier_token = self.get_token();
                match identifier_token.kind {
                    TT::Identifier => {
                        self.advance();
                        let name = identifier_token.lexeme;

                        let exports = self.exposing()?;

                        let imports = self.imports()?;

                        let (mut modules, definitions) =
                            self.module_definitions(top_level, &module_token, vec![], vec![])?;

                        let name = Node {
                            value: name.to_string(),
                            start: identifier_token.position,
                            end: identifier_token.end_position,
                            line: identifier_token.line,
                            column: identifier_token.column,
                        };

                        modules.push(Rc::new(Module {
                            name,
                            exports,
                            imports,
                            definitions,
                        }));

                        Ok(Some(modules))
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

    fn exposing(&mut self) -> ParseResult<'source, 'tokens, Vec<Rc<Export>>> {
        // ○ exposing       → "exposing" "(" IDENTIFIER ( "," IDENTIFIER )* ")"
        match self.get_token().kind {
            TT::Exposing => {
                self.advance();

                match self.get_token().kind {
                    TT::LeftParen => {
                        self.advance();

                        let export = self.export()?;

                        let exports = self.exposing_rest(vec![Rc::new(export)])?;

                        match self.get_token().kind {
                            TT::RightParen => {
                                self.advance();

                                Ok(exports)
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
            _ => Ok(vec![]),
        }
    }
    fn exposing_rest(
        &mut self,
        mut exports: Vec<Rc<Export>>,
    ) -> ParseResult<'source, 'tokens, Vec<Rc<Export>>> {
        match self.get_token().kind {
            TT::Comma => {
                self.advance();

                let export = self.export()?;
                exports.push(Rc::new(export));

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
    fn imports(&mut self) -> ParseResult<'source, 'tokens, Vec<Rc<Import>>> {
        let mut imports = vec![];

        while let Some(import) = self.import()? {
            imports.push(Rc::new(import));
        }

        Ok(imports)
    }

    fn import(&mut self) -> ParseResult<'source, 'tokens, Option<Import>> {
        match self.get_token().kind {
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
                                            alias_token.lexeme.to_string(),
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

                        let exposing = self.exposing()?;

                        Ok(Some(Node {
                            value: Import_ {
                                module_name: Node::new(
                                    identifier_token.lexeme.to_string(),
                                    &identifier_token,
                                    &identifier_token,
                                ),
                                alias,
                                exposing,
                            },
                            start: identifier_token.position,
                            end: self.get_token().position,
                            line: identifier_token.line,
                            column: identifier_token.column,
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
        mut modules: Vec<Rc<Module>>,
        mut definitions: Vec<Rc<Definition>>,
    ) -> ParseResult<'source, 'tokens, (Vec<Rc<Module>>, Vec<Rc<Definition>>)> {
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
                            value: Let(bindings, Rc::new(body)),
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
        mut bindings: Vec<Rc<Definition>>,
    ) -> ParseResult<'source, 'tokens, Vec<Rc<Definition>>> {
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

    fn binding(&mut self) -> ParseResult<'source, 'tokens, Option<Rc<Definition>>> {
        let pattern = self.pattern()?;

        match pattern {
            Some(pattern) => {
                let equal_token = self.get_token();
                match equal_token.kind {
                    Equal => {
                        self.advance();

                        let value = self.expression()?;
                        Ok(Some(Rc::new(Definition {
                            pattern: Rc::new(pattern),
                            value: Rc::new(value),
                        })))
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
                                    value: If(Rc::new(condition), Rc::new(then), Rc::new(else_)),
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
                            value: Lambda(params, Rc::new(body)),
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

    fn params(&mut self) -> ParseResult<'source, 'tokens, Vec<Rc<Pattern>>> {
        let pattern = self.pattern()?;

        match pattern {
            Some(pattern) => {
                let mut params = vec![Rc::new(pattern)];

                loop {
                    let pattern = self.pattern()?;

                    match pattern {
                        Some(pattern) => {
                            params.push(Rc::new(pattern));
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
                    Pattern_::Identifier(token.lexeme.to_string()),
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
                    value: Unary(op, Rc::new(expr)),
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
                        value: FnCall(Rc::new(expr), args),
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
        mut args: Vec<Rc<Expression>>,
    ) -> ParseResult<'source, 'tokens, Vec<Rc<Expression>>> {
        if self.is_token_in_same_line_or_nested_indent_from(first_token) {
            self.primary().and_then(|arg| {
                match arg {
                    // We tried to get an argument, but there was no match, or it was not well indented
                    None => Ok(args),

                    Some(arg) => {
                        args.push(Rc::new(arg));
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
                Identifier(token.lexeme.to_string()),
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
                        value: Binary(Rc::new(left), op, Rc::new(right)),
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
) -> ParseResult<'source, 'tokens, Vec<Rc<Module>>> {
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
) -> ParseResult<'source, 'tokens, Rc<Expression>> {
    let mut parser = State {
        source,
        tokens,
        current: 0,
    };

    parser.repl_entry().map(|e| Rc::new(e))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::{Token, Type as TT};
    use crate::tokenizer;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_expression_parser() {
        let tests = vec![
            (
                "True",
                Ok(Rc::new(Node {
                    value: Bool(true),
                    line: 1,
                    column: 0,
                    start: 0,
                    end: 4,
                })),
            ),
            (
                "False",
                Ok(Rc::new(Node {
                    value: Bool(false),
                    line: 1,
                    column: 0,
                    start: 0,
                    end: 5,
                })),
            ),
            (
                "()",
                Ok(Rc::new(Node {
                    value: Unit,
                    line: 1,
                    column: 0,
                    start: 0,
                    end: 2,
                })),
            ),
            (
                "123",
                Ok(Rc::new(Node {
                    value: Float(123.0),
                    line: 1,
                    column: 0,
                    start: 0,
                    end: 3,
                })),
            ),
            (
                "123.2",
                Ok(Rc::new(Node {
                    value: Float(123.2),
                    line: 1,
                    column: 0,
                    start: 0,
                    end: 5,
                })),
            ),
            (
                "variableOne",
                Ok(Rc::new(Node {
                    value: Identifier("variableOne".to_string()),
                    line: 1,
                    column: 0,
                    start: 0,
                    end: 11,
                })),
            ),
            (
                "variable_one",
                Ok(Rc::new(Node {
                    value: Identifier("variable_one".to_string()),
                    line: 1,
                    column: 0,
                    start: 0,
                    end: 12,
                })),
            ),
            (
                "espaÆàʥñÑol",
                Ok(Rc::new(Node {
                    value: Identifier("espaÆàʥñÑol".to_string()),
                    line: 1,
                    column: 0,
                    start: 0,
                    end: 16,
                })),
            ),
            (
                "\"😄\"",
                Ok(Rc::new(Node {
                    value: String_("😄".to_string()),
                    line: 1,
                    column: 0,
                    start: 0,
                    end: 6,
                })),
            ),
            (
                "\"\n\"",
                Ok(Rc::new(Node {
                    value: String_("\n".to_string()),
                    line: 1,
                    column: 0,
                    start: 0,
                    end: 3,
                })),
            ),
            (
                "\"\"",
                Ok(Rc::new(Node {
                    value: String_("".to_string()),
                    line: 1,
                    column: 0,
                    start: 0,
                    end: 2,
                })),
            ),
            (
                "(\"\")",
                Ok(Rc::new(Node {
                    value: String_("".to_string()),
                    line: 1,
                    column: 1,
                    start: 1,
                    end: 3,
                })),
            ),
            (
                "(((1)))",
                Ok(Rc::new(Node {
                    value: Float(1.0),
                    line: 1,
                    column: 3,
                    start: 3,
                    end: 4,
                })),
            ),
            (
                "(((1))",
                Err(Error {
                    message: {
                        "1:6: Expected ')' after parenthesized expression, but instead found: '[End of file]'

  1│  (((1))
   │  ↑".to_string()
                    },
                    token: &Token {
                        kind: TT::Eof,
                        line: 1,
                        indent: 0,
                        column: 6,
                        position: 6,
                        end_position: 6,
                        lexeme: "[End of file]",
                    },
                }),
            ),
            (
                "(((1))))",
                Err(Error {
                    message: "1:7: Expected the end of input, but instead found: ')'

  1│  (((1))))
   │         ↑"
                        .to_string(),
                    token: &Token {
                        kind: TT::RightParen,
                        line: 1,
                        indent: 0,
                        column: 7,
                        position: 7,
                        end_position: 8,
                        lexeme: ")",
                    },
                }),
            ),
            (
                "(
  ((1))
)",
                Ok(Rc::new(Node {
                    value: Float(1.0),
                    line: 2,
                    column: 4,
                    start: 6,
                    end: 7,
                })),
            ),
            (
                "fun arg",
                Ok(Rc::new(Node {
                    value: FnCall(
                        Rc::new(Node {
                            value: Identifier("fun".to_string()),
                            line: 1,
                            column: 0,
                            start: 0,
                            end: 3,
                        }),
                        vec![Rc::new(Node {
                            value: Identifier("arg".to_string()),
                            line: 1,
                            column: 4,
                            start: 4,
                            end: 7,
                        })],
                    ),
                    line: 1,
                    column: 0,
                    start: 0,
                    end: 7,
                })),
            ),
            (
                "fun\n arg",
                Ok(Rc::new(Node {
                    value: FnCall(
                        Rc::new(Node {
                            value: Identifier("fun".to_string()),
                            line: 1,
                            column: 0,
                            start: 0,
                            end: 3,
                        }),
                        vec![Rc::new(Node {
                            value: Identifier("arg".to_string()),
                            line: 2,
                            column: 1,
                            start: 5,
                            end: 8,
                        })],
                    ),
                    line: 1,
                    column: 0,
                    start: 0,
                    end: 8,
                })),
            ),
            (
                "  fun\n    arg",
                Ok(Rc::new(Node {
                    value: FnCall(
                        Rc::new(Node {
                            value: Identifier("fun".to_string()),
                            line: 1,
                            column: 2,
                            start: 2,
                            end: 5,
                        }),
                        vec![Rc::new(Node {
                            value: Identifier("arg".to_string()),
                            line: 2,
                            column: 4,
                            start: 10,
                            end: 13,
                        })],
                    ),
                    line: 1,
                    column: 2,
                    start: 2,
                    end: 13,
                })),
            ),
            (
                "fun\narg",
                Err(Error {
                    message: "2:0: Expected the end of input, but instead found: 'arg'

  1│  fun
  2│  arg
   │  ↑↑↑"
                        .to_string(),
                    token: &Token {
                        kind: TT::Identifier,
                        position: 4,
                        end_position: 7,
                        lexeme: "arg",
                        line: 2,
                        indent: 0,
                        column: 0,
                    },
                }),
            ),
            (
                "
fun arg1
  arg2 arg3
  arg4",
                Ok(Rc::new(Node {
                    value: FnCall(
                        Rc::new(Node {
                            value: Identifier("fun".to_string()),
                            line: 2,
                            column: 0,
                            start: 1,
                            end: 4,
                        }),
                        vec![
                            Rc::new(Node {
                                value: Identifier("arg1".to_string()),
                                line: 2,
                                column: 4,
                                start: 5,
                                end: 9,
                            }),
                            Rc::new(Node {
                                value: Identifier("arg2".to_string()),
                                line: 3,
                                column: 2,
                                start: 12,
                                end: 16,
                            }),
                            Rc::new(Node {
                                value: Identifier("arg3".to_string()),
                                line: 3,
                                column: 7,
                                start: 17,
                                end: 21,
                            }),
                            Rc::new(Node {
                                value: Identifier("arg4".to_string()),
                                line: 4,
                                column: 2,
                                start: 24,
                                end: 28,
                            }),
                        ],
                    ),
                    line: 2,
                    column: 0,
                    start: 1,
                    end: 28,
                })),
            ),
            (
                "hello ()",
                Ok(Rc::new(Node {
                    value: FnCall(
                        Rc::new(Node {
                            value: Identifier("hello".to_string()),
                            start: 0,
                            end: 5,
                            line: 1,
                            column: 0,
                        }),
                        vec![Rc::new(Node {
                            value: Unit,
                            start: 6,
                            end: 8,
                            line: 1,
                            column: 6,
                        })],
                    ),
                    start: 0,
                    end: 8,
                    line: 1,
                    column: 0,
                })),
            ),
            (
                "not False",
                Ok(Rc::new(Node {
                    value: Unary(
                        Node {
                            value: Not,
                            line: 1,
                            column: 0,
                            start: 0,
                            end: 3,
                        },
                        Rc::new(Node {
                            value: Bool(false),
                            line: 1,
                            column: 4,
                            start: 4,
                            end: 9,
                        }),
                    ),
                    line: 1,
                    column: 0,
                    start: 0,
                    end: 9,
                })),
            ),
            (
                "- 5",
                Ok(Rc::new(Node {
                    value: Unary(
                        Node {
                            value: Minus,
                            line: 1,
                            column: 0,
                            start: 0,
                            end: 1,
                        },
                        Rc::new(Node {
                            value: Float(5.),
                            line: 1,
                            column: 2,
                            start: 2,
                            end: 3,
                        }),
                    ),
                    line: 1,
                    column: 0,
                    start: 0,
                    end: 3,
                })),
            ),
            (
                "incr (-5)",
                Ok(Rc::new(Node {
                    value: FnCall(
                        Rc::new(Node {
                            value: Identifier("incr".to_string()),
                            line: 1,
                            column: 0,
                            start: 0,
                            end: 4,
                        }),
                        vec![Rc::new(Node {
                            value: Unary(
                                Node {
                                    value: Minus,
                                    line: 1,
                                    column: 6,
                                    start: 6,
                                    end: 7,
                                },
                                Rc::new(Node {
                                    value: Float(5.),
                                    line: 1,
                                    column: 7,
                                    start: 7,
                                    end: 8,
                                }),
                            ),
                            line: 1,
                            column: 6,
                            start: 6,
                            end: 8,
                        })],
                    ),
                    line: 1,
                    column: 0,
                    start: 0,
                    end: 8,
                })),
            ),
            (
                "1 - 5",
                Ok(Rc::new(Node {
                    value: Binary(
                        Rc::new(Node {
                            value: Float(1.),
                            line: 1,
                            column: 0,
                            start: 0,
                            end: 1,
                        }),
                        Node {
                            value: SUBSTRACTION.clone(),
                            line: 1,
                            column: 2,
                            start: 2,
                            end: 3,
                        },
                        Rc::new(Node {
                            value: Float(5.),
                            line: 1,
                            column: 4,
                            start: 4,
                            end: 5,
                        }),
                    ),
                    line: 1,
                    column: 0,
                    start: 0,
                    end: 5,
                })),
            ),
            (
                "1 - -5",
                Ok(Rc::new(Node {
                    value: Binary(
                        Rc::new(Node {
                            value: Float(1.),
                            line: 1,
                            column: 0,
                            start: 0,
                            end: 1,
                        }),
                        Node {
                            value: SUBSTRACTION.clone(),
                            line: 1,
                            column: 2,
                            start: 2,
                            end: 3,
                        },
                        Rc::new(Node {
                            value: Unary(
                                Node {
                                    value: Minus,
                                    line: 1,
                                    column: 4,
                                    start: 4,
                                    end: 5,
                                },
                                Rc::new(Node {
                                    value: Float(5.),
                                    line: 1,
                                    column: 5,
                                    start: 5,
                                    end: 6,
                                }),
                            ),
                            line: 1,
                            column: 4,
                            start: 4,
                            end: 6,
                        }),
                    ),
                    line: 1,
                    column: 0,
                    start: 0,
                    end: 6,
                })),
            ),
            (
                "1 + 2 / 3",
                Ok(Rc::new(Node {
                    line: 1,
                    column: 0,
                    start: 0,
                    end: 9,
                    value: Binary(
                        Rc::new(Node {
                            line: 1,
                            column: 0,
                            start: 0,
                            end: 1,
                            value: Float(1.),
                        }),
                        Node {
                            line: 1,
                            column: 2,
                            start: 2,
                            end: 3,
                            value: ADDITION.clone(),
                        },
                        Rc::new(Node {
                            line: 1,
                            column: 4,
                            start: 4,
                            end: 9,
                            value: Binary(
                                Rc::new(Node {
                                    line: 1,
                                    column: 4,
                                    start: 4,
                                    end: 5,
                                    value: Float(2.),
                                }),
                                Node {
                                    line: 1,
                                    column: 6,
                                    start: 6,
                                    end: 7,
                                    value: DIVISION.clone(),
                                },
                                Rc::new(Node {
                                    line: 1,
                                    column: 8,
                                    start: 8,
                                    end: 9,
                                    value: Float(3.),
                                }),
                            ),
                        }),
                    ),
                })),
            ),
            (
                "1 == 2 / 3",
                Ok(Rc::new(Node {
                    line: 1,
                    column: 0,
                    start: 0,
                    end: 10,
                    value: Binary(
                        Rc::new(Node {
                            line: 1,
                            column: 0,
                            start: 0,
                            end: 1,
                            value: Float(1.),
                        }),
                        Node {
                            line: 1,
                            column: 2,
                            start: 2,
                            end: 4,
                            value: EQUAL.clone(),
                        },
                        Rc::new(Node {
                            line: 1,
                            column: 5,
                            start: 5,
                            end: 10,
                            value: Binary(
                                Rc::new(Node {
                                    line: 1,
                                    column: 5,
                                    start: 5,
                                    end: 6,
                                    value: Float(2.),
                                }),
                                Node {
                                    line: 1,
                                    column: 7,
                                    start: 7,
                                    end: 8,
                                    value: DIVISION.clone(),
                                },
                                Rc::new(Node {
                                    line: 1,
                                    column: 9,
                                    start: 9,
                                    end: 10,
                                    value: Float(3.),
                                }),
                            ),
                        }),
                    ),
                })),
            ),
            (
                "\\a -> a",
                Ok(Rc::new(Node {
                    value: Lambda(
                        vec![Rc::new(Node {
                            value: Pattern_::Identifier("a".to_string()),
                            line: 1,
                            column: 1,
                            start: 1,
                            end: 2,
                        })],
                        Rc::new(Node {
                            value: Identifier("a".to_string()),
                            line: 1,
                            column: 6,
                            start: 6,
                            end: 7,
                        }),
                    ),
                    line: 1,
                    column: 0,
                    start: 0,
                    end: 7,
                })),
            ),
            (
                "\\a -> \\b -> a",
                Ok(Rc::new(Node {
                    value: Lambda(
                        vec![Rc::new(Node {
                            value: Pattern_::Identifier("a".to_string()),
                            line: 1,
                            column: 1,
                            start: 1,
                            end: 2,
                        })],
                        Rc::new(Node {
                            value: Lambda(
                                vec![Rc::new(Node {
                                    value: Pattern_::Identifier("b".to_string()),
                                    line: 1,
                                    column: 7,
                                    start: 7,
                                    end: 8,
                                })],
                                Rc::new(Node {
                                    value: Identifier("a".to_string()),
                                    line: 1,
                                    column: 12,
                                    start: 12,
                                    end: 13,
                                }),
                            ),
                            line: 1,
                            column: 6,
                            start: 6,
                            end: 13,
                        }),
                    ),
                    line: 1,
                    column: 0,
                    start: 0,
                    end: 13,
                })),
            ),
            (
                "\\a b -> a",
                Ok(Rc::new(Node {
                    value: Lambda(
                        vec![
                            Rc::new(Node {
                                value: Pattern_::Identifier("a".to_string()),
                                line: 1,
                                column: 1,
                                start: 1,
                                end: 2,
                            }),
                            Rc::new(Node {
                                value: Pattern_::Identifier("b".to_string()),
                                line: 1,
                                column: 3,
                                start: 3,
                                end: 4,
                            }),
                        ],
                        Rc::new(Node {
                            value: Identifier("a".to_string()),
                            line: 1,
                            column: 8,
                            start: 8,
                            end: 9,
                        }),
                    ),
                    line: 1,
                    column: 0,
                    start: 0,
                    end: 9,
                })),
            ),
            (
                "if True then 1 else 2",
                Ok(Rc::new(Node {
                    value: If(
                        Rc::new(Node {
                            value: Bool(true),
                            line: 1,
                            column: 3,
                            start: 3,
                            end: 7,
                        }),
                        Rc::new(Node {
                            value: Float(1.),
                            line: 1,
                            column: 13,
                            start: 13,
                            end: 14,
                        }),
                        Rc::new(Node {
                            value: Float(2.),
                            line: 1,
                            column: 20,
                            start: 20,
                            end: 21,
                        }),
                    ),
                    line: 1,
                    column: 0,
                    start: 0,
                    end: 21,
                })),
            ),
            (
                "
if True then
  1

else
  2",
                Ok(Rc::new(Node {
                    value: If(
                        Rc::new(Node {
                            value: Bool(true),
                            line: 2,
                            column: 3,
                            start: 4,
                            end: 8,
                        }),
                        Rc::new(Node {
                            value: Float(1.),
                            line: 3,
                            column: 2,
                            start: 16,
                            end: 17,
                        }),
                        Rc::new(Node {
                            value: Float(2.),
                            line: 6,
                            column: 2,
                            start: 26,
                            end: 27,
                        }),
                    ),
                    line: 2,
                    column: 0,
                    start: 1,
                    end: 27,
                })),
            ),
            (
                "if True then incr 1 else 2",
                Ok(Rc::new(Node {
                    value: If(
                        Rc::new(Node {
                            value: Bool(true),
                            line: 1,
                            column: 3,
                            start: 3,
                            end: 7,
                        }),
                        Rc::new(Node {
                            value: FnCall(
                                Rc::new(Node {
                                    value: Identifier("incr".to_string()),
                                    line: 1,
                                    column: 13,
                                    start: 13,
                                    end: 17,
                                }),
                                vec![Rc::new(Node {
                                    value: Float(1.),
                                    line: 1,
                                    column: 18,
                                    start: 18,
                                    end: 19,
                                })],
                            ),
                            line: 1,
                            column: 13,
                            start: 13,
                            end: 19,
                        }),
                        Rc::new(Node {
                            value: Float(2.),
                            line: 1,
                            column: 25,
                            start: 25,
                            end: 26,
                        }),
                    ),
                    line: 1,
                    column: 0,
                    start: 0,
                    end: 26,
                })),
            ),
            (
                "if True then if False then 1 else 3 else 2",
                Ok(Rc::new(Node {
                    value: If(
                        Rc::new(Node {
                            value: Bool(true),
                            line: 1,
                            column: 3,
                            start: 3,
                            end: 7,
                        }),
                        Rc::new(Node {
                            value: If(
                                Rc::new(Node {
                                    value: Bool(false),
                                    line: 1,
                                    column: 16,
                                    start: 16,
                                    end: 21,
                                }),
                                Rc::new(Node {
                                    value: Float(1.),
                                    line: 1,
                                    column: 27,
                                    start: 27,
                                    end: 28,
                                }),
                                Rc::new(Node {
                                    value: Float(3.),
                                    line: 1,
                                    column: 34,
                                    start: 34,
                                    end: 35,
                                }),
                            ),
                            line: 1,
                            column: 13,
                            start: 13,
                            end: 35,
                        }),
                        Rc::new(Node {
                            value: Float(2.),
                            line: 1,
                            column: 41,
                            start: 41,
                            end: 42,
                        }),
                    ),
                    line: 1,
                    column: 0,
                    start: 0,
                    end: 42,
                })),
            ),
            (
                "if True { 1 } else 2",
                Err(Error {
                    message: {
                        "1:8: Expected the keyword `then` and an expression to parse the if expression, but instead found: '{'\n
  1│  if True { 1 } else 2
   │          ↑".to_string()
                    },
                    token: &Token {
                        kind: LeftBrace,
                        position: 8,
                        end_position: 9,
                        lexeme: "{",
                        line: 1,
                        column: 8,
                        indent: 0,
                    },
                }),
            ),
            (
                "if True then 1",
                Err(Error {
                    message: {
                        "1:14: Expected the `else` branch of the if expression, but instead found: '[End of file]'

  1│  if True then 1
   │                ↑".to_string()
                    },
                    token: &Token {
                        kind: Eof,
                        position: 14,
                        end_position: 14,
                        lexeme: "[End of file]",
                        line: 1,
                        column: 14,
                        indent: 0,
                    },
                }),
            ),
            (
                "let x = 1\nx",
                Ok(Rc::new(Node {
                    value: Let(
                        vec![Rc::new(Definition {
                            pattern: Rc::new(Node {
                                value: Pattern_::Identifier("x".to_string()),
                                line: 1,
                                column: 4,
                                start: 4,
                                end: 5,
                            }),
                            value: Rc::new(Node {
                                value: Float(1.),
                                line: 1,
                                column: 8,
                                start: 8,
                                end: 9,
                            }),
                        })],
                        Rc::new(Node {
                            value: Identifier("x".to_string()),
                            line: 2,
                            column: 0,
                            start: 10,
                            end: 11,
                        }),
                    ),
                    line: 1,
                    column: 0,
                    start: 0,
                    end: 11,
                })),
            ),
            (
                "let x = a\n  x",
                Err(Error {
                    message: {
                        "2:3: Expected the let definition to be followed by another expression in the next line and same indentation, but instead found: '[End of file]'

  1│  let x = a
  2│    x
   │     ↑".to_string()
                    },
                    token: &Token {
                        kind: Eof,
                        position: 13,
                        end_position: 13,
                        lexeme: "[End of file]",
                        line: 2,
                        column: 3,
                        indent: 2,
                    },
                }),
            ),
            (
                "let x = a\n  x\nx",
                Ok(Rc::new(Node {
                    value: Let(
                        vec![Rc::new(Definition {
                            pattern: Rc::new(Node {
                                value: Pattern_::Identifier("x".to_string()),
                                line: 1,
                                column: 4,
                                start: 4,
                                end: 5,
                            }),
                            value: Rc::new(Node {
                                value: FnCall(
                                    Rc::new(Node {
                                        value: Identifier("a".to_string()),
                                        line: 1,
                                        column: 8,
                                        start: 8,
                                        end: 9,
                                    }),
                                    vec![Rc::new(Node {
                                        value: Identifier("x".to_string()),
                                        line: 2,
                                        column: 2,
                                        start: 12,
                                        end: 13,
                                    })],
                                ),
                                line: 1,
                                column: 8,
                                start: 8,
                                end: 13,
                            }),
                        })],
                        Rc::new(Node {
                            value: Identifier("x".to_string()),
                            line: 3,
                            column: 0,
                            start: 14,
                            end: 15,
                        }),
                    ),
                    line: 1,
                    column: 0,
                    start: 0,
                    end: 15,
                })),
            ),
            (
                "let x = a x in x",
                Ok(Rc::new(Node {
                    value: Let(
                        vec![Rc::new(Definition {
                            pattern: Rc::new(Node {
                                value: Pattern_::Identifier("x".to_string()),
                                line: 1,
                                column: 4,
                                start: 4,
                                end: 5,
                            }),
                            value: Rc::new(Node {
                                value: FnCall(
                                    Rc::new(Node {
                                        value: Identifier("a".to_string()),
                                        line: 1,
                                        column: 8,
                                        start: 8,
                                        end: 9,
                                    }),
                                    vec![Rc::new(Node {
                                        value: Identifier("x".to_string()),
                                        line: 1,
                                        column: 10,
                                        start: 10,
                                        end: 11,
                                    })],
                                ),
                                line: 1,
                                column: 8,
                                start: 8,
                                end: 11,
                            }),
                        })],
                        Rc::new(Node {
                            value: Identifier("x".to_string()),
                            line: 1,
                            column: 15,
                            start: 15,
                            end: 16,
                        }),
                    ),
                    line: 1,
                    column: 0,
                    start: 0,
                    end: 16,
                })),
            ),
            (
                "let x = a\n  x\nin\nx",
                Ok(Rc::new(Node {
                    value: Let(
                        vec![Rc::new(Definition {
                            pattern: Rc::new(Node {
                                value: Pattern_::Identifier("x".to_string()),
                                line: 1,
                                column: 4,
                                start: 4,
                                end: 5,
                            }),
                            value: Rc::new(Node {
                                value: FnCall(
                                    Rc::new(Node {
                                        value: Identifier("a".to_string()),
                                        line: 1,
                                        column: 8,
                                        start: 8,
                                        end: 9,
                                    }),
                                    vec![Rc::new(Node {
                                        value: Identifier("x".to_string()),
                                        line: 2,
                                        column: 2,
                                        start: 12,
                                        end: 13,
                                    })],
                                ),
                                line: 1,
                                column: 8,
                                start: 8,
                                end: 13,
                            }),
                        })],
                        Rc::new(Node {
                            value: Identifier("x".to_string()),
                            line: 4,
                            column: 0,
                            start: 17,
                            end: 18,
                        }),
                    ),
                    line: 1,
                    column: 0,
                    start: 0,
                    end: 18,
                })),
            ),
            (
                "let\n  x = a\n  x\nin\nx",
                Err(Error {
                    message: {
                        "4:0: Expected an = and an expression for the right side of let expression, but instead found: \'in\'\n\n  2│    x = a\n  3│    x\n  4│  in\n   │  ↑↑\n  5│  x".to_string()
                    },
                    token: &Token {
                        kind: TT::In,
                        position: 16,
                        end_position: 18,
                        lexeme: "in",
                        line: 4,
                        column: 0,
                        indent: 0,
                    },
                }),
            ),
            (
                "let\n  x = a\n    x\nin\nx",
                Ok(Rc::new(Node {
                    value: Let(
                        vec![Rc::new(Definition {
                            pattern: Rc::new(Node {
                                value: Pattern_::Identifier("x".to_string()),
                                line: 2,
                                column: 2,
                                start: 6,
                                end: 7,
                            }),
                            value: Rc::new(Node {
                                value: FnCall(
                                    Rc::new(Node {
                                        value: Identifier("a".to_string()),
                                        line: 2,
                                        column: 6,
                                        start: 10,
                                        end: 11,
                                    }),
                                    vec![Rc::new(Node {
                                        value: Identifier("x".to_string()),
                                        line: 3,
                                        column: 4,
                                        start: 16,
                                        end: 17,
                                    })],
                                ),
                                line: 2,
                                column: 6,
                                start: 10,
                                end: 17,
                            }),
                        })],
                        Rc::new(Node {
                            value: Identifier("x".to_string()),
                            line: 5,
                            column: 0,
                            start: 21,
                            end: 22,
                        }),
                    ),
                    line: 1,
                    column: 0,
                    start: 0,
                    end: 22,
                })),
            ),
            (
                "let\n  x = a\n    x\n b = 5\nin\nx",
                Ok(Rc::new(Node {
                    value: Let(
                        vec![
                            Rc::new(Definition {
                                pattern: Rc::new(Node {
                                    value: Pattern_::Identifier("x".to_string()),
                                    line: 2,
                                    column: 2,
                                    start: 6,
                                    end: 7,
                                }),
                                value: Rc::new(Node {
                                    value: FnCall(
                                        Rc::new(Node {
                                            value: Identifier("a".to_string()),
                                            line: 2,
                                            column: 6,
                                            start: 10,
                                            end: 11,
                                        }),
                                        vec![Rc::new(Node {
                                            value: Identifier("x".to_string()),
                                            line: 3,
                                            column: 4,
                                            start: 16,
                                            end: 17,
                                        })],
                                    ),
                                    line: 2,
                                    column: 6,
                                    start: 10,
                                    end: 17,
                                }),
                            }),
                            Rc::new(Definition {
                                pattern: Rc::new(Node {
                                    start: 19,
                                    end: 20,
                                    line: 4,
                                    column: 1,
                                    value: Pattern_::Identifier("b".to_string()),
                                }),
                                value: Rc::new(Node {
                                    start: 23,
                                    end: 24,
                                    line: 4,
                                    column: 5,
                                    value: Float(5.0),
                                }),
                            }),
                        ],
                        Rc::new(Node {
                            value: Identifier("x".to_string()),
                            line: 6,
                            column: 0,
                            start: 28,
                            end: 29,
                        }),
                    ),
                    line: 1,
                    column: 0,
                    start: 0,
                    end: 29,
                })),
            ),
        ];

        for (code, expected) in tests {
            let source = Source::new_orphan(&code);
            let tokens = tokenizer::parse(&source).unwrap();
            let result = parse_repl(&source, &tokens);
            assert_eq!(
                result,
                expected,
                "\n\nInput:\n\n{}\n\nExpected:\n\n{}",
                &code,
                match &result {
                    Ok(ast) => format!("{:#?}", ast),
                    Err(e) => e.message.clone(),
                }
            );
        }
    }

    #[test]
    fn test_module_parser() {
        let tests = vec![
            (
                "True",
                Err(Error {
                    message: {
                        "1:0: Expected `module FileName` at the start of the file, but instead found: \'True\'\n\n  1│  True\n   │  ↑↑↑↑".to_string()
                    },
                    token: &Token {
                        kind: True,
                        position: 0,
                        end_position: 4,
                        lexeme: "True",
                        line: 1,
                        column: 0,
                        indent: 0,
                    },
                }),
            ),
            (
                "module Test",
                Ok(vec![Rc::new(Module {
                    name: Node {
                        start: 7,
                        end: 11,
                        line: 1,
                        column: 7,
                        value: "Test".to_string(),
                    },
                    exports: vec![],
                    imports: vec![],
                    definitions: vec![],
                })]),
            ),
            (
                "module Test\n\na = 1",
                Ok(vec![Rc::new(Module {
                    name: Node {
                        start: 7,
                        end: 11,
                        line: 1,
                        column: 7,
                        value: "Test".to_string(),
                    },
                    exports: vec![],
                    imports: vec![],
                    definitions: vec![Rc::new(Definition {
                        pattern: Rc::new(Node {
                            start: 13,
                            end: 14,
                            line: 3,
                            column: 0,
                            value: Pattern_::Identifier("a".to_string()),
                        }),
                        value: Rc::new(Node {
                            start: 17,
                            end: 18,
                            line: 3,
                            column: 4,
                            value: Float(1.0),
                        }),
                    })],
                })]),
            ),
            (
                "module Test\n\na = 1\n\nb = True",
                Ok(vec![Rc::new(Module {
                    name: Node {
                        start: 7,
                        end: 11,
                        line: 1,
                        column: 7,
                        value: "Test".to_string(),
                    },
                    exports: vec![],
                    imports: vec![],
                    definitions: vec![
                        Rc::new(Definition {
                            pattern: Rc::new(Node {
                                start: 13,
                                end: 14,
                                line: 3,
                                column: 0,
                                value: Pattern_::Identifier("a".to_string()),
                            }),
                            value: Rc::new(Node {
                                start: 17,
                                end: 18,
                                line: 3,
                                column: 4,
                                value: Float(1.0),
                            }),
                        }),
                        Rc::new(Definition {
                            pattern: Rc::new(Node {
                                start: 20,
                                end: 21,
                                line: 5,
                                column: 0,
                                value: Pattern_::Identifier("b".to_string()),
                            }),
                            value: Rc::new(Node {
                                start: 24,
                                end: 28,
                                line: 5,
                                column: 4,
                                value: Bool(true),
                            }),
                        }),
                    ],
                })]),
            ),
            (
                "module Test\n\na = 1 + 2) + 3\n\nb = * add\n  5",
                Err(Error {
                    message: {
                        "3:9: Expected a definition like `n = 5` or `add x y = x + y`, but instead found: \')\'\n\n  1│  module Test\n  2│  \n  3│  a = 1 + 2) + 3\n   │           ↑\n  4│  \n  5│  b = * add".to_string()
                    },
                    token: &Token {
                        kind: RightParen,
                        position: 22,
                        end_position: 23,
                        lexeme: ")",
                        line: 3,
                        column: 9,
                        indent: 0,
                    },
                }),
            ),
            (
                "module Test\n\na = 1 + 2 + 3\n\nb = * add\n  5",
                Err(Error {
                    message: {
                        "5:4: Expected an expression (a number, string, a let binding, function call, an identifier, etc.), but instead found: \'*\'\n\n  3│  a = 1 + 2 + 3\n  4│  \n  5│  b = * add\n   │      ↑\n  6│    5".to_string()
                    },
                    token: &Token {
                        kind: Star,
                        position: 32,
                        end_position: 33,
                        lexeme: "*",
                        line: 5,
                        column: 4,
                        indent: 0,
                    },
                }),
            ),
            (
                "module Parent\n\nmodule Test",
                Ok(vec![
                    Rc::new(Module {
                        name: Node {
                            start: 22,
                            end: 26,
                            line: 3,
                            column: 7,
                            value: "Test".to_string(),
                        },
                        exports: vec![],
                        imports: vec![],
                        definitions: vec![],
                    }),
                    Rc::new(Module {
                        name: Node {
                            start: 7,
                            end: 13,
                            line: 1,
                            column: 7,
                            value: "Parent".to_string(),
                        },
                        exports: vec![],
                        imports: vec![],
                        definitions: vec![],
                    }),
                ]),
            ),
            (
                "module Parent\n\nmodule Test\n\n  a = 1",
                Ok(vec![
                    Rc::new(Module {
                        name: Node {
                            start: 22,
                            end: 26,
                            line: 3,
                            column: 7,
                            value: "Test".to_string(),
                        },
                        exports: vec![],
                        imports: vec![],
                        definitions: vec![Rc::new(Definition {
                            pattern: Rc::new(Node {
                                start: 30,
                                end: 31,
                                line: 5,
                                column: 2,
                                value: Pattern_::Identifier("a".to_string()),
                            }),
                            value: Rc::new(Node {
                                start: 34,
                                end: 35,
                                line: 5,
                                column: 6,
                                value: Float(1.0),
                            }),
                        })],
                    }),
                    Rc::new(Module {
                        name: Node {
                            start: 7,
                            end: 13,
                            line: 1,
                            column: 7,
                            value: "Parent".to_string(),
                        },
                        exports: vec![],
                        imports: vec![],
                        definitions: vec![],
                    }),
                ]),
            ),
            (
                "module Parent\n\nmodule Test\n\n  a = 1\n\na = 1\n",
                Ok(vec![
                    Rc::new(Module {
                        name: Node {
                            start: 22,
                            end: 26,
                            line: 3,
                            column: 7,
                            value: "Test".to_string(),
                        },
                        exports: vec![],
                        imports: vec![],
                        definitions: vec![Rc::new(Definition {
                            pattern: Rc::new(Node {
                                start: 30,
                                end: 31,
                                line: 5,
                                column: 2,
                                value: Pattern_::Identifier("a".to_string()),
                            }),
                            value: Rc::new(Node {
                                start: 34,
                                end: 35,
                                line: 5,
                                column: 6,
                                value: Float(1.0),
                            }),
                        })],
                    }),
                    Rc::new(Module {
                        name: Node {
                            start: 7,
                            end: 13,
                            line: 1,
                            column: 7,
                            value: "Parent".to_string(),
                        },
                        exports: vec![],
                        imports: vec![],
                        definitions: vec![Rc::new(Definition {
                            pattern: Rc::new(Node {
                                start: 37,
                                end: 38,
                                line: 7,
                                column: 0,
                                value: Pattern_::Identifier("a".to_string()),
                            }),
                            value: Rc::new(Node {
                                start: 41,
                                end: 42,
                                line: 7,
                                column: 4,
                                value: Float(1.0),
                            }),
                        })],
                    }),
                ]),
            ),
            (
                {
                    "module Parent\n\nmodule Test\n\n  a = 1 + 2) + 3\n\n  b = * add\n    5\n\na = 1 + 2) + 3\n\nb = * add\n  5"
                },
                Err(Error {
                    message: {
                        "5:11: Expected a definition like `n = 5` or `add x y = x + y`, but instead found: \')\'\n\n  3│  module Test\n  4│  \n  5│    a = 1 + 2) + 3\n   │             ↑\n  6│  \n  7│    b = * add".to_string()
                    },
                    token: &Token {
                        kind: RightParen,
                        position: 39,
                        end_position: 40,
                        lexeme: ")",
                        line: 5,
                        column: 11,
                        indent: 2,
                    },
                }),
            ),
            (
                "module Parent

module Test1
  a = 1

  module Test1Test
    b = 1

module Test2
    c = 5
",
                Ok(vec![
                    Rc::new(Module {
                        name: Node {
                            start: 46,
                            end: 55,
                            line: 6,
                            column: 9,
                            value: "Test1Test".to_string(),
                        },
                        exports: vec![],
                        imports: vec![],
                        definitions: vec![Rc::new(Definition {
                            pattern: Rc::new(Node {
                                start: 60,
                                end: 61,
                                line: 7,
                                column: 4,
                                value: Pattern_::Identifier("b".to_string()),
                            }),
                            value: Rc::new(Node {
                                start: 64,
                                end: 65,
                                line: 7,
                                column: 8,
                                value: Float(1.0),
                            }),
                        })],
                    }),
                    Rc::new(Module {
                        name: Node {
                            start: 22,
                            end: 27,
                            line: 3,
                            column: 7,
                            value: "Test1".to_string(),
                        },
                        exports: vec![],
                        imports: vec![],
                        definitions: vec![Rc::new(Definition {
                            pattern: Rc::new(Node {
                                start: 30,
                                end: 31,
                                line: 4,
                                column: 2,
                                value: Pattern_::Identifier("a".to_string()),
                            }),
                            value: Rc::new(Node {
                                start: 34,
                                end: 35,
                                line: 4,
                                column: 6,
                                value: Float(1.0),
                            }),
                        })],
                    }),
                    Rc::new(Module {
                        name: Node {
                            start: 74,
                            end: 79,
                            line: 9,
                            column: 7,
                            value: "Test2".to_string(),
                        },
                        exports: vec![],
                        imports: vec![],
                        definitions: vec![Rc::new(Definition {
                            pattern: Rc::new(Node {
                                start: 84,
                                end: 85,
                                line: 10,
                                column: 4,
                                value: Pattern_::Identifier("c".to_string()),
                            }),
                            value: Rc::new(Node {
                                start: 88,
                                end: 89,
                                line: 10,
                                column: 8,
                                value: Float(5.0),
                            }),
                        })],
                    }),
                    Rc::new(Module {
                        name: Node {
                            start: 7,
                            end: 13,
                            line: 1,
                            column: 7,
                            value: "Parent".to_string(),
                        },
                        exports: vec![],
                        imports: vec![],
                        definitions: vec![],
                    }),
                ]),
            ),
            (
                "module Test exposing (a)\n\na = 1\n\nb = True",
                Ok(vec![Rc::new(Module {
                    name: Node {
                        start: 7,
                        end: 11,
                        line: 1,
                        column: 7,
                        value: "Test".to_string(),
                    },
                    exports: vec![Rc::new(Node {
                        start: 22,
                        end: 23,
                        line: 1,
                        column: 22,
                        value: Export_("a".to_string()),
                    })],
                    imports: vec![],
                    definitions: vec![
                        Rc::new(Definition {
                            pattern: Rc::new(Node {
                                start: 26,
                                end: 27,
                                line: 3,
                                column: 0,
                                value: Pattern_::Identifier("a".to_string()),
                            }),
                            value: Rc::new(Node {
                                start: 30,
                                end: 31,
                                line: 3,
                                column: 4,
                                value: Float(1.0),
                            }),
                        }),
                        Rc::new(Definition {
                            pattern: Rc::new(Node {
                                start: 33,
                                end: 34,
                                line: 5,
                                column: 0,
                                value: Pattern_::Identifier("b".to_string()),
                            }),
                            value: Rc::new(Node {
                                start: 37,
                                end: 41,
                                line: 5,
                                column: 4,
                                value: Bool(true),
                            }),
                        }),
                    ],
                })]),
            ),
            (
                "module Test exposing (a, b)\n\na = 1\n\nb = True",
                Ok(vec![Rc::new(Module {
                    name: Node {
                        start: 7,
                        end: 11,
                        line: 1,
                        column: 7,
                        value: "Test".to_string(),
                    },
                    exports: vec![
                        Rc::new(Node {
                            start: 22,
                            end: 23,
                            line: 1,
                            column: 22,
                            value: Export_("a".to_string()),
                        }),
                        Rc::new(Node {
                            start: 25,
                            end: 26,
                            line: 1,
                            column: 25,
                            value: Export_("b".to_string()),
                        }),
                    ],
                    imports: vec![],
                    definitions: vec![
                        Rc::new(Definition {
                            pattern: Rc::new(Node {
                                start: 29,
                                end: 30,
                                line: 3,
                                column: 0,
                                value: Pattern_::Identifier("a".to_string()),
                            }),
                            value: Rc::new(Node {
                                start: 33,
                                end: 34,
                                line: 3,
                                column: 4,
                                value: Float(1.0),
                            }),
                        }),
                        Rc::new(Definition {
                            pattern: Rc::new(Node {
                                start: 36,
                                end: 37,
                                line: 5,
                                column: 0,
                                value: Pattern_::Identifier("b".to_string()),
                            }),
                            value: Rc::new(Node {
                                start: 40,
                                end: 44,
                                line: 5,
                                column: 4,
                                value: Bool(true),
                            }),
                        }),
                    ],
                })]),
            ),
            (
                "module Test exposing a, b\n\na = 1\n\nb = True",
                Err(Error {
                    message: {
                        "1:21: Parsing the module exports expected a comma separated list of exports inside parenthesis, but instead found: \'a\'\n\n  1│  module Test exposing a, b\n   │                       ↑\n  2│  \n  3│  a = 1".to_string()
                    },
                    token: &Token {
                        kind: TT::Identifier,
                        position: 21,
                        end_position: 22,
                        lexeme: "a",
                        line: 1,
                        column: 21,
                        indent: 0,
                    },
                }),
            ),
            (
                "module Test exposing (a b)\n\na = 1\n\nb = True",
                Err(Error {
                    message: {
                        "1:24: Parsing the module exports expected a comma separated list of exports inside parenthesis, but instead found: \'b\'\n\n  1│  module Test exposing (a b)\n   │                          ↑\n  2│  \n  3│  a = 1".to_string()
                    },
                    token: &Token {
                        kind: TT::Identifier,
                        position: 24,
                        end_position: 25,
                        lexeme: "b",
                        line: 1,
                        column: 24,
                        indent: 0,
                    },
                }),
            ),
            (
                "module Test exposing (a, b\n\na = 1\n\nb = True",
                Err(Error {
                    message: {
                        "3:0: Parsing the module exports expected a comma separated list of exports inside parenthesis, but instead found: \'a\'\n\n  1│  module Test exposing (a, b\n  2│  \n  3│  a = 1\n   │  ↑\n  4│  \n  5│  b = True".to_string()
                    },
                    token: &Token {
                        kind: TT::Identifier,
                        position: 28,
                        end_position: 29,
                        lexeme: "a",
                        line: 3,
                        column: 0,
                        indent: 0,
                    },
                }),
            ),
            (
                "module Test\n\nimport Banana",
                Ok(vec![Rc::new(Module {
                    name: Node {
                        start: 7,
                        end: 11,
                        line: 1,
                        column: 7,
                        value: "Test".to_string(),
                    },
                    exports: vec![],
                    imports: vec![Rc::new(Node {
                        start: 20,
                        end: 26,
                        line: 3,
                        column: 7,
                        value: Import_ {
                            module_name: Node {
                                start: 20,
                                end: 26,
                                line: 3,
                                column: 7,
                                value: "Banana".to_string(),
                            },
                            alias: None,
                            exposing: vec![],
                        },
                    })],
                    definitions: vec![],
                })]),
            ),
            (
                "module Test\n\nimport Banana as B",
                Ok(vec![Rc::new(Module {
                    name: Node {
                        start: 7,
                        end: 11,
                        line: 1,
                        column: 7,
                        value: "Test".to_string(),
                    },
                    exports: vec![],
                    imports: vec![Rc::new(Node {
                        start: 20,
                        end: 31,
                        line: 3,
                        column: 7,
                        value: Import_ {
                            module_name: Node {
                                start: 20,
                                end: 26,
                                line: 3,
                                column: 7,
                                value: "Banana".to_string(),
                            },
                            alias: Some(Node {
                                start: 30,
                                end: 31,
                                line: 3,
                                column: 17,
                                value: "B".to_string(),
                            }),
                            exposing: vec![],
                        },
                    })],
                    definitions: vec![],
                })]),
            ),
            (
                "module Test\n\nimport Banana exposing (phone)",
                Ok(vec![Rc::new(Module {
                    name: Node {
                        start: 7,
                        end: 11,
                        line: 1,
                        column: 7,
                        value: "Test".to_string(),
                    },
                    exports: vec![],
                    imports: vec![Rc::new(Node {
                        start: 20,
                        end: 43,
                        line: 3,
                        column: 7,
                        value: Import_ {
                            module_name: Node {
                                start: 20,
                                end: 26,
                                line: 3,
                                column: 7,
                                value: "Banana".to_string(),
                            },
                            alias: None,
                            exposing: vec![Rc::new(Node {
                                start: 37,
                                end: 42,
                                line: 3,
                                column: 24,
                                value: Export_("phone".to_string()),
                            })],
                        },
                    })],
                    definitions: vec![],
                })]),
            ),
            (
                "module Test

import Banana as B

import Phone exposing (raffi)

import Apple as A exposing (orange)",
                Ok(vec![Rc::new(Module {
                    name: Node {
                        start: 7,
                        end: 11,
                        line: 1,
                        column: 7,
                        value: "Test".to_string(),
                    },
                    exports: vec![],
                    imports: vec![
                        Rc::new(Node {
                            start: 20,
                            end: 33,
                            line: 3,
                            column: 7,
                            value: Import_ {
                                module_name: Node {
                                    start: 20,
                                    end: 26,
                                    line: 3,
                                    column: 7,
                                    value: "Banana".to_string(),
                                },
                                alias: Some(Node {
                                    start: 30,
                                    end: 31,
                                    line: 3,
                                    column: 17,
                                    value: "B".to_string(),
                                }),
                                exposing: vec![],
                            },
                        }),
                        Rc::new(Node {
                            start: 40,
                            end: 64,
                            line: 5,
                            column: 7,
                            value: Import_ {
                                module_name: Node {
                                    start: 40,
                                    end: 45,
                                    line: 5,
                                    column: 7,
                                    value: "Phone".to_string(),
                                },
                                alias: None,
                                exposing: vec![Rc::new(Node {
                                    start: 56,
                                    end: 61,
                                    line: 5,
                                    column: 23,
                                    value: Export_("raffi".to_string()),
                                })],
                            },
                        }),
                        Rc::new(Node {
                            start: 71,
                            end: 99,
                            line: 7,
                            column: 7,
                            value: Import_ {
                                module_name: Node {
                                    start: 71,
                                    end: 76,
                                    line: 7,
                                    column: 7,
                                    value: "Apple".to_string(),
                                },
                                alias: Some(Node {
                                    start: 80,
                                    end: 81,
                                    line: 7,
                                    column: 16,
                                    value: "A".to_string(),
                                }),
                                exposing: vec![Rc::new(Node {
                                    start: 92,
                                    end: 98,
                                    line: 7,
                                    column: 28,
                                    value: Export_("orange".to_string()),
                                })],
                            },
                        }),
                    ],
                    definitions: vec![],
                })]),
            ),
        ];

        for (code, expected) in tests {
            let source = Source::new_orphan(&code);
            let tokens = tokenizer::parse(&source).unwrap();
            let result = parse(&source, &tokens);
            assert_eq!(
                result,
                expected,
                "\n\nInput:\n\n{}\n\nExpected:\n\n{}",
                &code,
                match &result {
                    Ok(ast) => format!("{:#?}", ast),
                    Err(e) => e.message.clone(),
                }
            );
        }
    }
}
