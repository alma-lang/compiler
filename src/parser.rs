/* Grammar draft (●○):
    ● file           → expression EOF
    ● expression     → let | lambda | if | binary
    ● let            → "let" binding "=" expression expression
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
    Node, Pattern, Pattern_,
    Unary_::{Minus, Not},
};
use crate::source::Source;
use crate::token::{
    self, Token,
    Type::{self as TT, *},
};

#[derive(PartialEq, Debug)]
pub struct Error {
    message: String,
    token: Token,
}

impl Error {
    pub fn to_string(&self, source: &Source) -> String {
        let mut msg = String::new();

        msg.push_str(&format!(
            "{}:{}:{}\n\n{}",
            source.name(),
            self.token.line,
            self.token.column,
            self.message
        ));

        msg
    }
}

impl Error {
    fn new(
        source: &Source,
        token: &Token,
        point_at_token: Option<&Token>,
        message: String,
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

        Self {
            message,
            token: (*token).clone(),
        }
    }

    fn expected_but_found(
        source: &Source,
        token: &Token,
        point_at_token: Option<&Token>,
        message: String,
    ) -> Self {
        Self::new(
            source,
            token,
            point_at_token,
            format!("{}, but instead found: '{}'", message, token.lexeme(source)),
        )
    }
}

type ParseResults<A> = Result<A, Vec<Error>>;
type ParseResult<A> = Result<A, Error>;

#[derive(Debug)]
struct State<'a> {
    source: &'a Source<'a>,
    tokens: Vec<Token>,
    current: usize,
}

impl<'a> State<'a> {
    fn get_token(&self) -> &Token {
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

    fn is_next_line_same_indent(&self, parent_token: &Token) -> bool {
        let token = self.get_token();
        parent_token.line < token.line && token.indent == parent_token.indent
    }

    // ---

    pub fn file(&mut self) -> ParseResults<Expression> {
        match self.expression() {
            Err(e) => Err(vec![e]),
            Ok(a) => match self.get_token() {
                Token {
                    kind: token::Type::Eof,
                    ..
                } => Ok(a),
                token => Err(vec![Error::expected_but_found(
                    self.source,
                    token,
                    None,
                    "Expected the end of input".to_owned(),
                )]),
            },
        }
    }

    fn expression(&mut self) -> ParseResult<Expression> {
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

    fn let_(&mut self) -> ParseResult<Option<Expression>> {
        let token = self.get_token().clone();

        match token.kind {
            TT::Let => {
                self.advance();

                let pattern = self.pattern()?;

                match pattern {
                    Some(pattern) => {
                        let equal_token = self.get_token();
                        match equal_token.kind {
                            Equal => {
                                self.advance();

                                let value = self.expression()?;

                                if self.is_next_line_same_indent(&token) {
                                    let body = self.expression()?;

                                    let line = token.line;
                                    let column = token.column;
                                    let start = token.position;
                                    let end = body.end;
                                    Ok(Some(Node {
                                        value: Let(pattern, Rc::new(value), Rc::new(body)),
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
                                        to be followed by another let \
                                        or expression in the next line \
                                        and same indentation"
                                            .to_owned(),
                                    ))
                                }
                            }

                            _ => Err(Error::expected_but_found(
                                self.source,
                                &equal_token,
                                None,
                                "Expected an = and an expression \
                                for the right side of let expression"
                                    .to_owned(),
                            )),
                        }
                    }
                    None => Err(Error::expected_but_found(
                        self.source,
                        &token,
                        None,
                        "Expected a pattern for the left side of the let expression".to_owned(),
                    )),
                }
            }

            _ => Ok(None),
        }
    }

    fn if_(&mut self) -> ParseResult<Option<Expression>> {
        let token = self.get_token().clone();

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
                                "Expected the `else` branch of the if expression".to_owned(),
                            )),
                        }
                    }

                    _ => Err(Error::expected_but_found(
                        self.source,
                        &then_token,
                        None,
                        "Expected the keyword `then` and an expression to parse the if expression"
                            .to_owned(),
                    )),
                }
            }

            _ => Ok(None),
        }
    }

    fn lambda(&mut self) -> ParseResult<Option<Expression>> {
        let token = self.get_token().clone();

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
                        "Expected a -> after the list of parameters for the function".to_owned(),
                    )),
                }
            }
            _ => Ok(None),
        }
    }

    fn params(&mut self) -> ParseResult<Vec<Rc<Pattern>>> {
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
                "Expected a list of parameters".to_owned(),
            )),
        }
    }

    fn pattern(&mut self) -> ParseResult<Option<Pattern>> {
        let token = self.get_token().clone();

        match token.kind {
            TT::Identifier => {
                self.advance();
                Ok(Some(Node::new(
                    Pattern_::Identifier(token.lexeme(&self.source).to_string()),
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

    fn binary(&mut self) -> ParseResult<Expression> {
        let expr = self.unary()?;

        self.binary_step().map(|mut binops| {
            // Make the binops options to be able to take them later
            let mut binops: Vec<Option<(Binop, Expression)>> =
                binops.drain(..).map(|b| Some(b)).collect();

            organize_binops(expr, &mut binops, &mut (0), 0)
        })
    }
    fn binary_step(&mut self) -> ParseResult<Vec<(Binop, Expression)>> {
        let mut binops = vec![];

        loop {
            let token = self.get_token().clone();

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

    fn unary(&mut self) -> ParseResult<Expression> {
        let token = self.get_token().clone();

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

    fn call(&mut self) -> ParseResult<Expression> {
        let token = self.get_token().clone();

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
                           function call, an identifier, etc.)"
                    .to_owned();
                Err(Error::expected_but_found(self.source, &token, None, msg))
            }
        })
    }

    fn arguments(
        &mut self,
        first_token: &Token,
        mut args: Vec<Rc<Expression>>,
    ) -> ParseResult<Vec<Rc<Expression>>> {
        if !self.is_nested_indent(first_token) {
            Ok(args)
        } else {
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
        }
    }

    fn primary(&mut self) -> ParseResult<Option<Expression>> {
        let token = self.get_token().clone();

        let result = match token.kind {
            False => Ok(Some(Node::new(Bool(false), &token, &token))),
            True => Ok(Some(Node::new(Bool(true), &token, &token))),

            TT::Float => {
                let lexeme = token.lexeme(&self.source);
                let n = lexeme.parse::<f64>().map_err(|_| {
                    Error::new(
                        &self.source,
                        &token,
                        None,
                        format!("Failed to parse number token '{}'", lexeme),
                    )
                })?;

                Ok(Some(Node::new(Float(n), &token, &token)))
            }

            TT::Identifier => Ok(Some(Node::new(
                Identifier(token.lexeme(&self.source).to_string()),
                &token,
                &token,
            ))),

            TT::String_ => {
                let lexeme = token.lexeme(&self.source);
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
                                "Expected ')' after parenthesized expression".to_owned(),
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

    fn is_nested_indent(&self, parent_token: &Token) -> bool {
        let token = self.get_token();
        parent_token.line == token.line
            || (parent_token.line < token.line && token.indent > parent_token.indent)
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

pub fn parse(source: &Source, tokens: Vec<Token>) -> ParseResults<Rc<Expression>> {
    let mut parser = State {
        source,
        tokens,
        current: 0,
    };

    parser.file().map(|e| Rc::new(e))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::{Token, Type as TT};
    use crate::tokenizer;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_parser() {
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
                    value: Identifier("variableOne".to_owned()),
                    line: 1,
                    column: 0,
                    start: 0,
                    end: 11,
                })),
            ),
            (
                "variable_one",
                Ok(Rc::new(Node {
                    value: Identifier("variable_one".to_owned()),
                    line: 1,
                    column: 0,
                    start: 0,
                    end: 12,
                })),
            ),
            (
                "espaÆàʥñÑol",
                Ok(Rc::new(Node {
                    value: Identifier("espaÆàʥñÑol".to_owned()),
                    line: 1,
                    column: 0,
                    start: 0,
                    end: 16,
                })),
            ),
            (
                "\"😄\"",
                Ok(Rc::new(Node {
                    value: String_("😄".to_owned()),
                    line: 1,
                    column: 0,
                    start: 0,
                    end: 6,
                })),
            ),
            (
                "\"\n\"",
                Ok(Rc::new(Node {
                    value: String_("\n".to_owned()),
                    line: 1,
                    column: 0,
                    start: 0,
                    end: 3,
                })),
            ),
            (
                "\"\"",
                Ok(Rc::new(Node {
                    value: String_("".to_owned()),
                    line: 1,
                    column: 0,
                    start: 0,
                    end: 2,
                })),
            ),
            (
                "(\"\")",
                Ok(Rc::new(Node {
                    value: String_("".to_owned()),
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
                Err(vec![Error {
                    message: {
                        "1:6: Expected ')' after parenthesized expression, but instead found: '[End of file]'

  1│  (((1))
   │  ↑".to_owned()
                    },
                    token: Token {
                        kind: TT::Eof,
                        line: 1,
                        indent: 0,
                        column: 6,
                        position: 6,
                        end_position: 6,
                    },
                }]),
            ),
            (
                "(((1))))",
                Err(vec![Error {
                    message: "1:7: Expected the end of input, but instead found: ')'

  1│  (((1))))
   │         ↑"
                        .to_owned(),
                    token: Token {
                        kind: TT::RightParen,
                        line: 1,
                        indent: 0,
                        column: 7,
                        position: 7,
                        end_position: 8,
                    },
                }]),
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
                            value: Identifier("fun".to_owned()),
                            line: 1,
                            column: 0,
                            start: 0,
                            end: 3,
                        }),
                        vec![Rc::new(Node {
                            value: Identifier("arg".to_owned()),
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
                            value: Identifier("fun".to_owned()),
                            line: 1,
                            column: 0,
                            start: 0,
                            end: 3,
                        }),
                        vec![Rc::new(Node {
                            value: Identifier("arg".to_owned()),
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
                            value: Identifier("fun".to_owned()),
                            line: 1,
                            column: 2,
                            start: 2,
                            end: 5,
                        }),
                        vec![Rc::new(Node {
                            value: Identifier("arg".to_owned()),
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
                Err(vec![Error {
                    message: "2:0: Expected the end of input, but instead found: 'arg'

  1│  fun
  2│  arg
   │  ↑↑↑"
                        .to_owned(),
                    token: Token {
                        kind: TT::Identifier,
                        position: 4,
                        end_position: 7,
                        line: 2,
                        indent: 0,
                        column: 0,
                    },
                }]),
            ),
            (
                "
fun arg1
  arg2 arg3
  arg4",
                Ok(Rc::new(Node {
                    value: FnCall(
                        Rc::new(Node {
                            value: Identifier("fun".to_owned()),
                            line: 2,
                            column: 0,
                            start: 1,
                            end: 4,
                        }),
                        vec![
                            Rc::new(Node {
                                value: Identifier("arg1".to_owned()),
                                line: 2,
                                column: 4,
                                start: 5,
                                end: 9,
                            }),
                            Rc::new(Node {
                                value: Identifier("arg2".to_owned()),
                                line: 3,
                                column: 2,
                                start: 12,
                                end: 16,
                            }),
                            Rc::new(Node {
                                value: Identifier("arg3".to_owned()),
                                line: 3,
                                column: 7,
                                start: 17,
                                end: 21,
                            }),
                            Rc::new(Node {
                                value: Identifier("arg4".to_owned()),
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
                            value: Identifier("hello".to_owned()),
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
                            value: Identifier("incr".to_owned()),
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
                            value: Pattern_::Identifier("a".to_owned()),
                            line: 1,
                            column: 1,
                            start: 1,
                            end: 2,
                        })],
                        Rc::new(Node {
                            value: Identifier("a".to_owned()),
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
                            value: Pattern_::Identifier("a".to_owned()),
                            line: 1,
                            column: 1,
                            start: 1,
                            end: 2,
                        })],
                        Rc::new(Node {
                            value: Lambda(
                                vec![Rc::new(Node {
                                    value: Pattern_::Identifier("b".to_owned()),
                                    line: 1,
                                    column: 7,
                                    start: 7,
                                    end: 8,
                                })],
                                Rc::new(Node {
                                    value: Identifier("a".to_owned()),
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
                                value: Pattern_::Identifier("a".to_owned()),
                                line: 1,
                                column: 1,
                                start: 1,
                                end: 2,
                            }),
                            Rc::new(Node {
                                value: Pattern_::Identifier("b".to_owned()),
                                line: 1,
                                column: 3,
                                start: 3,
                                end: 4,
                            }),
                        ],
                        Rc::new(Node {
                            value: Identifier("a".to_owned()),
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
                                    value: Identifier("incr".to_owned()),
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
                Err(vec![Error {
                    message: {
                        "1:8: Expected the keyword `then` and an expression to parse the if expression, but instead found: '{'\n
  1│  if True { 1 } else 2
   │          ↑".to_owned()
                    },
                    token: Token {
                        kind: LeftBrace,
                        position: 8,
                        end_position: 9,
                        line: 1,
                        column: 8,
                        indent: 0,
                    },
                }]),
            ),
            (
                "if True then 1",
                Err(vec![Error {
                    message: {
                        "1:14: Expected the `else` branch of the if expression, but instead found: '[End of file]'

  1│  if True then 1
   │                ↑".to_owned()
                    },
                    token: Token {
                        kind: Eof,
                        position: 14,
                        end_position: 14,
                        line: 1,
                        column: 14,
                        indent: 0,
                    },
                }]),
            ),
            (
                "let x = 1\nx",
                Ok(Rc::new(Node {
                    value: Let(
                        Node {
                            value: Pattern_::Identifier("x".to_owned()),
                            line: 1,
                            column: 4,
                            start: 4,
                            end: 5,
                        },
                        Rc::new(Node {
                            value: Float(1.),
                            line: 1,
                            column: 8,
                            start: 8,
                            end: 9,
                        }),
                        Rc::new(Node {
                            value: Identifier("x".to_owned()),
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
                Err(vec![Error {
                    message: {
                        "2:3: Expected the let definition to be followed by another let or expression in the next line and same indentation, but instead found: '[End of file]'

  1│  let x = a
  2│    x
   │     ↑".to_owned()
                    },
                    token: Token {
                        kind: Eof,
                        position: 13,
                        end_position: 13,
                        line: 2,
                        column: 3,
                        indent: 2,
                    },
                }]),
            ),
            (
                "let x = a\n  x\nx",
                Ok(Rc::new(Node {
                    value: Let(
                        Node {
                            value: Pattern_::Identifier("x".to_owned()),
                            line: 1,
                            column: 4,
                            start: 4,
                            end: 5,
                        },
                        Rc::new(Node {
                            value: FnCall(
                                Rc::new(Node {
                                    value: Identifier("a".to_owned()),
                                    line: 1,
                                    column: 8,
                                    start: 8,
                                    end: 9,
                                }),
                                vec![Rc::new(Node {
                                    value: Identifier("x".to_owned()),
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
                        Rc::new(Node {
                            value: Identifier("x".to_owned()),
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
        ];

        for (code, expected) in tests {
            let source = Source::new_orphan(&code);
            let tokens = tokenizer::parse(&source).unwrap();
            let result = parse(&source, tokens);
            assert_eq!(
                result,
                expected,
                "\n\nInput:\n\n{:#?}\n\nExpected:\n\n{}",
                &code,
                match &result {
                    Ok(ast) => format!("{:#?}", ast),
                    Err(e) => e
                        .iter()
                        .map(|e| e.message.clone())
                        .collect::<Vec<String>>()
                        .join("\n\n"),
                }
            );
        }
    }
}
