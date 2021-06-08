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

use crate::ast::{
    self,
    Expression_::{Float, Identifier, String_, *},
    Node,
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
    fn new(input: &Source, token: &Token, point_at_token: Option<&Token>, message: String) -> Self {
        let (position, end, line_number) = match point_at_token {
            Some(t) => (t.position, t.position + t.lexeme.len(), t.line),
            None => (
                token.position,
                token.position + token.lexeme.len(),
                token.line,
            ),
        };

        let message = format!(
            "{}:{}: {}\n\n{}",
            token.line,
            token.column,
            message,
            input
                .lines_report_at_position_with_pointer(position, Some(end), line_number,)
                .unwrap_or("".to_owned())
        );

        Self {
            message,
            token: (*token).clone(),
        }
    }

    fn expected_but_found(
        input: &Source,
        token: &Token,
        point_at_token: Option<&Token>,
        message: String,
    ) -> Self {
        Self::new(
            input,
            token,
            point_at_token,
            format!("{}, but instead found: '{}'", message, token.lexeme),
        )
    }
}

type ParseResults<A> = Result<A, Vec<Error>>;
type ParseResult<A> = Result<A, Error>;

#[derive(Debug)]
struct State<'a> {
    input: &'a Source<'a>,
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

    pub fn file(&mut self) -> ParseResults<ast::Expression> {
        match self.expression() {
            Err(e) => Err(vec![e]),
            Ok(a) => match self.get_token() {
                Token {
                    kind: token::Type::Eof,
                    ..
                } => Ok(a),
                token => Err(vec![Error::expected_but_found(
                    self.input,
                    token,
                    None,
                    "Expected the end of input".to_owned(),
                )]),
            },
        }
    }

    fn expression(&mut self) -> ParseResult<ast::Expression> {
        self.primary()
            .and_then(|maybe_ast| {
                match maybe_ast {
                    Some(ast) => Ok(ast),
                    None => Err(
                        Error::expected_but_found(
                        self.input,
                        self.get_token(),
                        None,
                        "Expected an expression (a number, string, a let binding, function call, an identifier, etc.)".to_owned(),
                        ),
                    )
                }
            })
    }

    fn primary(&mut self) -> ParseResult<Option<ast::Expression>> {
        let token = self.get_token().clone();

        let result = match token.kind {
            False => Ok(Some(Node::new(Bool(false), &token, &token))),
            True => Ok(Some(Node::new(Bool(true), &token, &token))),

            TT::Float => {
                let n = token.lexeme.parse::<f64>().map_err(|_| {
                    Error::new(
                        &self.input,
                        &token,
                        None,
                        format!("Failed to parse number token '{}'", token.lexeme),
                    )
                })?;

                Ok(Some(Node::new(Float(n), &token, &token)))
            }

            TT::Identifier => Ok(Some(Node::new(
                Identifier(token.lexeme.clone()),
                &token,
                &token,
            ))),

            TT::String_ => {
                let value = token.lexeme[1..(token.lexeme.len() - 1)].to_string();
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
                                self.input,
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
}

pub fn parse(input: &Source, tokens: Vec<Token>) -> ParseResults<ast::Expression> {
    let mut parser = State {
        input,
        tokens,
        current: 0,
    };

    parser.file()
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
                    Ok(Node {
                        value: Bool(true),
                        line: 1,
                        column: 0,
                        start: 0,
                        end: 4,
                    }),
                ),
                (
                    "False",
                    Ok(Node {
                        value: Bool(false),
                        line: 1,
                        column: 0,
                        start: 0,
                        end: 5,
                    }),
                ),
                (
                    "()",
                    Ok(Node {
                        value: Unit,
                        line: 1,
                        column: 0,
                        start: 0,
                        end: 2,
                    }),
                ),
                (
                    "123",
                    Ok(Node {
                        value: Float(123.0),
                        line: 1,
                        column: 0,
                        start: 0,
                        end: 3,
                    }),
                ),
                (
                    "123.2",
                    Ok(Node {
                        value: Float(123.2),
                        line: 1,
                        column: 0,
                        start: 0,
                        end: 5,
                    }),
                ),
                (
                    "variableOne",
                    Ok(Node {
                        value: Identifier("variableOne".to_owned()),
                        line: 1,
                        column: 0,
                        start: 0,
                        end: 11,
                    }),
                ),
                (
                    "variable_one",
                    Ok(Node {
                        value: Identifier("variable_one".to_owned()),
                        line: 1,
                        column: 0,
                        start: 0,
                        end: 12,
                    }),
                ),
                (
                    "espaÆàʥñÑol",
                    Ok(Node {
                        value: Identifier("espaÆàʥñÑol".to_owned()),
                        line: 1,
                        column: 0,
                        start: 0,
                        end: 16,
                    }),
                ),
                (
                    "\"😄\"",
                    Ok(Node {
                        value: String_("😄".to_owned()),
                        line: 1,
                        column: 0,
                        start: 0,
                        end: 6,
                    }),
                ),
                (
                    "\"\n\"",
                    Ok(Node {
                        value: String_("\n".to_owned()),
                        line: 1,
                        column: 0,
                        start: 0,
                        end: 3,
                    }),
                ),
                (
                    "\"\"",
                    Ok(Node {
                        value: String_("".to_owned()),
                        line: 1,
                        column: 0,
                        start: 0,
                        end: 2,
                    }),
                ),

        ("(\"\")", Ok(Node  {value: String_("".to_owned()), line: 1, column: 1, start: 1, end: 3})),
        ("(((1)))", Ok(Node {value: Float(1.0), line: 1, column: 3, start: 3, end: 4})),
        (
          "(((1))",
          Err(vec![
            Error {
              message: "1:6: Expected ')' after parenthesized expression, but instead found: '[End of file]'

  1│  (((1))
   │  ↑
".to_owned(),
              token: Token {
                kind: TT::Eof,
                lexeme: "[End of file]".to_owned(),
                line: 1,
                indent: 0,
                column: 6,
                position: 6,
              },
            },
          ]),
        ),
        (
          "(((1))))",
          Err(vec![
            Error {
              message: "1:7: Expected the end of input, but instead found: ')'

  1│  (((1))))
   │         ↑
".to_owned(),
              token: Token {
                kind: TT::RightParen,
                lexeme: ")".to_owned(),
                line: 1,
                indent: 0,
                column: 7,
                position: 7,
              },
            },
          ]),
        ),
        (
          "(
  ((1))
)",
          Ok(Node {value: Float(1.0), line: 2, column: 4, start: 6, end: 7}),
        ),
            ];

        for (code, expected) in tests {
            let source = Source::new_orphan(&code);
            let tokens = tokenizer::parse(&source).unwrap();
            let result = parse(&source, tokens);
            assert_eq!(
                result,
                expected,
                "\n\nInput:\n\n{:?}\n\nExpected:\n\n{}",
                &code,
                match &result {
                    Ok(ast) => format!("{:?}", ast),
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
