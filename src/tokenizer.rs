use std::iter::Peekable;
use std::str::CharIndices;

use crate::source::Source;
use crate::token::{self, Token};

#[derive(PartialEq, Debug)]
pub struct Error {
    pub line: u32,
    pub column: u32,
    pub message: String,
}

enum Status {
    Reset,
    SingleToken,
    DoubleToken(token::Type),
    LineCommentToken,
    WhitespaceToken(bool),
    StringToken(bool),
    NumberToken(bool),
    IdentifierToken(bool),
}

struct State<'a> {
    status: Status,
    source: &'a Source<'a>,
    chars: Peekable<CharIndices<'a>>,
    tokens: Vec<Token>,
    errors: Vec<Error>,
    start: usize,
    start_line: u32,
    current: usize,
    line: u32,
    line_start_position: usize,
    indent: u32,
}

impl<'a> State<'a> {
    fn new(source: &'a Source) -> State<'a> {
        State {
            status: Status::SingleToken,
            tokens: vec![],
            errors: vec![],
            start: 0,
            start_line: 1,
            current: 0,
            line: 1,
            line_start_position: 0,
            indent: 0,
            source,
            chars: source.char_indices().peekable(),
        }
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().map(|(_i, c)| *c)
    }

    fn add_token(&mut self, kind: token::Type) {
        self.push_token(Token {
            kind,
            lexeme: self.lexeme(),
            line: self.line,
            column: self.start as u32 - self.line_start_position as u32,
            position: self.start,
            indent: self.indent,
        });

        self.status = Status::Reset
    }

    fn add_error(&mut self, message: &str, use_start: bool) {
        let mut message = message.to_owned();

        let (position, line, column) = if use_start {
            (
                self.start,
                self.start_line,
                (self.start - self.line_start_position) as u32,
            )
        } else {
            (
                self.current,
                self.line,
                (self.current - self.line_start_position) as u32,
            )
        };

        message.push('\n');
        message.push('\n');
        message.push_str(
            &self
                .source
                .lines_report_at_position_with_pointer(position, None, line)
                .unwrap(),
        );

        self.errors.push(Error {
            line: self.line,
            column: column as u32,
            message,
        });

        self.status = Status::Reset;
    }

    fn lexeme(&self) -> String {
        self.source
            .text_at(self.start..=self.current)
            .unwrap()
            .to_owned()
    }

    fn push_token(&mut self, token: Token) {
        self.tokens.push(token);
    }

    fn parse_token(&mut self, ch: Option<char>) {
        use crate::token::Type::*;

        match self.status {
            Status::SingleToken => match ch {
                None => self.status = Status::Reset,
                Some(ch) => match ch {
                    '(' => self.add_token(LeftParen),
                    ')' => self.add_token(RightParen),
                    '{' => self.add_token(LeftBrace),
                    '}' => self.add_token(RightBrace),
                    ',' => self.add_token(Comma),
                    '.' => self.add_token(Dot),
                    '+' => self.add_token(Plus),
                    ';' => self.add_token(Semicolon),
                    ':' => self.add_token(Colon),
                    '*' => self.add_token(Star),
                    '\\' => self.add_token(Backslash),

                    '-' => match self.peek() {
                        Some('>') => self.status = Status::DoubleToken(Arrow),
                        _ => self.add_token(Minus),
                    },

                    '!' => match self.peek() {
                        Some('=') => self.status = Status::DoubleToken(BangEqual),
                        _ => self.add_token(Bang),
                    },

                    '=' => match self.peek() {
                        Some('=') => self.status = Status::DoubleToken(EqualEqual),
                        _ => self.add_token(Equal),
                    },

                    '<' => match self.peek() {
                        Some('=') => self.status = Status::DoubleToken(LessEqual),
                        _ => self.add_token(Less),
                    },

                    '>' => match self.peek() {
                        Some('=') => self.status = Status::DoubleToken(GreaterEqual),
                        _ => self.add_token(Greater),
                    },

                    '/' => match self.peek() {
                        Some('/') => self.status = Status::LineCommentToken,
                        _ => self.add_token(Slash),
                    },

                    '"' => self.status = Status::StringToken(false),

                    ' ' | '\n' => {
                        self.status = Status::WhitespaceToken(false);
                        self.parse_token(Some(ch));
                    }

                    '\r' | '\t' => self.status = Status::Reset,

                    ch if ch.is_digit(10) => {
                        self.status = Status::NumberToken(false);
                        self.parse_token(Some(ch));
                    }

                    ch if ch.is_alphabetic() => {
                        self.status = Status::IdentifierToken(true);
                        self.parse_token(Some(ch));
                    }

                    ch => self.add_error(&format!("Unexpected character '{}'.", ch), false),
                },
            },

            Status::DoubleToken(token_type) => self.add_token(token_type),

            Status::LineCommentToken => match ch {
                Some('\n') | None => self.add_token(Comment),
                _ => (),
            },

            Status::WhitespaceToken(line_started) => {
                match ch {
                    Some(' ') => {
                        if line_started {
                            self.indent = self.indent + 1;
                        }
                        self.status = Status::WhitespaceToken(line_started);
                    }

                    Some('\n') => {
                        self.status = Status::WhitespaceToken(true);
                        self.line = self.line + 1;
                        self.indent = 0;
                        self.line_start_position = self.current + 1;
                    }

                    _ => panic!(
                        "Got to the whitespace tokenizer without a valid space or newline char."
                    ),
                }

                match self.peek() {
                    Some('\n') | Some(' ') => (),
                    _ => self.status = Status::Reset,
                }
            }

            Status::StringToken(prev_was_backslash) => match ch {
                Some('\\') => self.status = Status::StringToken(true),
                Some('"') if !prev_was_backslash => self.add_token(String),
                None => self.add_error("Unclosed string.", true),
                _ => {
                    if prev_was_backslash {
                        self.status = Status::StringToken(false);
                    }
                }
            },

            Status::NumberToken(seen_dot) => match ch {
                Some('.') => {
                    if seen_dot {
                        self.add_error("Multiple dots while parsing a number.", false);
                    } else {
                        self.status = Status::NumberToken(true);
                    }

                    match self.peek() {
                        Some(c) if c.is_digit(10) => (),
                        _ => self.add_error("Expected more digits after a dot in a number.", false),
                    }
                }

                Some(c) if c.is_digit(10) => match self.peek() {
                    // Continue munching until there are no numbers or dot
                    Some('.') => (),
                    Some(c) if c.is_digit(10) => (),

                    // Anything else we find ends the number token
                    _ => self.add_token(Float),
                },

                _ => panic!("Got to the number tokenizer without a valid number digit."),
            },

            Status::IdentifierToken(is_start) => match ch {
                Some(c) if (is_start && c.is_alphabetic()) || c.is_alphanumeric() => {
                    let is_start = false;
                    self.status = Status::IdentifierToken(is_start);

                    match self.peek() {
                        Some(c) if (is_start && c.is_alphabetic()) || c.is_alphanumeric() => (),

                        // Anything else we find ends the identifier token
                        _ => self.add_token(match self.lexeme().as_str() {
                            "and" => And,
                            "or" => Or,
                            "not" => Not,
                            "if" => If,
                            "then" => Then,
                            "else" => Else,
                            "true" => True,
                            "false" => False,
                            "let" => Let,
                            "import" => Import,
                            "as" => As,
                            "exports" => Exports,
                            "module" => Module,
                            _ => Identifier,
                        }),
                    }
                }

                _ => panic!("Got to the identifier tokenizer without a valid number digit."),
            },

            Status::Reset => {
                self.start = self.current;
                self.status = Status::SingleToken;

                self.parse_token(ch);
            }
        }
    }

    fn parse(&mut self) {
        while let Some((i, c)) = self.chars.next() {
            if let Status::Reset = &self.status {}

            self.current = i;

            self.parse_token(Some(c));
        }

        self.parse_token(None);

        self.tokens.push(Token {
            kind: token::Type::Eof,
            lexeme: "[End of file]".to_owned(),
            line: self.line,
            // If the last char is a \n, then line_start_position may be bigger than the last \n
            // position. Default to column 0 then.
            column: self
                .source
                .len()
                .checked_sub(self.line_start_position)
                .unwrap_or(0) as u32,
            position: self.source.len(),
            indent: self.indent,
        });
    }
}

pub fn parse<'a>(source: Source<'a>) -> Result<Vec<Token>, Vec<Error>> {
    let mut tokenizer = State::new(&source);

    tokenizer.parse();

    if tokenizer.errors.len() > 0 {
        Err(tokenizer.errors)
    } else {
        Ok(tokenizer.tokens)
    }
}

mod tests {
    use super::*;
    use crate::source::Source;
    use crate::token::{Token, Type::*};

    #[test]
    fn test_scan_tokens() {
        let tests = vec![
            (
                "".to_owned(),
                Ok(vec![Token {
                    kind: Eof,
                    lexeme: "[End of file]".to_owned(),
                    position: 0,
                    line: 1,
                    indent: 0,
                    column: 0,
                }]),
            ),
            (
                "123".to_owned(),
                Ok(vec![
                    Token {
                        kind: Float,
                        lexeme: "123".to_owned(),
                        position: 0,
                        line: 1,
                        indent: 0,
                        column: 0,
                    },
                    Token {
                        kind: Eof,
                        lexeme: "[End of file]".to_owned(),
                        position: 3,
                        line: 1,
                        indent: 0,
                        column: 3,
                    },
                ]),
            ),
            (
                "123.345".to_owned(),
                Ok(vec![
                    Token {
                        kind: Float,
                        lexeme: "123.345".to_owned(),
                        position: 0,
                        line: 1,
                        indent: 0,
                        column: 0,
                    },
                    Token {
                        kind: Eof,
                        lexeme: "[End of file]".to_owned(),
                        position: 7,
                        line: 1,
                        indent: 0,
                        column: 7,
                    },
                ]),
            ),
            (
                "123.sd".to_owned(),
                Err(vec![Error {
                    line: 1,
                    column: 3,
                    message: "Expected more digits after a dot in a number.

  1│  123.sd
   │     ↑
"
                    .to_owned(),
                }]),
            ),
            (
                "123\n or \"abc\"".to_owned(),
                Ok(vec![
                    Token {
                        kind: Float,
                        lexeme: "123".to_owned(),
                        position: 0,
                        line: 1,
                        indent: 0,
                        column: 0,
                    },
                    Token {
                        kind: Or,
                        lexeme: "or".to_owned(),
                        position: 5,
                        line: 2,
                        indent: 1,
                        column: 1,
                    },
                    Token {
                        kind: String,
                        lexeme: "\"abc\"".to_owned(),
                        position: 8,
                        line: 2,
                        indent: 1,
                        column: 4,
                    },
                    Token {
                        kind: Eof,
                        lexeme: "[End of file]".to_owned(),
                        position: 13,
                        line: 2,
                        indent: 1,
                        column: 9,
                    },
                ]),
            ),
            (
                "123\n or &\"abc\"".to_owned(),
                Err(vec![Error {
                    line: 2,
                    column: 4,
                    message: "Unexpected character \'&\'.

  1│  123
  2│   or &\"abc\"
   │      ↑
"
                    .to_owned(),
                }]),
            ),
            (
                "\"asdf\\\"asdf\"".to_owned(),
                Ok(vec![
                    Token {
                        kind: String,
                        lexeme: "\"asdf\\\"asdf\"".to_owned(),
                        position: 0,
                        line: 1,
                        indent: 0,
                        column: 0,
                    },
                    Token {
                        kind: Eof,
                        lexeme: "[End of file]".to_owned(),
                        position: 12,
                        line: 1,
                        indent: 0,
                        column: 12,
                    },
                ]),
            ),
        ];

        for (code, expected) in tests {
            let result = parse(Source::new_orphan(&code));
            assert_eq!(result, expected, "\nCode:\n{:?}", &code);
        }
    }
}
/*
Test.suite("Tokenizer", ({test}) => {
  open Token

  let testCases: array<(string, result<array<Token.t>, array<Tokenizer.error>>)> = [
    ("", Ok([{kind: Eof, lexeme: "[End of file]", position: 0, line: 1, indent: 0, column: 0}])),
    (
      "123",
      Ok([
        {kind: Float, lexeme: "123", position: 0, line: 1, indent: 0, column: 0},
        {kind: Eof, lexeme: "[End of file]", position: 3, line: 1, indent: 0, column: 3},
      ]),
    ),
    (
      "123.345",
      Ok([
        {kind: Float, lexeme: "123.345", position: 0, line: 1, indent: 0, column: 0},
        {kind: Eof, lexeme: "[End of file]", position: 7, line: 1, indent: 0, column: 7},
      ]),
    ),
    (
      "123.sd",
      Error([
        {
          line: 1,
          column: 3,
          message: `1:3
Expected more digits after a dot in a number.
  1│  123.sd
   │     ↑`,
        },
      ]),
    ),
    (
      "123\n or \"abc\"",
      Ok([
        {kind: Float, lexeme: "123", position: 0, line: 1, indent: 0, column: 0},
        {kind: Or, lexeme: "or", position: 5, line: 2, indent: 1, column: 1},
        {kind: String, lexeme: "\"abc\"", position: 8, line: 2, indent: 1, column: 4},
        {kind: Eof, lexeme: "[End of file]", position: 13, line: 2, indent: 1, column: 9},
      ]),
    ),
    (
      "123\n or &\"abc\"",
      Error([
        {
          line: 2,
          column: 4,
          message: `2:4
Unexpected character '&'.
  1│  123
  2│   or &"abc"
   │      ↑`,
        },
      ]),
    ),
    (
      `"asdf\\\"asdf"`,
      Ok([
        {kind: String, lexeme: `"asdf\\\"asdf"`, position: 0, line: 1, indent: 0, column: 0},
        {kind: Eof, lexeme: "[End of file]", position: 12, line: 1, indent: 0, column: 12},
      ]),
    ),
  ]

  testCases->Array.forEachWithIndex((i, (input, expected)) => {
    let subject = input->Js.String2.split("\n")->Array.getExn(0)
    test(j`$i. "$subject"`, () => {
      Test.assertEquals(Tokenizer.parse(input), expected, ``)
    })
  })
})
*/