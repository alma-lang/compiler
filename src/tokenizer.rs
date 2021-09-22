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

impl Error {
    pub fn to_string(&self, source: &Source) -> String {
        let mut msg = String::new();

        msg.push_str(&format!(
            "{}:{}:{}\n\n{}",
            source.name(),
            self.line,
            self.column,
            self.message
        ));

        msg
    }
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

struct State<'source> {
    status: Status,
    source: &'source Source,
    chars: Peekable<CharIndices<'source>>,
    tokens: Vec<Token<'source>>,
    errors: Vec<Error>,
    start: usize,
    start_line: u32,
    current: usize,
    current_char: char,
    line: u32,
    line_start_position: usize,
    indent: u32,
}

impl<'source> State<'source> {
    fn new(source: &'source Source) -> State<'source> {
        State {
            status: Status::SingleToken,
            tokens: vec![],
            errors: vec![],
            start: 0,
            start_line: 1,
            // current and current_char will be properly initialized on self.parse
            current: 0,
            current_char: 'a',
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
        let position = self.start;
        let end_position = self.current + self.current_char.len_utf8();

        let lexeme = match kind {
            token::Type::Eof => "[End of file]",
            _ => self
                .source
                .text_at(position..end_position)
                .expect("Couldn't extract lexeme from token"),
        };

        self.push_token(Token {
            kind,

            position,
            end_position,
            lexeme,

            line: self.line,
            column: self.start as u32 - self.line_start_position as u32,
            indent: self.indent,
        });

        self.status = Status::Reset
    }

    fn add_error(&mut self, message: &str, use_start: bool) {
        let mut message = message.to_string();

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

    fn push_token(&mut self, token: Token<'source>) {
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
                    '/' => self.add_token(Slash),
                    '\\' => self.add_token(Backslash),
                    '_' => self.add_token(Underscore),
                    '|' => self.add_token(Pipe),

                    '-' => match self.peek() {
                        Some('-') => self.status = Status::LineCommentToken,
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
                            self.indent += 1;
                        }
                        self.status = Status::WhitespaceToken(line_started);
                    }

                    Some('\n') => {
                        self.status = Status::WhitespaceToken(true);
                        self.line += 1;
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
                Some('"') if !prev_was_backslash => self.add_token(String_),
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
                Some(c) if (is_start && c.is_alphabetic()) || is_identifier_rest(c) => {
                    let is_start = false;
                    self.status = Status::IdentifierToken(is_start);

                    match self.peek() {
                        Some(c) if (is_start && c.is_alphabetic()) || is_identifier_rest(c) => (),

                        // Anything else we find ends the identifier token
                        _ => self.add_token(
                            match self.source.text_at(self.start..=self.current).unwrap() {
                                "and" => And,
                                "or" => Or,
                                "not" => Not,
                                "if" => If,
                                "then" => Then,
                                "else" => Else,
                                "True" => True,
                                "False" => False,
                                "let" => Let,
                                "in" => In,
                                "import" => Import,
                                "as" => As,
                                "exposing" => Exposing,
                                "module" => Module,
                                _ => Identifier,
                            },
                        ),
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
            self.current_char = c;

            self.parse_token(Some(c));
        }

        self.parse_token(None);

        self.tokens.push(Token {
            kind: token::Type::Eof,
            line: self.line,
            // If the last char is a \n, then line_start_position may be bigger than the last \n
            // position. Default to column 0 then.
            column: self.source.len().saturating_sub(self.line_start_position) as u32,
            indent: self.indent,

            position: self.source.len(),
            end_position: self.source.len(),
            lexeme: "[End of file]",
        });
    }
}

pub fn parse<'source>(source: &'source Source) -> Result<Vec<Token<'source>>, Vec<Error>> {
    let mut tokenizer = State::new(source);

    tokenizer.parse();

    if !tokenizer.errors.is_empty() {
        Err(tokenizer.errors)
    } else {
        Ok(tokenizer.tokens)
    }
}

fn is_identifier_rest(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source::Source;
    use insta::assert_snapshot;

    #[test]
    fn test_scan_tokens() {
        fn tokenize<'a>(code: &'a str) -> String {
            let source = &Source::new_orphan(code.to_string());
            format!("{:#?}", parse(source))
        }

        assert_snapshot!(tokenize(""));

        assert_snapshot!(tokenize("123"));

        assert_snapshot!(tokenize("123.345"));

        assert_snapshot!(tokenize("123.sd"));

        assert_snapshot!(tokenize("123\n or \"abc\""));

        assert_snapshot!(tokenize("123\n or &\"abc\""));

        assert_snapshot!(tokenize(r#""asdf\"asdf""#));
    }
}
