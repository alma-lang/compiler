use std::iter::Peekable;
use std::str::CharIndices;

use crate::source::Source;
use crate::strings::{self, Strings};
use crate::token::{self, Token, Tokens};

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
            "{name}:{line}:{column}\n\n{message}",
            name = source.name(),
            line = self.line,
            column = self.column,
            message = self.message
        ));

        msg
    }
}

enum Status {
    Reset,
    SingleToken,
    DoubleToken(token::Type),
    LineCommentToken,
    WhitespaceToken {
        line_started: bool,
    },
    StringToken {
        prev_was_backslash: bool,
    },
    NumberToken {
        seen_dot: bool,
    },
    IdentifierToken {
        lexing_first_char: bool,
        is_capitalized: bool,
    },
}

struct State<'source, 'strings> {
    status: Status,
    strings: &'strings mut Strings,
    source: &'source Source,
    chars: Peekable<CharIndices<'source>>,
    tokens: Vec<Token>,
    errors: Vec<Error>,
    start: usize,
    start_line: u32,
    current: usize,
    current_char: char,
    line: u32,
    line_start_position: usize,
    indent: u32,
}

impl<'source, 'strings> State<'source, 'strings> {
    fn new(source: &'source Source, strings: &'strings mut Strings) -> State<'source, 'strings> {
        State {
            status: Status::SingleToken,
            tokens: Vec::new(),
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
            strings,
            chars: source.char_indices().peekable(),
        }
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().map(|(_i, c)| *c)
    }

    fn lexeme(&mut self) -> strings::Symbol {
        let start = self.start;
        let end = self.current + self.current_char.len_utf8();
        self.strings.get_or_intern(
            self.source
                .text_at(start..end)
                .expect("Couldn't extract lexeme from token"),
        )
    }

    fn add_token(&mut self, kind: token::Type) {
        let start = self.start;
        let end = self.current + self.current_char.len_utf8();

        self.push_token(Token {
            kind,
            start,
            end,
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

    fn new_line(&mut self) {
        self.line += 1;
        self.indent = 0;
        self.line_start_position = self.current + 1;
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

                    '"' => {
                        self.status = Status::StringToken {
                            prev_was_backslash: false,
                        }
                    }

                    ' ' | '\n' => {
                        self.status = Status::WhitespaceToken {
                            line_started: false,
                        };
                        self.parse_token(Some(ch));
                    }

                    '\r' | '\t' => self.status = Status::Reset,

                    ch if ch.is_digit(10) => {
                        self.status = Status::NumberToken { seen_dot: false };
                        self.parse_token(Some(ch));
                    }

                    ch if ch.is_alphabetic() => {
                        self.status = Status::IdentifierToken {
                            lexing_first_char: true,
                            is_capitalized: ch.is_uppercase(),
                        };
                        self.parse_token(Some(ch));
                    }

                    ch => self.add_error(&format!("Unexpected character '{ch}'."), false),
                },
            },

            Status::DoubleToken(token_type) => self.add_token(token_type),

            Status::LineCommentToken => match self.peek() {
                None | Some('\n') => self.add_token(Comment),
                _ => (),
            },

            Status::WhitespaceToken { line_started } => {
                match ch {
                    Some(' ') => {
                        if line_started {
                            self.indent += 1;
                        }
                        self.status = Status::WhitespaceToken { line_started };
                    }

                    Some('\n') => {
                        self.status = Status::WhitespaceToken { line_started: true };
                        self.new_line();
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

            Status::StringToken { prev_was_backslash } => match ch {
                Some('\\') => {
                    self.status = Status::StringToken {
                        prev_was_backslash: true,
                    }
                }
                Some('"') if !prev_was_backslash => {
                    let lexeme = {
                        let start = self.start + 1;
                        let end = self.current + self.current_char.len_utf8() - 1;
                        self.strings.get_or_intern(
                            self.source
                                .text_at(start..end)
                                .expect("Couldn't extract lexeme from token"),
                        )
                    };
                    self.add_token(String_(lexeme))
                }

                // // TODO: This here means we are storing the full string with quotes in the
                // // interner, and then now again without quotes. Quite a waste
                // // Remove the wrapper quotes from the value
                // &lexeme[1..(lexeme.len() - 1)]
                None => self.add_error("Unclosed string.", true),
                _ => {
                    if prev_was_backslash {
                        self.status = Status::StringToken {
                            prev_was_backslash: false,
                        };
                    }
                }
            },

            Status::NumberToken { seen_dot } => match ch {
                Some('.') => {
                    if seen_dot {
                        self.add_error("Multiple dots while parsing a number.", false);
                    } else {
                        self.status = Status::NumberToken { seen_dot: true };
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
                    _ => {
                        let lexeme = self.lexeme();
                        self.add_token(Float(lexeme))
                    }
                },

                _ => panic!("Got to the number tokenizer without a valid number digit."),
            },

            Status::IdentifierToken {
                lexing_first_char,
                is_capitalized,
            } => match ch {
                Some(c) if (lexing_first_char && c.is_alphabetic()) || is_identifier_rest(c) => {
                    let lexing_first_char = false;
                    self.status = Status::IdentifierToken {
                        lexing_first_char,
                        is_capitalized,
                    };

                    match self.peek() {
                        Some(c)
                            if (lexing_first_char && c.is_alphabetic())
                                || is_identifier_rest(c) =>
                        {
                            ()
                        }

                        // Anything else we find ends the identifier token
                        _ => {
                            let token =
                                match self.source.text_at(self.start..=self.current).unwrap() {
                                    "and" => And,
                                    "or" => Or,
                                    "not" => Not,
                                    "if" => If,
                                    "then" => Then,
                                    "else" => Else,
                                    "let" => Let,
                                    "in" => In,
                                    "import" => Import,
                                    "as" => As,
                                    "exposing" => Exposing,
                                    "module" => Module,
                                    "type" => Type,
                                    "external" => External,
                                    _ => {
                                        if is_capitalized {
                                            CapitalizedIdentifier(self.lexeme())
                                        } else {
                                            Identifier(self.lexeme())
                                        }
                                    }
                                };

                            self.add_token(token)
                        }
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
            indent: self.indent,
            start: self.source.len(),
            end: self.source.len(),
            line: self.line,
            // If the last char is a \n, then line_start_position may be bigger than the last \n
            // position. Default to column 0 then.
            column: self.source.len().saturating_sub(self.line_start_position) as u32,
        });
    }
}

pub fn parse(
    source: &Source,
    strings: &mut Strings,
    tokens: &mut Tokens,
) -> Result<(), Vec<Error>> {
    let mut tokenizer = State::new(source, strings);

    tokenizer.parse();

    tokens.extend(tokenizer.tokens);

    if !tokenizer.errors.is_empty() {
        Err(tokenizer.errors)
    } else {
        Ok(())
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

    fn tokenize<'a>(code: &'a str) -> String {
        let source = &Source::new_orphan(code.to_string());
        let mut tokens = Tokens::new();
        let result = parse(source, &mut Strings::new(), &mut tokens);
        format!("{:#?}", result.map(|_| tokens))
    }

    #[test]
    fn empty_file() {
        assert_snapshot!(tokenize(""));
    }

    #[test]
    fn test_tokenize_int() {
        assert_snapshot!(tokenize("123"));
    }

    #[test]
    fn test_tokenize_float() {
        assert_snapshot!(tokenize("123.345"));
    }

    #[test]
    fn test_tokenize_bad_float() {
        assert_snapshot!(tokenize("123.sd"));
    }

    #[test]
    fn test_tokenize_whitespace_number_or_string() {
        assert_snapshot!(tokenize("123\n or \"abc\""));
    }

    #[test]
    fn test_tokenize_ampersand() {
        assert_snapshot!(tokenize("123\n or &\"abc\""));
    }

    #[test]
    fn test_tokenize_string() {
        assert_snapshot!(tokenize(r#""asdf\"asdf""#));
    }

    #[test]
    fn test_tokenize_single_line_comment() {
        assert_snapshot!(tokenize("123\n--Banana\n321"));
    }
}
