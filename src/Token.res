type typ =
  // SingleToken-character tokens.
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Colon
  | Slash
  | Star

  // One or two character tokens.
  | Bang
  | BangEqual
  | Equal
  | EqualEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual

  // Literals.
  | Identifier
  | String
  | Number

  // Keywords.
  | And
  | Or
  | Not
  | If
  | Else
  | True
  | False
  | Fun
  | Let
  | Import
  | As
  | Exports
  | Module

  | Comment
  | Newline
  | Eof

type t = {
  kind: typ,
  lexeme: string,
  position: int,
  line: int,
  column: int,
}
