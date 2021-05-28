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
  | Float

  // Keywords.
  | And
  | Or
  | Not
  | If
  | Then
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
  | Eof

type t = {
  kind: typ,
  lexeme: string,
  position: int,
  line: int,
  column: int,
  indent: int,
}
