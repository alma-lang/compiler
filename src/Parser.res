/* Grammar draft (●○):
    ○ file           → expression EOF
    ○ expression     → elet | lambda | if | operators
    ○ elet           → "let" binding "=" expression "\n" expression
    ○ lambda         → "fn" params? "{" expression "}"
    ○ params         → "(" binding ( "," binding )* ")"
    ○ binding        → IDENTIFIER ( ":" IDENTIFIER )?
    ○ if             → "if" operators "{" expression "}" ( else "{" expression "}" )?
    // Operators
    ○ operators      → logic_or
    ○ logic_or       → logic_and ( "or" logic_and )*
    ○ logic_and      → equality ( "and" equality )*
    ○ equality       → comparison ( ( "!=" | "==" ) comparison )*
    ○ comparison     → addition ( ( ">" | ">=" | "<" | "<=" ) addition )*
    ○ addition       → multiplication ( ( "+" | "-" ) multiplication )*
    ○ multiplication → unary ( ( "/" | "*" ) unary )*
    ○ unary          → ( "!" | "-" ) unary | call
    // Other primitives
    ○ call           → primary ( "(" arguments? ")" )*
    ○ arguments      → expression ( "," expression )*
    ● primary        → NUMBER | STRING | IDENTIFIER | "false" | "true"
                    | "(" expression ")"
*/

module String = Js.String2

module ParseError = {
  type t = {
    message: string,
    token: Token.t,
  }

  let make = (_input: string, token: Token.t, message: string): t => {
    // TODO: Improve error messages
    let message = `${token.line->Int.toString}:${token.column->Int.toString}: ${message}`
    {message: message, token: token}
  }

  let expectedButFound = (input: string, token: Token.t, message: string): t => {
    make(input, token, `${message}, but instead found: '${token.lexeme}'`)
  }
}

type state = {
  input: string,
  tokens: array<Token.t>,
  mutable current: int,
}

let peek = (state): Token.t => {
  switch state.tokens[state.current] {
  | Some(token) => token
  | None => Js.Exn.raiseTypeError("Out of bounds access to tokens array")
  }
}

let advance = state => {
  let token = state->peek
  if token.kind != Token.Eof {
    state.current = state.current + 1
  }
}

/*
let previous = (state): Token.t => {
  state.tokens[
    if state.current > 0 {
      state.current - 1
    } else {
      0
    }
  ]->Option.getUnsafe
}
*/

let rec expression = (state: state): result<Node.t<Ast.expr>, ParseError.t> => {
  state->primary
}

and primary = (state: state): result<Node.t<Ast.expr>, ParseError.t> => {
  let token = state->peek
  switch token.kind {
  | False => Ok(Node.make(Ast.Bool(false), token, token))
  | True => Ok(Node.make(Ast.Bool(true), token, token))
  | Identifier => Ok(Node.make(Ast.Identifier(token.lexeme), token, token))
  | Number =>
    switch Float.fromString(token.lexeme) {
    | Some(n) => Ok(Node.make(Ast.Number(n), token, token))
    | None =>
      Error(ParseError.make(state.input, token, `Failed to parse number token '${token.lexeme}'`))
    }
  | String => {
      let value = token.lexeme->String.substring(~from=1, ~to_=token.lexeme->String.length - 1)
      Ok(Node.make(Ast.String(value), token, token))
    }
  | LeftParen =>
    state->advance
    state
    ->expression
    ->Result.flatMap(expr => {
      let token = state->peek
      switch token.kind {
      | Token.RightParen => Ok(expr)
      | _ =>
        Error(
          ParseError.expectedButFound(
            state.input,
            token,
            "Expected ')' after parenthesized expression",
          ),
        )
      }
    })
  | _ =>
    Error(
      ParseError.expectedButFound(
        state.input,
        token,
        "Expected an expression (let, if, a lambda, some math, or primitive types)",
      ),
    )
  }
}

let parse = (input: string, tokens: array<Token.t>): result<
  Node.t<Ast.expr>,
  array<ParseError.t>,
> => {
  let state = {
    input: input,
    tokens: tokens,
    current: 0,
  }

  let result = primary(state)

  switch result {
  | Error(e) => Error([e])
  | Ok(a) => Ok(a)
  }
}
