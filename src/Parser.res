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
    ● call           → primary ( primary )*
    ● primary        → NUMBER | STRING | IDENTIFIER | "false" | "true"
                     | "(" expression ")"
*/

module String = Js.String2
module JsArray = Js.Array2

module ParseError = {
  type t = {
    message: string,
    token: Token.t,
  }

  let make = (input: string, token: Token.t, message: string): t => {
    let position = token.position
    let lineNumber = token.line
    let columnNumber = token.column

    let message =
      `${token.line->Int.toString}:${token.column->Int.toString}: ${message}\n\n` ++
      Input.linesReportAtPositionWithPointer(
        input,
        ~position,
        ~lineNumber,
        ~columnNumber,
      )->Option.getWithDefault("")

    {message: message, token: token}
  }

  let expectedButFound = (input: string, token: Token.t, message: string): t => {
    make(input, token, `${message}, but instead found: '${token.lexeme}'`)
  }
}

type parseResultErrs<'a> = result<'a, array<ParseError.t>>
type parseResult<'a> = result<'a, ParseError.t>

type state = {
  input: string,
  tokens: array<Token.t>,
  mutable current: int,
}

let getToken = (parser): Token.t => {
  switch parser.tokens[parser.current] {
  | Some(token) => token
  | None => Js.Exn.raiseTypeError("Out of bounds access to tokens array")
  }
}

let advance = parser => {
  let token = parser->getToken
  if token.kind != Token.Eof {
    parser.current = parser.current + 1
  }
}

let rec expression = (parser: state): parseResult<Node.t<Ast.expr>> => {
  parser->call'
}

and call' = (parser: state): parseResult<Node.t<Ast.expr>> => {
  parser
  ->primary
  ->Result.flatMap(expr =>
    switch expr {
    | Some(expr) => parser->callR'(expr)
    | None =>
      Error(
        ParseError.expectedButFound(
          parser.input,
          parser->getToken,
          "Expected an expression (let binding, function call, an identifier, etc.)",
        ),
      )
    }
  )
}
and callR' = (parser: state, returnExpr: Node.t<Ast.expr>): parseResult<Node.t<Ast.expr>> => {
  parser
  ->primary
  ->Result.flatMap(arg =>
    switch arg {
    // We tried to get an argument, but there was no match
    | None => Ok(returnExpr)
    | Some(arg) =>
      parser->callR'({
        Node.value: Ast.FnCall(returnExpr, arg),
        start: returnExpr.start,
        end: arg.end,
      })
    }
  )
}

and primary = (parser: state): parseResult<option<Node.t<Ast.expr>>> => {
  let token = parser->getToken

  let result = switch token.kind {
  | False => Ok(Some(Node.make(Ast.Bool(false), token, token)))
  | True => Ok(Some(Node.make(Ast.Bool(true), token, token)))

  | Identifier => Ok(Some(Node.make(Ast.Identifier(token.lexeme), token, token)))

  | Number =>
    switch Float.fromString(token.lexeme) {
    | Some(n) => Ok(Some(Node.make(Ast.Number(n), token, token)))
    | None =>
      Error(ParseError.make(parser.input, token, `Failed to parse number token '${token.lexeme}'`))
    }

  | String => {
      let value = token.lexeme->String.substring(~from=1, ~to_=token.lexeme->String.length - 1)
      Ok(Some(Node.make(Ast.String(value), token, token)))
    }

  | LeftParen =>
    parser->advance
    parser
    ->expression
    ->Result.flatMap(expr => {
      let token = parser->getToken
      switch token.kind {
      | Token.RightParen => Ok(Some(expr))
      | _ =>
        Error(
          ParseError.expectedButFound(
            parser.input,
            token,
            "Expected ')' after parenthesized expression",
          ),
        )
      }
    })

  | _ => Ok(None)
  }

  // None of the branches advance after the last successful token, so we do it
  // here to avoid repetition
  switch result {
  | Ok(Some(_)) => parser->advance
  | _ => ()
  }

  result
}

let file = (parser: state): parseResultErrs<Node.t<Ast.expr>> => {
  let result = parser->expression

  switch result {
  | Error(e) => Error([e])
  | Ok(a) =>
    switch parser->getToken {
    | {kind: Token.Eof} => Ok(a)
    | token =>
      Error([ParseError.expectedButFound(parser.input, token, "Expected the end of input")])
    }
  }
}

let parse = (input: string, tokens: array<Token.t>): parseResultErrs<Node.t<Ast.expr>> => {
  let parser = {
    input: input,
    tokens: tokens,
    current: 0,
  }

  parser->file
}
