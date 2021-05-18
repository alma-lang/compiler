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
    ● call           → primary ( "(" arguments? ")" )*
    ● arguments      → expression ( "," expression )*
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

  let make = (_input: string, token: Token.t, message: string): t => {
    // TODO: Improve error messages
    let message = `${token.line->Int.toString}:${token.column->Int.toString}: ${message}`
    {message: message, token: token}
  }

  let expectedButFound = (input: string, token: Token.t, message: string): t => {
    make(input, token, `${message}, but instead found: '${token.lexeme}'`)
  }
}

type parseResult<'a> = result<'a, ParseError.t>

type state = {
  input: string,
  tokens: array<Token.t>,
  mutable current: int,
}

let peek = (parser): Token.t => {
  switch parser.tokens[parser.current] {
  | Some(token) => token
  | None => Js.Exn.raiseTypeError("Out of bounds access to tokens array")
  }
}

let advance = parser => {
  let token = parser->peek
  if token.kind != Token.Eof {
    parser.current = parser.current + 1
  }
}

let rec expression = (parser: state): parseResult<Node.t<Ast.expr>> => {
  parser->call
}

and call = (parser: state): parseResult<Node.t<Ast.expr>> => {
  let firstToken = parser->peek
  parser->primary->Result.flatMap(expr => parser->callR(firstToken, expr))
}

and callR = (parser: state, firstToken: Token.t, returnExpr: Node.t<Ast.expr>): parseResult<
  Node.t<Ast.expr>,
> => {
  let callFirstToken = parser->peek
  switch callFirstToken.kind {
  | LeftParen => {
      parser->advance

      switch (parser->peek).kind {
      | RightParen => {
          let lastToken = parser->peek
          parser->advance
          Ok(
            Node.make(
              Ast.FnCall(returnExpr, Node.make(Ast.Unit, callFirstToken, lastToken)),
              firstToken,
              lastToken,
            ),
          )
        }

      | _ =>
        parser
        ->arguments([])
        ->Result.flatMap(args => {
          let lastToken = parser->peek
          switch lastToken.kind {
          | RightParen => {
              parser->advance
              // Convert args lists with commas to single argument lambdas
              let expr = args->Array.reduce(returnExpr, (expr, arg) => {
                Node.make(Ast.FnCall(expr, arg), firstToken, lastToken)
              })
              parser->callR(firstToken, expr)
            }
          | _ =>
            Error(
              ParseError.expectedButFound(
                parser.input,
                parser->peek,
                "Expected a ',' and more arguments or a closing ')' for the list of arguments of the function call",
              ),
            )
          }
        })
      }
    }

  // Not a parenthesized call, return parsed up to now
  | _ => Ok(returnExpr)
  }
}

and arguments = (parser: state, args: array<Node.t<Ast.expr>>): result<
  array<Node.t<Ast.expr>>,
  ParseError.t,
> => {
  parser
  ->expression
  ->Result.flatMap(expr => {
    args->JsArray.push(expr)->ignore

    switch (parser->peek).kind {
    | Comma => {
        parser->advance
        parser->arguments(args)
      }

    | _ => Ok(args)
    }
  })
}

and primary = (parser: state): parseResult<Node.t<Ast.expr>> => {
  let token = parser->peek
  let result = switch token.kind {
  | False => Ok(Node.make(Ast.Bool(false), token, token))
  | True => Ok(Node.make(Ast.Bool(true), token, token))

  | Identifier => Ok(Node.make(Ast.Identifier(token.lexeme), token, token))

  | Number =>
    switch Float.fromString(token.lexeme) {
    | Some(n) => Ok(Node.make(Ast.Number(n), token, token))
    | None =>
      Error(ParseError.make(parser.input, token, `Failed to parse number token '${token.lexeme}'`))
    }

  | String => {
      let value = token.lexeme->String.substring(~from=1, ~to_=token.lexeme->String.length - 1)
      Ok(Node.make(Ast.String(value), token, token))
    }

  | LeftParen =>
    parser->advance
    parser
    ->expression
    ->Result.flatMap(expr => {
      let token = parser->peek
      switch token.kind {
      | Token.RightParen => Ok(expr)
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

  | _ =>
    Error(
      ParseError.expectedButFound(
        parser.input,
        token,
        "Expected an expression (let, if, a lambda, some math, or primitive types)",
      ),
    )
  }

  // None of the branches advance after the last successful token, so we do it
  // here to avoid repetition
  if result->Result.isOk {
    parser->advance
  }

  result
}

let parse = (input: string, tokens: array<Token.t>): result<
  Node.t<Ast.expr>,
  array<ParseError.t>,
> => {
  let parser = {
    input: input,
    tokens: tokens,
    current: 0,
  }

  let result = parser->expression

  switch result {
  | Error(e) => Error([e])
  | Ok(a) => Ok(a)
  }
}
