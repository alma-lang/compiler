/* Grammar draft (●○):
    ○ file           → expression EOF
    ○ expression     → elet | lambda | if | operators
    ○ elet           → let expression
    ○ let            → "let" binding "=" expression "\n"
    ○ lambda         → "fn" params? "{" expression "}"
    ○ params         → "(" binding ( "," binding )* ")"
    ○ binding        → IDENTIFIER ( ":" IDENTIFIER )?
    ○ if             → "if" operators "{" expression "}" ( else "{" expression "}" )?
    // Operators
    ● binary         → binary ( binop binary )*
    ● binop          → // parsed from Ast.Binop operator list
    ● unary          → ( "not" | "-" )? call
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

let rec organizeBinops = (
  left: Node.t<Ast.expr>,
  binops: array<(Node.t<Ast.Binop.t>, Node.t<Ast.expr>)>,
  current: ref<int>,
  minPrecedence: int,
): Node.t<Ast.expr> => {
  open Ast.Binop
  // https://eli.thegreenplace.net/2012/08/02/parsing-expressions-by-precedence-climbing
  // better than the wikipedia article for precedence climibing

  let left = ref(left)
  let outerBreak = ref(false)

  while !outerBreak.contents {
    switch binops[current.contents] {
    | Some((op, rhs)) if op.value.precedence >= minPrecedence => {
        current := current.contents + 1

        let nextMinPrecedence = op.value.precedence + (op.value.associativity == LTR ? 1 : 0)

        let right = organizeBinops(rhs, binops, current, nextMinPrecedence)

        left := {
            Node.value: Ast.Binary(left.contents, op, right),
            start: left.contents.start,
            end: right.end,
          }
      }
    | _ => outerBreak := true
    }
  }

  left.contents
}

let rec expression = (parser: state): parseResult<Node.t<Ast.expr>> => {
  parser->binary
}

and binary = (parser: state): parseResult<Node.t<Ast.expr>> => {
  parser
  ->unary
  ->Result.flatMap(expr =>
    parser->binaryStep([])->Result.map(binops => organizeBinops(expr, binops, ref(0), 0))
  )
}
and binaryStep = (
  parser: state,
  binops: array<(Node.t<Ast.Binop.t>, Node.t<Ast.expr>)>,
): parseResult<array<(Node.t<Ast.Binop.t>, Node.t<Ast.expr>)>> => {
  let token = parser->getToken

  let op = switch token.kind {
  | Slash => Some(Ast.Binop.division)
  | Star => Some(Ast.Binop.multiplication)
  | Plus => Some(Ast.Binop.addition)
  | Minus => Some(Ast.Binop.substraction)
  | BangEqual => Some(Ast.Binop.notEqual)
  | EqualEqual => Some(Ast.Binop.equal)
  | Greater => Some(Ast.Binop.greaterThan)
  | GreaterEqual => Some(Ast.Binop.greaterEqualThan)
  | Less => Some(Ast.Binop.lessThan)
  | LessEqual => Some(Ast.Binop.lessEqualThan)
  | And => Some(Ast.Binop.and_)
  | Or => Some(Ast.Binop.or)
  | _ => None
  }

  switch op {
  | Some(op) => {
      let opNode = Node.make(op, token, token)

      parser->advance
      parser
      ->unary
      ->Result.flatMap(right => {
        binops->JsArray.push((opNode, right))->ignore
        parser->binaryStep(binops)
      })
    }

  | None => Ok(binops)
  }
}

and unary = (parser: state): parseResult<Node.t<Ast.expr>> => {
  let token = parser->getToken

  let u = switch token.kind {
  | Not => {
      parser->advance
      Some(Ast.Not)
    }
  | Minus => {
      parser->advance
      Some(Ast.Minus)
    }
  | _ => None
  }

  parser
  ->call
  ->Result.flatMap(expr =>
    switch u {
    | Some(u) => {
        let op = Node.make(u, token, token)
        Ok({
          Node.value: Ast.Unary(op, expr),
          start: op.start,
          end: expr.end,
        })
      }
    | None => Ok(expr)
    }
  )
}

and call = (parser: state): parseResult<Node.t<Ast.expr>> => {
  parser
  ->primary
  ->Result.flatMap(expr =>
    switch expr {
    | Some(expr) => parser->callStep(expr)
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
and callStep = (parser: state, returnExpr: Node.t<Ast.expr>): parseResult<Node.t<Ast.expr>> => {
  parser
  ->primary
  ->Result.flatMap(arg =>
    switch arg {
    // We tried to get an argument, but there was no match
    | None => Ok(returnExpr)
    | Some(arg) =>
      parser->callStep({
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
