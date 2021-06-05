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

module String = Js.String2
module JsArray = Js.Array2

module ParseError = {
  type t = {
    message: string,
    token: Token.t,
  }

  let make = (
    input: string,
    token: Token.t,
    ~pointAtToken: option<Token.t>=?,
    message: string,
  ): t => {
    let pointAtToken = pointAtToken->Option.getWithDefault(token)
    let position = pointAtToken.position
    let end = pointAtToken.position + pointAtToken.lexeme->String.length
    let lineNumber = pointAtToken.line

    let message =
      `${token.line->Int.toString}:${token.column->Int.toString}: ${message}\n\n` ++
      Input.linesReportAtPositionWithPointer(
        input,
        ~position,
        ~end,
        ~lineNumber,
      )->Option.getWithDefault("")

    {message: message, token: token}
  }

  let expectedButFound = (
    input: string,
    token: Token.t,
    ~pointAtToken: option<Token.t>=?,
    message: string,
  ): t => {
    make(input, token, `${message}, but instead found: '${token.lexeme}'`, ~pointAtToken?)
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
  parser
}

let rec organizeBinops = (
  left: Node.t<Ast.Expression.t>,
  binops: array<(Node.t<Ast.Binop.t>, Node.t<Ast.Expression.t>)>,
  current: ref<int>,
  minPrecedence: int,
): Node.t<Ast.Expression.t> => {
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
            Node.value: Ast.Expression.Binary(left.contents, op, right),
            line: left.contents.line,
            column: left.contents.column,
            start: left.contents.start,
            end: right.end,
          }
      }
    | _ => outerBreak := true
    }
  }

  left.contents
}

let isNestedIndent = (parser: state, parentToken: Token.t): bool => {
  let token = parser->getToken
  parentToken.line == token.line ||
    (parentToken.line < token.line && token.indent > parentToken.indent)
}

let isNextLineSameIndent = (parser: state, parentToken: Token.t): bool => {
  let token = parser->getToken
  parentToken.line < token.line && token.indent == parentToken.indent
}

let rec expression = (parser: state): parseResult<Node.t<Ast.Expression.t>> => {
  switch parser->lambda {
  | Ok(Some(lambda)) => Ok(lambda)
  | Ok(None) =>
    switch parser->if_ {
    | Ok(Some(if_)) => Ok(if_)
    | Ok(None) =>
      switch parser->let_ {
      | Ok(Some(let_)) => Ok(let_)
      | Ok(None) => parser->binary
      | Error(e) => Error(e)
      }
    | Error(e) => Error(e)
    }
  | Error(e) => Error(e)
  }
}

and let_ = (parser: state): parseResult<option<Node.t<Ast.Expression.t>>> => {
  let token = parser->getToken

  switch token.kind {
  | Let =>
    parser
    ->advance
    ->pattern
    ->Result.flatMap(pattern => {
      switch pattern {
      | Some(pattern) =>
        switch (parser->getToken).kind {
        | Equal =>
          parser
          ->advance
          ->expression
          ->Result.flatMap(value => {
            if parser->isNextLineSameIndent(token) {
              parser
              ->expression
              ->Result.map(body => Some({
                Node.value: Ast.Expression.Let(pattern, value, body),
                line: token.line,
                column: token.column,
                start: token.position,
                end: body.end,
              }))
            } else {
              Error(
                ParseError.expectedButFound(
                  parser.input,
                  parser->getToken,
                  "Expected the let definition to be followed by another let or expression in the next line and same indentation",
                ),
              )
            }
          })

        | _ =>
          Error(
            ParseError.expectedButFound(
              parser.input,
              parser->getToken,
              "Expected an = and an expression for the right side of let expression",
            ),
          )
        }
      | None =>
        Error(
          ParseError.expectedButFound(
            parser.input,
            parser->getToken,
            "Expected a pattern for the left side of the let expression",
          ),
        )
      }
    })
  | _ => Ok(None)
  }
}

and if_ = (parser: state): parseResult<option<Node.t<Ast.Expression.t>>> => {
  let token = parser->getToken

  switch token.kind {
  | If =>
    parser
    ->advance
    ->binary
    ->Result.flatMap(condition => {
      switch (parser->getToken).kind {
      | Then =>
        parser
        ->advance
        ->expression
        ->Result.flatMap(then => {
          switch (parser->getToken).kind {
          | Else =>
            parser
            ->advance
            ->expression
            ->Result.map(else_ => Some({
              Node.value: Ast.Expression.If(condition, then, else_),
              line: token.line,
              column: token.column,
              start: token.position,
              end: else_.end,
            }))

          | _ =>
            Error(
              ParseError.expectedButFound(
                parser.input,
                parser->getToken,
                "Expected the `else` branch of the if expression",
              ),
            )
          }
        })

      | _ =>
        Error(
          ParseError.expectedButFound(
            parser.input,
            parser->getToken,
            "Expected the keyword `then` and an expression to parse the if expression",
          ),
        )
      }
    })
  | _ => Ok(None)
  }
}

and lambda = (parser: state): parseResult<option<Node.t<Ast.Expression.t>>> => {
  let token = parser->getToken

  switch token.kind {
  | Backslash =>
    parser
    ->advance
    ->params
    ->Result.flatMap(params => {
      switch (parser->getToken).kind {
      | Arrow =>
        parser
        ->advance
        ->expression
        ->Result.map(expr => Some({
          Node.value: Ast.Expression.Lambda(params, expr),
          line: token.line,
          column: token.column,
          start: token.position,
          end: expr.end,
        }))

      | _ =>
        Error(
          ParseError.expectedButFound(
            parser.input,
            parser->getToken,
            "Expected a -> after the list of parameters for the function",
          ),
        )
      }
    })
  | _ => Ok(None)
  }
}

and params = (parser: state): parseResult<array<Node.t<Ast.Pattern.t>>> => {
  parser
  ->pattern
  ->Result.flatMap(pattern =>
    switch pattern {
    | Some(pattern) => parser->paramsStep([pattern])
    | None =>
      Error(
        ParseError.expectedButFound(
          parser.input,
          parser->getToken,
          "Expected a list of parameters",
        ),
      )
    }
  )
}
and paramsStep = (parser: state, patterns: array<Node.t<Ast.Pattern.t>>): parseResult<
  array<Node.t<Ast.Pattern.t>>,
> => {
  parser
  ->pattern
  ->Result.flatMap(pattern => {
    switch pattern {
    | Some(pattern) => {
        patterns->JsArray.push(pattern)->ignore
        parser->paramsStep(patterns)
      }
    | None => Ok(patterns)
    }
  })
}

and pattern = (parser: state): parseResult<option<Node.t<Ast.Pattern.t>>> => {
  let token = parser->getToken

  switch token.kind {
  | Identifier => {
      parser->advance->ignore
      Ok(Some(Node.make(Ast.Pattern.Identifier(token.lexeme), token, token)))
    }
  | _ => Ok(None)
  }
  /* For when we need to error out for more complex patterns:
    Error(
      ParseError.expectedButFound(
        parser.input,
        parser->getToken,
        "Expected a pattern (an identifier, destructuring a data structure, etc)",
      ),
    ) */
}

and binary = (parser: state): parseResult<Node.t<Ast.Expression.t>> => {
  parser
  ->unary
  ->Result.flatMap(expr =>
    parser->binaryStep([])->Result.map(binops => organizeBinops(expr, binops, ref(0), 0))
  )
}
and binaryStep = (
  parser: state,
  binops: array<(Node.t<Ast.Binop.t>, Node.t<Ast.Expression.t>)>,
): parseResult<array<(Node.t<Ast.Binop.t>, Node.t<Ast.Expression.t>)>> => {
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

      parser
      ->advance
      ->unary
      ->Result.flatMap(right => {
        binops->JsArray.push((opNode, right))->ignore
        parser->binaryStep(binops)
      })
    }

  | None => Ok(binops)
  }
}

and unary = (parser: state): parseResult<Node.t<Ast.Expression.t>> => {
  let token = parser->getToken

  let u = switch token.kind {
  | Not => {
      parser->advance->ignore
      Some(Ast.Not)
    }
  | Minus => {
      parser->advance->ignore
      Some(Ast.Minus)
    }
  | _ => None
  }

  parser
  ->call
  ->Result.map(expr =>
    switch u {
    | Some(u) => {
        let op = Node.make(u, token, token)
        {
          Node.value: Ast.Expression.Unary(op, expr),
          line: op.line,
          column: op.column,
          start: op.start,
          end: expr.end,
        }
      }
    | None => expr
    }
  )
}

and call = (parser: state): parseResult<Node.t<Ast.Expression.t>> => {
  let token = parser->getToken

  parser
  ->primary
  ->Result.flatMap(expr =>
    switch expr {
    | Some(expr) =>
      parser
      ->arguments(token, [])
      ->Result.map(args => {
        if args->Array.length == 0 {
          expr
        } else {
          let lastArg = args->Array.getExn(Array.length(args) - 1)
          {
            Node.value: Ast.Expression.FnCall(expr, args),
            line: expr.line,
            column: expr.column,
            start: expr.start,
            end: lastArg.end,
          }
        }
      })
    | None =>
      Error(
        ParseError.expectedButFound(
          parser.input,
          parser->getToken,
          "Expected an expression (a number, string, a let binding, function call, an identifier, etc.)",
        ),
      )
    }
  )
}

and arguments = (
  parser: state,
  firstToken: Token.t,
  args: array<Node.t<Ast.Expression.t>>,
): parseResult<array<Node.t<Ast.Expression.t>>> => {
  if !(parser->isNestedIndent(firstToken)) {
    Ok(args)
  } else {
    parser
    ->primary
    ->Result.flatMap(arg =>
      switch arg {
      // We tried to get an argument, but there was no match, or it was not well indented
      | None => Ok(args)

      | Some(arg) => {
          args->JsArray.push(arg)->ignore
          parser->arguments(firstToken, args)
        }
      }
    )
  }
}

and primary = (parser: state): parseResult<option<Node.t<Ast.Expression.t>>> => {
  let token = parser->getToken

  let result = switch token.kind {
  | False => Ok(Some(Node.make(Ast.Expression.Bool(false), token, token)))
  | True => Ok(Some(Node.make(Ast.Expression.Bool(true), token, token)))

  | Identifier => Ok(Some(Node.make(Ast.Expression.Identifier(token.lexeme), token, token)))

  | Float =>
    switch Float.fromString(token.lexeme) {
    | Some(n) => Ok(Some(Node.make(Ast.Expression.Float(n), token, token)))
    | None =>
      Error(ParseError.make(parser.input, token, `Failed to parse number token '${token.lexeme}'`))
    }

  | String => {
      let value = token.lexeme->String.substring(~from=1, ~to_=token.lexeme->String.length - 1)
      Ok(Some(Node.make(Ast.Expression.String(value), token, token)))
    }

  | LeftParen => {
      let nextToken = parser->advance->getToken

      switch nextToken.kind {
      | Token.RightParen => Ok(Node.make(Ast.Expression.Unit, token, nextToken))

      | _ =>
        parser
        ->expression
        ->Result.flatMap(expr => {
          let lastToken = parser->getToken

          switch lastToken.kind {
          | Token.RightParen => Ok(expr)
          | _ =>
            Error(
              ParseError.expectedButFound(
                parser.input,
                lastToken,
                ~pointAtToken=token,
                "Expected ')' after parenthesized expression",
              ),
            )
          }
        })
      }->Result.map(ast => Some(ast))
    }

  | _ => Ok(None)
  }

  // None of the branches advance after the last successful token, so we do it
  // here to avoid repetition
  switch result {
  | Ok(Some(_)) => parser->advance->ignore
  | _ => ()
  }

  result
}

let file = (parser: state): parseResultErrs<Node.t<Ast.Expression.t>> => {
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

let parse = (input: string, tokens: array<Token.t>): parseResultErrs<Node.t<Ast.Expression.t>> => {
  let parser = {
    input: input,
    tokens: tokens,
    current: 0,
  }

  parser->file
}
