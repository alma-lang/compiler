let forEachCodePoint: (string, int => unit) => unit = %raw(`
function(input, fn) {
  for (let codePoint of input) {
    fn(codePoint.codePointAt(0))
  }
}
`)

module String = Js.String2
module JsArray = Js.Array2

type error = {
  line: int,
  column: int,
  message: string,
}

type status =
  | SingleToken
  | DoubleToken(Token.typ)
  | LineCommentToken
  | WhitespaceToken(bool)
  | StringToken(bool)
  | NumberToken(bool)
  | IdentifierToken(bool)

type state = {
  mutable status: status,
  input: string,
  tokens: array<Token.t>,
  errors: array<error>,
  mutable start: int,
  mutable current: int,
  mutable line: int,
  mutable lineStartPosition: int,
  mutable indent: int,
  mutable currentChar: string,
}

let isDigit = {
  let zero = String.charCodeAt("0", 0)->Float.toInt
  let nine = String.charCodeAt("9", 0)->Float.toInt
  s => {
    switch String.codePointAt(s, 0) {
    | Some(cp) => cp >= zero && cp <= nine
    | None => false
    }
  }
}

let identifierStartRe = %re("/[\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}\p{Nl}_]/u")
let identifierRestRe = %re("/[\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}\p{Nl}\p{Mn}\p{Mc}\p{Nd}\p{Pc}_]/u")
let isIdentifierChar = (s, ~isStart) => {
  // Similar to https://es5.github.io/#x7.6
  Js.Re.test_(isStart ? identifierStartRe : identifierRestRe, s)
}

let peek = state => {
  let nextCharStart = state.current + state.currentChar->String.length
  if nextCharStart >= state.input->String.length {
    None
  } else {
    state.input->String.codePointAt(nextCharStart)->Option.map(String.fromCodePoint)
  }
}

let peekSecond = state => {
  switch state->peek {
  | Some(c) =>
    let secondCharStart = state.current + state.currentChar->String.length + c->String.length
    if secondCharStart >= state.input->String.length {
      None
    } else {
      state.input->String.codePointAt(secondCharStart)->Option.map(String.fromCodePoint)
    }
  | None => None
  }
}

let pushToken = (state, token) => state.tokens->JsArray.push(token)->ignore

let nextTokenStart = state => state.current + state.currentChar->String.length

let resetForNextToken = state => {
  state.start = state->nextTokenStart
  state.status = SingleToken
}

let lexeme = state => state.input->String.substring(~from=state.start, ~to_=state->nextTokenStart)

let addToken = (state, kind) => {
  state->pushToken({
    kind: kind,
    lexeme: state->lexeme,
    line: state.line,
    column: state.start - state.lineStartPosition,
    position: state.start,
    indent: state.indent,
  })

  state->resetForNextToken
}

let addError = (state, ~useStart=false, message) => {
  let (position, column) = if useStart {
    (state.start, state.start - state.lineStartPosition)
  } else {
    (state.current, state.current - state.lineStartPosition)
  }

  let message =
    `${state.line->Int.toString}:${column->Int.toString}\n${message}\n` ++
    Input.linesReportAtPositionWithPointer(
      state.input,
      ~position,
      ~lineNumber=state.line,
      ~columnNumber=column,
    )->Option.getWithDefault("")

  state.errors
  ->JsArray.push({
    line: state.line,
    column: column,
    message: message,
  })
  ->ignore

  state->resetForNextToken
}

let rec parseToken = (state: state): unit => {
  switch state.status {
  | SingleToken =>
    switch state.currentChar {
    | "(" => state->addToken(LeftParen)
    | ")" => state->addToken(RightParen)
    | "{" => state->addToken(LeftBrace)
    | "}" => state->addToken(RightBrace)
    | "," => state->addToken(Comma)
    | "." => state->addToken(Dot)
    | "+" => state->addToken(Plus)
    | ";" => state->addToken(Semicolon)
    | ":" => state->addToken(Colon)
    | "*" => state->addToken(Star)
    | "\\" => state->addToken(Backslash)

    | "-" =>
      switch state->peek {
      | Some(">") => state.status = DoubleToken(Arrow)
      | _ => state->addToken(Minus)
      }

    | "!" =>
      switch state->peek {
      | Some("=") => state.status = DoubleToken(BangEqual)
      | _ => state->addToken(Bang)
      }

    | "=" =>
      switch state->peek {
      | Some("=") => state.status = DoubleToken(EqualEqual)
      | _ => state->addToken(Equal)
      }

    | "<" =>
      switch state->peek {
      | Some("=") => state.status = DoubleToken(LessEqual)
      | _ => state->addToken(Less)
      }

    | ">" =>
      switch state->peek {
      | Some("=") => state.status = DoubleToken(GreaterEqual)
      | _ => state->addToken(Greater)
      }

    | "/" =>
      switch state->peek {
      | Some("/") => state.status = LineCommentToken
      | _ => state->addToken(Slash)
      }

    | "\"" => state.status = StringToken(false)

    | " "
    | "\n" => {
        state.status = WhitespaceToken(false)
        parseToken(state)
      }

    | "\r"
    | "\t"
    | "" =>
      state->resetForNextToken

    | ch if isDigit(ch) => {
        state.status = NumberToken(false)
        parseToken(state)
      }

    | ch if isIdentifierChar(ch, ~isStart=true) => {
        state.status = IdentifierToken(true)
        parseToken(state)
      }

    | ch => state->addError(`Unexpected character '${ch}'.`)
    }

  | DoubleToken(tokenType) => state->addToken(tokenType)

  | LineCommentToken =>
    switch state.currentChar {
    | "" | "\n" => state->addToken(Comment)
    | _ => ()
    }

  | WhitespaceToken(lineStarted) => {
      switch state.currentChar {
      | " " =>
        if lineStarted {
          state.indent = state.indent + 1
        }
        state.status = WhitespaceToken(lineStarted)

      | "\n" => {
          state.status = WhitespaceToken(true)
          state.line = state.line + 1
          state.indent = 0
          state.lineStartPosition = state.current + 1
        }

      | _ =>
        Js.Exn.raiseError("Got to the whitespace tokenizer without a valid space or newline char.")
      }

      switch state->peek {
      | Some("\n")
      | Some(" ") => ()
      | _ => state->resetForNextToken
      }
    }

  | StringToken(prevWasBackslash) =>
    switch state.currentChar {
    | `\\` => state.status = StringToken(true)
    | `"` if !prevWasBackslash => state->addToken(String)
    | "" => state->addError("Unclosed string.", ~useStart=true)
    | _ => prevWasBackslash ? state.status = StringToken(false) : ()
    }

  | NumberToken(seenDot) =>
    switch state.currentChar {
    | "." => {
        if seenDot {
          state->addError(`Multiple dots while parsing a number.`)
        } else {
          state.status = NumberToken(true)
        }

        switch state->peek {
        | Some(c) if isDigit(c) => ()
        | _ => state->addError(`Expected more digits after a dot in a number.`)
        }
      }

    | c if isDigit(c) =>
      switch state->peek {
      // Continue munching until there are no numbers or dot
      | Some(".") => ()
      | Some(c) if isDigit(c) => ()

      // Anything else we find ends the number token
      | _ => state->addToken(Float)
      }

    | _ => Js.Exn.raiseError("Got to the number tokenizer without a valid number digit.")
    }

  | IdentifierToken(isStart) =>
    switch state.currentChar {
    | c if isIdentifierChar(c, ~isStart) =>
      let isStart = false
      state.status = IdentifierToken(isStart)

      switch state->peek {
      | Some(c) if isIdentifierChar(c, ~isStart) => ()
      // Anything else we find ends the identifier token
      | _ =>
        state->addToken(
          switch state->lexeme {
          | "and" => And
          | "or" => Or
          | "not" => Not
          | "if" => If
          | "then" => Then
          | "else" => Else
          | "true" => True
          | "false" => False
          | "let" => Let
          | "import" => Import
          | "as" => As
          | "exports" => Exports
          | "module" => Module
          | _ => Identifier
          },
        )
      }

    | _ => Js.Exn.raiseError("Got to the identifier tokenizer without a valid number digit.")
    }
  }
}

let parse = (input): result<array<Token.t>, array<error>> => {
  let state: state = {
    status: SingleToken,
    tokens: [],
    errors: [],
    start: 0,
    current: 0,
    line: 1,
    lineStartPosition: 0,
    indent: 0,
    input: input,
    currentChar: "",
  }

  input->forEachCodePoint(i => {
    state.currentChar = String.fromCodePoint(i)

    parseToken(state)

    state.current = state.current + state.currentChar->String.length
  })

  // End of file found, represented as empty string char for the state machine
  // to process
  state.currentChar = ""
  parseToken(state)
  state->pushToken({
    kind: Eof,
    lexeme: "[End of file]",
    line: state.line,
    // If the last char is a \n, then lineStartPosition may be bigger than the last \n
    // position. Default to column 0 then.
    column: max(state.current - state.lineStartPosition, 0),
    // Set the position of the token in the string to the last character in the
    // string so that somewhere in the string can be pointed at. The column will
    // be offset one+ from this one.
    position: input->String.length - 1,
    indent: state.indent,
  })

  if state.errors->Array.length > 0 {
    Error(state.errors)
  } else {
    Ok(state.tokens)
  }
}
