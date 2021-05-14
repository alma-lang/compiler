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
  | StringToken
  | NumberToken(bool)
  | IdentifierToken

type state = {
  mutable status: status,
  input: string,
  tokens: array<Token.t>,
  errors: array<error>,
  mutable start: int,
  mutable current: int,
  mutable line: int,
  mutable lineStartPosition: int,
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

let isIdentifierChar = s => {
  let word = %re("/[\p{L}\p{S}\p{P}]/u")
  Js.Re.test_(word, s)
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

let addToken = (state, kind) => {
  let lexeme = state.input->String.substring(~from=state.start, ~to_=state->nextTokenStart)

  state->pushToken({
    kind: kind,
    lexeme: lexeme,
    line: state.line,
    column: state.start - state.lineStartPosition,
    position: state.start,
  })

  state->resetForNextToken
}

let addError = (state, message) => {
  let column = state.current - state.lineStartPosition

  // TODO: Better tokenizer error messages
  let message = `${state.line->Int.toString}:${column->Int.toString}\n${message}`

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
    | "(" => state->addToken(Token.LeftParen)
    | ")" => state->addToken(Token.RightParen)
    | "{" => state->addToken(Token.LeftBrace)
    | "}" => state->addToken(Token.RightBrace)
    | "," => state->addToken(Token.Comma)
    | "." => state->addToken(Token.Dot)
    | "-" => state->addToken(Token.Minus)
    | "+" => state->addToken(Token.Plus)
    | ";" => state->addToken(Token.Semicolon)
    | ":" => state->addToken(Token.Colon)
    | "*" => state->addToken(Token.Star)

    | "!" =>
      switch state->peek {
      | Some("=") => state.status = DoubleToken(Token.BangEqual)
      | _ => state->addToken(Token.Bang)
      }

    | "=" =>
      switch state->peek {
      | Some("=") => state.status = DoubleToken(Token.EqualEqual)
      | _ => state->addToken(Token.Equal)
      }

    | "<" =>
      switch state->peek {
      | Some("=") => state.status = DoubleToken(Token.LessEqual)
      | _ => state->addToken(Token.Less)
      }

    | ">" =>
      switch state->peek {
      | Some("=") => state.status = DoubleToken(Token.GreaterEqual)
      | _ => state->addToken(Token.Greater)
      }

    | "/" =>
      switch state->peek {
      | Some("/") => state.status = LineCommentToken
      | _ => state->addToken(Token.Slash)
      }

    | "\"" => state.status = StringToken

    | "\n" => {
        // Ignore newlines, but set line meta in the tokenizer state
        state.line = state.line + 1
        state.lineStartPosition = state.current + 1
        state->resetForNextToken
      }

    | " "
    | "\r"
    | "\t"
    | "" =>
      state->resetForNextToken

    | ch if isDigit(ch) => {
        state.status = NumberToken(false)
        parseToken(state)
      }

    | ch if isIdentifierChar(ch) => {
        state.status = IdentifierToken
        parseToken(state)
      }

    | ch => state->addError(`Unexpected character '${ch}'.`)
    }

  | DoubleToken(tokenType) => state->addToken(tokenType)

  | LineCommentToken =>
    switch state.currentChar {
    | "" | "\n" => state->addToken(Token.Comment)
    | _ => ()
    }

  | StringToken =>
    switch state.currentChar {
    | "\"" => state->addToken(Token.String)
    | "" => state->addError("Unclosed string.")
    | _ => ()
    }

  | NumberToken(seenDot) =>
    switch state.currentChar {
    | "." =>
      if seenDot {
        state->addError(`Multiple dots while parsing a number.`)
      } else {
        state.status = NumberToken(true)
      }

    | c if isDigit(c) =>
      switch state->peek {
      // Continue munching until there are no numbers or dot
      | Some(".") => ()
      | Some(c) if isDigit(c) => ()

      // Anything else we find ends the number token
      | _ => state->addToken(Token.Number)
      }

    | _ => Js.Exn.raiseError("Got to the number tokenizer without a valid number digit.")
    }

  | IdentifierToken =>
    switch state.currentChar {
    | c if isIdentifierChar(c) =>
      switch state->peek {
      | Some(c) if isIdentifierChar(c) => ()
      // Anything else we find ends the identifier token
      | _ => state->addToken(Token.Identifier)
      }

    | _ => Js.Exn.raiseError("Got to the identifier tokenizer without a valid number digit.")
    }
  }
}

let parse = (input): (array<Token.t>, array<error>) => {
  let state: state = {
    status: SingleToken,
    tokens: [],
    errors: [],
    start: 0,
    current: 0,
    line: 1,
    lineStartPosition: 0,
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
    kind: Token.Eof,
    lexeme: "[End of file]",
    line: state.line,
    // If the last char is a \n, then lineStartPosition may be bigger than the last \n
    // position. Default to column 0 then.
    column: max(state.current - state.lineStartPosition, 0),
    position: input->String.length,
  })

  (state.tokens, state.errors)
}
