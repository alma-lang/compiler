module String = Js.String2

type t<'a> = {
  value: 'a,
  line: int,
  column: int,
  start: int,
  end: int,
}

let make = (value: 'a, firstToken: Token.t, lastToken: Token.t) => {
  value: value,
  line: firstToken.line,
  column: firstToken.column,
  start: firstToken.position,
  end: lastToken.position + String.length(lastToken.lexeme),
}
