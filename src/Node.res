module String = Js.String2

type t<'a> = {
  value: 'a,
  start: int,
  end: int,
}

let make = (value: 'a, firstToken: Token.t, lastToken: Token.t) => {
  value: value,
  start: firstToken.position,
  end: lastToken.position + String.length(lastToken.lexeme),
}
