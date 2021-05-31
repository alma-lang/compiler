module JsArray = Js.Array2

type unary =
  | Not
  | Minus

module Binop = {
  type typ =
    | Or
    | And
    | Equal
    | NotEqual
    | GreaterThan
    | GreaterEqualThan
    | LessThan
    | LessEqualThan
    | Addition
    | Substraction
    | Multiplication
    | Division

  type associativity =
    | LTR
    | RTL

  type t = {
    typ: typ,
    precedence: int,
    associativity: associativity,
    fn: string,
  }

  let or = {typ: Or, precedence: 6, associativity: LTR, fn: "(or)"}
  let and_ = {typ: And, precedence: 7, associativity: LTR, fn: "(and)"}
  let equal = {typ: Equal, precedence: 11, associativity: LTR, fn: "(==)"}
  let notEqual = {typ: NotEqual, precedence: 11, associativity: LTR, fn: "(!=)"}
  let greaterThan = {typ: GreaterThan, precedence: 12, associativity: LTR, fn: "(>)"}
  let greaterEqualThan = {typ: GreaterEqualThan, precedence: 12, associativity: LTR, fn: "(>=)"}
  let lessThan = {typ: LessThan, precedence: 12, associativity: LTR, fn: "(<)"}
  let lessEqualThan = {typ: LessEqualThan, precedence: 12, associativity: LTR, fn: "(<=)"}
  let addition = {typ: Addition, precedence: 14, associativity: LTR, fn: "(+)"}
  let substraction = {typ: Substraction, precedence: 14, associativity: LTR, fn: "(-)"}
  let multiplication = {typ: Multiplication, precedence: 15, associativity: LTR, fn: "(*)"}
  let division = {typ: Division, precedence: 15, associativity: LTR, fn: "(/)"}
}

module Pattern = {
  type t = Identifier(string)

  let toString = p =>
    switch p {
    | Identifier(s) => s
    }
}

module Expression = {
  type rec t =
    | Unit
    | Bool(bool)
    | Float(float)
    | String(string)
    | Identifier(string)
    | Unary(Node.t<unary>, Node.t<t>)
    | Binary(Node.t<t>, Node.t<Binop.t>, Node.t<t>)
    | Lambda(array<Node.t<Pattern.t>>, Node.t<t>)
    | FnCall(Node.t<t>, Node.t<t>)
    | Let(Node.t<Pattern.t>, Node.t<t>, Node.t<t>)
    | If(Node.t<t>, Node.t<t>, Node.t<t>)

  let rec toString = (expr: t) => {
    switch expr {
    | Unit => "()"
    | Bool(b) =>
      if b {
        "true"
      } else {
        "false"
      }
    | Float(f) => Float.toString(f)
    | String(s) => `"${s}"`
    | Identifier(x) => x
    | Unary({value: Not}, expr) => `!${toString(expr.value)}`
    | Unary({value: Minus}, expr) => `-${toString(expr.value)}`
    | Binary(left, op, right) => {
        let opS = switch op.value.typ {
        | Or => "||"
        | And => "&&"
        | Equal => "=="
        | NotEqual => "!="
        | GreaterThan => ">"
        | GreaterEqualThan => ">="
        | LessThan => "<"
        | LessEqualThan => "<="
        | Addition => "+"
        | Substraction => "-"
        | Multiplication => "*"
        | Division => "/"
        }
        `(${toString(left.value)} ${opS} ${toString(right.value)})`
      }
    | Lambda(params, expr) => {
        let paramsStr = params->Array.map(p => Pattern.toString(p.value))->JsArray.joinWith(", ")
        `(${paramsStr}) => ${toString(expr.value)}`
      }
    | FnCall(callee, arg) => `${toString(callee.value)}(${toString(arg.value)})`
    | Let(pattern, value, body) =>
      `let ${Pattern.toString(pattern.value)} = (${toString(value.value)}); ${toString(body.value)}`
    | If(condition, then, else_) =>
      `(${toString(condition.value)} ? ${toString(then.value)} : ${toString(else_.value)})`
    }
  }
}
