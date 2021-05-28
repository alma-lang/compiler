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

type rec expr =
  | Unit
  | Bool(bool)
  | Float(float)
  | String(string)
  | Identifier(string)
  | Unary(Node.t<unary>, Node.t<expr>)
  | Binary(Node.t<expr>, Node.t<Binop.t>, Node.t<expr>)
  | Lambda(Node.t<Pattern.t>, Node.t<expr>)
  | FnCall(Node.t<expr>, Node.t<expr>)
  | Let(string, Node.t<expr>, Node.t<expr>)

let rec exprToString = (expr: expr) => {
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
  | Unary({value: Not}, expr) => `!${exprToString(expr.value)}`
  | Unary({value: Minus}, expr) => `-${exprToString(expr.value)}`
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
      `(${exprToString(left.value)} ${opS} ${exprToString(right.value)})`
    }
  | Lambda(param, expr) => `${Pattern.toString(param.value)} => ${exprToString(expr.value)}`
  | FnCall(callee, arg) => `${exprToString(callee.value)}(${exprToString(arg.value)})`
  | Let(binding, value, body) =>
    `let ${binding} = (${exprToString(value.value)}); ${exprToString(body.value)}`
  }
}
