type rec expr =
  | Unit
  | Bool(bool)
  | Number(float)
  | String(string)
  | Identifier(string)
  | Lambda(string, Node.t<expr>)
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
  | Number(f) => Float.toString(f)
  | String(s) => `"${s}"`
  | Identifier(x) => x
  | Lambda(param, expr) => `${param} => ${exprToString(expr.value)}`
  | FnCall(callee, arg) => `${exprToString(callee.value)}(${exprToString(arg.value)})`
  | Let(binding, value, body) =>
    `let ${binding} = (${exprToString(value.value)}); ${exprToString(body.value)}`
  }
}
