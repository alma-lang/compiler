type rec expr =
  | Unit
  | Identifier(string)
  | Lambda(string, expr)
  | FnCall(expr, expr)
  | Let(string, expr, expr)

let rec exprToString = (expr: expr) => {
  switch expr {
  | Unit => "()"
  | Identifier(x) => x
  | Lambda(param, expr) => `λ${param}. ${exprToString(expr)}`
  | FnCall(callee, arg) => `(${exprToString(callee)} ${exprToString(arg)})`
  | Let(binding, value, body) => `let ${binding} = ${exprToString(value)} in ${exprToString(body)}`
  }
}
