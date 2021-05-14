let parse = (input: string): Ast.expr => {
  let (tokens, errors) = Tokenizer.parse(input)
  Js.log2(tokens, errors)
  Ast.Unit
}
