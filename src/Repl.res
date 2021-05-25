let run = () => {
  let rec loop = () => {
    let input = Readline.prompt(">")
    switch input {
    | None
    | Some(".exit") =>
      Js.log(`👋`)

    | Some("") => loop()

    | Some(input) =>
      switch Tokenizer.parse(input) {
      | Error(errors) => Array.forEach(errors, error => Js.log(error.message))

      | Ok(tokens) =>
        switch Parser.parse(input, tokens) {
        | Error(errors) => Array.forEach(errors, error => Js.log(error.message))

        | Ok(ast) =>
          try {
            ast->Infer.infer->Type.print
          } catch {
          | Infer.TypeError => Js.log("type error")
          | Not_found => Js.log("variable not found")
          }

          Js.log(Ast.exprToString(ast.value))
        }
      }
      loop()
    }
  }
  loop()
}
