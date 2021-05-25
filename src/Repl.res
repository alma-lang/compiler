let run = () => {
  let readline = Readline.make()
  let rec loop = () => {
    readline->Readline.question("> ", input => {
      switch input {
      | ".exit" => {
          Js.log(`👋`)
          readline->Readline.close
        }

      | _ =>
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
    })
  }
  loop()
}
