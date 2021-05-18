let run = () => {
  let readline = Readline.make()
  let rec loop = () => {
    readline->Readline.question("> ", input => {
      switch input {
      | ".exit" => {
          Js.log(`👋`)
          readline->Readline.close
        }
      | _ => switch Tokenizer.parse(input) {
        | Error(errors) => Array.forEach(errors, Js.log)
        | Ok(tokens) =>
          switch Parser.parse(input, tokens) {
          | Error(errors) => Array.forEach(errors, Js.log)
          | Ok(ast) =>
            try {
              ast->Infer.infer(TypeEnv.empty()) |> Type.print
            } catch {
            | Infer.TypeError => Js.log("type error")
            | Not_found => Js.log("variable not found")
            }
            loop()
          }
        }
      }
    })
  }
  loop()
}
