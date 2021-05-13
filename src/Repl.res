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
        try {
          input->Parser.parse->Infer.infer(TypeEnv.empty()) |> Type.print
        } catch {
        | Infer.TypeError => Js.log("type error")
        | Not_found => Js.log("variable not found")
        | Failure(_s) => Js.log("lexing failure, invalid symbol")
        }
        loop()
      }
    })
  }
  loop()
}
