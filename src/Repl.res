let run = () => {
  Js.log(`alma repl.
  .exit      Exit the repl session
`)

  let rec loop = () => {
    let input = Readline.prompt(">")
    switch input {
    | None
    | Some(".exit") =>
      Js.log(`👋`)

    | Some("") => loop()

    | Some(input) =>
      switch Compiler.compile(input) {
      | Error(e) => Js.log(e)
      | Ok(o) => Js.log(o)
      }

      loop()
    }
  }
  loop()
}
