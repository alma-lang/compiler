let args: list<string> = %raw(`Deno.args`)->List.fromArray

let help = () =>
  Js.log("
  repl            Start the REPL
  run [file.mal]      Run [file.mal]
")

switch args {
| list{"repl"} => Repl.run()
| list{"run", filePath} => Run.file(filePath)
| _ => help()
}
