let args: array<string> = %raw(`Deno.args`)

let help = () =>
  Js.log("
  repl            Start the REPL
")

switch args {
| ["repl"] => Repl.run()
| _ => help()
}
