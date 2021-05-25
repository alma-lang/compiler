let args: array<string> = %raw(`Deno.args`)

let help = () =>
  Js.log("
  repl            Start the REPL
  compiler-tests  Run the compiler tests
")

switch args {
| ["repl"] => Repl.run()
| ["compiler-tests"] => InferTest.run()
| _ => help()
}
