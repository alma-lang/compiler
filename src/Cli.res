let args: array<string> = %raw(`process.argv.slice(2)`)

let help = () => Js.log("
  repl            Start the REPL
  compiler-tests  Run the compiler tests
")

switch args {
| ["repl"] => Repl.run()
| ["compiler-tests"] => InferTest.run()
| _ => help()
}
