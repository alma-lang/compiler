let file = path => {
  File.read(path)->Js.Promise.then_(input => {
    switch Compiler.compile(input) {
    | Error(e) => Js.log(e)
    | Ok(o) => Js.log(o)
    }

    Js.Promise.resolve()
  }, _)->ignore
}
