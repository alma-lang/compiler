let compile = (input: string): result<string, string> =>
  switch Tokenizer.parse(input) {
  | Error(errors) => Error(Array.joinWith(errors, "\n\n", error => error.message))

  | Ok(tokens) =>
    switch Parser.parse(input, tokens) {
    | Error(errors) => Error(Array.joinWith(errors, "\n\n", error => error.message))

    | Ok(ast) =>
      switch ast->Infer.infer {
      | Ok(typ) => Ok(Type.toString(typ))
      | Error(errors) => Error(Infer.InferError.toStringMany(errors, input))
      }
    }
  }
