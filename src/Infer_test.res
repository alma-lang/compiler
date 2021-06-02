Test.suite("Infer", ({test}) => {
  let testCases = [
    ("\\f -> \\x -> f x", "(a -> b) -> a -> b"),
    ("\\f -> \\x -> f (f x)", "(a -> a) -> a -> a"),
    // (+):
    ("\\m -> \\n -> \\f -> \\x -> m f (n f x)", "(a -> b -> c) -> (a -> d -> b) -> a -> d -> c"),
    // succ:
    ("\\n -> \\f -> \\x -> f (n f x)", "((a -> b) -> c -> a) -> (a -> b) -> c -> b"),
    // mult:
    ("\\m -> \\n -> \\f -> \\x -> m (n f) x", "(a -> b -> c) -> (d -> a) -> d -> b -> c"),
    // pred:
    (
      "\\n -> \\f -> \\x -> n (\\g -> \\h -> h (g f)) (\\u -> x) (\\u -> u)",
      "(((a -> b) -> (b -> c) -> c) -> (d -> e) -> (f -> f) -> g) -> a -> e -> g",
    ),
    // if:
    ("if 1 == 1 then true else false", "Bool"),
    ("if 1 == 1 then 1 else 2", "Float"),
    ("if 1 == 1 then if 1 + 1 > 2 then 5 else 1 / 1 else 2 + 2", "Float"),
    (
      "if 1 then 5 else 1",
      `1:3: Type mismatch:  Float  ≠  Bool

Expected

  1│ if 1 then 5 else 1
   │    ↑

to be

Bool

but seems to be

Float`,
    ),
    // let generalization tests
    (
      "
      \\x ->
        let y = x
        y
    ",
      "a -> a",
    ),
    (
      "
      \\x ->
        let y = \\z -> x
        y
      ",
      "a -> b -> a",
    ),
    (
      "let incr = \\n -> n + 1

incr true",
      `3:0: Type mismatch:  Bool -> a  ≠  Float -> Float

Expected

  1│ let incr = \\\n -> n + 1
  2│ 
  3│ incr true
   │ ↑

to be

Float -> Float

but seems to be

Bool -> a`,
    ),
  ]

  testCases->Array.forEachWithIndex((i, (input, expected)) =>
    test(j`$i`, () => {
      switch input->Tokenizer.parse {
      | Ok(tokens) =>
        switch Parser.parse(input, tokens) {
        | Ok(ast) =>
          switch Infer.infer(ast) {
          | Ok(typ) => {
              let typStr = Type.toString(typ)
              let astStr = Ast.Expression.toString(ast.value)

              if !Test.equal(typStr, expected) {
                Js.log2("\n", typ->Json.stringifyAnyWithSpace(4))
                Js.log2("\n", ast->Json.stringifyAnyWithSpace(4))

                Js.log2("\n\n", typStr->Json.stringifyAnyWithSpace(4))
                Js.log2("\n", astStr->Json.stringifyAnyWithSpace(4))
              }

              Test.assertEquals(typStr, expected, "")
            }
          | Error(errors) =>
            errors->Infer.InferError.toStringMany(input)->Test.assertEquals(expected, "")
          }

        | Error(es) => {
            let ss = es->Array.map(a => a.message)
            ss
            ->Js.Array2.spliceInPlace(~pos=0, ~remove=0, ~add=[`Error parsing test input`])
            ->ignore
            Test.fail(ss->Js.Array2.joinWith("\n\n"))
          }
        }

      | Error(es) => {
          let ss = es->Array.map(a => a.message)
          ss
          ->Js.Array2.spliceInPlace(~pos=0, ~remove=0, ~add=[`Error tokenizing test input`])
          ->ignore
          Test.fail(ss->Js.Array2.joinWith("\n\n"))
        }
      }
    })
  )
})
