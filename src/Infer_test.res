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
    ("if 1 then 5 else 1", "type error"),
    // let generalization tests
    // ("\\x -> let y = x in y", "a -> a"),
    // ("\\x -> let y = \\z -> x in y", "a -> b -> a"),
  ]

  testCases->Array.forEachWithIndex((i, (input, expected)) =>
    test(j`$i`, () => {
      switch input->Tokenizer.parse {
      | Ok(tokens) =>
        switch Parser.parse(input, tokens) {
        | Ok(ast) =>
          try {
            let typ = Infer.infer(ast)
            let typStr = Type.toString(typ)
            let astStr = Ast.Expression.toString(ast.value)

            if !Test.equal(typStr, expected) {
              Js.log2("\n", typ->Json.stringifyAnyWithSpace(4))
              Js.log2("\n", ast->Json.stringifyAnyWithSpace(4))

              Js.log2("\n\n", typStr->Json.stringifyAnyWithSpace(4))
              Js.log2("\n", astStr->Json.stringifyAnyWithSpace(4))
            }

            Test.assertEquals(typStr, expected, "")
          } catch {
          | Infer.TypeError => Test.assertEquals("type error", expected, "")
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
