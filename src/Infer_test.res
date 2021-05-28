Test.suite("Infer", ({test}) => {
  let testCases = []

  testCases->Array.forEachWithIndex((i, (input, expected)) =>
    test(j`$i`, () => {
      switch input->Tokenizer.parse {
      | Ok(tokens) =>
        switch Parser.parse(input, tokens) {
        | Ok(ast) =>
          try {
            let typ = Infer.infer(ast)
            let typStr = Type.toString(typ)
            let astStr = Ast.exprToString(ast.value)

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

/* Tests */
/*

let node = (value): Node.t<Ast.expr> => {value: value, start: 0, end: 0}
let lambda = (a, b) => Lambda(a, b)->node
let fncall = (a, b) => FnCall(a, b)->node
let identifier = a => Identifier(a)->node
let let_ = (a, b, c) => Let(a, b, c)->node

let run = () => {
  // 1:    \f.\x. f x  :  (a -> b) -> a -> b
  test(
    "1",
    lambda("f", lambda("x", fncall(identifier("f"), identifier("x")))),
    "(a -> b) -> a -> b",
  )

  // 2:    \f.\x. f (f x) : (a -> a) -> a -> a
  test(
    "2",
    lambda("f", lambda("x", fncall(identifier("f"), fncall(identifier("f"), identifier("x"))))),
    "(a -> a) -> a -> a",
  )

  // (+):  \m.\n.\f.\x. m f (n f x)  :  (a -> b -> c) -> (a -> d -> b) -> a -> d -> c
  test(
    "(+)",
    lambda(
      "m",
      lambda(
        "n",
        lambda(
          "f",
          lambda(
            "x",
            fncall(
              fncall(identifier("m"), identifier("f")),
              fncall(fncall(identifier("n"), identifier("f")), identifier("x")),
            ),
          ),
        ),
      ),
    ),
    "(a -> b -> c) -> (a -> d -> b) -> a -> d -> c",
  )

  // succ: \n.\f.\x. f (n f x)  :  ((a -> b) -> c -> a) -> (a -> b) -> c -> b
  test(
    "succ",
    lambda(
      "n",
      lambda(
        "f",
        lambda(
          "x",
          fncall(
            identifier("f"),
            fncall(fncall(identifier("n"), identifier("f")), identifier("x")),
          ),
        ),
      ),
    ),
    "((a -> b) -> c -> a) -> (a -> b) -> c -> b",
  )

  // mult: \m.\n.\f.\x. m (n f) x  :  (a -> b -> c) -> (d -> a) -> d -> b -> c
  test(
    "mult",
    lambda(
      "m",
      lambda(
        "n",
        lambda(
          "f",
          lambda(
            "x",
            fncall(
              fncall(identifier("m"), fncall(identifier("n"), identifier("f"))),
              identifier("x"),
            ),
          ),
        ),
      ),
    ),
    "(a -> b -> c) -> (d -> a) -> d -> b -> c",
  )

  // pred: \n.\f.\x. n (\g.\h. h (g f)) (\u.x) (\u.u)  :  (((a -> b) -> (b -> c) -> c) -> (d -> e) -> (f -> f) -> g) -> a -> e -> g
  test(
    "pred",
    lambda(
      "n",
      lambda(
        "f",
        lambda(
          "x",
          fncall(
            fncall(
              fncall(
                identifier("n"),
                lambda(
                  "g",
                  lambda("h", fncall(identifier("h"), fncall(identifier("g"), identifier("f")))),
                ),
              ),
              lambda("u", identifier("x")),
            ),
            lambda("u", identifier("u")),
          ),
        ),
      ),
    ),
    "(((a -> b) -> (b -> c) -> c) -> (d -> e) -> (f -> f) -> g) -> a -> e -> g",
  )

  /* let_ generalization tests */

  // \x. let y = x in y      : 'a -> 'a
  test("let1", lambda("x", let_("y", identifier("x"), identifier("y"))), "a -> a")

  // \x. let y = \z.x in y   : 'a -> 'b -> 'a
  test("let2", lambda("x", let_("y", lambda("z", identifier("x")), identifier("y"))), "a -> b -> a")
}
*/
