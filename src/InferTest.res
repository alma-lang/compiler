let test = (name: string, ast: Ast.expr, expected: string) => {
  try {
    let typ = Infer.infer(ast, TypeEnv.empty())
    let correct = expected == Type.toString(typ)
    Js.log(`  ${correct ? "" : name} ${correct ? "" : ":"} ${correct ? "" : Type.toString(typ)}`)
    Js.log(`${correct ? " " : "x"} ${name} : ${Type.toString(typ)}`)
    Js.log(`  ${name} : ${Ast.exprToString(ast)}`)
  } catch {
  | Infer.TypeError => Js.log("type error")
  }
}

/* Tests */

let run = () => {
  // 1:    \f.\x. f x  :  (a -> b) -> a -> b
  test(
    "1",
    Lambda("f", Lambda("x", FnCall(Identifier("f"), Identifier("x")))),
    "(a -> b) -> a -> b",
  )

  // 2:    \f.\x. f (f x) : (a -> a) -> a -> a
  test(
    "2",
    Lambda("f", Lambda("x", FnCall(Identifier("f"), FnCall(Identifier("f"), Identifier("x"))))),
    "(a -> a) -> a -> a",
  )

  // (+):  \m.\n.\f.\x. m f (n f x)  :  (a -> b -> c) -> (a -> d -> b) -> a -> d -> c
  test(
    "(+)",
    Lambda(
      "m",
      Lambda(
        "n",
        Lambda(
          "f",
          Lambda(
            "x",
            FnCall(
              FnCall(Identifier("m"), Identifier("f")),
              FnCall(FnCall(Identifier("n"), Identifier("f")), Identifier("x")),
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
    Lambda(
      "n",
      Lambda(
        "f",
        Lambda(
          "x",
          FnCall(
            Identifier("f"),
            FnCall(FnCall(Identifier("n"), Identifier("f")), Identifier("x")),
          ),
        ),
      ),
    ),
    "((a -> b) -> c -> a) -> (a -> b) -> c -> b",
  )

  // mult: \m.\n.\f.\x. m (n f) x  :  (a -> b -> c) -> (d -> a) -> d -> b -> c
  test(
    "mult",
    Lambda(
      "m",
      Lambda(
        "n",
        Lambda(
          "f",
          Lambda(
            "x",
            FnCall(
              FnCall(Identifier("m"), FnCall(Identifier("n"), Identifier("f"))),
              Identifier("x"),
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
    Lambda(
      "n",
      Lambda(
        "f",
        Lambda(
          "x",
          FnCall(
            FnCall(
              FnCall(
                Identifier("n"),
                Lambda(
                  "g",
                  Lambda("h", FnCall(Identifier("h"), FnCall(Identifier("g"), Identifier("f")))),
                ),
              ),
              Lambda("u", Identifier("x")),
            ),
            Lambda("u", Identifier("u")),
          ),
        ),
      ),
    ),
    "(((a -> b) -> (b -> c) -> c) -> (d -> e) -> (f -> f) -> g) -> a -> e -> g",
  )

  /* Let generalization tests */

  // \x. let y = x in y      : 'a -> 'a
  test("let1", Lambda("x", Let("y", Identifier("x"), Identifier("y"))), "a -> a")

  // \x. let y = \z.x in y   : 'a -> 'b -> 'a
  test("let2", Lambda("x", Let("y", Lambda("z", Identifier("x")), Identifier("y"))), "a -> b -> a")
}
