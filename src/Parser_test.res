Test.suite("Parser", ({test}) => {
  let testCases: array<(string, Parser.parseResultErrs<Node.t<Ast.expr>>)> = [
    // Primary
    (`123`, Ok({value: Ast.Number(123.0), start: 0, end: 3})),
    (`123.2`, Ok({value: Ast.Number(123.2), start: 0, end: 5})),
    (`true`, Ok({value: Ast.Bool(true), start: 0, end: 4})),
    (`false`, Ok({value: Ast.Bool(false), start: 0, end: 5})),
    (`variableOne`, Ok({value: Ast.Identifier(`variableOne`), start: 0, end: 11})),
    (`variable_one`, Ok({value: Ast.Identifier(`variable_one`), start: 0, end: 12})),
    (`espaÆàʥñÑol`, Ok({value: Ast.Identifier(`espaÆàʥñÑol`), start: 0, end: 11})),
    (`"😄"`, Ok({value: Ast.String(`😄`), start: 0, end: 4})),
    (`"\n"`, Ok({value: Ast.String(`\n`), start: 0, end: 3})),
    (`""`, Ok({value: Ast.String(``), start: 0, end: 2})),
    (`("")`, Ok({value: Ast.String(``), start: 1, end: 3})),
    (`(((1)))`, Ok({value: Ast.Number(1.0), start: 3, end: 4})),
    (
      `(((1))`,
      Error([
        {
          message: `1:6: Expected ')' after parenthesized expression, but instead found: '[End of file]'

  1│ (((1))
   │ ↑`,
          token: {
            kind: Token.Eof,
            lexeme: "[End of file]",
            line: 1,
            indent: 0,
            column: 6,
            position: 5,
          },
        },
      ]),
    ),
    (
      `(((1))))`,
      Error([
        {
          message: `1:7: Expected the end of input, but instead found: ')'

  1│ (((1))))
   │        ↑`,
          token: {
            kind: Token.RightParen,
            lexeme: ")",
            line: 1,
            indent: 0,
            column: 7,
            position: 7,
          },
        },
      ]),
    ),
    (
      `(
  ((1))
)`,
      Ok({value: Ast.Number(1.0), start: 6, end: 7}),
    ),
    (
      `fun arg`,
      Ok({
        value: Ast.FnCall(
          {value: Ast.Identifier("fun"), start: 0, end: 3},
          {value: Ast.Identifier("arg"), start: 4, end: 7},
        ),
        start: 0,
        end: 7,
      }),
    ),
    (
      `fun
 arg`,
      Ok({
        value: Ast.FnCall(
          {value: Ast.Identifier("fun"), start: 0, end: 3},
          {value: Ast.Identifier("arg"), start: 5, end: 8},
        ),
        start: 0,
        end: 8,
      }),
    ),
    (
      `  fun
    arg`,
      Ok({
        value: Ast.FnCall(
          {value: Ast.Identifier("fun"), start: 2, end: 5},
          {value: Ast.Identifier("arg"), start: 10, end: 13},
        ),
        start: 2,
        end: 13,
      }),
    ),
    // TODO: This is a list of expressions, it shouldn't parse as an expression,
    // now should it? Should they implicitly be assigned to let _ = expr? as an
    // AST transform?
    /* ( */
    /* `fun\narg`, */
    /* Ok({ */
    /* value: Ast.FnCall( */
    /* {value: Ast.Identifier("fun"), start: 2, end: 5}, */
    /* {value: Ast.Identifier("arg"), start: 10, end: 13}, */
    /* ), */
    /* start: 2, */
    /* end: 13, */
    /* }), */
    /* ), */
  ]

  testCases->Array.forEachWithIndex((i, (input, expected)) =>
    test(j`$i`, () => {
      switch input->Tokenizer.parse {
      | Ok(tokens) => {
          let actual = Parser.parse(input, tokens)
          if !Test.equal(actual, expected) {
            Js.log2("\n", actual->Json.stringifyAnyWithSpace(4))
          }
          Test.assertEquals(actual, expected, "")
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
