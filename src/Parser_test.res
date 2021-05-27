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
            column: 6,
            kind: Token.Eof,
            lexeme: "[End of file]",
            line: 1,
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
            column: 7,
            kind: Token.RightParen,
            lexeme: ")",
            line: 1,
            position: 7,
          },
        },
      ]),
    ),
  ]

  testCases->Array.forEachWithIndex((i, (input, expected)) =>
    test(j`$i`, () => {
      switch input->Tokenizer.parse {
      | Ok(tokens) => {
          let actual = Parser.parse(input, tokens)
          /* Js.log(actual->Js.Json.stringifyAny) */
          if !Test.equal(actual, expected) {
            Js.log2(
              "\n",
              actual
              ->Js.Json.stringifyAny
              ->Option.getExn
              ->Js.Json.parseExn
              ->Js.Json.stringifyWithSpace(4),
            )
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
