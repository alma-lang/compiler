Test.suite("Parser", ({test}) => {
  let testCases: array<(string, Parser.parseResultErrs<Node.t<Ast.Expression.t>>)> = [
    // Primary
    (`123`, Ok({value: Ast.Expression.Float(123.0), start: 0, end: 3})),
    (`123.2`, Ok({value: Ast.Expression.Float(123.2), start: 0, end: 5})),
    (`true`, Ok({value: Ast.Expression.Bool(true), start: 0, end: 4})),
    (`false`, Ok({value: Ast.Expression.Bool(false), start: 0, end: 5})),
    (`variableOne`, Ok({value: Ast.Expression.Identifier(`variableOne`), start: 0, end: 11})),
    (`variable_one`, Ok({value: Ast.Expression.Identifier(`variable_one`), start: 0, end: 12})),
    (
      `espaÆàʥñÑol`,
      Ok({value: Ast.Expression.Identifier(`espaÆàʥñÑol`), start: 0, end: 11}),
    ),
    (`"😄"`, Ok({value: Ast.Expression.String(`😄`), start: 0, end: 4})),
    (`"\n"`, Ok({value: Ast.Expression.String(`\n`), start: 0, end: 3})),
    (`""`, Ok({value: Ast.Expression.String(``), start: 0, end: 2})),
    (`("")`, Ok({value: Ast.Expression.String(``), start: 1, end: 3})),
    (`(((1)))`, Ok({value: Ast.Expression.Float(1.0), start: 3, end: 4})),
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
      Ok({value: Ast.Expression.Float(1.0), start: 6, end: 7}),
    ),
    (
      `fun arg`,
      Ok({
        value: Ast.Expression.FnCall(
          {value: Ast.Expression.Identifier("fun"), start: 0, end: 3},
          {value: Ast.Expression.Identifier("arg"), start: 4, end: 7},
        ),
        start: 0,
        end: 7,
      }),
    ),
    (
      `fun
 arg`,
      Ok({
        value: Ast.Expression.FnCall(
          {value: Ast.Expression.Identifier("fun"), start: 0, end: 3},
          {value: Ast.Expression.Identifier("arg"), start: 5, end: 8},
        ),
        start: 0,
        end: 8,
      }),
    ),
    (
      `  fun
    arg`,
      Ok({
        value: Ast.Expression.FnCall(
          {value: Ast.Expression.Identifier("fun"), start: 2, end: 5},
          {value: Ast.Expression.Identifier("arg"), start: 10, end: 13},
        ),
        start: 2,
        end: 13,
      }),
    ),
    (
      `fun\narg`,
      Error([
        {
          message: `2:0: Expected the end of input, but instead found: 'arg'\n\n  1│ fun\n  2│ arg\n   │ ↑`,
          token: {
            kind: Token.Identifier,
            lexeme: "arg",
            position: 4,
            line: 2,
            indent: 0,
            column: 0,
          },
        },
      ]),
    ),
    (
      `
fun arg1
  arg2 arg3
  arg4`,
      Ok({
        value: Ast.Expression.FnCall(
          {
            value: Ast.Expression.FnCall(
              {
                value: Ast.Expression.FnCall(
                  {
                    value: Ast.Expression.FnCall(
                      {
                        value: Ast.Expression.Identifier("fun"),
                        start: 1,
                        end: 4,
                      },
                      {
                        value: Ast.Expression.Identifier("arg1"),
                        start: 5,
                        end: 9,
                      },
                    ),
                    start: 1,
                    end: 9,
                  },
                  {
                    value: Ast.Expression.Identifier("arg2"),
                    start: 12,
                    end: 16,
                  },
                ),
                start: 1,
                end: 16,
              },
              {
                value: Ast.Expression.Identifier("arg3"),
                start: 17,
                end: 21,
              },
            ),
            start: 1,
            end: 21,
          },
          {
            value: Ast.Expression.Identifier("arg4"),
            start: 24,
            end: 28,
          },
        ),
        start: 1,
        end: 28,
      }),
    ),
    (
      `not false`,
      Ok({
        value: Ast.Expression.Unary(
          {
            value: Ast.Not,
            start: 0,
            end: 3,
          },
          {
            value: Ast.Expression.Bool(false),
            start: 4,
            end: 9,
          },
        ),
        start: 0,
        end: 9,
      }),
    ),
    (
      `- 5`,
      Ok({
        value: Ast.Expression.Unary(
          {
            value: Ast.Minus,
            start: 0,
            end: 1,
          },
          {
            value: Ast.Expression.Float(5.),
            start: 2,
            end: 3,
          },
        ),
        start: 0,
        end: 3,
      }),
    ),
    (
      `incr (-5)`,
      Ok({
        value: Ast.Expression.FnCall(
          {
            value: Ast.Expression.Identifier("incr"),
            start: 0,
            end: 4,
          },
          {
            value: Ast.Expression.Unary(
              {
                value: Ast.Minus,
                start: 6,
                end: 7,
              },
              {
                value: Ast.Expression.Float(5.),
                start: 7,
                end: 8,
              },
            ),
            start: 6,
            end: 8,
          },
        ),
        start: 0,
        end: 8,
      }),
    ),
    (
      `1 - 5`,
      Ok({
        value: Ast.Expression.Binary(
          {
            value: Ast.Expression.Float(1.),
            start: 0,
            end: 1,
          },
          {
            value: Ast.Binop.substraction,
            start: 2,
            end: 3,
          },
          {
            value: Ast.Expression.Float(5.),
            start: 4,
            end: 5,
          },
        ),
        start: 0,
        end: 5,
      }),
    ),
    (
      `1 - -5`,
      Ok({
        value: Ast.Expression.Binary(
          {
            value: Ast.Expression.Float(1.),
            start: 0,
            end: 1,
          },
          {
            value: Ast.Binop.substraction,
            start: 2,
            end: 3,
          },
          {
            value: Ast.Expression.Unary(
              {
                value: Ast.Minus,
                start: 4,
                end: 5,
              },
              {
                value: Ast.Expression.Float(5.),
                start: 5,
                end: 6,
              },
            ),
            start: 4,
            end: 6,
          },
        ),
        start: 0,
        end: 6,
      }),
    ),
    (
      `1 + 2 / 3`,
      Ok({
        start: 0,
        end: 9,
        value: Ast.Expression.Binary(
          {
            start: 0,
            end: 1,
            value: Float(1.),
          },
          {
            start: 2,
            end: 3,
            value: {
              associativity: LTR,
              fn: "(+)",
              precedence: 14,
              typ: Ast.Binop.Addition,
            },
          },
          {
            start: 4,
            end: 9,
            value: Binary(
              {
                start: 4,
                end: 5,
                value: Float(2.),
              },
              {
                start: 6,
                end: 7,
                value: {
                  associativity: LTR,
                  fn: "(/)",
                  precedence: 15,
                  typ: Ast.Binop.Division,
                },
              },
              {
                start: 8,
                end: 9,
                value: Float(3.),
              },
            ),
          },
        ),
      }),
    ),
    (
      `1 == 2 / 3`,
      Ok({
        start: 0,
        end: 10,
        value: Ast.Expression.Binary(
          {
            start: 0,
            end: 1,
            value: Float(1.),
          },
          {
            start: 2,
            end: 4,
            value: {
              associativity: LTR,
              fn: "(==)",
              precedence: 11,
              typ: Ast.Binop.Equal,
            },
          },
          {
            start: 5,
            end: 10,
            value: Binary(
              {
                start: 5,
                end: 6,
                value: Float(2.),
              },
              {
                start: 7,
                end: 8,
                value: {
                  associativity: LTR,
                  fn: "(/)",
                  precedence: 15,
                  typ: Ast.Binop.Division,
                },
              },
              {
                start: 9,
                end: 10,
                value: Float(3.),
              },
            ),
          },
        ),
      }),
    ),
    (
      "\\a -> a",
      Ok({
        value: Ast.Expression.Lambda(
          [
            {
              value: Ast.Pattern.Identifier("a"),
              start: 1,
              end: 2,
            },
          ],
          {
            value: Ast.Expression.Identifier("a"),
            start: 6,
            end: 7,
          },
        ),
        start: 0,
        end: 7,
      }),
    ),
    (
      "\\a -> \\b -> a",
      Ok({
        value: Ast.Expression.Lambda(
          [
            {
              value: Ast.Pattern.Identifier("a"),
              start: 1,
              end: 2,
            },
          ],
          {
            value: Ast.Expression.Lambda(
              [
                {
                  value: Ast.Pattern.Identifier("b"),
                  start: 7,
                  end: 8,
                },
              ],
              {
                value: Ast.Expression.Identifier("a"),
                start: 12,
                end: 13,
              },
            ),
            start: 6,
            end: 13,
          },
        ),
        start: 0,
        end: 13,
      }),
    ),
    (
      "\\a b -> a",
      Ok({
        value: Ast.Expression.Lambda(
          [
            {
              value: Ast.Pattern.Identifier("a"),
              start: 1,
              end: 2,
            },
            {
              value: Ast.Pattern.Identifier("b"),
              start: 3,
              end: 4,
            },
          ],
          {
            value: Ast.Expression.Identifier("a"),
            start: 8,
            end: 9,
          },
        ),
        start: 0,
        end: 9,
      }),
    ),
  ]

  testCases->Array.forEachWithIndex((i, (input, expected)) =>
    test(j`$i`, () => {
      switch input->Tokenizer.parse {
      | Ok(tokens) => {
          let actual = Parser.parse(input, tokens)
          if !Test.equal(actual, expected) {
            Js.log2("\n", actual->Json.stringifyAnyWithSpace(4))
            Js.log2("\n", input)
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
