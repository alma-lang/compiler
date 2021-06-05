Test.suite("Parser", ({test}) => {
  let testCases: array<(string, Parser.parseResultErrs<Node.t<Ast.Expression.t>>)> = [
    // Primary
    (`()`, Ok({value: Ast.Expression.Unit, line: 1, column: 0, start: 0, end: 2})),
    (`123`, Ok({value: Ast.Expression.Float(123.0), line: 1, column: 0, start: 0, end: 3})),
    (`123.2`, Ok({value: Ast.Expression.Float(123.2), line: 1, column: 0, start: 0, end: 5})),
    (`true`, Ok({value: Ast.Expression.Bool(true), line: 1, column: 0, start: 0, end: 4})),
    (`false`, Ok({value: Ast.Expression.Bool(false), line: 1, column: 0, start: 0, end: 5})),
    (
      `variableOne`,
      Ok({value: Ast.Expression.Identifier(`variableOne`), line: 1, column: 0, start: 0, end: 11}),
    ),
    (
      `variable_one`,
      Ok({value: Ast.Expression.Identifier(`variable_one`), line: 1, column: 0, start: 0, end: 12}),
    ),
    (
      `espaÆàʥñÑol`,
      Ok({
        value: Ast.Expression.Identifier(`espaÆàʥñÑol`),
        line: 1,
        column: 0,
        start: 0,
        end: 11,
      }),
    ),
    (`"😄"`, Ok({value: Ast.Expression.String(`😄`), line: 1, column: 0, start: 0, end: 4})),
    (`"\n"`, Ok({value: Ast.Expression.String(`\n`), line: 1, column: 0, start: 0, end: 3})),
    (`""`, Ok({value: Ast.Expression.String(``), line: 1, column: 0, start: 0, end: 2})),
    (`("")`, Ok({value: Ast.Expression.String(``), line: 1, column: 1, start: 1, end: 3})),
    (`(((1)))`, Ok({value: Ast.Expression.Float(1.0), line: 1, column: 3, start: 3, end: 4})),
    (
      `(((1))`,
      Error([
        {
          message: `1:6: Expected ')' after parenthesized expression, but instead found: '[End of file]'

  1│  (((1))
   │  ↑`,
          token: {
            kind: Token.Eof,
            lexeme: "[End of file]",
            line: 1,
            indent: 0,
            column: 6,
            position: 6,
          },
        },
      ]),
    ),
    (
      `(((1))))`,
      Error([
        {
          message: `1:7: Expected the end of input, but instead found: ')'

  1│  (((1))))
   │         ↑`,
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
      Ok({value: Ast.Expression.Float(1.0), line: 2, column: 4, start: 6, end: 7}),
    ),
    (
      `fun arg`,
      Ok({
        value: Ast.Expression.FnCall(
          {value: Ast.Expression.Identifier("fun"), line: 1, column: 0, start: 0, end: 3},
          [{value: Ast.Expression.Identifier("arg"), line: 1, column: 4, start: 4, end: 7}],
        ),
        line: 1,
        column: 0,
        start: 0,
        end: 7,
      }),
    ),
    (
      `fun
 arg`,
      Ok({
        value: Ast.Expression.FnCall(
          {value: Ast.Expression.Identifier("fun"), line: 1, column: 0, start: 0, end: 3},
          [{value: Ast.Expression.Identifier("arg"), line: 2, column: 1, start: 5, end: 8}],
        ),
        line: 1,
        column: 0,
        start: 0,
        end: 8,
      }),
    ),
    (
      `  fun
    arg`,
      Ok({
        value: Ast.Expression.FnCall(
          {value: Ast.Expression.Identifier("fun"), line: 1, column: 2, start: 2, end: 5},
          [{value: Ast.Expression.Identifier("arg"), line: 2, column: 4, start: 10, end: 13}],
        ),
        line: 1,
        column: 2,
        start: 2,
        end: 13,
      }),
    ),
    (
      `fun\narg`,
      Error([
        {
          message: `2:0: Expected the end of input, but instead found: 'arg'

  1│  fun
  2│  arg
   │  ↑↑↑`,
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
            value: Ast.Expression.Identifier("fun"),
            line: 2,
            column: 0,
            start: 1,
            end: 4,
          },
          [
            {
              value: Ast.Expression.Identifier("arg1"),
              line: 2,
              column: 4,
              start: 5,
              end: 9,
            },
            {
              value: Ast.Expression.Identifier("arg2"),
              line: 3,
              column: 2,
              start: 12,
              end: 16,
            },
            {
              value: Ast.Expression.Identifier("arg3"),
              line: 3,
              column: 7,
              start: 17,
              end: 21,
            },
            {
              value: Ast.Expression.Identifier("arg4"),
              line: 4,
              column: 2,
              start: 24,
              end: 28,
            },
          ],
        ),
        line: 2,
        column: 0,
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
            line: 1,
            column: 0,
            start: 0,
            end: 3,
          },
          {
            value: Ast.Expression.Bool(false),
            line: 1,
            column: 4,
            start: 4,
            end: 9,
          },
        ),
        line: 1,
        column: 0,
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
            line: 1,
            column: 0,
            start: 0,
            end: 1,
          },
          {
            value: Ast.Expression.Float(5.),
            line: 1,
            column: 2,
            start: 2,
            end: 3,
          },
        ),
        line: 1,
        column: 0,
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
            line: 1,
            column: 0,
            start: 0,
            end: 4,
          },
          [
            {
              value: Ast.Expression.Unary(
                {
                  value: Ast.Minus,
                  line: 1,
                  column: 6,
                  start: 6,
                  end: 7,
                },
                {
                  value: Ast.Expression.Float(5.),
                  line: 1,
                  column: 7,
                  start: 7,
                  end: 8,
                },
              ),
              line: 1,
              column: 6,
              start: 6,
              end: 8,
            },
          ],
        ),
        line: 1,
        column: 0,
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
            line: 1,
            column: 0,
            start: 0,
            end: 1,
          },
          {
            value: Ast.Binop.substraction,
            line: 1,
            column: 2,
            start: 2,
            end: 3,
          },
          {
            value: Ast.Expression.Float(5.),
            line: 1,
            column: 4,
            start: 4,
            end: 5,
          },
        ),
        line: 1,
        column: 0,
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
            line: 1,
            column: 0,
            start: 0,
            end: 1,
          },
          {
            value: Ast.Binop.substraction,
            line: 1,
            column: 2,
            start: 2,
            end: 3,
          },
          {
            value: Ast.Expression.Unary(
              {
                value: Ast.Minus,
                line: 1,
                column: 4,
                start: 4,
                end: 5,
              },
              {
                value: Ast.Expression.Float(5.),
                line: 1,
                column: 5,
                start: 5,
                end: 6,
              },
            ),
            line: 1,
            column: 4,
            start: 4,
            end: 6,
          },
        ),
        line: 1,
        column: 0,
        start: 0,
        end: 6,
      }),
    ),
    (
      `1 + 2 / 3`,
      Ok({
        line: 1,
        column: 0,
        start: 0,
        end: 9,
        value: Ast.Expression.Binary(
          {
            line: 1,
            column: 0,
            start: 0,
            end: 1,
            value: Float(1.),
          },
          {
            line: 1,
            column: 2,
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
            line: 1,
            column: 4,
            start: 4,
            end: 9,
            value: Binary(
              {
                line: 1,
                column: 4,
                start: 4,
                end: 5,
                value: Float(2.),
              },
              {
                line: 1,
                column: 6,
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
                line: 1,
                column: 8,
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
        line: 1,
        column: 0,
        start: 0,
        end: 10,
        value: Ast.Expression.Binary(
          {
            line: 1,
            column: 0,
            start: 0,
            end: 1,
            value: Float(1.),
          },
          {
            line: 1,
            column: 2,
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
            line: 1,
            column: 5,
            start: 5,
            end: 10,
            value: Binary(
              {
                line: 1,
                column: 5,
                start: 5,
                end: 6,
                value: Float(2.),
              },
              {
                line: 1,
                column: 7,
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
                line: 1,
                column: 9,
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
              line: 1,
              column: 1,
              start: 1,
              end: 2,
            },
          ],
          {
            value: Ast.Expression.Identifier("a"),
            line: 1,
            column: 6,
            start: 6,
            end: 7,
          },
        ),
        line: 1,
        column: 0,
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
              line: 1,
              column: 1,
              start: 1,
              end: 2,
            },
          ],
          {
            value: Ast.Expression.Lambda(
              [
                {
                  value: Ast.Pattern.Identifier("b"),
                  line: 1,
                  column: 7,
                  start: 7,
                  end: 8,
                },
              ],
              {
                value: Ast.Expression.Identifier("a"),
                line: 1,
                column: 12,
                start: 12,
                end: 13,
              },
            ),
            line: 1,
            column: 6,
            start: 6,
            end: 13,
          },
        ),
        line: 1,
        column: 0,
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
              line: 1,
              column: 1,
              start: 1,
              end: 2,
            },
            {
              value: Ast.Pattern.Identifier("b"),
              line: 1,
              column: 3,
              start: 3,
              end: 4,
            },
          ],
          {
            value: Ast.Expression.Identifier("a"),
            line: 1,
            column: 8,
            start: 8,
            end: 9,
          },
        ),
        line: 1,
        column: 0,
        start: 0,
        end: 9,
      }),
    ),
    (
      "if true then 1 else 2",
      Ok({
        value: Ast.Expression.If(
          {
            value: Ast.Expression.Bool(true),
            line: 1,
            column: 3,
            start: 3,
            end: 7,
          },
          {
            value: Ast.Expression.Float(1.),
            line: 1,
            column: 13,
            start: 13,
            end: 14,
          },
          {
            value: Ast.Expression.Float(2.),
            line: 1,
            column: 20,
            start: 20,
            end: 21,
          },
        ),
        line: 1,
        column: 0,
        start: 0,
        end: 21,
      }),
    ),
    (
      "
if true then
  1

else
  2",
      Ok({
        value: Ast.Expression.If(
          {
            value: Ast.Expression.Bool(true),
            line: 2,
            column: 3,
            start: 4,
            end: 8,
          },
          {
            value: Ast.Expression.Float(1.),
            line: 3,
            column: 2,
            start: 16,
            end: 17,
          },
          {
            value: Ast.Expression.Float(2.),
            line: 6,
            column: 2,
            start: 26,
            end: 27,
          },
        ),
        line: 2,
        column: 0,
        start: 1,
        end: 27,
      }),
    ),
    (
      "if true then incr 1 else 2",
      Ok({
        value: Ast.Expression.If(
          {
            value: Ast.Expression.Bool(true),
            line: 1,
            column: 3,
            start: 3,
            end: 7,
          },
          {
            value: Ast.Expression.FnCall(
              {
                value: Ast.Expression.Identifier("incr"),
                line: 1,
                column: 13,
                start: 13,
                end: 17,
              },
              [
                {
                  value: Ast.Expression.Float(1.),
                  line: 1,
                  column: 18,
                  start: 18,
                  end: 19,
                },
              ],
            ),
            line: 1,
            column: 13,
            start: 13,
            end: 19,
          },
          {
            value: Ast.Expression.Float(2.),
            line: 1,
            column: 25,
            start: 25,
            end: 26,
          },
        ),
        line: 1,
        column: 0,
        start: 0,
        end: 26,
      }),
    ),
    (
      "if true then if false then 1 else 3 else 2",
      Ok({
        value: Ast.Expression.If(
          {
            value: Ast.Expression.Bool(true),
            line: 1,
            column: 3,
            start: 3,
            end: 7,
          },
          {
            value: Ast.Expression.If(
              {
                value: Ast.Expression.Bool(false),
                line: 1,
                column: 16,
                start: 16,
                end: 21,
              },
              {
                value: Ast.Expression.Float(1.),
                line: 1,
                column: 27,
                start: 27,
                end: 28,
              },
              {
                value: Ast.Expression.Float(3.),
                line: 1,
                column: 34,
                start: 34,
                end: 35,
              },
            ),
            line: 1,
            column: 13,
            start: 13,
            end: 35,
          },
          {
            value: Ast.Expression.Float(2.),
            line: 1,
            column: 41,
            start: 41,
            end: 42,
          },
        ),
        line: 1,
        column: 0,
        start: 0,
        end: 42,
      }),
    ),
    (
      "if true { 1 } else 2",
      Error([
        {
          message: `1:8: Expected the keyword \`then\` and an expression to parse the if expression, but instead found: '{'\n
  1│  if true { 1 } else 2
   │          ↑`,
          token: {
            kind: Token.LeftBrace,
            lexeme: "{",
            position: 8,
            line: 1,
            column: 8,
            indent: 0,
          },
        },
      ]),
    ),
    (
      "if true then 1",
      Error([
        {
          message: `1:14: Expected the \`else\` branch of the if expression, but instead found: '[End of file]'

  1│  if true then 1
   │                ↑`,
          token: {
            kind: Token.Eof,
            lexeme: "[End of file]",
            position: 14,
            line: 1,
            column: 14,
            indent: 0,
          },
        },
      ]),
    ),
    (
      "let x = 1\nx",
      Ok({
        value: Ast.Expression.Let(
          {
            value: Ast.Pattern.Identifier("x"),
            line: 1,
            column: 4,
            start: 4,
            end: 5,
          },
          {
            value: Ast.Expression.Float(1.),
            line: 1,
            column: 8,
            start: 8,
            end: 9,
          },
          {
            value: Ast.Expression.Identifier("x"),
            line: 2,
            column: 0,
            start: 10,
            end: 11,
          },
        ),
        line: 1,
        column: 0,
        start: 0,
        end: 11,
      }),
    ),
    (
      "let x = a\n  x",
      Error([
        {
          message: `2:3: Expected the let definition to be followed by another let or expression in the next line and same indentation, but instead found: '[End of file]'

  1│  let x = a
  2│    x
   │     ↑`,
          token: {
            kind: Token.Eof,
            lexeme: "[End of file]",
            position: 13,
            line: 2,
            column: 3,
            indent: 2,
          },
        },
      ]),
    ),
    (
      "let x = a\n  x\nx",
      Ok({
        value: Ast.Expression.Let(
          {
            value: Ast.Pattern.Identifier("x"),
            line: 1,
            column: 4,
            start: 4,
            end: 5,
          },
          {
            value: Ast.Expression.FnCall(
              {value: Ast.Expression.Identifier("a"), line: 1, column: 8, start: 8, end: 9},
              [{value: Ast.Expression.Identifier("x"), line: 2, column: 2, start: 12, end: 13}],
            ),
            line: 1,
            column: 8,
            start: 8,
            end: 13,
          },
          {
            value: Ast.Expression.Identifier("x"),
            line: 3,
            column: 0,
            start: 14,
            end: 15,
          },
        ),
        line: 1,
        column: 0,
        start: 0,
        end: 15,
      }),
    ),
    (
      "hello ()",
      Ok({
        value: Ast.Expression.FnCall(
          {value: Ast.Expression.Identifier("hello"), start: 0, end: 5, line: 1, column: 0},
          [{value: Ast.Expression.Unit, start: 6, end: 8, line: 1, column: 6}],
        ),
        start: 0,
        end: 8,
        line: 1,
        column: 0,
      }),
    ),
  ]

  testCases->Array.forEachWithIndex((i, (input, expected)) => {
    let subject =
      input->Js.String2.split("\n")->Array.keep(s => String.length(s) > 0)->Array.getExn(0)
    test(j`$i. "$subject"`, () => {
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
  })
})
