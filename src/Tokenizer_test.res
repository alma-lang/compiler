Test.suite("Tokenizer", ({test}) => {
  open Token

  let testCases: array<(string, result<array<Token.t>, array<Tokenizer.error>>)> = [
    ("", Ok([{kind: Eof, lexeme: "[End of file]", position: -1, line: 1, indent: 0, column: 0}])),
    (
      "123",
      Ok([
        {kind: Float, lexeme: "123", position: 0, line: 1, indent: 0, column: 0},
        {kind: Eof, lexeme: "[End of file]", position: 2, line: 1, indent: 0, column: 3},
      ]),
    ),
    (
      "123.345",
      Ok([
        {kind: Float, lexeme: "123.345", position: 0, line: 1, indent: 0, column: 0},
        {kind: Eof, lexeme: "[End of file]", position: 6, line: 1, indent: 0, column: 7},
      ]),
    ),
    (
      "123.sd",
      Error([
        {
          line: 1,
          column: 3,
          message: `1:3
Expected more digits after a dot in a number.
  1│ 123.sd
   │    ↑`,
        },
      ]),
    ),
    (
      "123\n or \"abc\"",
      Ok([
        {kind: Float, lexeme: "123", position: 0, line: 1, indent: 0, column: 0},
        {kind: Or, lexeme: "or", position: 5, line: 2, indent: 1, column: 1},
        {kind: String, lexeme: "\"abc\"", position: 8, line: 2, indent: 1, column: 4},
        {kind: Eof, lexeme: "[End of file]", position: 12, line: 2, indent: 1, column: 9},
      ]),
    ),
    (
      "123\n or &\"abc\"",
      Error([
        {
          line: 2,
          column: 4,
          message: `2:4
Unexpected character '&'.
  1│ 123
  2│  or &"abc"
   │     ↑`,
        },
      ]),
    ),
  ]

  testCases->Array.forEachWithIndex((i, (input, expected)) =>
    test(j`test $i`, () => Test.assertEquals(Tokenizer.parse(input), expected, ""))
  )
})
