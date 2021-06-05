Test.suite("Input", ({test}) => {
  let source1 = "line1
line2
line3

line5
line6"

  let testCases = [
    (
      source1,
      2,
      None,
      1,
      Some(`  1│  line1
   │    ↑
  2│  line2
  3│  line3`),
    ),
    (
      source1,
      12,
      None,
      3,
      Some(`  1│  line1
  2│  line2
  3│  line3
   │  ↑
  4│  
  5│  line5`),
    ),
    (
      source1,
      18,
      None,
      4,
      Some(`  2│  line2
  3│  line3
  4│  
   │  ↑
  5│  line5
  6│  line6`),
    ),
    (
      source1,
      source1->String.length - 1,
      None,
      6,
      Some(`  4│  
  5│  line5
  6│  line6
   │      ↑`),
    ),
    (
      source1,
      source1->String.length - 1,
      None,
      6,
      Some(`  4│  
  5│  line5
  6│  line6
   │      ↑`),
    ),
    (
      source1,
      source1->String.length,
      None,
      6,
      Some(`  4│  
  5│  line5
  6│  line6
   │       ↑`),
    ),
    (
      source1,
      0,
      Some(4),
      1,
      Some(`  1│  line1
   │  ↑↑↑↑
  2│  line2
  3│  line3`),
    ),
    (
      source1,
      6,
      Some(11),
      2,
      Some(`  1│  line1
  2│  line2
   │  ↑↑↑↑↑
  3│  line3
  4│  `),
    ),
    (
      source1,
      7,
      Some(15),
      2,
      Some(`  1│  line1
  2│→ line2
  3│→ line3
  4│  
  5│  line5`),
    ),
    (
      source1,
      7,
      Some(source1->String.length),
      2,
      Some(`  1│  line1
  2│→ line2
  3│→ line3
  4│→ 
  5│→ line5
  6│→ line6`),
    ),
  ]

  testCases->Array.forEachWithIndex((i, (input, position, end, lineNumber, expected)) => {
    let subject = {
      let firstLine =
        input->Js.String2.split("\n")->Array.keep(s => String.length(s) > 0)->Array.getExn(0)
      j`p$position:l$lineNumber:c$end:$firstLine`
    }
    test(j`$i. "$subject"`, () => {
      let actual = Input.linesReportAtPositionWithPointer(input, ~position, ~lineNumber, ~end?)

      if !Test.equal(actual, expected) {
        Js.log2("\n", actual->Json.stringifyAnyWithSpace(4))
        Js.log2("\n", input)
      }
      Test.assertEquals(actual, expected, "")
    })
  })
})
