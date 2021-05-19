module String = Js.String2
module JsArray = Js.Array2

@send
external padStart: (string, int, string) => string = "padStart"

type t = string

let newLine = "\n"

let lineSubstring = (input: t, ~from: int, ~to_: int): string => {
  input->String.substring(
    ~from,
    ~to_=if to_ == input->String.length - 1 && input->String.charAt(to_) != newLine {
      // to is the last position in the string, and not a new line so should be
      // included
      to_ + 1
    } else {
      to_
    },
  )
}

let lineAt = (input: t, position: int): option<(int, int)> => {
  if input->String.length == 0 || position < 0 || position >= input->String.length {
    None
  } else {
    let start = {
      let position = ref(position)
      let break = ref(false)
      while !break.contents {
        let prevPosition = position.contents - 1
        if prevPosition < 0 || input->String.charAt(prevPosition) === newLine {
          break := true
        } else {
          position := prevPosition
        }
      }
      position.contents
    }
    let end = {
      let position = ref(position)
      let break = ref(false)
      while !break.contents {
        let nextPosition = position.contents + 1
        if (
          nextPosition >= input->String.length ||
            input->String.charAt(position.contents) === newLine
        ) {
          break := true
        } else {
          position := nextPosition
        }
      }
      position.contents
    }
    Some((start, end))
  }
}

let rec linesBeforePosition = (
  input: t,
  position: int,
  howManyLines: int,
  lines: array<string>,
): array<string> => {
  switch input->lineAt(position) {
  | Some((lineStart, lineEnd)) if howManyLines > 0 => {
      let line = input->lineSubstring(~from=lineStart, ~to_=lineEnd)
      lines->JsArray.push(line)->ignore
      input->linesBeforePosition(lineStart - 1, howManyLines - 1, lines)
    }
  | _ => lines->JsArray.reverseInPlace
  }
}

let rec linesAfterPosition = (
  input: t,
  position: int,
  howManyLines: int,
  lines: array<string>,
): array<string> => {
  switch input->lineAt(position) {
  | Some((lineStart, lineEnd)) if howManyLines > 0 => {
      let line = input->lineSubstring(~from=lineStart, ~to_=lineEnd)
      lines->JsArray.push(line)->ignore
      input->linesAfterPosition(lineEnd + 1, howManyLines - 1, lines)
    }
  | _ => lines
  }
}

let linesAroundPosition = (input: t, position: int, numberOfLines: int): option<(
  array<string>,
  string,
  array<string>,
)> => {
  input
  ->lineAt(position)
  ->Option.flatMap(((lineStart, lineEnd)) => {
    let line = input->lineSubstring(~from=lineStart, ~to_=lineEnd)
    let linesBefore = input->linesBeforePosition(lineStart - 1, numberOfLines, [])
    let linesAfter = input->linesAfterPosition(lineEnd + 1, numberOfLines, [])
    Some(linesBefore, line, linesAfter)
  })
}

let linesReportAtPositionWithPointer = (
  input: string,
  position: int,
  lineNumber: int,
  columnNumber: int,
): option<string> =>
  input
  ->linesAroundPosition(position, 2)
  ->Option.flatMap(((linesBefore, line, linesAfter)) => {
    Js.log3(linesBefore, line, linesAfter)

    let out = []

    let lineNumberWidth = (lineNumber + linesAfter->Array.length)->Int.toString->String.length

    linesBefore->Array.forEachWithIndex((i, l) => {
      let ln =
        (lineNumber - linesBefore->Array.length + i)->Int.toString->padStart(lineNumberWidth, " ")
      out->JsArray.push(`  ${ln}│ ${l}`)->ignore
    })

    let ln = lineNumber->Int.toString->padStart(lineNumberWidth, " ")
    out->JsArray.push(`  ${ln}│ ${line}`)->ignore

    let lineNumberAsSpaces = " "->padStart(lineNumberWidth, " ")
    out->JsArray.push(`  ${lineNumberAsSpaces}│ ${String.repeat(" ", columnNumber)}↑`)->ignore

    linesAfter->Array.forEachWithIndex((i, l) => {
      let ln = (lineNumber + 1 + i)->Int.toString->padStart(lineNumberWidth, " ")
      out->JsArray.push(`  ${ln}│ ${l}`)->ignore
    })

    Some(out->JsArray.joinWith("\n"))
  })

/*
Js.log("\n")
Js.log(
  linesReportAtPositionWithPointer(
    "line1
line2
line3

line5
line6",
    18,
    4,
    0,
  ),
)
*/
