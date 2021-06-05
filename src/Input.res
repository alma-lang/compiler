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
  lines: array<string>,
  ~position: int,
  ~howManyLines: int,
): array<string> => {
  switch input->lineAt(position) {
  | Some((lineStart, lineEnd)) if howManyLines > 0 => {
      let line = input->lineSubstring(~from=lineStart, ~to_=lineEnd)
      lines->JsArray.push(line)->ignore
      input->linesBeforePosition(lines, ~position=lineStart - 1, ~howManyLines=howManyLines - 1)
    }
  | _ => lines->JsArray.reverseInPlace
  }
}

let rec linesAfterPosition = (
  input: t,
  lines: array<string>,
  ~position: int,
  ~howManyLines: int,
): array<string> => {
  switch input->lineAt(position) {
  | Some((lineStart, lineEnd)) if howManyLines > 0 => {
      let line = input->lineSubstring(~from=lineStart, ~to_=lineEnd)
      lines->JsArray.push(line)->ignore
      input->linesAfterPosition(lines, ~position=lineEnd + 1, ~howManyLines=howManyLines - 1)
    }
  | _ => lines
  }
}

let linesAroundPosition = (
  input: t,
  position: int,
  ~end: option<int>=?,
  numberOfLines: int,
): option<(array<string>, (int, int, array<string>), array<string>)> => {
  let maybeStartLine = input->lineAt(position)
  // end is exclusive, so substract 1 to get the line at the actual last
  // character
  let maybeEndLine = end->Option.flatMap(end => input->lineAt(end - 1))

  switch (maybeStartLine, end, maybeEndLine) {
  | (Some((lineStart, lineEnd)), None, _) => {
      let line = input->lineSubstring(~from=lineStart, ~to_=lineEnd)

      let linesBefore =
        input->linesBeforePosition([], ~position=lineStart - 1, ~howManyLines=numberOfLines)

      let linesAfter =
        input->linesAfterPosition([], ~position=lineEnd + 1, ~howManyLines=numberOfLines)

      Some(linesBefore, (lineStart, lineEnd, [line]), linesAfter)
    }
  | (Some((lineStart, _)), Some(_), Some((_, endLineEnd))) => {
      let lines = input->lineSubstring(~from=lineStart, ~to_=endLineEnd)->String.split("\n")

      let linesBefore =
        input->linesBeforePosition([], ~position=lineStart - 1, ~howManyLines=numberOfLines)

      let linesAfter =
        input->linesAfterPosition([], ~position=endLineEnd + 1, ~howManyLines=numberOfLines)

      Some(linesBefore, (lineStart, endLineEnd, lines), linesAfter)
    }
  | _ => None
  }
}

let linesReportAtPositionWithPointer = (
  ~position: int,
  ~lineNumber: int,
  ~end: option<int>=?,
  input: string,
): option<string> => {
  let pointToEndOfInput = position == String.length(input)
  let position = pointToEndOfInput ? position - 1 : position
  let end = pointToEndOfInput ? None : end

  input
  ->linesAroundPosition(position, ~end?, 2)
  ->Option.map(((linesBefore, (linesStart, _linesEnd, lines), linesAfter)) => {
    let out = []

    let lineNumberWidth =
      (lineNumber + (lines->Array.length - 1) + linesAfter->Array.length)
      ->Int.toString
      ->String.length

    let columnNumber = position - linesStart

    linesBefore->Array.forEachWithIndex((i, l) => {
      let ln =
        (lineNumber - linesBefore->Array.length + i)->Int.toString->padStart(lineNumberWidth, " ")
      out->JsArray.push(`  ${ln}│  ${l}`)->ignore
    })

    switch lines {
    | [] => Js.Exn.raiseError(j`Couldn't locate code:\n\n$input\n\n$position, $lineNumber, $end`)
    | [line] => {
        let ln = lineNumber->Int.toString->padStart(lineNumberWidth, " ")
        out->JsArray.push(`  ${ln}│  ${line}`)->ignore

        let lineNumberAsSpaces = " "->padStart(lineNumberWidth, " ")

        let numPointers = pointToEndOfInput
          ? 1
          : max(1, end->Option.getWithDefault(position) - position)
        let pointers = String.repeat(`↑`, numPointers)

        let numSpaces = pointToEndOfInput ? columnNumber + 1 : columnNumber
        let spaces = String.repeat(" ", numSpaces)

        out->JsArray.push(`  ${lineNumberAsSpaces}│  ${spaces}${pointers}`)->ignore
      }
    | lines =>
      lines->Array.forEachWithIndex((index, line) => {
        let ln = (lineNumber + index)->Int.toString->padStart(lineNumberWidth, " ")
        out->JsArray.push(`  ${ln}│→ ${line}`)->ignore
      })
    }

    linesAfter->Array.forEachWithIndex((i, l) => {
      let ln = (lineNumber + lines->Array.length + i)->Int.toString->padStart(lineNumberWidth, " ")
      out->JsArray.push(`  ${ln}│  ${l}`)->ignore
    })

    out->JsArray.joinWith("\n")
  })
}
