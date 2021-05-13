type t

type stdout
type stdin

@scope("process") @val
external stdout: stdout = "stdout"
@scope("process") @val
external stdin: stdin = "stdin"

@module("readline") @val
external createReadline: 'a => t = "createInterface"
@send
external close: t => unit = "close"
@send
external prompt: t => unit = "prompt"
@send
external question: (t, string, string => unit) => unit = "question"

let make = (): t => {
  createReadline({"input": stdin, "output": stdout, "prompt": "> "})
}

let question = (rl: t, q: string, onAnswer: string => unit): unit => question(rl, q, onAnswer)

let close = close
