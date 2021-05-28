@scope("JSON") @val
external stringify: ('a, Js.Null.t<'b>, int) => string = "stringify"

let stringifyAnyWithSpace = (o, s) => stringify(o, Js.null, s)
