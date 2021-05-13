type t = Map.String.t<Type.typ>

let empty = () => Map.String.empty

let getExn = (env: t, x: string) => Map.String.getExn(env, x)
