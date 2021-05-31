type t = Map.String.t<Type.typ>

exception Not_found(string)

let empty = (typeVar: unit => Type.typ, generalize: Type.typ => Type.typ): t => {
  let env = Map.String.empty
  env
  ->Map.String.set("(or)", Type.Fn(Type.bool_, Type.Fn(Type.bool_, Type.bool_)))
  ->Map.String.set("(and)", Type.Fn(Type.bool_, Type.Fn(Type.bool_, Type.bool_)))
  ->Map.String.set(
    "(==)",
    {
      let a = typeVar()
      generalize(Type.Fn(a, Type.Fn(a, Type.bool_)))
    },
  )
  ->Map.String.set(
    "(!=)",
    {
      let a = typeVar()
      generalize(Type.Fn(a, Type.Fn(a, Type.bool_)))
    },
  )
  ->Map.String.set("(>)", Type.Fn(Type.float_, Type.Fn(Type.float_, Type.bool_)))
  ->Map.String.set("(>=)", Type.Fn(Type.float_, Type.Fn(Type.float_, Type.bool_)))
  ->Map.String.set("(<)", Type.Fn(Type.float_, Type.Fn(Type.float_, Type.bool_)))
  ->Map.String.set("(<=)", Type.Fn(Type.float_, Type.Fn(Type.float_, Type.bool_)))
  ->Map.String.set("(+)", Type.Fn(Type.float_, Type.Fn(Type.float_, Type.float_)))
  ->Map.String.set("(-)", Type.Fn(Type.float_, Type.Fn(Type.float_, Type.float_)))
  ->Map.String.set("(*)", Type.Fn(Type.float_, Type.Fn(Type.float_, Type.float_)))
  ->Map.String.set("(/)", Type.Fn(Type.float_, Type.Fn(Type.float_, Type.float_)))
}

let getExn = (env: t, x: string) =>
  switch Map.String.get(env, x) {
  | Some(x) => x
  | None => raise(Not_found(x))
  }

let set = (env: t, x: string, t: Type.typ): t => Map.String.set(env, x, t)
