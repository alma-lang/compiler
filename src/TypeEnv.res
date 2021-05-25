type t = Map.String.t<Type.typ>

let empty = () => {
  let env = Map.String.empty
  env
  ->Map.String.set("(or)", Type.Fn(Type.bool_, Type.Fn(Type.bool_, Type.bool_)))
  ->Map.String.set("(and)", Type.Fn(Type.bool_, Type.Fn(Type.bool_, Type.bool_)))
  /* ->Map.String.set("(==)", Type.Fn(Type.number, Type.Fn(Type.number, Type.bool_))) */
  /* ->Map.String.set("(!=)", Type.Fn(Type.number, Type.Fn(Type.number, Type.bool_))) */
  ->Map.String.set("(>)", Type.Fn(Type.number, Type.Fn(Type.number, Type.bool_)))
  ->Map.String.set("(>=)", Type.Fn(Type.number, Type.Fn(Type.number, Type.bool_)))
  ->Map.String.set("(<)", Type.Fn(Type.number, Type.Fn(Type.number, Type.bool_)))
  ->Map.String.set("(<=)", Type.Fn(Type.number, Type.Fn(Type.number, Type.bool_)))
  ->Map.String.set("(+)", Type.Fn(Type.number, Type.Fn(Type.number, Type.number)))
  ->Map.String.set("(-)", Type.Fn(Type.number, Type.Fn(Type.number, Type.number)))
  ->Map.String.set("(*)", Type.Fn(Type.number, Type.Fn(Type.number, Type.number)))
  ->Map.String.set("(/)", Type.Fn(Type.number, Type.Fn(Type.number, Type.number)))
}

let getExn = (env: t, x: string) => Map.String.getExn(env, x)
