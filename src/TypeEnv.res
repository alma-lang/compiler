type t = Map.String.t<Type.typ>

let empty = () => {
  let env = Map.String.empty
  env->Map.String.set("add", Type.Fn(Type.number, Type.Fn(Type.number, Type.number)))
}

let getExn = (env: t, x: string) => Map.String.getExn(env, x)
