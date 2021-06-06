module JsArray = Js.Array2

type typeVarId = int
type level = int

type rec typ =
  | /* unit type */
  Unit

  | /* Named type */
  Named(string, list<typ>)

  /* 'a, 'b, etc.
   *
   * A reference to a bound or unbound typeVar, set during unification.
   * This is unique to algorithm J where mutation is needed to remember
   * some substitutions.
   * The level of this typeVar identifies how many let-bindings deep it was
   * declared in. This is used to prevent generalization of typevars that
   * escape outside the current let-binding scope.
   */
  | Var(ref<typeVar>)

  /* 'a -> 'b, all functions are single-argument only
   * e.g. \a b c.c  is automatically translated to \a.\b.\c.c
   * Currying is also automatic
   */
  | Fn(typ, typ)

  /* Polytypes in the form  forall 'a 'b ... 'y . 'z
   * The typeVar list will be a list of all monomorphic typevars in 'z
   * Used only in let-bindings to make the declaration polymorphic
   */
  | PolyType(list<typeVarId>, typ)

and typeVar =
  | Bound(typ)
  | Unbound(typeVarId, level)

/* If this type is the a in a -> b, should it be parenthesized?
 * Note this is recursive in case bound typevars are used
 */
let rec shouldParenthesize = x =>
  switch x {
  | Var({contents: Bound(t')}) => shouldParenthesize(t')
  | Fn(_, _) | PolyType(_, _) => true
  | _ => false
  }

/* Return the next unique lowercase-letter string after the given one, e.g:
 *
 *   nextLetter "'a" = "'b"
 *   nextLetter "'b" = "'c"
 *   nextLetter "'z" = "'{"   This can be fixed but most examples shouldn't have > 26 typevars anyway
 */
let nextLetter = (s: ref<string>) => {
  open Js.String2
  let c = charCodeAt(s.contents, 0)
  s := fromCharCode(Float.toInt(c) + 1)
}

let parameters = (t: typ): array<typ> => {
  let rec parametersR = (params, t) =>
    switch t {
    | Fn(arg, body) => {
        params->JsArray.push(arg)->ignore
        parametersR(params, body)
      }
    | _ => params
    }
  parametersR([], t)
}

/* pretty printing types */
let toString = (t: typ): string => {
  /* Keep track of number to character bindings for typevars
   * e.g. '2 => 'a, '5 => 'b, etc.
   * Letters are assigned to typevars by the order in which the typevars
   * appear in the type, left to right
   */
  let rec toStringRec = (curTypeVarName, typeVarNames, x) =>
    switch x {
    | Unit => "unit"

    | Named(name, params) =>
      if params->List.length == 0 {
        name
      } else {
        let paramsString: string =
          params->List.toArray->Array.joinWith(" ", toStringRec(curTypeVarName, typeVarNames))
        `${name} ${paramsString}`
      }

    | Var({contents: Bound(t_)}) => toStringRec(curTypeVarName, typeVarNames, t_)
    | Var({contents: Unbound(n, _)}) =>
      switch typeVarNames->HashMap.Int.get(n) {
      | Some(s) => s
      | None =>
        let s = curTypeVarName.contents
        HashMap.Int.set(typeVarNames, n, s)
        nextLetter(curTypeVarName)
        s
      }

    | Fn(a, b) =>
      let aStr = toStringRec(curTypeVarName, typeVarNames, a)
      let bStr = toStringRec(curTypeVarName, typeVarNames, b)
      if shouldParenthesize(a) {
        "(" ++ (aStr ++ (") -> " ++ bStr))
      } else {
        aStr ++ (" -> " ++ bStr)
      }

    | PolyType(tvs, t) =>
      let typeVarToString = t =>
        toStringRec(
          curTypeVarName,
          typeVarNames,
          Var(ref(Unbound(t, Js.Int.max /* This level doesn't matter for printing */))),
        )
      let tvsStr = List.reduce(tvs, "", (s, tv) => s ++ (" '" ++ typeVarToString(tv)))
      "∀" ++ (tvsStr ++ (" . " ++ toStringRec(curTypeVarName, typeVarNames, t)))
    }

  toStringRec(ref("a"), HashMap.Int.make(~hintSize=1), t)
}

let print = (t: typ): unit => Js.log(toString(t))

// Primitive types

let float_ = Named("Float", list{})
let bool_ = Named("Bool", list{})
let string_ = Named("String", list{})
