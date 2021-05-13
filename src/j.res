open Belt

/*
 *  This implementation follows the type inference rules given at
 *  https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Algorithm_J
 *
 *  The algorithm itself uses most of the names from the above link, with
 *  a few changed for ease of typing:
 *       Γ (gamma) => env
 *       ⊢ⱼ (perpendicular symbol with j subscript, a.k.a. algorithm J) => infer
 *       Γ¯ (gamma bar) => generalize
 *
 *  And some expr constructors changed to match their more colloquial names
 *  to hopefully make this somewhat more approachable:
 *       Var => Identifier
 *       App => FnCall
 *       Abs => Lambda
 *
 *  Note that a let-binding (or Declaration here) can be of either
 *  a variable or a function
 *
 *  Additionally, implementation of "levels" for efficient generalization is
 *  taken from http://okmij.org/ftp/ML/generalization.html
 */

/*
 * Working with a simple language with unit, variables,
 * type annotations, lambdas, and function application
 */
type rec expr =
  | Unit
  | Identifier(string)
  | Lambda(string, expr)
  | FnCall(expr, expr)
  | Let(string, expr, expr)

exception TypeError

module Type = {
  type typeVarId = int
  type level = int

  type rec typ =
    | /* unit type */
    TUnit

    /* 'a, 'b, etc. */
    /* A reference to a bound or unbound typeVar, set during unification.
     * This is unique to algorithm J where mutation is needed to remember
     * some substitutions.
     * The level of this typeVar identifies how many let-bindings deep it was
     * declared in. This is used to prevent generalization of typevars that
     * escape outside the current let-binding scope. */
    | TVar(ref<typeVar>)

    /* 'a -> 'b, all functions are single-argument only */
    /* e.g. \a b c.c  is automatically translated to \a.\b.\c.c */
    /* Currying is also automatic */
    | Fn(typ, typ)

    /* Polytypes in the form  forall 'a 'b ... 'y . 'z */
    /* The typeVar list will be a list of all monomorphic typevars in 'z */
    /* Used only in let-bindings to make the declaration polymorphic */
    | PolyType(list<typeVarId>, typ)

  and typeVar =
    | Bound(typ)
    | Unbound(typeVarId, level)

  let currentLevel: ref<level> = ref(1)
  let currentTypeVar: ref<typeVarId> = ref(0)

  let enterLevel = () => incr(currentLevel)
  let exitLevel = () => decr(currentLevel)

  let newVar = () => {
    currentTypeVar := currentTypeVar.contents + 1
    currentTypeVar.contents
  }

  let newTypeVar = () => TVar(ref(Unbound(newVar(), currentLevel.contents)))

  /* Return the next unique lowercase-letter string after the given one, e.g: */
  /* nextLetter "'a" = "'b"
   *   nextLetter "'b" = "'c"
   *   nextLetter "'z" = "'{"   This can be fixed but most examples shouldn't have > 26 typevars anyway
   *
   */
  let nextLetter = (s: ref<string>) => {
    open Js.String2
    let c = charCodeAt(s.contents, 0)
    s := fromCharCode(Float.toInt(c) + 1)
  }

  /* If this type is the a in a -> b, should it be parenthesized? */
  /* Note this is recursive in case bound typevars are used */
  let rec shouldParenthesize = x =>
    switch x {
    | TVar({contents: Bound(t')}) => shouldParenthesize(t')
    | Fn(_, _) | PolyType(_, _) => true
    | _ => false
    }

  /* pretty printing types */
  let toString = (t: typ): string => {
    /* Keep track of number to character bindings for typevars
     * e.g. '2 => 'a, '5 => 'b, etc.
     * Letters are assigned to typevars by the order in which the typevars
     * appear in the type, left to right */
    let rec toStringRec = (curTypeVarName, typeVarNames, x) =>
      switch x {
      | TUnit => "unit"
      | TVar({contents: Bound(t_)}) => toStringRec(curTypeVarName, typeVarNames, t_)
      | TVar({contents: Unbound(n, _)}) =>
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
        let curriedFn = t =>
          toStringRec(curTypeVarName, typeVarNames, TVar(ref(Unbound(t, currentLevel.contents))))
        let tvsStr = List.reduce(tvs, "", (s, tv) => s ++ (" '" ++ curriedFn(tv)))
        "forall" ++ (tvsStr ++ (" . " ++ toStringRec(curTypeVarName, typeVarNames, t)))
      }

    toStringRec(ref("a"), HashMap.Int.make(~hintSize=1), t)
  }

  let printType = (t: typ): unit => Js.log(toString(t))

  type tenv = Map.String.t<typ>

  /* specializes the polytype s by copying the term and replacing the
   * bound type variables consistently by new monotype variables
   * E.g.   inst (forall a b. a -> b -> a) = c -> d -> c */
  let inst = (s: typ): typ => {
    /* Replace any typevars found in the Hashtbl with the
     * associated value in the same table, leave them otherwise */
    let rec replaceTvs = (tbl, x) =>
      switch x {
      | TUnit => TUnit
      | TVar({contents: Bound(t)}) => replaceTvs(tbl, t)
      | TVar({contents: Unbound(n, _level)}) as t =>
        switch tbl->HashMap.Int.get(n) {
        | Some(t_) => t_
        | None => t
        }
      | Fn(a, b) => Fn(replaceTvs(tbl, a), replaceTvs(tbl, b))
      | PolyType(tvs, typ) =>
        let tblCpy = HashMap.Int.copy(tbl)
        List.forEach(tvs, HashMap.Int.remove(tblCpy))
        PolyType(tvs, replaceTvs(tblCpy, typ))
      }

    switch s {
    /* Note that the returned type is no longer a PolyType,
     * this means it is now monomorphic and not forall-quantified */
    | PolyType(typevars, typ) =>
      let tvsToReplace = HashMap.Int.make(~hintSize=1)
      List.forEach(typevars, tv => HashMap.Int.set(tvsToReplace, tv, newTypeVar()))
      replaceTvs(tvsToReplace, typ)
    | other => other
    }
  }

  /* The find for our union-find like algorithm */
  /* Go through the given type, replacing all typevars with their bound types when possible */

  /* Can a monomorphic TVar(a) be found inside this type? */
  let rec occurs = (aId: typeVarId, aLevel: level, x: typ) =>
    /* in */
    switch x {
    | TUnit => false
    | TVar({contents: Bound(t)}) => occurs(aId, aLevel, t)
    | TVar({contents: Unbound(bId, bLevel)} as bTypevar) =>
      let minLevel = min(aLevel, bLevel)
      bTypevar := Unbound(bId, minLevel)
      aId == bId

    | Fn(b, c) => occurs(aId, aLevel, b) || occurs(aId, aLevel, c)
    | PolyType(tvs, t) =>
      if List.has(tvs, aId, (tv, aId) => aId == tv) {
        false
      } else {
        occurs(aId, aLevel, t)
      }
    }

  let rec unify = (t1: typ, t2: typ): unit =>
    switch (t1, t2) {
    | (TUnit, TUnit) => ()

    /* These two recursive calls to the bound typeVar replace
     * the 'find' in the union-find algorithm */
    | (TVar({contents: Bound(a')}), b) => unify(a', b)
    | (a, TVar({contents: Bound(b')})) => unify(a, b')

    | (TVar({contents: Unbound(aId, aLevel)} as a), b) =>
      /* create binding for boundTy that is currently empty */
      if t1 == t2 {
        ()
      } else if (
        /* a = a, but dont create a recursive binding to itself */
        occurs(aId, aLevel, b)
      ) {
        raise(TypeError)
      } else {
        a := Bound(b)
      }

    | (a, TVar({contents: Unbound(bId, bLevel)} as b)) =>
      /* create binding for boundTy that is currently empty */
      if t1 == t2 {
        ()
      } else if occurs(bId, bLevel, a) {
        raise(TypeError)
      } else {
        b := Bound(a)
      }

    | (Fn(a, b), Fn(c, d)) =>
      unify(a, c)
      unify(b, d)

    | (PolyType(_a, t), PolyType(_b, u)) =>
      /* NOTE: Unneeded rule, never used due to [Var] and inst */
      unify(t, u)

    | (_a, _b) => raise(TypeError)
    }

  /* Find all typevars and wrap the type in a PolyType */
  /* e.g.  generalize (a -> b -> b) = forall a b. a -> b -> b */
  let generalize = (t: typ): typ => {
    /* collect all the monomorphic typevars */
    let rec findAllTvs = x =>
      switch x {
      | TUnit => list{}
      | TVar({contents: Bound(t)}) => findAllTvs(t)
      | TVar({contents: Unbound(n, level)}) =>
        if level > currentLevel.contents {
          list{n}
        } else {
          list{}
        }
      | Fn(a, b) => List.concat(findAllTvs(a), findAllTvs(b))
      | PolyType(tvs, typ) =>
        /* Remove all of tvs from findAllTvs typ, this could be faster */
        List.keep(findAllTvs(typ), tv => !List.has(tvs, tv, (a, b) => a == b))
      }

    // TODO: Missing uniq for this list
    findAllTvs(t)->List.sort((a, b) => a - b) |> (l => PolyType(l, t))
  }

  /* The main entry point to type inference */
  /* All branches (except for the trivial Unit) of the first match in this function
   are translated directly from the rules for algorithm J, given in comments */
  /* infer : typ SMap.t -> Expr -> Type */
  let rec infer = (env): (expr => typ) =>
    x =>
      switch x {
      | Unit => TUnit

      /* Var
       *   x : s ∊ env
       *   t = inst s
       *   -----------
       *   infer env x = t
       */
      | Identifier(x) =>
        let s = Map.String.getExn(env, x)
        let t = inst(s)
        t

      /* App
       *   infer env f = t0
       *   infer env x = t1
       *   t' = newVar ()
       *   unify t0 (t1 -> t')
       *   ---------------
       *   infer env (f x) = t'
       */
      | FnCall(f, x) =>
        let t0 = infer(env, f)
        let t1 = infer(env, x)
        let t' = newTypeVar()
        unify(t0, Fn(t1, t'))
        t'

      /* Abs
       *   t = newVar ()
       *   infer (SMap.add x t env) e = t'
       *   -------------
       *   infer env (fun x -> e) = t -> t'
       */
      | Lambda(x, e) =>
        let t = newTypeVar()
        let t' = infer(Map.String.set(env, x, t), e)
        Fn(t, t')

      /* Let
       *   infer env e0 = t
       *   infer (SMap.add x (generalize t) env) e1 = t'
       *   -----------------
       *   infer env (let x = e0 in e1) = t'
       *
       * enter/exitLevel optimizations are from
       * http://okmij.org/ftp/ML/generalization.html
       * In this implementation, they're required so we
       * don't generalize types that escape into the environment.
       */
      | Let(x, e0, e1) =>
        enterLevel()
        let t = infer(env, e0)
        exitLevel()
        let t' = infer(Map.String.set(env, x, generalize(t)), e1)
        t'
      }
}

/* *****************************************************************************
                       front-end for parsing exprs on
                        the command line and showing
                             their computed types
******************************************************************************/

/* The classic read-eval-printline-loop */
/* let rec main = () => { */
/* try { */
/* Js.log("> ") */
/* read_line() |> Lexer.parse |> infer(SMap.empty) |> printType */
/* Js.log("\n") */
/* } catch { */
/* | TypeError => Js.log("type error") */
/* | Not_found => Js.log("variable not found") */
/* | Failure(s) => Js.log("lexing failure, invalid symbol") */
/* } */
/* currentTypeVar := 0 */
/* main() */
/* } */
/* let () = main() */

let rec exprToString = (ast: expr) => {
  switch ast {
  | Unit => "()"
  | Identifier(x) => x
  | Lambda(param, expr) => `λ${param}. ${exprToString(expr)}`
  | FnCall(callee, arg) => `(${exprToString(callee)} ${exprToString(arg)})`
  | Let(binding, value, body) => `let ${binding} = ${exprToString(value)} in ${exprToString(body)}`
  }
}

let test = (name: string, ast: expr, expected: string) => {
  try {
    let typ = Type.infer(Map.String.empty, ast)
    let correct = expected == Type.toString(typ)
    Js.log(`  ${correct ? "" : name} ${correct ? "" : ":"} ${correct ? "" : Type.toString(typ)}`)
    Js.log(`${correct ? " " : "x"} ${name} : ${Type.toString(typ)}`)
    Js.log(`  ${name} : ${exprToString(ast)}`)
  } catch {
  | TypeError => Js.log("type error")
  }
  Type.currentTypeVar := 0
}

/*
type rec expr =
  | Unit
  | Identifier(string)
  | Lambda(string, expr)
  | FnCall(expr, expr)
  | Let(string, expr, expr)
*/

/* Tests */

// 1:    \f.\x. f x  :  (a -> b) -> a -> b
test("1", Lambda("f", Lambda("x", FnCall(Identifier("f"), Identifier("x")))), "(a -> b) -> a -> b")

// 2:    \f.\x. f (f x) : (a -> a) -> a -> a
test(
  "2",
  Lambda("f", Lambda("x", FnCall(Identifier("f"), FnCall(Identifier("f"), Identifier("x"))))),
  "(a -> a) -> a -> a",
)

// (+):  \m.\n.\f.\x. m f (n f x)  :  (a -> b -> c) -> (a -> d -> b) -> a -> d -> c
test(
  "(+)",
  Lambda(
    "m",
    Lambda(
      "n",
      Lambda(
        "f",
        Lambda(
          "x",
          FnCall(
            FnCall(Identifier("m"), Identifier("f")),
            FnCall(FnCall(Identifier("n"), Identifier("f")), Identifier("x")),
          ),
        ),
      ),
    ),
  ),
  "(a -> b -> c) -> (a -> d -> b) -> a -> d -> c",
)

// succ: \n.\f.\x. f (n f x)  :  ((a -> b) -> c -> a) -> (a -> b) -> c -> b
test(
  "succ",
  Lambda(
    "n",
    Lambda(
      "f",
      Lambda(
        "x",
        FnCall(Identifier("f"), FnCall(FnCall(Identifier("n"), Identifier("f")), Identifier("x"))),
      ),
    ),
  ),
  "((a -> b) -> c -> a) -> (a -> b) -> c -> b",
)

// mult: \m.\n.\f.\x. m (n f) x  :  (a -> b -> c) -> (d -> a) -> d -> b -> c
test(
  "mult",
  Lambda(
    "m",
    Lambda(
      "n",
      Lambda(
        "f",
        Lambda(
          "x",
          FnCall(
            FnCall(Identifier("m"), FnCall(Identifier("n"), Identifier("f"))),
            Identifier("x"),
          ),
        ),
      ),
    ),
  ),
  "(a -> b -> c) -> (d -> a) -> d -> b -> c",
)

// pred: \n.\f.\x. n (\g.\h. h (g f)) (\u.x) (\u.u)  :  (((a -> b) -> (b -> c) -> c) -> (d -> e) -> (f -> f) -> g) -> a -> e -> g
test(
  "pred",
  Lambda(
    "n",
    Lambda(
      "f",
      Lambda(
        "x",
        FnCall(
          FnCall(
            FnCall(
              Identifier("n"),
              Lambda(
                "g",
                Lambda("h", FnCall(Identifier("h"), FnCall(Identifier("g"), Identifier("f")))),
              ),
            ),
            Lambda("u", Identifier("x")),
          ),
          Lambda("u", Identifier("u")),
        ),
      ),
    ),
  ),
  "(((a -> b) -> (b -> c) -> c) -> (d -> e) -> (f -> f) -> g) -> a -> e -> g",
)

/* Let generalization tests */

// \x. let y = x in y      : 'a -> 'a
test("let1", Lambda("x", Let("y", Identifier("x"), Identifier("y"))), "a -> a")

// \x. let y = \z.x in y   : 'a -> 'b -> 'a
test("let2", Lambda("x", Let("y", Lambda("z", Identifier("x")), Identifier("y"))), "a -> b -> a")
