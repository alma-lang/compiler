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

open Type

module UnifyError = {
  type t =
    | TypeMismatch
    | InfiniteType
}
exception TypeError(UnifyError.t)

module InferError = {
  type t =
    | UndefinedIdentifier(Node.t<Ast.Expression.t>)
    | TypeMismatch(Node.t<Ast.Expression.t>, Type.typ, option<Node.t<Ast.Expression.t>>, Type.typ)
    | InfiniteType(Node.t<Ast.Expression.t>, Type.typ)

  let toString = (e: t, input: string): string => {
    let prefix = switch e {
    | UndefinedIdentifier({line, column}) => j`$line:$column`
    | TypeMismatch({line, column}, _, _, _) => j`$line:$column`
    | InfiniteType({line, column}, _) => j`$line:$column`
    }

    let (position, lineNumber, columnNumber) = switch e {
    | UndefinedIdentifier({line, column, start}) => (start, line, column)
    | TypeMismatch({line, column, start}, _, _, _) => (start, line, column)
    | InfiniteType({line, column, start}, _) => (start, line, column)
    }

    let code =
      Input.linesReportAtPositionWithPointer(
        input,
        ~position,
        ~lineNumber,
        ~columnNumber,
      )->Option.getWithDefault("")

    j`$prefix: ` ++
    switch e {
    | UndefinedIdentifier(expr) => `Undefined identifier ${Ast.Expression.toString(expr.value)}`
    | TypeMismatch(_expr, typ, None, typ2) => {
        let t1 = Type.toString(typ)
        let t2 = Type.toString(typ2)

        `Type mismatch:  ${t1}  ≠  ${t2}

Expected

${code}

to be

${t2}

but seems to be

${t1}`
      }
    | TypeMismatch(_expr, typ, Some(expr2), typ2) => {
        let code2 =
          Input.linesReportAtPositionWithPointer(
            input,
            ~position=expr2.start,
            ~lineNumber=expr2.line,
            ~columnNumber=expr2.column,
          )->Option.getWithDefault("")
        let t1 = Type.toString(typ)
        let t2 = Type.toString(typ2)

        `Type mismatch:  ${t1}  ≠  ${t2}

Expected

${code}

with type

${t1}

to have the same type as

${code2}

with type

${t2}`
      }
    | InfiniteType(_expr, _typ) => `Infinite type\n\n${code}`
    }
  }

  let toStringMany = (errors: array<t>, input): string =>
    Array.joinWith(errors, "\n\n----------------------------------------\n\n", error =>
      toString(error, input)
    )
}

module State = {
  type t = {
    currentLevel: ref<Type.level>,
    currentTypeVar: ref<Type.typeVarId>,
  }

  let empty = () => {
    currentLevel: ref(1),
    currentTypeVar: ref(0),
  }

  let enterLevel = state => incr(state.currentLevel)
  let exitLevel = state => decr(state.currentLevel)

  let newVar = state => {
    state.currentTypeVar := state.currentTypeVar.contents + 1
    state.currentTypeVar.contents
  }

  let newTypeVar = state => Var(ref(Unbound(newVar(state), state.currentLevel.contents)))
}

/* specializes the polytype s by copying the term and replacing the
 * bound type variables consistently by new monotype variables
 * E.g.   inst (forall a b. a -> b -> a) = c -> d -> c */
let inst = (s: Type.typ, state: State.t): Type.typ => {
  /* Replace any typevars found in the Hashtbl with the
   * associated value in the same table, leave them otherwise */
  let rec replaceTvs = (tbl, x) =>
    switch x {
    | Unit => Unit
    | Named(_) => x
    | Var({contents: Bound(t)}) => replaceTvs(tbl, t)
    | Var({contents: Unbound(n, _level)}) as t =>
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
    List.forEach(typevars, tv => HashMap.Int.set(tvsToReplace, tv, State.newTypeVar(state)))
    replaceTvs(tvsToReplace, typ)
  | other => other
  }
}

/* The find for our union-find like algorithm */
/* Go through the given type, replacing all typevars with their bound types when possible */

/* Can a monomorphic Var(a) be found inside this type? */
let rec occurs = (aId: typeVarId, aLevel: level, x: Type.typ) =>
  /* in */
  switch x {
  | Unit => false
  | Named(_) => false
  | Var({contents: Bound(t)}) => occurs(aId, aLevel, t)
  | Var({contents: Unbound(bId, bLevel)} as bTypevar) =>
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

let unify = (
  ast: Node.t<Ast.Expression.t>,
  t1: Type.typ,
  ast2: option<Node.t<Ast.Expression.t>>,
  t2: Type.typ,
): result<unit, InferError.t> => {
  let rec unifyExn = (t1: Type.typ, t2: Type.typ): unit =>
    switch (t1, t2) {
    | (Unit, Unit) => ()

    | (Named(name, args), Named(name2, args2)) => {
        if name != name2 {
          raise(TypeError(TypeMismatch))
        }
        List.forEach2(args, args2, (a1, a2) => unifyExn(a1, a2))
      }

    /* These two recursive calls to the bound typeVar replace
     * the 'find' in the union-find algorithm */
    | (Var({contents: Bound(a')}), b) => unifyExn(a', b)
    | (a, Var({contents: Bound(b')})) => unifyExn(a, b')

    | (Var({contents: Unbound(aId, aLevel)} as a), b) =>
      /* create binding for boundTy that is currently empty */
      if t1 == t2 {
        ()
      } else if (
        /* a = a, but dont create a recursive binding to itself */
        occurs(aId, aLevel, b)
      ) {
        raise(TypeError(InfiniteType))
      } else {
        a := Bound(b)
      }

    | (a, Var({contents: Unbound(bId, bLevel)} as b)) =>
      /* create binding for boundTy that is currently empty */
      if t1 == t2 {
        ()
      } else if occurs(bId, bLevel, a) {
        raise(TypeError(InfiniteType))
      } else {
        b := Bound(a)
      }

    | (Fn(a, b), Fn(c, d)) =>
      unifyExn(a, c)
      unifyExn(b, d)

    | (PolyType(_a, t), PolyType(_b, u)) =>
      /* NOTE: Unneeded rule, never used due to [Var] and inst */
      unifyExn(t, u)

    | (_a, _b) => raise(TypeError(TypeMismatch))
    }

  try {
    Ok(unifyExn(t1, t2))
  } catch {
  | TypeError(e) =>
    Error(
      switch e {
      | UnifyError.TypeMismatch => InferError.TypeMismatch(ast, t1, ast2, t2)
      | UnifyError.InfiniteType => InferError.InfiniteType(ast, t1)
      },
    )
  }
}

/* Find all typevars and wrap the type in a PolyType */
/* e.g.  generalize (a -> b -> b) = forall a b. a -> b -> b */
let generalize = (t: Type.typ, state: State.t): Type.typ => {
  /* collect all the monomorphic typevars */
  let rec findAllTvs = x =>
    switch x {
    | Unit => list{}
    // TODO: Dumb conversions to list / array here :/
    | Named(_, args) => args->List.map(findAllTvs)->List.toArray->List.concatMany
    | Var({contents: Bound(t)}) => findAllTvs(t)
    | Var({contents: Unbound(n, level)}) =>
      if level > state.currentLevel.contents {
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
let infer = (ast: Node.t<Ast.Expression.t>): result<Type.typ, array<InferError.t>> => {
  let state = State.empty()
  let env = TypeEnv.empty(() => State.newTypeVar(state), t => generalize(t, state))
  let errors: array<InferError.t> = []
  let addError = (r: result<unit, InferError.t>) =>
    switch r {
    | Error(e) => errors->Js.Array2.push(e)->ignore
    | _ => ()
    }

  let rec inferRec = (env: TypeEnv.t, ast: Node.t<Ast.Expression.t>): Type.typ => {
    switch ast.value {
    | Unit => Unit

    | Bool(_) => Type.bool_
    | Float(_) => Type.float_
    | String(_) => Type.string_

    | Unary(op, e) => {
        let t = inferRec(env, e)

        switch op.value {
        | Ast.Not => unify(e, t, None, Type.bool_)
        | Ast.Minus => unify(e, t, None, Type.float_)
        }->addError

        t
      }

    | Binary(left, op, right) => {
        // Convert the binary AST to function calls
        let fnCall = {
          ...ast,
          value: Ast.Expression.FnCall(
            {
              value: Ast.Expression.FnCall(
                {...op, value: Ast.Expression.Identifier(op.value.fn)},
                left,
              ),
              line: left.line,
              column: left.column,
              start: left.start,
              end: op.end,
            },
            right,
          ),
        }

        // Infer the binary op as a function call
        inferRec(env, fnCall)
      }

    | If(condition, then, else_) => {
        /* If
         * infer env condition = t0
         * unify t0 bool
         * infer env then = t1
         * infer env else_ = t2
         * unify t1 t2
         * infer env (if condition then else) = t2
         */

        let t = inferRec(env, condition)
        unify(condition, t, None, Type.bool_)->addError

        let t1 = inferRec(env, then)
        let t2 = inferRec(env, else_)
        unify(then, t1, Some(else_), t2)->addError

        t2
      }

    | Identifier(x) =>
      /*
       * Var
       *   x : s ∊ env
       *   t = inst s
       *   -----------
       *   infer env x = t
       */
      switch TypeEnv.get(env, x) {
      | Some(s) => {
          let t = inst(s, state)
          t
        }
      | None => {
          addError(Error(InferError.UndefinedIdentifier(ast)))
          State.newTypeVar(state)
        }
      }

    | FnCall(f, x) => {
        /* App
         *   infer env f = t0
         *   infer env x = t1
         *   t' = newVar ()
         *   unify t0 (t1 -> t')
         *   ---------------
         *   infer env (f x) = t'
         */
        let t0 = inferRec(env, f)
        let t1 = inferRec(env, x)
        let t' = State.newTypeVar(state)

        unify(f, Fn(t1, t'), None, t0)->addError

        t'
      }

    | Lambda(params, e) => {
        /* Abs
         *   t = newVar ()
         *   infer (SMap.add x t env) e = t'
         *   -------------
         *   infer env (fun x -> e) = t -> t'
         */

        let paramsWithType = params->Array.map(p => (p, State.newTypeVar(state)))

        let env = paramsWithType->Array.reduce(env, (env, (param, paramType)) =>
          switch param {
          | {value: Ast.Pattern.Identifier(x)} => TypeEnv.set(env, x, paramType)
          }
        )

        let returnType = inferRec(env, e)

        paramsWithType->Array.reduceReverse(returnType, (returnType, (_, paramType)) => Fn(
          paramType,
          returnType,
        ))
      }

    | Let(p, e0, e1) => {
        /* Let
         *   infer env e0 = t
         *   infer (SMap.add x (generalize t) env) e1 = t'
         *   -----------------
         *   infer env (let x = e0 in e1) = t'
         *
         * enter/exitLevel optimizations are from
         * http://okmij.org/ftp/ML/generalization.html
         * In this implementation, they're required so we don't generalize types
         * that escape into the environment.
         */

        State.enterLevel(state)
        let t = inferRec(env, e0)
        State.exitLevel(state)

        let env = switch p.value {
        | Ast.Pattern.Identifier(x) => TypeEnv.set(env, x, generalize(t, state))
        }
        let t' = inferRec(env, e1)
        t'
      }
    }
  }

  let t = inferRec(env, ast)

  switch errors->Array.length {
  | 0 => Ok(t)
  | _ => Error(errors)
  }
}
