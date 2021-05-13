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

exception TypeError

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

  let newTypeVar = state => TVar(ref(Unbound(newVar(state), state.currentLevel.contents)))
}

/* specializes the polytype s by copying the term and replacing the
 * bound type variables consistently by new monotype variables
 * E.g.   inst (forall a b. a -> b -> a) = c -> d -> c */
let inst = (s: typ, state: State.t): typ => {
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
    List.forEach(typevars, tv => HashMap.Int.set(tvsToReplace, tv, State.newTypeVar(state)))
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
let generalize = (t: typ, state: State.t): typ => {
  /* collect all the monomorphic typevars */
  let rec findAllTvs = x =>
    switch x {
    | TUnit => list{}
    | TVar({contents: Bound(t)}) => findAllTvs(t)
    | TVar({contents: Unbound(n, level)}) =>
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
/* infer : typ SMap.t -> Expr -> Type */
let infer = (x: Ast.expr, env: TypeEnv.t): typ => {
  let state = State.empty()

  let rec inferRec = (env: TypeEnv.t, x: Ast.expr): typ => {
    switch x {
    | Unit => TUnit

    /* Var
     *   x : s ∊ env
     *   t = inst s
     *   -----------
     *   infer env x = t
     */
    | Identifier(x) =>
      let s = TypeEnv.getExn(env, x)
      let t = inst(s, state)
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
      let t0 = inferRec(env, f)
      let t1 = inferRec(env, x)
      let t' = State.newTypeVar(state)
      unify(t0, Fn(t1, t'))
      t'

    /* Abs
     *   t = newVar ()
     *   infer (SMap.add x t env) e = t'
     *   -------------
     *   infer env (fun x -> e) = t -> t'
     */
    | Lambda(x, e) =>
      let t = State.newTypeVar(state)
      let t' = inferRec(Map.String.set(env, x, t), e)
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
      State.enterLevel(state)
      let t = inferRec(env, e0)
      State.exitLevel(state)
      let t' = inferRec(Map.String.set(env, x, generalize(t, state)), e1)
      t'
    }
  }

  inferRec(env, x)
}
