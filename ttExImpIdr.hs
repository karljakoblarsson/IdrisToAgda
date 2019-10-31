
{- data N = Zero | Suc N -}
(Main.N,(TyDecl (TCon {nt_tag = 8, nt_arity = 0}) (TType ./exImpIdr.idr.u),RigW,False,public export,not yet checked for totality,EmptyMI))

{- add : N -> N -> N -}
{- add Zero s = s -}
{- add (Suc a) b = add a (Suc b) -}
(Main.add,(CaseOp (CaseInfo {case_inlinable = False, case_alwaysinline = False, tc_dictionary = False}) (Bind __pi_arg (Pi {binderCount = RigW, binderImpl = Nothing, binderTy = P (TCon {nt_tag = 8, nt_arity = 0}) Main.N Erased, binderKind = TType ./exImpIdr.idr.z}) (Bind __pi_arg1 (Pi {binderCount = RigW, binderImpl = Nothing, binderTy = P (TCon {nt_tag = 8, nt_arity = 0}) Main.N Erased, binderKind = TType ./exImpIdr.idr.a1}) (P (TCon {nt_tag = 8, nt_arity = 0}) Main.N Erased))) [(P (TCon {nt_tag = 8, nt_arity = 0}) Main.N Erased,True),(P (TCon {nt_tag = 8, nt_arity = 0}) Main.N Erased,True)] [] [] (CaseDefs {cases_compiletime = ([{e_0},{e_1}],case {e_0} of
    Main.Suc({e_2}) => App Complete (App Complete (P Ref Main.add Erased) (P Bound {e_2} Erased)) (App Complete (P (DCon {nt_tag = 1, nt_arity = 1, nt_unique = False}) Main.Suc Erased) (P Bound {e_1} Erased))
    Main.Zero() => P Bound {e_1} Erased), cases_runtime = ([{e_0},{e_1}],case {e_0} of
    Main.Suc({e_2}) => App Complete (App Complete (P Ref Main.add Erased) (P Bound {e_2} Erased)) (App Complete (P (DCon {nt_tag = 1, nt_arity = 1, nt_unique = False}) Main.Suc Erased) (P Bound {e_1} Erased))
    Main.Zero() => P Bound {e_1} Erased)}),RigW,False,public export,Total,EmptyMI))

{- data Vec : (a : Type) -> N -> Type where -}
{-   Nil : Vec a Zero -}
{-   Cons : {n : N} -> a -> Vec a n -> Vec a (Suc n) -}
(Main.Vec,(TyDecl (TCon {nt_tag = 11, nt_arity = 2}) (Bind a (Pi {binderCount = RigW, binderImpl = Nothing, binderTy = TType ./exImpIdr.idr.h1, binderKind = TType ./exImpIdr.idr.j1}) (Bind __pi_arg (Pi {binderCount = RigW, binderImpl = Nothing, binderTy = P (TCon {nt_tag = 8, nt_arity = 0}) Main.N Erased, binderKind = TType ./exImpIdr.idr.k1}) (TType ./exImpIdr.idr.l1))),RigW,False,public export,not yet checked for totality,EmptyMI))



{- ccExp : {g : Type} -> {a : N} -> {b : N} -> Vec g a -> Vec g b -> Vec g (add a b) -}
TyDecl
  Ref -- NameType
  (Bind g -- Name
    (Pi { binderCount = RigW, -- A function binding
      binderImpl = Just (Impl { tcimplementation = False,
        toplevel_imp = True,

        machine_gen = False}),
      binderTy = TType ./exImpIdr.idr.b4, -- Term
      binderKind = TType ./exImpIdr.idr.d4 }) -- Binder Term
    (Bind a -- Name -- Recursive what it is bound to
      (Pi { binderCount = RigW,
        binderImpl = Just (Impl {tcimplementation = False,
          toplevel_imp = True, machine_gen = False}),
        binderTy = P (TCon {nt_tag = 8, nt_arity = 0}) Main.N Erased,
        binderKind = TType ./exImpIdr.idr.e4 })
      (Bind b
        (Pi {binderCount = RigW,
          binderImpl = Just (Impl {tcimplementation = False,
            toplevel_imp = True, machine_gen = False}),
          binderTy = P (TCon {nt_tag = 8, nt_arity = 0}) Main.N Erased,
          binderKind = TType ./exImpIdr.idr.f4})
        (Bind __pi_arg -- Vec g a
          (Pi {binderCount = RigW, binderImpl = Nothing,
            binderTy = App Complete (
              App Complete
                (P (TCon {nt_tag = 11, nt_arity = 2}) Main.Vec Erased)
                (V 2))
              (V 1),
            binderKind = TType ./exImpIdr.idr.g4})
          (Bind __pi_arg4 -- Vec g b
            (Pi {binderCount = RigW, binderImpl = Nothing,
              binderTy = App Complete (
                App Complete
                  (P (TCon {nt_tag = 11, nt_arity = 2}) Main.Vec Erased)
                  (V 3))
                (V 1),
              binderKind = TType ./exImpIdr.idr.h4})
            (App Complete -- Result: Vec g (add a b)
              (App Complete
                (P (TCon {nt_tag = 11, nt_arity = 2}) Main.Vec Erased)
                (V 4))
              (App Complete -- add a b
                (App Complete
                  (P Ref Main.add Erased)
                  (V 3))
                (V 2)
              )
            )
          )
        )
      )
    ) -- Term
  ), -- TT Name/Term

(TyDecl ...,  RigW,False,public export,not yet checked for totality,EmptyMI)


{- ccImp : Vec g a -> Vec g b -> Vec g (add a b) -}
V 0 = 
V 1 = 
V 2 = g
V 3 = a
V 4 = b
TyDecl
  Ref
  (Bind b
    (Pi {binderCount = RigW,
      binderImpl = Just (Impl { tcimplementation = False, -- ImplicitInfo
          toplevel_imp = True, -- True if the binding was a scoped implicit in
                               -- high level Idris.
          machine_gen = False }),
      binderTy = P (TCon {nt_tag = 8, nt_arity = 0}) Main.N Erased,
      binderKind = TType ./exImpIdr.idr.p4})
    (Bind a
      (Pi {binderCount = RigW,
        binderImpl = Just (Impl {tcimplementation = False,
          toplevel_imp = True, machine_gen = False}),
        binderTy = P (TCon {nt_tag = 8, nt_arity = 0}) Main.N Erased,
        binderKind = TType ./exImpIdr.idr.q4})
      (Bind g
        (Pi {binderCount = RigW,
          binderImpl = Just (Impl {tcimplementation = False,
            toplevel_imp = True, machine_gen = False}),
          binderTy = TType ./exImpIdr.idr.r4,
          binderKind = TType ./exImpIdr.idr.t4})
        (Bind __pi_arg
          (Pi {binderCount = RigW, binderImpl = Nothing,
            binderTy = App Complete
              (App Complete
                (P (TCon {nt_tag = 11, nt_arity = 2}) Main.Vec Erased)
                (V 0))
              (V 1),
            binderKind = TType ./exImpIdr.idr.u4})
          (Bind __pi_arg4
            (Pi {binderCount = RigW, binderImpl = Nothing,
              binderTy = App Complete
                (App Complete
                  (P (TCon {nt_tag = 11, nt_arity = 2}) Main.Vec Erased)
                  (V 1))
                (V 3),
              binderKind = TType ./exImpIdr.idr.v4})
            (App Complete
              (App Complete
                (P (TCon {nt_tag = 11, nt_arity = 2}) Main.Vec Erased)
                (V 2))
              (App Complete
                (App Complete
                  (P Ref Main.add Erased)
                  (V 3))
                (V 4))
            )
          )
        )
      )
    )
  )

(TyDecl ... ,RigW,False,public export,not yet checked for totality,EmptyMI)
