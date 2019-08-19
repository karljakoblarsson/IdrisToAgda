{- data N : Set where -}
{-   Z : N -}
{-   suc : N -> N -}
Data FC,15 Inductive N [] (RawApp FC [Set FC]) [TypeSig (ArgInfo {argInfoHiding = NotHidden, argInfoModality = Modality {modRelevance = Relevant, modQuantity = Quantityω}, argInfoOrigin = UserWritten, argInfoFreeVariables = UnknownFVs}) Z (Generalized (RawApp FC [Ident N])),TypeSig (ArgInfo {argInfoHiding = NotHidden, argInfoModality = Modality {modRelevance = Relevant, modQuantity = Quantityω}, argInfoOrigin = UserWritten, argInfoFreeVariables = UnknownFVs}) suc (Generalized (Fun FC (Arg {argInfo = ArgInfo {argInfoHiding = NotHidden, argInfoModality = Modality {modRelevance = Relevant, modQuantity = Quantityω}, argInfoOrigin = UserWritten, argInfoFreeVariables = UnknownFVs}, unArg = RawApp FC [Ident N]}) (RawApp FC [Ident N])))]

{- one : N -}
TypeSig (ArgInfo {argInfoHiding = NotHidden, argInfoModality = Modality {modRelevance = Relevant, modQuantity = Quantityω}, argInfoOrigin = UserWritten, argInfoFreeVariables = UnknownFVs}) one (Generalized (RawApp FC [Ident N]))

{- one = suc Z -}
FunClause (LHS {lhsOriginalPattern = RawAppP FC [IdentP one], lhsRewriteEqn = [], lhsWithExpr = []}) (RHS (RawApp FC [Ident suc,Ident Z])) NoWhere False

{- addOne : N -> N -}
TypeSig (ArgInfo {argInfoHiding = NotHidden, argInfoModality = Modality {modRelevance = Relevant, modQuantity = Quantityω}, argInfoOrigin = UserWritten, argInfoFreeVariables = UnknownFVs}) addOne (Generalized (Fun FC (Arg {argInfo = ArgInfo {argInfoHiding = NotHidden, argInfoModality = Modality {modRelevance = Relevant, modQuantity = Quantityω}, argInfoOrigin = UserWritten, argInfoFreeVariables = UnknownFVs}, unArg = RawApp FC [Ident N]}) (RawApp FC [Ident N])))

{- addOne Z = suc Z -}
FunClause (LHS {lhsOriginalPattern = RawAppP FC [IdentP addOne,IdentP Z], lhsRewriteEqn = [], lhsWithExpr = []}) (RHS (RawApp FC [Ident suc,Ident Z])) NoWhere False

{- addOne (suc a) = suc (suc a) -}
FunClause (LHS {lhsOriginalPattern = RawAppP FC [IdentP addOne,ParenP FC (RawAppP FC [IdentP suc,IdentP a])], lhsRewriteEqn = [], lhsWithExpr = []}) (RHS (RawApp FC [Ident suc,Paren FC (RawApp FC [Ident suc,Ident a])])) NoWhere False

{- add : N -> N -> N -}
TypeSig (ArgInfo {argInfoHiding = NotHidden, argInfoModality = Modality {modRelevance = Relevant, modQuantity = Quantityω}, argInfoOrigin = UserWritten, argInfoFreeVariables = UnknownFVs}) add (Generalized (Fun FC (Arg {argInfo = ArgInfo {argInfoHiding = NotHidden, argInfoModality = Modality {modRelevance = Relevant, modQuantity = Quantityω}, argInfoOrigin = UserWritten, argInfoFreeVariables = UnknownFVs}, unArg = RawApp FC [Ident N]}) (Fun FC (Arg {argInfo = ArgInfo {argInfoHiding = NotHidden, argInfoModality = Modality {modRelevance = Relevant, modQuantity = Quantityω}, argInfoOrigin = UserWritten, argInfoFreeVariables = UnknownFVs}, unArg = RawApp FC [Ident N]}) (RawApp FC [Ident N]))))

{- add Z s = s -}
FunClause (LHS {lhsOriginalPattern = RawAppP FC [IdentP add,IdentP Z,IdentP s], lhsRewriteEqn = [], lhsWithExpr = []}) (RHS (RawApp FC [Ident s])) NoWhere False

{- add (suc a) b = add a (suc b) -}
FunClause (LHS {lhsOriginalPattern = RawAppP FC [IdentP add,ParenP FC (RawAppP FC [IdentP suc,IdentP a]),IdentP b], lhsRewriteEqn = [], lhsWithExpr = []}) (RHS (RawApp FC [Ident add,Ident a,Paren FC (RawApp FC [Ident suc,Ident b])])) NoWhere False

{- data Vec (A : Set) : N -> Set where -}
{-   Nil : Vec A Z  -}
{-   _::_ : {n : N} -> A -> Vec A n -> Vec A (suc n) -}
Data FC,50 Inductive Vec [DomainFull (TBind FC [Arg {argInfo = ArgInfo {argInfoHiding = NotHidden, argInfoModality = Modality {modRelevance = Relevant, modQuantity = Quantityω}, argInfoOrigin = UserWritten, argInfoFreeVariables = UnknownFVs}, unArg = Named {nameOf = Nothing, namedThing = BName {boundName = A, bnameFixity = Fixity' {theFixity = Fixity {fixityRange = , fixityLevel = Unrelated, fixityAssoc = NonAssoc}, theNotation = [], theNameRange = }}}}] (RawApp FC [Set FC]))] (Fun FC (Arg {argInfo = ArgInfo {argInfoHiding = NotHidden, argInfoModality = Modality {modRelevance = Relevant, modQuantity = Quantityω}, argInfoOrigin = UserWritten, argInfoFreeVariables = UnknownFVs}, unArg = RawApp FC [Ident N]}) (RawApp FC [Set FC])) [TypeSig (ArgInfo {argInfoHiding = NotHidden, argInfoModality = Modality {modRelevance = Relevant, modQuantity = Quantityω}, argInfoOrigin = UserWritten, argInfoFreeVariables = UnknownFVs}) Nil (Generalized (RawApp FC [Ident Vec,Ident A,Ident Z])),
  TypeSig (ArgInfo { argInfoHiding = NotHidden,
                     argInfoModality = Modality {modRelevance = Relevant, modQuantity = Quantityω},
                     argInfoOrigin = UserWritten,
                     argInfoFreeVariables = UnknownFVs})
           _::_ (Generalized
              (Pi
                [TBind -- {n : N} -> A
                  FC
                  [ Arg {
                      argInfo = ArgInfo {
                          argInfoHiding = Hidden,
                          argInfoModality = Modality {modRelevance = Relevant, modQuantity = Quantityω},
                          argInfoOrigin = UserWritten,
                          argInfoFreeVariables = UnknownFVs},
                      unArg = Named {
                          nameOf = Nothing,
                          namedThing = BName {
                              boundName = n,
                              bnameFixity = Fixity' {
                                  theFixity = Fixity {
                                     fixityRange = ,
                                     fixityLevel = Unrelated,
                                     fixityAssoc = NonAssoc},
                                  theNotation = [],
                                  theNameRange = } } } }]
                  (RawApp FC [Ident N]) ]
                (Fun FC
                  (Arg {
                      argInfo = ArgInfo {
                         argInfoHiding = NotHidden,
                         argInfoModality = Modality {modRelevance = Relevant, modQuantity = Quantityω},
                         argInfoOrigin = UserWritten,
                         argInfoFreeVariables = UnknownFVs},
                      unArg = RawApp FC [Ident A]})
                  (Fun FC
                    (Arg {
                        argInfo = ArgInfo {
                            argInfoHiding = NotHidden,
                            argInfoModality = Modality {modRelevance = Relevant, modQuantity = Quantityω},
                            argInfoOrigin = UserWritten,
                            argInfoFreeVariables = UnknownFVs},
                        unArg = RawApp FC [Ident Vec, Ident A, Ident n]})
                     (RawApp FC [Ident Vec, Ident A, Paren FC (RawApp FC [Ident suc, Ident n])])
                  ))))]

{- empt : Vec N Z -}
TypeSig (ArgInfo {argInfoHiding = NotHidden, argInfoModality = Modality {modRelevance = Relevant, modQuantity = Quantityω}, argInfoOrigin = UserWritten, argInfoFreeVariables = UnknownFVs}) empt (Generalized (RawApp FC [Ident Vec,Ident N,Ident Z]))

{- empt = Nil -}
FunClause (LHS {lhsOriginalPattern = RawAppP FC [IdentP empt], lhsRewriteEqn = [], lhsWithExpr = []}) (RHS (RawApp FC [Ident Nil])) NoWhere False

{- open import Agda.Builtin.Nat -}
Import FC Agda.Builtin.Nat Nothing DoOpen (ImportDirective {importDirRange = , using = UseEverything, hiding = [], impRenaming = [], publicOpen = False})

{- test : Vec Nat (suc (suc Z)) -}
TypeSig (ArgInfo {argInfoHiding = NotHidden, argInfoModality = Modality {modRelevance = Relevant, modQuantity = Quantityω}, argInfoOrigin = UserWritten, argInfoFreeVariables = UnknownFVs}) test (Generalized (RawApp FC [Ident Vec,Ident Nat,Paren FC (RawApp FC [Ident suc,Paren FC (RawApp FC [Ident suc,Ident Z])])]))

{- test = 1 :: (2 :: Nil) -}
FunClause (LHS {lhsOriginalPattern = RawAppP FC [IdentP test], lhsRewriteEqn = [], lhsWithExpr = []}) (RHS (RawApp FC [Lit (LitNat _ 1),Ident ::,Paren FC (RawApp FC [Lit (LitNat _ 2),Ident ::,Ident Nil])])) NoWhere False

{- test = 1 :: (2 :: Nil) -}
TypeSig (ArgInfo {argInfoHiding = NotHidden, argInfoModality = Modality {modRelevance = Relevant, modQuantity = Quantityω}, argInfoOrigin = UserWritten, argInfoFreeVariables = UnknownFVs}) test2 (Generalized (RawApp FC [Ident Vec,Ident Nat,Paren FC (RawApp FC [Ident suc,Paren FC (RawApp FC [Ident suc,Paren FC (RawApp FC [Ident suc,Ident Z])])])]))

{- test2 = 3 :: (4 :: (5 :: Nil)) -}
FunClause (LHS {lhsOriginalPattern = RawAppP FC [IdentP test2], lhsRewriteEqn = [], lhsWithExpr = []}) (RHS (RawApp FC [Lit (LitNat _ 3),Ident ::,Paren FC (RawApp FC [Lit (LitNat _ 4),Ident ::,Paren FC (RawApp FC [Lit (LitNat _ 5),Ident ::,Ident Nil])])])) NoWhere False

{- concat : {a b : N} {g : Set} -> (Vec g a) -> (Vec g b) -> (Vec g (add a b)) -}
TypeSig (ArgInfo {argInfoHiding = NotHidden, argInfoModality = Modality {modRelevance = Relevant, modQuantity = Quantityω}, argInfoOrigin = UserWritten, argInfoFreeVariables = UnknownFVs}) concat (Generalized (Pi [TBind FC [Arg {argInfo = ArgInfo {argInfoHiding = Hidden, argInfoModality = Modality {modRelevance = Relevant, modQuantity = Quantityω}, argInfoOrigin = UserWritten, argInfoFreeVariables = UnknownFVs}, unArg = Named {nameOf = Nothing, namedThing = BName {boundName = a, bnameFixity = Fixity' {theFixity = Fixity {fixityRange = , fixityLevel = Unrelated, fixityAssoc = NonAssoc}, theNotation = [], theNameRange = }}}},Arg {argInfo = ArgInfo {argInfoHiding = Hidden, argInfoModality = Modality {modRelevance = Relevant, modQuantity = Quantityω}, argInfoOrigin = UserWritten, argInfoFreeVariables = UnknownFVs}, unArg = Named {nameOf = Nothing, namedThing = BName {boundName = b, bnameFixity = Fixity' {theFixity = Fixity {fixityRange = , fixityLevel = Unrelated, fixityAssoc = NonAssoc}, theNotation = [], theNameRange = }}}}] (RawApp FC [Ident N]),TBind FC [Arg {argInfo = ArgInfo {argInfoHiding = Hidden, argInfoModality = Modality {modRelevance = Relevant, modQuantity = Quantityω}, argInfoOrigin = UserWritten, argInfoFreeVariables = UnknownFVs}, unArg = Named {nameOf = Nothing, namedThing = BName {boundName = g, bnameFixity = Fixity' {theFixity = Fixity {fixityRange = , fixityLevel = Unrelated, fixityAssoc = NonAssoc}, theNotation = [], theNameRange = }}}}] (RawApp FC [Set FC])] (Fun FC (Arg {argInfo = ArgInfo {argInfoHiding = NotHidden, argInfoModality = Modality {modRelevance = Relevant, modQuantity = Quantityω}, argInfoOrigin = UserWritten, argInfoFreeVariables = UnknownFVs}, unArg = RawApp FC [Paren FC (RawApp FC [Ident Vec,Ident g,Ident a])]}) (Fun FC (Arg {argInfo = ArgInfo {argInfoHiding = NotHidden, argInfoModality = Modality {modRelevance = Relevant, modQuantity = Quantityω}, argInfoOrigin = UserWritten, argInfoFreeVariables = UnknownFVs}, unArg = RawApp FC [Paren FC (RawApp FC [Ident Vec,Ident g,Ident b])]}) (RawApp FC [Paren FC (RawApp FC [Ident Vec,Ident g,Paren FC (RawApp FC [Ident add,Ident a,Ident b])])])))))

{- concat Nil rest = rest -}
FunClause (LHS {lhsOriginalPattern = RawAppP FC [IdentP concat,IdentP Nil,IdentP rest], lhsRewriteEqn = [], lhsWithExpr = []}) (RHS (RawApp FC [Ident rest])) NoWhere False

{- concat (a :: rest) b = concat rest (a :: b) -}
FunClause (LHS {lhsOriginalPattern = RawAppP FC [IdentP concat,ParenP FC (RawAppP FC [IdentP a,IdentP ::,IdentP rest]),IdentP b], lhsRewriteEqn = [], lhsWithExpr = []}) (RHS (RawApp FC [Ident concat,Ident rest,Paren FC (RawApp FC [Ident a,Ident ::,Ident b])])) NoWhere False

{- t3 : Vec Nat (addOne (addOne (addOne (addOne one)))) -}
TypeSig (ArgInfo {argInfoHiding = NotHidden, argInfoModality = Modality {modRelevance = Relevant, modQuantity = Quantityω}, argInfoOrigin = UserWritten, argInfoFreeVariables = UnknownFVs}) t3 (Generalized (RawApp FC [Ident Vec,Ident Nat,Paren FC (RawApp FC [Ident addOne,Paren FC (RawApp FC [Ident addOne,Paren FC (RawApp FC [Ident addOne,Paren FC (RawApp FC [Ident addOne,Ident one])])])])]))

{- t3 = concat test test2 -}
FunClause (LHS {lhsOriginalPattern = RawAppP FC [IdentP t3], lhsRewriteEqn = [], lhsWithExpr = []}) (RHS (RawApp FC [Ident concat,Ident test,Ident test2])) NoWhere False
