{- data N = Z | Suc N -}
PData
  (DocString (Options {sanitize = True, allowRawHtml = False, preserveHardBreaks = True, debug = False}) (fromList []))
  []
  (Syn {using = [], syn_params = [], syn_namespace = [], no_imp = [], imp_methods = [], decoration = <<fn>>, inPattern = False, implicitAllowed = False, constraintAllowed = False, maxline = Nothing, mut_nesting = 0, dsl_info = DSL {dsl_bind = PRef (builtin) [] >>=, dsl_apply = PRef (builtin) [] <*>, dsl_pure = PRef (builtin) [] pure, dsl_var = Nothing, index_first = Nothing, index_next = Nothing, dsl_lambda = Nothing, dsl_let = Nothing, dsl_pi = Nothing}, syn_in_quasiquote = 0, syn_toplevel = True, withAppAllowed = True})
  FC
  []
  (PDatadecl {
    d_name = N,
    d_name_fc = FC,
    d_tcon = PType FC,
    d_cons = [
      (DocString (Options {sanitize = True, allowRawHtml = False, preserveHardBreaks = True, debug = False}) (fromList []),
       [],
       Z,
       FC,
       PRef FC [] N,
       FC,
       []
       ),
      (DocString (Options {sanitize = True, allowRawHtml = False, preserveHardBreaks = True, debug = False}) (fromList []),
       [],
       Suc,
       FC,
       (PPi
         (Exp {pargopts = [], pstatic = Dynamic, pparam = False, pcount = RigW})
         {_t_0}
         No location
         (PRef FC [FC] N)
         (PRef FC [] N)),
       FC,
       []
      )]
    }
  )

{- one : N -}
PTy (DocString (Options {sanitize = True, allowRawHtml = False, preserveHardBreaks = True, debug = False}) (fromList [])) [] (Syn {using = [], syn_params = [], syn_namespace = [], no_imp = [], imp_methods = [], decoration = <<fn>>, inPattern = False, implicitAllowed = False, constraintAllowed = False, maxline = Nothing, mut_nesting = 0, dsl_info = DSL {dsl_bind = PRef (builtin) [] >>=, dsl_apply = PRef (builtin) [] <*>, dsl_pure = PRef (builtin) [] pure, dsl_var = Nothing, index_first = Nothing, index_next = Nothing, dsl_lambda = Nothing, dsl_let = Nothing, dsl_pi = Nothing}, syn_in_quasiquote = 0, syn_toplevel = True, withAppAllowed = True}) FC [] one FC (PRef FC [FC] N)

{- one = Suc Z -}
PClauses FC [] {__2} [PClause FC one (PApp FC (PRef FC [FC] one) []) [] (PApp FC (PRef FC [FC] Suc) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] Z}]) []]

{- one = Suc Z -}
PTy (DocString (Options {sanitize = True, allowRawHtml = False, preserveHardBreaks = True, debug = False}) (fromList [])) [] (Syn {using = [], syn_params = [], syn_namespace = [], no_imp = [], imp_methods = [], decoration = <<fn>>, inPattern = False, implicitAllowed = False, constraintAllowed = False, maxline = Nothing, mut_nesting = 0, dsl_info = DSL {dsl_bind = PRef (builtin) [] >>=, dsl_apply = PRef (builtin) [] <*>, dsl_pure = PRef (builtin) [] pure, dsl_var = Nothing, index_first = Nothing, index_next = Nothing, dsl_lambda = Nothing, dsl_let = Nothing, dsl_pi = Nothing}, syn_in_quasiquote = 0, syn_toplevel = True, withAppAllowed = True}) FC [] addOne FC (PPi (Exp {pargopts = [], pstatic = Dynamic, pparam = False, pcount = RigW}) __pi_arg No location (PRef FC [FC] N) (PRef FC [FC] N))

{- addOne Z = Suc Z -}
PClauses FC [] {__2} [PClause FC addOne (PApp FC (PRef FC [FC] addOne) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] Z}]) [] (PApp FC (PRef FC [FC] Suc) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] Z}]) []]

{- addOne (Suc n) = Suc (Suc n) -}
PClauses FC [] {__2} [PClause FC addOne (PApp FC (PRef FC [FC] addOne) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PApp FC (PRef FC [FC] Suc) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] n}]}]) [] (PApp FC (PRef FC [FC] Suc) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PApp FC (PRef FC [FC] Suc) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] n}]}]) []]

{- add : N -> N -> N -}
PTy (DocString (Options {sanitize = True, allowRawHtml = False, preserveHardBreaks = True, debug = False}) (fromList [])) [] (Syn {using = [], syn_params = [], syn_namespace = [], no_imp = [], imp_methods = [], decoration = <<fn>>, inPattern = False, implicitAllowed = False, constraintAllowed = False, maxline = Nothing, mut_nesting = 0, dsl_info = DSL {dsl_bind = PRef (builtin) [] >>=, dsl_apply = PRef (builtin) [] <*>, dsl_pure = PRef (builtin) [] pure, dsl_var = Nothing, index_first = Nothing, index_next = Nothing, dsl_lambda = Nothing, dsl_let = Nothing, dsl_pi = Nothing}, syn_in_quasiquote = 0, syn_toplevel = True, withAppAllowed = True}) FC [] add FC (PPi (Exp {pargopts = [], pstatic = Dynamic, pparam = False, pcount = RigW}) __pi_arg No location (PRef FC [FC] N) (PPi (Exp {pargopts = [], pstatic = Dynamic, pparam = False, pcount = RigW}) __pi_arg No location (PRef FC [FC] N) (PRef FC [FC] N)))

{- add Z s = s -}
PClauses FC [] {__2} [PClause FC add (PApp FC (PRef FC [FC] add) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] Z},PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] s}]) [] (PRef FC [FC] s) []]

{- add (Suc a) b = add a (Suc b) -}
PClauses FC [] {__2} [PClause FC add (PApp FC (PRef FC [FC] add) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PApp FC (PRef FC [FC] Suc) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] a}]},PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] b}]) [] (PApp FC (PRef FC [FC] add) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] a},PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PApp FC (PRef FC [FC] Suc) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] b}]}]) []]

{- infixr 10 :: -}
PFix FC infixr 10 ["::"]

{- data Vec : N -> Type -> Type where -}
{-   Nil : Vec Z a -}
{-   (::) : a -> Vec n a -> Vec (Suc n) a -}
PData (DocString (Options {sanitize = True, allowRawHtml = False, preserveHardBreaks = True, debug = False}) (fromList [])) [] (Syn {using = [], syn_params = [], syn_namespace = [], no_imp = [], imp_methods = [], decoration = <<fn>>, inPattern = False, implicitAllowed = False, constraintAllowed = False, maxline = Nothing, mut_nesting = 0, dsl_info = DSL {dsl_bind = PRef (builtin) [] >>=, dsl_apply = PRef (builtin) [] <*>, dsl_pure = PRef (builtin) [] pure, dsl_var = Nothing, index_first = Nothing, index_next = Nothing, dsl_lambda = Nothing, dsl_let = Nothing, dsl_pi = Nothing}, syn_in_quasiquote = 0, syn_toplevel = True, withAppAllowed = True}) FC [] (PDatadecl {d_name = Vec, d_name_fc = FC,

d_tcon = PPi
    (Exp {pargopts = [], pstatic = Dynamic, pparam = False, pcount = RigW})
    __pi_arg
    No location
    (PRef FC [FC] N)
    (PPi
      (Exp {pargopts = [], pstatic = Dynamic, pparam = False, pcount = RigW})
      __pi_arg
      No location
      (PType FC)
      (PType FC)
    ),

d_cons = [

(DocString (Options {sanitize = True, allowRawHtml = False, preserveHardBreaks = True, debug = False}) (fromList [])
,[]
,Nil
,FC
,PApp FC (PRef FC [FC] Vec) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] Z},PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] a}]
,FC
,[]),

(DocString (Options {sanitize = True, allowRawHtml = False, preserveHardBreaks = True, debug = False}) (fromList []),[],::,FC,PPi (Exp {pargopts = [], pstatic = Dynamic, pparam = False, pcount = RigW}) __pi_arg No location (PRef FC [FC] a) (PPi (Exp {pargopts = [], pstatic = Dynamic, pparam = False, pcount = RigW}) __pi_arg No location (PApp FC (PRef FC [FC] Vec) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] n},PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] a}]) (PApp FC (PRef FC [FC] Vec) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PApp FC (PRef FC [FC] Suc) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] n}]},PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] a}])),FC,[])
]})

{- empt : Vec Z N -}
PTy (DocString (Options {sanitize = True, allowRawHtml = False, preserveHardBreaks = True, debug = False}) (fromList [])) [] (Syn {using = [], syn_params = [], syn_namespace = [], no_imp = [], imp_methods = [], decoration = <<fn>>, inPattern = False, implicitAllowed = False, constraintAllowed = False, maxline = Nothing, mut_nesting = 0, dsl_info = DSL {dsl_bind = PRef (builtin) [] >>=, dsl_apply = PRef (builtin) [] <*>, dsl_pure = PRef (builtin) [] pure, dsl_var = Nothing, index_first = Nothing, index_next = Nothing, dsl_lambda = Nothing, dsl_let = Nothing, dsl_pi = Nothing}, syn_in_quasiquote = 0, syn_toplevel = True, withAppAllowed = True}) FC [] empt FC (PApp FC (PRef FC [FC] Vec) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] Z},PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] N}])

{- empt = Nil -}
PClauses FC [] {__2} [PClause FC empt (PApp FC (PRef FC [FC] empt) []) [] (PRef FC [FC] Nil) []]

{- test : Vec (Suc Main.one) Nat -}
PTy (DocString (Options {sanitize = True, allowRawHtml = False, preserveHardBreaks = True, debug = False}) (fromList [])) [] (Syn {using = [], syn_params = [], syn_namespace = [], no_imp = [], imp_methods = [], decoration = <<fn>>, inPattern = False, implicitAllowed = False, constraintAllowed = False, maxline = Nothing, mut_nesting = 0, dsl_info = DSL {dsl_bind = PRef (builtin) [] >>=, dsl_apply = PRef (builtin) [] <*>, dsl_pure = PRef (builtin) [] pure, dsl_var = Nothing, index_first = Nothing, index_next = Nothing, dsl_lambda = Nothing, dsl_let = Nothing, dsl_pi = Nothing}, syn_in_quasiquote = 0, syn_toplevel = True, withAppAllowed = True}) FC [] test FC (PApp FC (PRef FC [FC] Vec) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PApp FC (PRef FC [FC] Suc) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] Main.one}]},PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] Nat}])

{- test = 1 :: 2 :: Nil -}
PClauses FC [] {__2} [PClause FC test (PApp FC (PRef FC [FC] test) []) [] (PApp FC (PRef FC [] ::) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PConstSugar FC (PAlternative [] FirstSuccess [PApp FC (PRef FC [] fromInteger) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PConstant No location 1}],PConstant FC 1,PConstant FC 1,PConstant FC 1,PConstant FC 1,PConstant FC 1,PConstant FC 1])},PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PApp FC (PRef FC [] ::) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PConstSugar FC (PAlternative [] FirstSuccess [PApp FC (PRef FC [] fromInteger) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PConstant No location 2}],PConstant FC 2,PConstant FC 2,PConstant FC 2,PConstant FC 2,PConstant FC 2,PConstant FC 2])},PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] Nil}]}]) []]

{- test2 : Vec (Suc (Suc Main.one)) Nat -}
PTy (DocString (Options {sanitize = True, allowRawHtml = False, preserveHardBreaks = True, debug = False}) (fromList [])) [] (Syn {using = [], syn_params = [], syn_namespace = [], no_imp = [], imp_methods = [], decoration = <<fn>>, inPattern = False, implicitAllowed = False, constraintAllowed = False, maxline = Nothing, mut_nesting = 0, dsl_info = DSL {dsl_bind = PRef (builtin) [] >>=, dsl_apply = PRef (builtin) [] <*>, dsl_pure = PRef (builtin) [] pure, dsl_var = Nothing, index_first = Nothing, index_next = Nothing, dsl_lambda = Nothing, dsl_let = Nothing, dsl_pi = Nothing}, syn_in_quasiquote = 0, syn_toplevel = True, withAppAllowed = True}) FC [] test2 FC (PApp FC (PRef FC [FC] Vec) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PApp FC (PRef FC [FC] Suc) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PApp FC (PRef FC [FC] Suc) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] Main.one}]}]},PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] Nat}])

{- test2 = 3 :: 4 :: 5 :: Nil -}
PClauses FC [] {__2} [PClause FC test2 (PApp FC (PRef FC [FC] test2) []) [] (PApp FC (PRef FC [] ::) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PConstSugar FC (PAlternative [] FirstSuccess [PApp FC (PRef FC [] fromInteger) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PConstant No location 3}],PConstant FC 3,PConstant FC 3,PConstant FC 3,PConstant FC 3,PConstant FC 3,PConstant FC 3])},PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PApp FC (PRef FC [] ::) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PConstSugar FC (PAlternative [] FirstSuccess [PApp FC (PRef FC [] fromInteger) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PConstant No location 4}],PConstant FC 4,PConstant FC 4,PConstant FC 4,PConstant FC 4,PConstant FC 4,PConstant FC 4])},PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PApp FC (PRef FC [] ::) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PConstSugar FC (PAlternative [] FirstSuccess [PApp FC (PRef FC [] fromInteger) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PConstant No location 5}],PConstant FC 5,PConstant FC 5,PConstant FC 5,PConstant FC 5,PConstant FC 5,PConstant FC 5])},PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] Nil}]}]}]) []]

{- concat : Vec a g -> Vec b g -> Vec (add a b) g -}
PTy (DocString (Options {sanitize = True, allowRawHtml = False, preserveHardBreaks = True, debug = False}) (fromList [])) [] (Syn {using = [], syn_params = [], syn_namespace = [], no_imp = [], imp_methods = [], decoration = <<fn>>, inPattern = False, implicitAllowed = False, constraintAllowed = False, maxline = Nothing, mut_nesting = 0, dsl_info = DSL {dsl_bind = PRef (builtin) [] >>=, dsl_apply = PRef (builtin) [] <*>, dsl_pure = PRef (builtin) [] pure, dsl_var = Nothing, index_first = Nothing, index_next = Nothing, dsl_lambda = Nothing, dsl_let = Nothing, dsl_pi = Nothing}, syn_in_quasiquote = 0, syn_toplevel = True, withAppAllowed = True}) FC [] concat FC (PPi (Exp {pargopts = [], pstatic = Dynamic, pparam = False, pcount = RigW}) __pi_arg No location (PApp FC (PRef FC [FC] Vec) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] a},PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] g}]) (PPi (Exp {pargopts = [], pstatic = Dynamic, pparam = False, pcount = RigW}) __pi_arg No location (PApp FC (PRef FC [FC] Vec) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] b},PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] g}]) (PApp FC (PRef FC [FC] Vec) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PApp FC (PRef FC [FC] add) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] a},PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] b}]},PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] g}])))

{- concat Nil rest = rest -}
PClauses FC [] {__2} [PClause FC concat (PApp FC (PRef FC [FC] concat) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] Nil},PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] rest}]) [] (PRef FC [FC] rest) []]

{- concat (a :: rest) b = concat rest (a :: b) -}
PClauses FC [] {__2} [PClause FC concat (PApp FC (PRef FC [FC] concat) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PApp FC (PRef FC [] ::) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] a},PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] rest}]},PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] b}]) [] (PApp FC (PRef FC [FC] concat) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] rest},PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PApp FC (PRef FC [] ::) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] a},PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] b}]}]) []]

{- t3 : Vec (addOne $ addOne $ addOne $ addOne Main.one) Nat -}
PTy (DocString (Options {sanitize = True, allowRawHtml = False, preserveHardBreaks = True, debug = False}) (fromList [])) [] (Syn {using = [], syn_params = [], syn_namespace = [], no_imp = [], imp_methods = [], decoration = <<fn>>, inPattern = False, implicitAllowed = False, constraintAllowed = False, maxline = Nothing, mut_nesting = 0, dsl_info = DSL {dsl_bind = PRef (builtin) [] >>=, dsl_apply = PRef (builtin) [] <*>, dsl_pure = PRef (builtin) [] pure, dsl_var = Nothing, index_first = Nothing, index_next = Nothing, dsl_lambda = Nothing, dsl_let = Nothing, dsl_pi = Nothing}, syn_in_quasiquote = 0, syn_toplevel = True, withAppAllowed = True}) FC [] t3 FC (PApp FC (PRef FC [FC] Vec) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PApp FC (PRef FC [FC] addOne) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PApp FC (PRef FC [FC] addOne) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PApp FC (PRef FC [FC] addOne) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PApp FC (PRef FC [FC] addOne) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] Main.one}]}]}]}]},PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] Nat}])

{- t3 = concat test test2 -}
PClauses FC [] {__2} [PClause FC t3 (PApp FC (PRef FC [FC] t3) []) [] (PApp FC (PRef FC [FC] concat) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] test},PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] test2}]) []]
