data N = Zero | Suc N
PData
    (DocString
      (Options
        {sanitize = True, allowRawHtml = False, preserveHardBreaks = True, debug = False})
      (fromList []))
    []
    (Syn {
      using = [],
      syn_params = [],
      syn_namespace = ["Main"],
      no_imp = [],
      imp_methods = [],
      decoration = <<fn>>,
      inPattern = False,
      implicitAllowed = False,
      constraintAllowed = False,
      maxline = Nothing,
      mut_nesting = 0,
      dsl_info = DSL {}
      syn_in_quasiquote = 0,
      syn_toplevel = True,
      withAppAllowed = True})
    FC
    []
    (PDatadecl {
      d_name = Main.N,
      d_name_fc = FC,
      d_tcon = PType FC,
      d_cons = [(DocString (Options {sanitize = True,
          allowRawHtml = False,
          preserveHardBreaks = True,
          debug = False})
        (fromList []),
        [],
        Main.Zero,
        FC,
        PRef FC [] Main.N,
        FC,
        []),
        (DocString (Options {sanitize = True,
          allowRawHtml = False,
          preserveHardBreaks = True,
          debug = False})
        (fromList []),
        [],
        Main.Suc,
        FC,
        PPi (Exp {pargopts = [],
            pstatic = Dynamic,
            pparam = False,
            pcount = RigW})
          {_t_0} No location (PRef FC [FC] N) (PRef FC [] Main.N),
        FC,[])
             ]
           })

add : N -> N -> N
PTy (DocString (Options {sanitize = True, allowRawHtml = False, preserveHardBreaks = True, debug = False}) (fromList [])) [] (Syn {using = [], syn_params = [], syn_namespace = ["Main"], no_imp = [], imp_methods = [], decoration = <<fn>>, inPattern = False, implicitAllowed = False, constraintAllowed = False, maxline = Nothing, mut_nesting = 0, dsl_info = DSL {dsl_bind = PRef (builtin) [] >>=, dsl_apply = PRef (builtin) [] <*>, dsl_pure = PRef (builtin) [] pure, dsl_var = Nothing, index_first = Nothing, index_next = Nothing, dsl_lambda = Nothing, dsl_let = Nothing, dsl_pi = Nothing}, syn_in_quasiquote = 0, syn_toplevel = True, withAppAllowed = True}) FC [] Main.add FC (PPi (Exp {pargopts = [], pstatic = Dynamic, pparam = False, pcount = RigW}) __pi_arg No location (PRef FC [FC] N) (PPi (Exp {pargopts = [], pstatic = Dynamic, pparam = False, pcount = RigW}) __pi_arg No location (PRef FC [FC] N) (PRef FC [FC] N)))

add Zero s = s
add (Suc a) b = add a (Suc b)
PClauses FC [] Main.add [PClause FC Main.add (PApp FC (PRef FC [FC] Main.add) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] Zero},PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] s}]) [] (PRef FC [FC] s) [],PClause FC Main.add (PApp FC (PRef FC [FC] Main.add) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PApp FC (PRef FC [FC] Suc) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] a}]},PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] b}]) [] (PApp FC (PRef FC [FC] add) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] a},PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PApp FC (PRef FC [FC] Suc) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] b}]}]) []]

data Vec : (a : Type) -> N -> Type where
  Nil : Vec a Zero
  Cons : {n : N} -> a -> Vec a n -> Vec a (Suc n)
PData (DocString (Options {sanitize = True, allowRawHtml = False, preserveHardBreaks = True, debug = False}) (fromList [])) [] (Syn {using = [], syn_params = [], syn_namespace = ["Main"], no_imp = [], imp_methods = [], decoration = <<fn>>, inPattern = False, implicitAllowed = False, constraintAllowed = False, maxline = Nothing, mut_nesting = 0, dsl_info = DSL {dsl_bind = PRef (builtin) [] >>=, dsl_apply = PRef (builtin) [] <*>, dsl_pure = PRef (builtin) [] pure, dsl_var = Nothing, index_first = Nothing, index_next = Nothing, dsl_lambda = Nothing, dsl_let = Nothing, dsl_pi = Nothing}, syn_in_quasiquote = 0, syn_toplevel = True, withAppAllowed = True}) FC [] (PDatadecl {d_name = Main.Vec, d_name_fc = FC, d_tcon = PPi (Exp {pargopts = [], pstatic = Dynamic, pparam = False, pcount = RigW}) a FC (PType FC) (PPi (Exp {pargopts = [], pstatic = Dynamic, pparam = False, pcount = RigW}) __pi_arg No location (PRef FC [FC] N) (PType FC)), d_cons = [(DocString (Options {sanitize = True, allowRawHtml = False, preserveHardBreaks = True, debug = False}) (fromList []),[],Main.Nil,FC,PApp FC (PRef FC [FC] Vec) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] a},PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] Zero}],FC,[]),(DocString (Options {sanitize = True, allowRawHtml = False, preserveHardBreaks = True, debug = False}) (fromList []),[],Main.Cons,FC,PPi (Imp {pargopts = [], pstatic = Dynamic, pparam = False, pscoped = Just (Impl {tcimplementation = False, toplevel_imp = True, machine_gen = False}), pinsource = False, pcount = RigW}) n FC (PRef FC [FC] N) (PPi (Exp {pargopts = [], pstatic = Dynamic, pparam = False, pcount = RigW}) __pi_arg No location (PRef FC [FC] a) (PPi (Exp {pargopts = [], pstatic = Dynamic, pparam = False, pcount = RigW}) __pi_arg No location (PApp FC (PRef FC [FC] Vec) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] a},PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] n}]) (PApp FC (PRef FC [FC] Vec) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] a},PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PApp FC (PRef FC [FC] Suc) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] n}]}]))),FC,[])]})

{- ccExp : {g : Type} -> {a : N} -> {b : N} -> Vec g a -> Vec g b -> Vec g (add a b) -}
PTy
  (DocString )
  [] -- [(Name, Docstring (Either Err PTerm))]
  (Syn StdSyntax) -- SyntaxInfo
  FC
  [] -- FnOpts
  Main.ccExp -- Name
  FC -- Precise name location
  (PPi
    (Imp {pargopts = [], pstatic = Dynamic, pparam = False, pscoped = Just (Impl {tcimplementation = False, toplevel_imp = True, machine_gen = False}), pinsource = False, pcount = RigW})
    g
    FC
    (PType FC)
    (PPi
      (Imp {pargopts = [], pstatic = Dynamic, pparam = False, pscoped = Just (Impl {tcimplementation = False, toplevel_imp = True, machine_gen = False}), pinsource = False, pcount = RigW})
      a
      FC
      (PRef FC [FC] N)
      (PPi
        (Imp {pargopts = [], pstatic = Dynamic, pparam = False, pscoped = Just (Impl {tcimplementation = False, toplevel_imp = True, machine_gen = False}), pinsource = False, pcount = RigW})
        b
        FC
        (PRef FC [FC] N)
        (PPi
          (Exp {pargopts = [], pstatic = Dynamic, pparam = False, pcount = RigW})
          __pi_arg
          No location
          (PApp FC (PRef FC [FC] Vec) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] g},PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] a}])
          (PPi
            (Exp {pargopts = [], pstatic = Dynamic, pparam = False, pcount = RigW})
            __pi_arg
            No location
            (PApp FC (PRef FC [FC] Vec) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] g},PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] b}])
            (PApp
              FC
              (PRef FC [FC] Vec)
              [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] g},PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PApp FC (PRef FC [FC] add) [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] a},PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] b}]}])))))) -- PTerm definition


{- ccImp : Vec g a -> Vec g b -> Vec g (add a b) -}
PTy
  (DocString
    (Options
      {sanitize = True, allowRawHtml = False, preserveHardBreaks = True, debug = False})
    (fromList []))
  []
  (Syn StdSyntax)
  FC
  []
  Main.ccImp
  FC
  (PPi
    (Exp {pargopts = [], pstatic = Dynamic, pparam = False, pcount = RigW})
    __pi_arg
    No location
    (PApp
      FC
      (PRef FC [FC] Vec)
      [PExp {priority = -1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] g}
        , PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] a}
      ])
    (PPi
      (Exp {pargopts = [], pstatic = Dynamic, pparam = False, pcount = RigW})
      __pi_arg No location
      (PApp
        FC
        (PRef FC [FC] Vec)
        [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] g}
          , PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] b}
        ])
      (PApp
        FC
        (PRef FC [FC] Vec)
        [PExp {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] g}
          , PExp {
            priority = 1,
            argopts = [],
            pname = {arg_0},
            getTm = PApp
              FC
              (PRef FC [FC] add)
              [PExp
                  {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] a}
              , PExp
                  {priority = 1, argopts = [], pname = {arg_0}, getTm = PRef FC [FC] b}
           ]}
        ])))

