module Main where

import Agda.Syntax.Concrete
import Agda.Syntax.Concrete.Pretty
import Agda.Utils.Pretty
import Agda.Syntax.Position
import Agda.Syntax.Literal
import Agda.Syntax.Common
import Agda.Syntax.Fixity
import Agda.Syntax.Notation
import Idris.Parser
import Idris.Parser.Stack
import Idris.AbsSyntax
import qualified Idris.Core.TT as TT

import Util.System (readSource)

import Data.List (intersperse)
import qualified Data.Text as Text
import Data.Foldable
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State.Strict (execStateT, runStateT)

mkName :: String -> Name
mkName n = Name NoRange InScope [(Id n)]
qname :: String -> QName
qname n = QName $ Main.mkName n

iden n = Ident $ qname n

lit :: Integer -> Expr
lit i = Lit $ LitNat NoRange i

id1 :: QName
id1 = QName $ Name NoRange InScope [(Id "id1")]
id2 :: QName
id2 = QName $ Name NoRange InScope [(Id "id2")]
id3 :: QName
id3 = QName $ Name NoRange InScope [(Id "id3")]

rapp = RawApp NoRange [(Ident id1), (Ident id2), Lit $ LitNat NoRange 123]

fundecl :: Declaration
fundecl = FunClause lh rh wh test
  where lh = LHS (IdentP id3) [] []
        rh = RHS rapp
        wh = NoWhere
        test = True

-- PData (DocString (Options {sanitize = True, allowRawHtml = False, preserveHardBreaks = True, debug = False}) (fromList [])) [] syntaxInfo (test):1:6 [] (PDatadecl {d_name = N, d_name_fc = (test):1:6, d_tcon = PType (test):1:6, d_cons = [(DocString (Options {sanitize = True, allowRawHtml = False, preserveHardBreaks = True, debug = False}) (fromList []) , [] ,Z ,(test):1:10,PRef (test):1:6 [] N ,(test):1:10 ,[]), (DocString (Options {sanitize = True, allowRawHtml = False, preserveHardBreaks = True, debug = False}) (fromList []),[],Suc,(test):1:14-16,PPi (Exp {pargopts = [], pstatic = Dynamic, pparam = False, pcount = RigW}) {_t_0} No location (PRef (test):1:18 [(test):1:18] N) (PRef (test):1:6 [] N),(test):1:14-18,[])]})

-- PTy (DocString (Options {sanitize = True, allowRawHtml = False, preserveHardBreaks = True, debug = False}) (fromList []))
--     []
--     (Syn {using = [], syn_params = [], syn_namespace = [], no_imp = [], imp_methods = [], decoration = <<fn>>, inPattern = False, implicitAllowed = False, constraintAllowed = False, maxline = Nothing, mut_nesting = 0, dsl_info = DSL {dsl_bind = PRef (builtin) [] >>=, dsl_apply = PRef (builtin) [] <*>, dsl_pure = PRef (builtin) [] pure, dsl_var = Nothing, index_first = Nothing, index_next = Nothing, dsl_lambda = Nothing, dsl_let = Nothing, dsl_pi = Nothing}, syn_in_quasiquote = 0, syn_toplevel = True, withAppAllowed = True})
--     (test):3:5
--     []
--     one
--     (test):3:1-3
--     (PRef (test):3:7 [(test):3:7] N)

-- PTy
--   (Docstring (Either Err PTerm))
--   [(Name, Docstring (Either Err PTerm))]
--   SyntaxInfo
--   FC
--   FnOpts
--   Name
--   FC
--   PTerm

-- PClauses FC FnOpts Name [PClause' t]

itaDecl :: PDecl -> Declaration
itaDecl (PData doc names synInfo range types
          (PDatadecl nameIdr _ typeconstructor dataconstructors)) =
    Data range induc name lbBind expr typesigs
  where range = NoRange
        induc = Inductive -- Inductive | CoInductive
        name = Main.mkName $ Main.prettyName $ nameIdr
        lbBind = [
          DomainFull (TBind NoRange [] (lit 333))
                 ]
        expr = Set NoRange
        typesigs = [
          typesig "Z" (iden "N")
          , typesig "suc" $ funcExpr "N" (iden "N")
                   ] -- [Declaration]
itaDecl (PTy doc names synInfo range fnopts nameIdr rangeName terms) = -- Type declaration
  typesig (Main.prettyName nameIdr) (itaTerm terms)
itaDecl (PClauses range fnopts name clauses) = -- Pattern clause
  itaClauses clauses
itaDecl (PFix fc fixIdr strings) = Infix fixAgda (map Main.mkName strings)
  where fixAgda = itaFixity fixIdr
itaDecl _ = undefined

itaFixity :: Idris.AbsSyntax.Fixity -> Agda.Syntax.Fixity.Fixity
itaFixity (Infixl prec) = Fixity NoRange (Related (toInteger prec)) LeftAssoc
itaFixity (Infixr prec) = Fixity NoRange (Related (toInteger prec)) RightAssoc
itaFixity (InfixN prec) = Fixity NoRange (Related (toInteger prec)) NonAssoc
itaFixity (PrefixN prec) = undefined -- I don't know that this is.

itaClauses :: [PClause] -> Declaration
itaClauses clauses = case length clauses of
  1 -> itaClause $ head clauses -- Safe because of the case stmt.
  _ -> undefined -- I don't know when this case happen. But it's not common.

  -- TODO START HERE The pattern and equals sign is not int the correct order
itaClause :: PClause -> Declaration
itaClause (PClause fc name whole with rhsIdr whrIdr) =
    FunClause lhs rhs whr bool
  where lhs = LHS ptn rewriteExpr withExpr
        rhs = RHS rhsexp -- Can also be 'AbsurdRHS'
        whr = NoWhere -- Can also be 'AnyWhere [Decls]'
        bool = False
        rhsexp = itaTerm rhsIdr
  -- TODO This does not work for general patterns
  -- But I talked about skipping dependent patterns in the planning report.
  -- TODO Research Problem
  -- The LHS in Agda is represented as a 'Pattern'-type while it's a simple
  -- 'PTerm' in Idris. It's not obvious how to convert betwen them.
  -- TODO This is wrong. The LHS pattern is much more complex than just a name
        ptn = IdentP $ qname $ Main.prettyName name
        rewriteExpr = []
        withExpr = []

pat :: Declaration
pat = FunClause lhs rhs whr bool
  where lhs = LHS ptn rewriteExpr withExpr
        rhs = RHS rhsexp -- Can also be 'AbsurdRHS'
        whr = NoWhere -- Can also be 'AnyWhere [Decls]'
        bool = True
        rhsexp = application "test" [iden "id1", iden "id2"]
        ptn = IdentP $ qname "abc"
        rewriteExpr = []
        withExpr = []

application :: String -> [Expr] -> Expr
application name args = RawApp NoRange ((iden name) : args)

--  One clause of a top-level definition. Term arguments to constructors are:
-- 1. The whole application (missing for PClauseR and PWithR because they're within a "with" clause)
-- 2. The list of extra 'with' patterns
-- 3. The right-hand side
-- 4. The where block (PDecl' t)
-- data PClause' t = PClause  FC Name t [t] t                    [PDecl' t] -- ^ A normal top-level definition.
--                 | PWith    FC Name t [t] t (Maybe (Name, FC)) [PDecl' t]
--                 | PClauseR FC        [t] t                    [PDecl' t]
--                 | PWithR   FC        [t] t (Maybe (Name, FC)) [PDecl' t]

itaTerm :: PTerm -> Expr
itaTerm (PRef range highlightRange name) = iden $ Main.prettyName name
itaTerm (PApp range fst args) = RawApp NoRange ((itaTerm fst) : (map itaArgs args))
itaTerm (PPi plicity name fc term1 term2) =
  Fun NoRange (Arg argInfo (itaTerm term1)) (itaTerm term2)
  where argInfo = (ArgInfo NotHidden modality UserWritten UnknownFVs)
itaTerm (PConstSugar fc term) = itaTerm term
itaTerm (PConstant fc const) = Lit (itaConst const)
itaTerm (PAlternative namePair alttype terms) = case length terms of
  1 -> itaTerm $ head terms -- Safe because of case stmt.
  0 -> undefined
  _ -> itaTerm $ head terms -- TODO Probably wrong. But it's safe at least
itaTerm _ = undefined

itaConst :: TT.Const -> Literal
itaConst (TT.I int) = LitNat NoRange (toInteger int)
itaConst (TT.BI integer) = LitNat NoRange integer
itaConst (TT.Fl double) = LitFloat NoRange double
itaConst (TT.Ch char) = LitChar NoRange char
itaConst (TT.Str string) = LitString NoRange string
itaConst (TT.B8 word8) = undefined
itaConst (TT.B16 word16) = undefined
itaConst (TT.B32 word32) = undefined
itaConst (TT.B64 word64) = undefined
itaConst (TT.AType arithty) = undefined
itaConst TT.StrType = undefined
itaConst TT.WorldType = undefined
itaConst TT.TheWorld = undefined
itaConst TT.VoidType = undefined
itaConst TT.Forgot = undefined

itaName :: TT.Name -> Name
itaName n = Main.mkName $ Main.prettyName n
-- TODO This is not strictly correct. But for 'RawApp' which only take Exprs as
-- args it's okay
-- type PArg = PArg' PTerm
-- getTm :: PTerm
itaArgs :: PArg -> Expr
itaArgs (PExp prio argopts pname getTm) = itaTerm getTm
itaArgs _ = undefined

addComment = (++)
  -- There is also AbsStyntaxTree.prettyName but it's harder to use.
prettyName :: TT.Name -> String
prettyName (TT.UN name) =  Text.unpack name
prettyName (TT.NS ns names) = concat $ [Main.prettyName ns] ++ (map Text.unpack names)
prettyName (TT.MN id name) = addComment
  ("Machine chosen name with id: " ++ (show id)) $ Text.unpack name
prettyName (TT.SN sn) = addComment "Decorated function name" $ prettySN sn
prettyName (TT.SymRef id) = addComment "Reference to IBC" $ show id

prettySN :: TT.SpecialName -> String
prettySN (TT.WhereN a b c) = "WhereN"
prettySN (TT.WithN a b) = "WithN"
prettySN (TT.ImplementationN a b) = "ImplementationN"
prettySN (TT.ParentN a b) = "ParentN"
prettySN (TT.MethodN a) = "MethodN"
prettySN (TT.CaseN a b) = "CaseN"
prettySN (TT.ImplementationCtorN a) = "ImplementationCtorN"
prettySN (TT.MetaN a b) = "MetaN"

datadecl :: Declaration
datadecl = Data range induc name lbBind expr typesigs
  where range = NoRange
        induc = Inductive -- Inductive | CoInductive
        name = Main.mkName "DataTest"
        lbBind = [
          DomainFull (TBind NoRange [] (lit 333))
                 ]
        expr = Set NoRange
        typesigs = [
          typesig "Z" (iden "N")
          , typesig "suc" $ funcExpr "N" (iden "N")
                   ] -- [Declaration]


a = putStrLn $ prettyShow datadecl

agda d = putStrLn $ prettyShow d

mkFixity :: Fixity'
mkFixity = Fixity' f not NoRange
  where f = Fixity NoRange Unrelated NonAssoc
        rstring = Ranged NoRange "RawName"
        not = [IdPart rstring]

-- TODO START HERE
-- Read in the filename. Parse that file as Idris. Then run a dummy
-- translation funciton on it. Then print some Agda code. Then write to file
main = tp
 
loadIdr :: FilePath -> IO (Idris [PDecl])
loadIdr f = do (file :: String) <- readSource f
               putStrLn file
               let syntax = defaultSyntax
               return $ parseProg syntax f file Nothing

testParse p = runparser (prog defaultSyntax) idrisInit "(test)" p

-- TODO START HERE
-- This functions parses and prints the PDecls in simpleIdris.idr successfully!
-- Now I need to make this robust and then its only to translate to the Agda AST.
tp = do (file :: String) <- readSource f
        let res = testParse file
        case res of
          Left err -> putStrLn $ prettyError err
          Right pd -> putStrLn $ prettyShow $ map itaDecl pd
          -- Right pd -> putStrLn $ prettyShow $ itaDecl $ head pd
          -- Right pd -> putPDecls pd
  -- where f = "../simpleIdris.idr"
  where f = "../simpleIdris.idr"
        putPDecls lst = putStrLn $ concat $ intersperse "\n\n" $ map show lst

printIdr :: IO ()
printIdr = do prog <- idr
              let e = runStateT prog idrisInit
              res <- runExceptT e
              print $ show res
  where idr = loadIdr "../simpleIdris.idr"


modality :: Modality
modality = Modality Relevant Quantity0

arg :: String -> a -> NamedArg a
arg name expr = Arg (ArgInfo NotHidden modality UserWritten UnknownFVs) $
  Named (Just (Ranged NoRange name)) expr

funcExpr :: String -> Expr -> Expr
funcExpr name body = Fun NoRange (Arg argInfo (iden name)) body
  where argInfo = (ArgInfo NotHidden modality UserWritten UnknownFVs)

typesig :: String -> Expr -> TypeSignature
typesig name body = TypeSig argInfo n body
  where argInfo = ArgInfo NotHidden modality UserWritten UnknownFVs
        n = mkName name
        expr = funcExpr "argTest" (iden "FunctionTest")

s = prettyShow  $ typesig "addOne" $ funcExpr "argTest" (iden "FunctionTest")
--   | Fun Range (Arg Expr) Expr                  -- ^ ex: @e -> e@ or @.e -> e@ (NYI: @{e} -> e@)

--   | Lam Range [LamBinding] Expr                -- ^ ex: @\\x {y} -> e@ or @\\(x:A){y:B} -> e@
  -- Makes a lambda function. Not what I want.
        
-- data Pattern
--   = IdentP QName                           -- ^ @c@ or @x@
--   | QuoteP Range                           -- ^ @quote@
--   | AppP Pattern (NamedArg Pattern)        -- ^ @p p'@ or @p {x = p'}@
--   | RawAppP Range [Pattern]                -- ^ @p1..pn@ before parsing operators
--   | OpAppP Range QName (Set A.Name)
--            [NamedArg Pattern]              -- ^ eg: @p => p'@ for operator @_=>_@
--                                            -- The 'QName' is possibly
--                                            -- ambiguous, but it must
--                                            -- correspond to one of
--                                            -- the names in the set.
--   | HiddenP Range (Named_ Pattern)         -- ^ @{p}@ or @{x = p}@
--   | InstanceP Range (Named_ Pattern)       -- ^ @{{p}}@ or @{{x = p}}@
--   | ParenP Range Pattern                   -- ^ @(p)@
--   | WildP Range                            -- ^ @_@
--   | AbsurdP Range                          -- ^ @()@
--   | AsP Range Name Pattern                 -- ^ @x\@p@ unused
--   | DotP Range Expr                        -- ^ @.e@
--   | LitP Literal                           -- ^ @0@, @1@, etc.
--   | RecP Range [FieldAssignment' Pattern]  -- ^ @record {x = p; y = q}@
--   | EqualP Range [(Expr,Expr)]             -- ^ @i = i1@ i.e. cubical face lattice generator
--   | EllipsisP Range                        -- ^ @...@, only as left-most pattern.
--   | WithP Range Pattern                    -- ^ @| p@, for with-patterns.


-- type TypeSignature = Declaration
-- data Declaration
--   = TypeSig ArgInfo Name Expr
--   -- ^ Axioms and functions can be irrelevant. (Hiding should be NotHidden)
--   | Generalize Range [TypeSignature] -- ^ Variables to be generalized, can be hidden and/or irrelevant.
--   | Field IsInstance Name (Arg Expr) -- ^ Record field, can be hidden and/or irrelevant.
--   | FunClause LHS RHS WhereClause Bool
--   | DataSig     Range Induction Name [LamBinding] Expr -- ^ lone data signature in mutual block
--   | Data        Range Induction Name [LamBinding] Expr [TypeSignatureOrInstanceBlock]
--   | DataDef     Range Induction Name [LamBinding] [TypeSignatureOrInstanceBlock]
--   | RecordSig   Range Name [LamBinding] Expr -- ^ lone record signature in mutual block
--   | RecordDef   Range Name (Maybe (Ranged Induction)) (Maybe HasEta) (Maybe (Name, IsInstance)) [LamBinding] [Declaration]
--   | Record      Range Name (Maybe (Ranged Induction)) (Maybe HasEta) (Maybe (Name, IsInstance)) [LamBinding] Expr [Declaration]
--     -- ^ The optional name is a name for the record constructor.
--   | Infix Fixity [Name]
--   | Syntax      Name Notation -- ^ notation declaration for a name
--   | PatternSyn  Range Name [Arg Name] Pattern
--   | Mutual      Range [Declaration]  -- @Range@ of the whole @mutual@ block.
--   | Abstract    Range [Declaration]
--   | Private     Range Origin [Declaration]
--     -- ^ In "Agda.Syntax.Concrete.Definitions" we generate private blocks
--     --   temporarily, which should be treated different that user-declared
--     --   private blocks.  Thus the 'Origin'.
--   | InstanceB   Range [Declaration]
--   | Macro       Range [Declaration]
--   | Postulate   Range [TypeSignatureOrInstanceBlock]
--   | Primitive   Range [TypeSignature]
--   | Open        Range QName ImportDirective
--   | Import      Range QName (Maybe AsName) !OpenShortHand ImportDirective
--   | ModuleMacro Range  Name ModuleApplication !OpenShortHand ImportDirective
--   | Module      Range QName Telescope [Declaration]
--   | UnquoteDecl Range [Name] Expr
--   | UnquoteDef  Range [Name] Expr
--   | Pragma      Pragma


-- data Expr
--   = Ident QName                                -- ^ ex: @x@
--   | Lit Literal                                -- ^ ex: @1@ or @\"foo\"@
--   | QuestionMark Range (Maybe Nat)             -- ^ ex: @?@ or @{! ... !}@
--   | Underscore Range (Maybe String)            -- ^ ex: @_@ or @_A_5@
--   | RawApp Range [Expr]                        -- ^ before parsing operators
--   | App Range Expr (NamedArg Expr)             -- ^ ex: @e e@, @e {e}@, or @e {x = e}@
--   | OpApp Range QName (Set A.Name)
--           [NamedArg
--              (MaybePlaceholder (OpApp Expr))]  -- ^ ex: @e + e@
--                                                -- The 'QName' is
--                                                -- possibly ambiguous,
--                                                -- but it must
--                                                -- correspond to one of
--                                                -- the names in the
--                                                -- set.
--   | WithApp Range Expr [Expr]                  -- ^ ex: @e | e1 | .. | en@
--   | HiddenArg Range (Named_ Expr)              -- ^ ex: @{e}@ or @{x=e}@
--   | InstanceArg Range (Named_ Expr)            -- ^ ex: @{{e}}@ or @{{x=e}}@
--   | Lam Range [LamBinding] Expr                -- ^ ex: @\\x {y} -> e@ or @\\(x:A){y:B} -> e@
--   | AbsurdLam Range Hiding                     -- ^ ex: @\\ ()@
--   | ExtendedLam Range [LamClause]              -- ^ ex: @\\ { p11 .. p1a -> e1 ; .. ; pn1 .. pnz -> en }@
--   | Fun Range (Arg Expr) Expr                  -- ^ ex: @e -> e@ or @.e -> e@ (NYI: @{e} -> e@)
--   | Pi Telescope Expr                          -- ^ ex: @(xs:e) -> e@ or @{xs:e} -> e@
--   | Set Range                                  -- ^ ex: @Set@
--   | Prop Range                                 -- ^ ex: @Prop@
--   | SetN Range Integer                         -- ^ ex: @Set0, Set1, ..@
--   | PropN Range Integer                        -- ^ ex: @Prop0, Prop1, ..@
--   | Rec Range RecordAssignments                -- ^ ex: @record {x = a; y = b}@, or @record { x = a; M1; M2 }@
--   | RecUpdate Range Expr [FieldAssignment]     -- ^ ex: @record e {x = a; y = b}@
--   | Let Range [Declaration] (Maybe Expr)       -- ^ ex: @let Ds in e@, missing body when parsing do-notation let
--   | Paren Range Expr                           -- ^ ex: @(e)@
--   | IdiomBrackets Range Expr                   -- ^ ex: @(| e |)@
--   | DoBlock Range [DoStmt]                     -- ^ ex: @do x <- m1; m2@
--   | Absurd Range                               -- ^ ex: @()@ or @{}@, only in patterns
--   | As Range Name Expr                         -- ^ ex: @x\@p@, only in patterns
--   | Dot Range Expr                             -- ^ ex: @.p@, only in patterns
--   | ETel Telescope                             -- ^ only used for printing telescopes
--   | QuoteGoal Range Name Expr                  -- ^ ex: @quoteGoal x in e@
--   | QuoteContext Range                         -- ^ ex: @quoteContext@
--   | Quote Range                                -- ^ ex: @quote@, should be applied to a name
--   | QuoteTerm Range                            -- ^ ex: @quoteTerm@, should be applied to a term
--   | Tactic Range Expr [Expr]                   -- ^ @tactic solve | subgoal1 | .. | subgoalN@
--   | Unquote Range                              -- ^ ex: @unquote@, should be applied to a term of type @Term@
--   | DontCare Expr                              -- ^ to print irrelevant things
--   | Equal Range Expr Expr                      -- ^ ex: @a = b@, used internally in the parser
--   | Ellipsis Range                             -- ^ @...@, used internally to parse patterns.
--   | Generalized Expr
--   deriving Data


-- type NamedArg a = Arg (Named_ a)
-- type RString = Ranged RawName
-- type Named_ = Named RString
-- data Named name a =
--     Named { nameOf     :: Maybe name
--           , namedThing :: a
-- data Hiding  = Hidden | Instance Overlappable | NotHidden
-- data Arg e  = Arg
--   { argInfo :: ArgInfo
--   , unArg :: e
-- data ArgInfo = ArgInfo
--   { argInfoHiding        :: Hiding
--   , argInfoModality      :: Modality
--   , argInfoOrigin        :: Origin
--   , argInfoFreeVariables :: FreeVariables
-- -- | We have a tuple of modalities, which might not be fully orthogonal.
-- --   For instance, irrelevant stuff is also run-time irrelevant.
-- data Modality = Modality
--   { modRelevance :: Relevance
--       -- ^ Legacy irrelevance.
--       --   See Pfenning, LiCS 2001; Abel/Vezzosi/Winterhalter, ICFP 2017.
--   , modQuantity  :: Quantity
--       -- ^ Cardinality / runtime erasure.
--       --   See Conor McBride, I got plenty o' nutting, Wadlerfest 2016.
--       --   See Bob Atkey, Syntax and Semantics of Quantitative Type Theory, LiCS 2018.
-- data Relevance
--   = Relevant    -- ^ The argument is (possibly) relevant at compile-time.
--   | NonStrict   -- ^ The argument may never flow into evaluation position.
--                 --   Therefore, it is irrelevant at run-time.
--                 --   It is treated relevantly during equality checking.
--   | Irrelevant  -- ^ The argument is irrelevant at compile- and runtime.
-- -- | Quantity for linearity.
-- --   A quantity is a set of natural numbers, indicating possible semantic
-- --   uses of a variable.  A singleton set @{n}@ requires that the
-- --   corresponding variable is used exactly @n@ times.
-- data Quantity
--   = Quantity0  -- ^ Zero uses @{0}@, erased at runtime.
--   | Quantity1  -- ^ Linear use @{1}@ (could be updated destructively).
--     -- Mostly TODO (needs postponable constraints between quantities to compute uses).
--   | Quantityω  -- ^ Unrestricted use @ℕ@.
-- data Origin
--   = UserWritten  -- ^ From the source file / user input.  (Preserve!)
--   | Inserted     -- ^ E.g. inserted hidden arguments.
--   | Reflected    -- ^ Produced by the reflection machinery.
--   | CaseSplit    -- ^ Produced by an interactive case split.
--   | Substitution -- ^ Named application produced to represent a substitution. E.g. "?0 (x = n)" instead of "?0 n"
-- data FreeVariables = UnknownFVs | KnownFVs IntSet


-- data Literal = LitNat    Range !Integer
--              | LitWord64 Range !Word64
--              | LitFloat  Range !Double
--              | LitString Range String
--              | LitChar   Range !Char
--              | LitQName  Range QName
--              | LitMeta   Range AbsolutePath MetaId

-- type TypedBinding = TypedBinding' Expr
-- data TypedBinding' e
--   = TBind Range [NamedArg BoundName] e  -- ^ Binding @(x1 ... xn : A)@.
--   | TLet  Range [Declaration]           -- ^ Let binding @(let Ds)@ or @(open M args)@.
--   deriving (Data, Functor, Foldable, Traversable)
-- -- | A telescope is a sequence of typed bindings. Bound variables are in scope
-- --   in later types.
-- type Telescope = [TypedBinding]

-- type Notation = [GenPart]
-- data GenPart
--   = BindHole Range (Ranged Int) --     -- ^ Argument is the position of the hole (with binding) where the binding should occur.
--   | NormalHole Range (NamedArg (Ranged Int)) --     -- ^ Argument is where the expression should go.
--   | WildHole (Ranged Int) --     -- ^ An underscore in binding position.
--   | IdPart RString


-- -- | The notation is handled as the fixity in the renamer.
-- --   Hence, they are grouped together in this type.
-- data Fixity' = Fixity'
--     { theFixity   :: !Fixity
--     , theNotation :: Notation
--     , theNameRange :: Range -- ^ Range of the name in the fixity declaration
-- data Fixity = Fixity
--   { fixityRange :: Range -- ^ Range of the whole fixity declaration.
--   , fixityLevel :: !PrecedenceLevel
--   , fixityAssoc :: !Associativity
-- data PrecedenceLevel
--   = Unrelated -- ^ No fixity declared.
--   | Related !Integer -- ^ Fixity level declared as the @Integer@.
-- data Associativity = NonAssoc | LeftAssoc | RightAssoc
  

-- data LHS = LHS
--   { lhsOriginalPattern :: Pattern       -- ^ e.g. @f ps | wps@
--   , lhsRewriteEqn      :: [RewriteEqn]  -- ^ @rewrite e@ (many)
--   , lhsWithExpr        :: [WithExpr]    -- ^ @with e@ (many)
--   } -- ^ Original pattern (including with-patterns), rewrite equations and with-expressions.
-- type RewriteEqn = Expr
-- type WithExpr   = Expr
-- type RHS = RHS' Expr
-- data RHS' e
--   = AbsurdRHS -- ^ No right hand side because of absurd match.
--   | RHS e
