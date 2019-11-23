module Main where
import Agda.Syntax.Concrete
import Agda.Syntax.Concrete.Pretty ()
import Agda.Syntax.Common
import Agda.Syntax.Fixity
import qualified Agda.Syntax.Abstract.Name as AAbstract
import Agda.Utils.Pretty (prettyShow)
import Agda.Syntax.Position (Range'(..))
import Agda.Syntax.Literal (Literal(..))
import Agda.Syntax.Notation (GenPart(IdPart))
import Idris.AbsSyntax
import Idris.Parser (loadModule, prog)
import Idris.Docstrings (Docstring, emptyDocstring)
import Idris.IBC (IBCPhase(..))
import Idris.Info (getIdrisLibDir)
import Idris.ElabDecls (elabPrims, elabDecls)
import Idris.Core.Evaluate (definitions, TTDecl, Def, Def(..))
import qualified Idris.Core.TT as TT

import Version_IdrisToAgda (gitHash)

import Util.System (readSource)

import Data.List (intersperse, nub, find)
import Data.Either (fromLeft, fromRight)
import qualified Data.Text as Text (unpack)
import qualified Data.Map as Map
  (lookup, keys, filterWithKey, elems, Map, empty, union, insert, intersectionWith, toList)
import qualified Data.Maybe as Maybe (fromJust, catMaybes)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State.Strict (evalStateT, execStateT, runStateT)
import Control.Monad.Trans (lift, liftIO)

import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.FilePath ((</>))

import Debug.Trace (trace)

-- Try to handle implict/explict arguments. The whole '{a : Set}' thing.
-- It's somewhat done. Probably not correct yet but working

-- The '{a : A}' is a definition of a implicit function space.
-- To give an implicit argument explicit to a function, enclose it in brackets.

-- TODO Add comments to the Agda concrete syntax

-- There seems to be no information in the Idris AST about implicit
-- arguments. Which probably is to be expected at this stage before type
-- checking. Do I need to run elaboration/typechecking before being able to
-- generate correct Agda? Agda is much more demanding about these things.

-- TODO Handle parameterized Datatypes.
-- TODO Handle indexed Datatypes.
-- The parameters are required to be the same for all constructors, but indices
-- vary. In 'data Vector (A : Set) : Nat → Set where', 'A' is parameter and
-- 'Nat' a index. Parameters are bound once for all constructors, indices are
-- bound locally for each constructor. (In Agda)
-- Agda and Idris handles indices and parameters differently.

-- Idris doesn't syntactically differentiate between indices and parameters,
-- while Agda does. I wonder when Idris computes that information? Because I
-- need that to generate correct Agda.

-- Instance arguments, is another thing I don't know well. They are solved by a
-- special instance resolution algorithm. Normal implicit arguments are solved
-- by unification. Instance arguments are similar to Haskell's type class
-- constraints. They are enclodsed in double brackets '{{ a : Show }}'

type AST = [PDecl]

-- TODO Should I use this representation instead?
-- itaDecls :: Map TT.Name (AST, TTDecl) -> [Declaration]
-- And the `itaDecl` has the same.
itaDecls :: AST -> [Declaration]
itaDecls pd = concat $ map itaDecl pd

itaDecl :: PDecl -> [Declaration]
-- TODO Add the Agda {a : Set} argument declaration
itaDecl (PData doc names synInfo range types
          (PDatadecl nameIdr _ typeconstructor dataconstructors)) =
    [Data range induc name lbBind expr typesigs]
  where range = NoRange
        induc = Inductive -- Inductive | CoInductive
        name = itaName nameIdr
        lbBind = [
          -- DomainFull (TBind NoRange [] (lit 333)) -- TODO This is not correct.
                 ]
        expr = itaTerm typeconstructor
        typesigs = map itaDC dataconstructors
-- TODO Preserve arg names.
itaDecl (PTy doc names synInfo range fnopts nameIdr rangeName terms) = -- Type declaration
  [typesig (itaName nameIdr) (itaTerm terms)]
itaDecl (PClauses range fnopts name clauses) = -- Pattern clause
  itaClauses clauses
itaDecl (PFix fc fixIdr strings) = [Infix fixAgda (map mkName strings)]
  where fixAgda = itaFixity fixIdr
itaDecl (PPostulate isExternal doc synInfo fc fcName fnopts name term) = [res]
  -- TODO Fix this
  -- undefined
  where res :: Declaration
        res = Postulate NoRange tysigs
        tysigs :: [TypeSignatureOrInstanceBlock]
        tysigs = [tysig]
        -- tysig :: TypeSignatureOrInstanceBlock
        tysig :: Declaration
        tysig = TypeSig argInfo (itaName name) (itaTerm term)
        argInfo = ArgInfo NotHidden modality UserWritten UnknownFVs
        expr = funcExpr "argTest" (iden "FunctionTest")
itaDecl (PCAF _ name _) = undefined
itaDecl (PParams _ _ _) = undefined
itaDecl (PNamespace _ _ _) = undefined
itaDecl (PRecord _ _ _ _ name _ _ _ _ _ _ _) = undefined
itaDecl (PInterface _ _ _ _ name _ _ _ _ _ _ _) = undefined
itaDecl (PImplementation _ _ _ _ _ _ _ _ _ _ _ _ _ name _) = undefined
itaDecl (PDSL name _) = undefined
itaDecl (PSyntax _ _) = undefined
  -- TODO Will not do.
itaDecl (PMutual fc terms) = [Mutual NoRange decls]
  -- hole "Mutual declarations will not be supported in this version of ita"
  where decls = concat dOfD
        dOfD = (map itaDecl terms)
itaDecl (PDirective _) = [] -- TODO This is wrong. But directives are
    -- implemention dependent som they are not applicable to Agda
  -- Some could be translate to Agda pragmas but not in general
itaDecl (PProvider _ _ _ _ _ name) = undefined
itaDecl (PTransform _ _ _ _) = undefined
itaDecl (PRunElabDecl _ _ _) = undefined

-- | Translate Data Constructors
itaDC :: ( Docstring (Either TT.Err PTerm)
         , [(TT.Name, Docstring (Either TT.Err PTerm))]
         , TT.Name
         , TT.FC
         , PTerm
         , TT.FC
         , [TT.Name]) -> TypeSignature
itaDC (doc, _, name, fc1, body, fc2, _) = typesig (itaName name) (itaTerm body)

-- TODO Properly display infix operators in the Agda AST.
itaFixity :: Idris.AbsSyntax.Fixity -> Agda.Syntax.Fixity.Fixity
itaFixity (Infixl prec) = Fixity NoRange (Related (toInteger prec)) LeftAssoc
itaFixity (Infixr prec) = Fixity NoRange (Related (toInteger prec)) RightAssoc
itaFixity (InfixN prec) = Fixity NoRange (Related (toInteger prec)) NonAssoc
itaFixity (PrefixN prec) = undefined -- I don't know that this is.

itaClauses :: [PClause] -> [Declaration]
itaClauses clauses = map itaClause clauses
  -- case length clauses of
  -- 1 -> itaClause $ head clauses -- Safe because of the case stmt.
  -- _ -> error ("PClause " ++ (show clauses)) -- When several pattern clauses

itaClause :: PClause -> Declaration
itaClause (PClause fc name whole with rhsIdr whrIdr) =
    FunClause lhs rhs whr bool
  where lhs = LHS ptn rewriteExpr withExpr
        rhs = RHS rhsexp -- Can also be 'AbsurdRHS'
        -- whr = NoWhere -- Can also be 'AnyWhere [Decls]'
        -- TODO Join where expression together
        -- TODO Only emit `where` if there should be one.
        whr = AnyWhere (concat (map itaDecl whrIdr))
        bool = False
        rhsexp = itaTerm rhsIdr
  -- TODO Research Problem
  -- This does not work for general patterns
  -- But I talked about skipping dependent patterns in the planning report.
  -- The LHS in Agda is represented as a 'Pattern'-type while it's a simple
  -- 'PTerm' in Idris. It's not obvious how to convert betwen them.
        ptn = itaPattern 0 whole
        rewriteExpr = []
        withExpr = []
itaClause (PWith _ _ _ _ _ _ _) = undefined
itaClause (PClauseR _ _ _ _) = undefined
itaClause (PWithR _ _ _ _ _) = undefined

-- The depth parameter is used to put parenthesis only when needed.
-- This is a ugly hack which should be redone.
-- We probably should retain the parenthesis placement from Idris, but I don't
-- think that's posssible
itaPattern :: Int -> PTerm -> Pattern
itaPattern _ (PRef _ _ name) = IdentP $ qname $ Main.prettyName name
-- With the simple parenthesis hack. This should be done correctly soon.
itaPattern 0 (PApp range fst args) =
    (RawAppP NoRange ((itaPattern 1 fst) : (map ((itaPattern 1) . itaArgsToTerm) args)))
itaPattern d (PApp range fst args) = ParenP NoRange
    (RawAppP
      NoRange
      ((itaPattern (d + 1) fst) : (map ((itaPattern (d + 1)) . itaArgsToTerm) args)))

itaPattern _ (PPi plicity name fc term1 term2) = undefined
itaPattern _ (PConstSugar fc term) = undefined
itaPattern _ (PConstant fc const) = undefined
itaPattern _ (PAlternative namePair alttype terms) = undefined
itaPattern _ (PType fc) = undefined
itaPattern _ (PIfThenElse fc ift thent elset) = undefined
itaPattern _ (PPair fc fcs puninfo termA termB) = undefined
itaPattern _ (PMetavar _ _) = undefined
itaPattern _ (PQuote raw) = undefined
itaPattern _ (PInferRef fc fcs name) = undefined
itaPattern _ (PPatvar fc name) = undefined
itaPattern _ (PLam fc name fc2 term1 term2) = undefined
itaPattern _ (PLet fc rigcount name fc2 term1 term2 term3) = undefined
itaPattern _ (PTyped pterm1 pterm2) = undefined
itaPattern _ (PWithApp fc pterm1 pterm2) = undefined
itaPattern _ (PAppImpl pterm implicitinfo) = undefined
itaPattern _ (PAppBind fc pterm pargs) = undefined
itaPattern _ (PMatchApp fc name) = undefined
itaPattern _ (PCase fc pterm1 pterm2) = undefined
itaPattern _ (PTrue fc puninfo) = undefined
itaPattern _ (PResolveTC fc) = undefined
itaPattern _ (PRewrite fc mname pterm1 pterm2 mpterm) = undefined
itaPattern _ (PDPair fc fcs puninfo term1 term3 term2) = undefined
itaPattern _ (PAs fc name term) = undefined
itaPattern _ (PHidden term) = undefined
itaPattern _ (PUniverse fc universe) = undefined
itaPattern _ (PGoal fc term name pterm) = undefined
itaPattern _ (Idris.AbsSyntax.Placeholder) = WildP NoRange
itaPattern _ (PDoBlock pdo) = undefined
itaPattern _ (PIdiom fc term) = undefined
itaPattern _ (PProof ptactics) = undefined
itaPattern _ (PTactics ptactics) = undefined
itaPattern _ (PElabError err) = undefined
itaPattern _ (PImpossible) = undefined
itaPattern _ (PCoerced term) = undefined
itaPattern _ (PDisamb strings term) = undefined
itaPattern _ (PUnifyLog term) = undefined
itaPattern _ (PNoImplicits term) = undefined
itaPattern _ (PQuasiquote term mpterm) = undefined
itaPattern _ (PUnquote term) = undefined
itaPattern _ (PQuoteName name bool fc) = undefined
itaPattern _ (PRunElab fc term strings) = undefined

itaArgsToTerm :: PArg -> PTerm
itaArgsToTerm (PExp priority argopts pname getTm) = getTm
itaArgsToTerm (PImp priority machine_info argopts pname getTm) = getTm
itaArgsToTerm (PConstraint priority argopts pnam getTm) = getTm
itaArgsToTerm (PTacImplicit priority argopts pname getScript getTm) = getTm

application :: Expr -> [Expr] -> Expr
application head args = RawApp NoRange (head : args)

paren :: Expr -> Expr
paren e = Paren NoRange e
-- Parenthesis are explicit in the concrete Agda AST but are not represented in
-- the Idris PDecl lang. So that information is lost. I need to reconstruct that
-- somehow to get the output to typecheck. It is certainly possible to do with a
-- post-processing pass. But the output won't match the input. And how hard is
-- it to do correctly?
-- Right now there is this ugly hack.

createTelescope :: TT.Name -> PTerm -> Telescope
createTelescope name pterm = [TBind NoRange bargs (itaTerm pterm)]
  where bargs = [barg]
        barg = hiddenArg name (mkBoundName_ (itaName name))

-- data ArgOpt = AlwaysShow
--             | HideDisplay
--             | InaccessibleArg
--             | UnknownImp
-- data Static = Static | Dynamic
-- data RigCount = Rig0 | Rig1 | RigW
  -- Rig = Quantity?
itaPi :: Plicity -> TT.Name -> PTerm -> PTerm -> Expr
itaPi (Exp pargopts pstatic pparam pcount) name term1 term2 =
  -- There is something with name...
  -- trace ((show pargopts) ++ (show name) ++ (show pstatic) ++ (show pparam) ++ (show pcount))
  if isMN name then
    Fun NoRange (Arg defaultArgInfo (itaTerm term1))  (itaTerm term2)
  else
  case Main.prettyName name of
    -- TODO For some reason the Idris parser sets the names of some things
    -- It calls arguments __pi_arg.
    "__pi_arg" -> Fun NoRange (Arg defaultArgInfo (itaTerm term1))  (itaTerm term2)
    _ -> Pi (createTelescope name term1) (itaTerm term2)
  -- Fun NoRange (Arg defaultArgInfo (itaTerm term1))  (itaTerm term2)
  -- Pi (createTelescope name term1) (itaTerm term2)
itaPi q@(Imp pargopts pstatic pparam pscoped pinsource pcount) name term1 term2 =
  Pi (createHiddenTelescope name term1) (itaTerm term2)
--  error ("itaPi: Imp problem with " ++ show q)

  -- TODO Type classes are implemented very differently in Agda. I probably wont do this.
{- example of parse of |{n : N} -> test n|
 Imp { pargopts = [],
       pstatic = Dynamic,
       pparam = False,
       pscoped = Just (Impl { tcimplementation = False,
                              toplevel_imp = True,
                              machine_gen = False}),
       pinsource = True,
       pcount = RigW
     }
-}

defaultArgInfoHidden :: ArgInfo
defaultArgInfoHidden =  ArgInfo
  { argInfoHiding        = Hidden
  , argInfoModality      = defaultModality
  , argInfoOrigin        = UserWritten
  , argInfoFreeVariables = UnknownFVs
  }


createHiddenTelescope :: TT.Name -> PTerm -> Telescope
createHiddenTelescope name pterm = [TBind NoRange bargs (itaTerm pterm)]
  where bargs = [barg]
        barg = hArg (itaName name) (mkBoundName_ (itaName name))

itaTerm :: PTerm -> Expr
itaTerm (PRef range highlightRange name) = iden $ Main.prettyName name
itaTerm whole@(PApp range fst args) = itaApp whole 0
itaTerm (PPi plicity name fc term1 term2) = itaPi plicity name term1 term2
itaTerm (PConstSugar fc term) = itaTerm term
itaTerm (PConstant fc const) = Lit (itaConst const)
itaTerm (PAlternative namePair alttype terms) = case length terms of
  1 -> itaTerm $ head terms -- Safe because of case stmt.
  0 -> undefined
  _ -> itaTerm $ head terms -- TODO Probably wrong. But it's safe at least
  -- TODO START HERE
  -- TODO Here I need to specify the universe levels
  -- I need info which is not present in the Idris AST.
  -- Should I modify it or provide that info in another way?
  -- Would be possible to re-define `PType fc` as
  -- `PType fc (Maybe (Either Int Iden))` or similar.
itaTerm (PType fc) = Set NoRange -- Simple `Set`
-- itaTerm (PType fc) = SetN NoRange n -- Specific level of `Set_n`
                     -- where n = 2
-- itaTerm (PType fc) = RawApp NoRange [Set NoRange, iden "level" ] -- Level var
itaTerm (PIfThenElse fc ift thent elset) = undefined
  -- TODO Pairs are not native in Agda. But are defined in StdLib
  -- TODO The hole comment is not used yet.
itaTerm (PPair fc fcs puninfo termA termB) = hole "Pairs are not defined in Agda"
  -- This should return a hole.
itaTerm (PMetavar _ _) = hole "Comment"
itaTerm (PQuote raw) = undefined
itaTerm (PInferRef fc fcs name) = undefined
itaTerm (PPatvar fc name) = undefined
itaTerm (PLam fc name fc2 term1 term2) = Lam NoRange lams expr
  -- TODO DomainFull
  -- TODO This is most likely wrong
  where lams = [DomainFree (args)]
        args = hArg (itaName name) (mkBoundName_ (itaName name))
        expr = itaTerm term2
  -- Agda  ex: @\\x {y} -> e@ or @\\(x:A){y:B} -> e@
  -- Idris A lambda abstraction. Second FC is name span.
itaTerm (PLet fc rigcount name fc2 term1 term2 term3) = hole "Let-bindings are not supported yet"
  -- Let Range [Declaration] (Maybe Expr)
  -- ex: @let Ds in e@, missing body when parsing do-notation let

  -- A Typed expression has no representation in Agda
  -- I Could translate it into a let/where binding.
  -- TODO
itaTerm (PTyped pterm1 pterm2) = hole "Typed bindings are not defined in Agda"
itaTerm (PWithApp fc pterm1 pterm2) = undefined
itaTerm (PAppImpl pterm implicitinfo) = undefined
itaTerm (PAppBind fc pterm pargs) = undefined
itaTerm (PMatchApp fc name) = undefined
itaTerm (PCase fc pterm1 pterm2) = undefined
itaTerm (PTrue fc puninfo) = undefined
itaTerm (PResolveTC fc) = undefined
itaTerm (PRewrite fc mname pterm1 pterm2 mpterm) = undefined
itaTerm (PDPair fc fcs puninfo term1 term3 term2) = undefined
itaTerm (PAs fc name term) = undefined
itaTerm (PHidden term) = undefined
itaTerm (PUniverse fc universe) = undefined
itaTerm (PGoal fc term name pterm) = undefined
itaTerm (Idris.AbsSyntax.Placeholder) = undefined
itaTerm (PDoBlock pdo) = undefined
itaTerm (PIdiom fc term) = undefined
itaTerm (PProof ptactics) = undefined
itaTerm (PTactics ptactics) = undefined
itaTerm (PElabError err) = undefined
itaTerm (PImpossible) = undefined
itaTerm (PCoerced term) = undefined
itaTerm (PDisamb strings term) = undefined
itaTerm (PUnifyLog term) = undefined
itaTerm (PNoImplicits term) = undefined
itaTerm (PQuasiquote term mpterm) = undefined
itaTerm (PUnquote term) = undefined
itaTerm (PQuoteName name bool fc) = undefined
itaTerm (PRunElab fc term strings) = undefined

-- Hack for 'fromInteger'
-- Deep pattern matching is bad form. But this is a ugly hack any way.
-- This seems to work but is ugly. Should be done in a pre-processing step to
-- remove all Idris quirks.
-- Also puts a pair of parenthesis around repeated function applications.
itaApp :: PTerm -> Int -> Expr
itaApp (PApp range fst@(PRef _ _ name) args) depth =
  if (Main.prettyName name) == "fromInteger"
  then head $ (map (itaArgs 0) args)
  else case depth of
    0 -> RawApp NoRange ((itaTerm fst) : (map (itaArgs 1) args))
    d -> paren $ RawApp NoRange ((itaTerm fst) : (map (itaArgs d) args))
  -- This is a fix for an issue which occured when developing tttPTerm.
  -- Previously I assumed that this case never happend.
itaApp (PApp range fst args) depth =
    RawApp NoRange ((itaTerm fst) : (map (itaArgs 1) args))

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
itaConst (TT.AType arithty) = itaAtype arithty
itaConst TT.StrType = undefined
itaConst TT.WorldType = undefined
itaConst TT.TheWorld = undefined
itaConst TT.VoidType = undefined
itaConst TT.Forgot = undefined

-- Arithmethic types?
-- Probably not. More like constants in types?
-- Or handles builtin types.
itaAtype :: TT.ArithTy -> Literal
itaAtype (TT.ATInt (TT.ITFixed nt)) = undefined
itaAtype (TT.ATInt TT.ITNative) = undefined
itaAtype (TT.ATInt TT.ITBig) = undefined
-- Char is a builtin type in Idris.
-- itaAtype (TT.ATInt TT.ITChar) = undefined
itaAtype (TT.ATInt TT.ITChar) = LitQName NoRange
  (AAbstract.QName (AAbstract.MName []) charName)
  where charName =
         AAbstract.Name { AAbstract.nameId          = NameId 1 1
                        , AAbstract.nameConcrete    = mkName "Char"
                        , AAbstract.nameBindingSite = NoRange
                        , AAbstract.nameFixity      = mkFixity
                        , AAbstract.nameIsRecordName = False
                        }
itaAtype (TT.ATFloat) = undefined

itaName :: TT.Name -> Name
-- itaName n = mkName $ prettyName n
itaName n = mkName $ show n

-- TODO This is not strictly correct. But for 'RawApp' which only take Exprs as
-- args it's okay
-- type PArg = PArg' PTerm -- getTm :: PTerm
-- This also has the depth hack to make sure we only put parenthesis when needed.
-- But the logic is probably not correct
itaArgs :: Int -> PArg -> Expr
itaArgs depth (PExp prio argopts pname getTm) =
  if (isPApp getTm)
     then itaApp getTm depth
     else itaTerm getTm
itaArgs depth (PImp priority machine_info argopts pname getTm) = 
  if (isPApp getTm)
     then itaApp getTm depth
     else itaTerm getTm
itaArgs depth (PConstraint priority argopts pnam getTm) = 
  if (isPApp getTm)
     then itaApp getTm depth
     else itaTerm getTm
itaArgs depth (PTacImplicit priority argopts pname getScript getTm) = 
  if (isPApp getTm)
     then itaApp getTm depth
     else itaTerm getTm

isPApp :: PTerm -> Bool
isPApp (PApp _ _ _) = True
isPApp _ = False

addComment = (++)
  -- There is also AbsStyntaxTree.prettyName but it's harder to use.
prettyName :: TT.Name -> String
prettyName (TT.UN name) =  Text.unpack name
prettyName (TT.MN id name) =  addComment
  ("Machine chosen name with id: " ++ (show id)) $ Text.unpack name
prettyName (TT.NS ns names) = concat $ intersperse "." $
  (map Text.unpack names) ++ [Main.prettyName ns]
  -- Names similar to "_t_0" are machine generated
-- prettyName (TT.SN sn) = addComment "Decorated function name" $ prettySN sn
-- prettyName (TT.SymRef id) = addComment "Reference to IBC" $ show id
prettyName _ = undefined

isMN :: TT.Name -> Bool
isMN (TT.MN _ _) = True
isMN _ = False

prettySN :: TT.SpecialName -> String
prettySN (TT.WhereN a b c) = "WhereN"
prettySN (TT.WithN a b) = "WithN"
prettySN (TT.ImplementationN a b) = "ImplementationN"
prettySN (TT.ParentN a b) = "ParentN"
prettySN (TT.MethodN a) = "MethodN"
prettySN (TT.CaseN a b) = "CaseN"
prettySN (TT.ImplementationCtorN a) = "ImplementationCtorN"
prettySN (TT.MetaN a b) = "MetaN"



--------------------------------------------------------------------------------
-- Command line interface

main :: IO ()
main = getArgs >>= parse >>= runITA

  -- Bool if to run implToExcpl TODO
parse :: [String] -> IO (FilePath, Maybe FilePath, Bool)
parse ["-h"] = usage >> exit
parse ["-v"] = version >> exit
parse [] = usage >> exit
parse ["-o", outfile, infile] = return (infile, Just outfile, False)
parse [infile] = return (infile, Nothing, False)
parse ["-f", infile] = return (infile, Nothing, True)

usage = putStrLn "Usage: ita [-vhf] [-o outfile.agda] [infile.idr]\n  -f Runs Impl to Excpl"
version = putStrLn ("IdrisToAgda 0.2-" ++ gitHash)
exit = exitWith ExitSuccess
die = exitWith (ExitFailure 1)
success outfile =
  putStrLn ("Successfuly compiled. Output written to: " ++ outfile) >> exit
errorMsg :: FilePath -> TT.Err -> IO ()
errorMsg infile err =
  putStrLn ("Error while compiling file: " ++ infile ++ "\n\n" ++
            (show err)) >> Main.die

runITA :: (FilePath, Maybe FilePath, Bool) -> IO ()
runITA (infile, outfile, impl) =
  do idrAST <- runIdr $ loadIdr infile >> parseF impl
     case idrAST of
       Left err -> errorMsg infile err
       Right idr -> let agda = prettyShow $ itaDecls idr in
         case outfile of
            Just outfilename -> writeFile outfilename agda >> success outfilename
            Nothing -> putStrLn agda

--------------------------------------------------------------------------------
-- Test interface for implementation

iPrint a = liftIO (print a)

runIdr :: Idris a -> IO (Either TT.Err a)
runIdr a = runExceptT (evalStateT a idrisInit)

printAgda d = putStrLn $ prettyShow d

-- test = do res <- runIdr $ loadIdr f >> parseF True
test = do res <- runIdr $ loadIdr f >> parseF True
          case res of
              Right pd -> putStrLn $ prettyShow $ itaDecls pd
              Left err -> putStrLn $ show err
  -- where f = "Blodwen/src/Core/Primitives.idr"
  -- where f = "../IdrisLibs/SequentialDecisionProblems/CoreTheory.lidr"
  -- where f = "../IdrisLibs/SequentialDecisionProblems/FullTheory.lidr"
  -- where f = "Idris-dev/test/basic001/basic001a.idr"
  -- where f = "Idris-dev/libs/prelude/Prelude/Algebra.idr"
  where f = "Idris-dev/test/basic003/test027.idr "
  -- where f = "patrik.idr" -- [working 2019-08-15]
  -- where f = "simpleIdris.idr"
  -- where f = "simpleIdrisImpl.idr"
  -- where f = "exImpIdr.idr"

getDefinitions c = Map.keys $ definitions c

loadIdr :: FilePath -> Idris ()
loadIdr f = do
        -- TODO load user defined libraries in the same package
        -- Will I ever do this really?
  
        -- This is a Workaround for not importing user defined libraries correctly
        -- I probably need to load the current directory as well. And maybe the
        -- whole project structure
        -- From IBC.hs:149
            -- -- | Load an entire package from its index file
            -- loadPkgIndex :: PkgName -> Idris ()
            -- loadPkgIndex pkg = do ddir <- runIO getIdrisLibDir
            --                       addImportDir (ddir </> unPkgName pkg)
            --                       fp <- findPkgIndex pkg
            --                      loadIBC True IBC_Building fp
        let pkgdirs = ["../IdrisLibs"]
        setImportDirs pkgdirs

        -- Load StdLib
        elabPrims
        addPkg "prelude"
        addPkg "base"
        loadModule "Builtins" (IBC_REPL False)
        addAutoImport "Builtins"
        loadModule "Prelude" (IBC_REPL False)
        addAutoImport "Prelude"

        loadModule f $ IBC_REPL True
        return ()
  where addPkg :: String -> Idris ()
        addPkg p = do ddir <- runIO getIdrisLibDir
                      addImportDir (ddir </> p)
                      addIBC (IBCImportDir (ddir </> p))

  -- Bool arg is if to run implToExcpl
parseF :: Bool -> Idris [PDecl]
parseF impex = do
        i <- getIState
        -- all definitions in scope including prelude, libraries, etc.
        let defs = definitions $ tt_ctxt i
        -- Find the user defined names
        let names = udNames (ast i)
        -- Drop the namespace
        let names' = map nn names

        let uDefs = Map.filterWithKey (\k v -> elem k names') defs
        -- I don't know if this is a good idea, probably not. We are loosing the
        -- order of the source.
        let spm = splitASTMap (ast i)
        let uDefs' = flattenMap uDefs
        let ptt = Map.intersectionWith (\a b -> (a, b)) spm uDefs'

        iPrint ("Userdefined TTDecls: " ++ (show $ length uDefs))
        iPrint ("Length PDecl: " ++ (show $ length (ast i)))
  -- TODO START HERE
  -- This seem to work okay for now.
  -- Now I only need to map each item in `uDefs` to each in `sp`
  -- They should match one-to-one
        iPrint ("Length splitAST: " ++ (show $ length sp))
        iPrint ("Length joined again splitAST: " ++ (show $ length $ concat sp))

        -- implEcplRefactor :: Map.Map TT.Name (AST, TTDecl) -> AST
        let reAST = implEcplRefactor ptt

        case impex of
          True -> return reAST
          False -> return (ast i)
  where nn :: TT.Name -> TT.Name
        nn n@(TT.UN name) = n
        nn n@(TT.MN id name) = n
        nn n@(TT.NS ns names) = ns
        nn _ = undefined
        flattenMap :: Map.Map TT.Name (Map.Map TT.Name TTDecl) -> Map.Map TT.Name TTDecl
        flattenMap map = foldr (\e m -> Map.union e m) Map.empty map

-- The explicit signature in Idris is:
    -- cc : {g : Type} -> {a : N} -> {b : N} -> Vec g a -> Vec g b -> Vec g (add a b)
-- The implicit signature in Idris is:
    -- cc :  Vec g a -> Vec g b -> Vec g (add a b)
-- The extracted signature from Term/Def/TT is: (When run on the implicit variant)
    -- {b : Main.N} -> {a : Main.N} -> {g : Type ./simpleIdrisImpl.idr.h4} -> Main.Vec g a -> Main.Vec g b -> Main.Vec g (Main.add a b)
-- Cleanly that is:
    -- {b : N} -> {a : N} -> {g : Type} -> Vec g a -> Vec g b -> Vec g (add a b)


--------------------------------------------------------------------------------
-- TT Translation

-- Maybe I need to cut up the [PDecl] ast into several, one for each definition.
-- Then I can match thoose to TT defs, refactor and then splice them together to
-- one AST again.
-- The plan is to do exactly that.


-- TODO START HERE
-- I should do this differently. I should traverse the `AST` and when needed (an
-- argument, or `Type`) I should get the Info I need for the transform from the
-- TT.

-- TODO This is what I want to do.
implEcplRefactor2 :: AST -> Map.Map TT.Name TTDecl -> AST
implEcplRefactor2 ast tt = undefined
  -- Somehow traverse the AST untill I find an argument.

  
-- implEcplRefactor :: (AST, TTDecl) -> PDecl
implEcplRefactor :: Map.Map TT.Name (AST, TTDecl) -> AST
implEcplRefactor defs = concat $ map snd $ Map.toList $ implEcplRefactor' defs

implEcplRefactor' :: Map.Map TT.Name (AST, TTDecl) -> Map.Map TT.Name AST
implEcplRefactor' defs = fmap ttTypeInPDecl defs


-- There is probably a bug in here. Or the translations don't work somehow.
-- Probably trans is never Just
ttTypeInPDecl :: (AST, TTDecl) -> AST
ttTypeInPDecl (ast, tt) = maybe ast (replaceTypeSig ast) trans
  where def = getDef tt
        ty = getTTType def
        trans = ty >>= tttPDecl

-- 1st arg: list of declarations (probably patterns in a fundecl) from parsing
-- 2nd arg: transformed type signature (with added explicit implicit arguments)
-- result:  list of refactored declarations
replaceTypeSig :: AST -> PDecl -> AST  -- AST = [PDecl]
replaceTypeSig ast newTySig = maybe ast fn pty
  where (pty, rest) = findPTy ast
        fn = (\_oldtysig -> newTySig : rest)


findPTy :: AST -> (Maybe PDecl, AST)
findPTy ast = (pty, filter (not . isPTy) ast)
  where pty = find isPTy ast
        isPTy (PTy _ _ _ _ _ _ _ _) = True
        isPTy (PData _ _ _ _ types
          (PDatadecl nameIdr _ typeconstructor dataconstructors)) = False
        isPTy _ = False

-- itaDecl (PTy doc names synInfo range fnopts nameIdr rangeName terms) = -- Type declaration

-- getDef :: TTDecl -> Def
-- getTTType :: Def -> Maybe TT.Type
-- tttPDecl :: TT.Type -> PDecl

-- Match definitions in TT with the statements in PDecl
-- matchPDeclTT :: AST -> Map.Map TT.Name (Map.Map TT.Name TTDecl) -> (AST, TTDecl)
-- matchPDeclTT pdecls defs = undefined


joinASTtoTT :: [AST] -> [TTDecl] -> [(AST,TTDecl)]
joinASTtoTT asts tts = undefined

-- Again, doing this with maps does not preserve the source order. But it is
-- maybe the sane way of doing it. I may need to keep track of the source
-- position of each statement and restore it after transformation.
joinASTtoTTMap :: (Map.Map TT.Name AST) -> (Map.Map TT.Name TTDecl) ->
                  Map.Map TT.Name (AST, TTDecl)
joinASTtoTTMap ast tt = Map.intersectionWith (\a b -> (a, b)) ast tt

-- TODO This does not retain statements which do not have a name. It also does
-- not preserve the order of statements, but that is not important. The first
-- point is however.
splitAST :: AST -> [AST]
splitAST pdecls = map (getStmts pdecls) names
  where names = nub $ Maybe.catMaybes $ map getDeclName pdecls

splitASTMap :: AST -> Map.Map TT.Name AST
splitASTMap pdecls = foldr
                   (\name m -> Map.insert name (getStmts pdecls name) m)
                   Map.empty
                   names
  where names = nub $ Maybe.catMaybes $ map getDeclName pdecls

-- Returns all statments with the specified name
getStmts :: AST -> TT.Name -> AST
getStmts ast name = filter (\e -> getDeclName e == Just name) ast

getDeclName :: PDecl -> Maybe TT.Name
getDeclName (PData doc names synInfo range types
          (PDatadecl nameIdr _ typeconstructor dataconstructors)) = Just nameIdr
getDeclName (PTy doc names synInfo range fnopts nameIdr rangeName terms) = Just nameIdr
getDeclName (PClauses range fnopts name clauses) = Just name
getDeclName (PFix fc fixIdr strings) = Nothing
getDeclName (PPostulate _ _ _ _ _ _ name _) = Just name
getDeclName (PCAF _ name _) = Just name
getDeclName (PParams _ _ _) = Nothing
getDeclName (PNamespace _ _ _) = Nothing
getDeclName (PRecord _ _ _ _ name _ _ _ _ _ _ _) = Just name
getDeclName (PInterface _ _ _ _ name _ _ _ _ _ _ _) = Just name
getDeclName (PImplementation _ _ _ _ _ _ _ _ _ _ _ _ _ name _) = name
getDeclName (PDSL name _) = Just name
getDeclName (PSyntax _ _) = Nothing
getDeclName (PMutual _ _) = Nothing
getDeclName (PDirective _) = Nothing
getDeclName (PProvider _ _ _ _ _ name) = Just name
getDeclName (PTransform _ _ _ _) = Nothing
getDeclName (PRunElabDecl _ _ _) = Nothing



-- Find user defined names of type declarations.
udNames :: [PDecl] -> [TT.Name]
udNames pd = concat $ map udName pd

udName :: PDecl -> [TT.Name]
-- TODO Add the Agda {a : Set} argument declaration
udName (PData doc names synInfo range types
          (PDatadecl nameIdr _ typeconstructor dataconstructors)) =
    [nameIdr]
udName (PTy doc names synInfo range fnopts nameIdr rangeName terms) = -- Type declaration
  [nameIdr]
udName _ = []

-------------------------------------------------------------------------------
-- TT to PDecl translation

-- TODO This should never be used anymore.
ttLookup :: TT.Name -> TT.Ctxt TTDecl -> Maybe TTDecl
ttLookup name ctxt = (Map.lookup name (Maybe.fromJust (Map.lookup name ctxt)))

getDef :: TTDecl -> Def
getDef (def, _, _, _, _, _)  = def

-- This never returns Just
-- Maybe this should be:
-- getTTType :: Def -> Maybe PDecl
getTTType :: Def -> Maybe TT.Type
  -- Use `vToP` to translate all de Bruijn-indicies into the name the reference.
  -- TODO It should be threaded in some where else.
getTTType (TyDecl nametype ty) = Just (TT.vToP ty)
getTTType (Function ty te) = Just (TT.vToP ty)
getTTType (Operator _ _ _) = undefined
  -- TODO Find out what I should return here
getTTType (CaseOp caseInfo ty argTypes origDef simplifedDef cases) =
  Just (TT.vToP ty)

-- | Terms in the core language. The type parameter is the type of
-- identifiers used for bindings and explicit named references;
-- usually we use @TT 'Name'@.
-- data TT n = P NameType n (TT n) -- ^ named references with type
--             -- (P for "Parameter", motivated by McKinna and Pollack's
--             -- Pure Type Systems Formalized)
--           | V !Int -- ^ a resolved de Bruijn-indexed variable
--           | Bind n !(Binder (TT n)) (TT n) -- ^ a binding
--           | App (AppStatus n) !(TT n) (TT n) -- ^ function, function type, arg
--           | Constant Const -- ^ constant
--           | Proj (TT n) !Int -- ^ argument projection; runtime only
--                              -- (-1) is a special case for 'subtract one from BI'
--           | Erased -- ^ an erased term
--           | Impossible -- ^ special case for totality checking
--           | Inferred (TT n) -- ^ For building case trees when coverage checkimg only.
--                             -- Marks a term as being inferred by the machine, rather than
--                             -- given by the programmer
--           | TType UExp -- ^ the type of types at some level
--           | UType Universe -- ^ Uniqueness type universe (disjoint from TType)

tName :: TT.Name
tName = TT.UN "TESTNAME"

apparg :: TT.Name
apparg = TT.UN "__app_arg"
  
showU :: TT.UExp -> String
showU (TT.UVar s l) = "Universe variable number: " ++ show l ++ " in file: " ++ show s
showU (TT.UVal l) = "Universe level: " ++ show l
  
tttPDecl :: TT.Type -> Maybe PDecl
tttPDecl (TT.P nametype n tt) = tttPDecl tt
tttPDecl (TT.V i) = undefined
tttPDecl b@(TT.Bind n binder tt) = Just
  -- TODO Critical BUG
  -- `n` is not the right name in the first position.
  -- Possibly the correct name is not here on this level. Correct. 
  -- This is everything that comes after `test :`
  -- But PTy is on a higher level
  -- The name is not contained here at all. It is stored in the context/map a
  -- lot above. TT and PDecl is very different levels
  -- I need to do something completly different.
  -- It is really stupid of me to stop here. There is a big task ahead, not a
  -- small one. Just do it! Jump right in!
  (PTy emptyDocstring [] defaultSyntax TT.NoFC [] tName TT.NoFC (tttPBind n binder tt))
  -- TODO This won't work
-- tttPDecl (TT.App appstatus tt1 tt2) = Just $ PApp TT.NoFC (tttPTerm tt1) [PExp 0 [] apparg (tttPTerm tt2)]
tttPDecl (TT.App appstatus tt1 tt2) = undefined
tttPDecl (TT.Constant const) = undefined
tttPDecl (TT.Proj tt i) = undefined
tttPDecl (TT.Erased) = Nothing
tttPDecl (TT.Impossible) = undefined
tttPDecl (TT.Inferred tt) = undefined
tttPDecl (TT.TType uexp) = trace ("### tttPDecl TT.TType: " ++ showU uexp) Nothing -- TODO Translate universe levels here!
    -- data UExp = UVar String Int -- ^ universe variable, with source file to
    --                                  disambiguate
    --           | UVal Int -- ^ explicit universe level
tttPDecl (TT.UType universe) = undefined

tttPTerm :: TT.Type -> PTerm
  -- P is a named reference with type. NameType can be Bound, Ref, DCon for data
  -- constructor or TCon for type constructor
tttPTerm (TT.P nametype n tt) = PRef TT.NoFC [] n
  -- A resolved de Bruijn-indexed variable
tttPTerm (TT.V i) = undefined -- TODO Here I need to look up the name in some cxt
  -- A binding
tttPTerm (TT.Bind n binder tt) = tttPBind n binder tt
  -- Application : function, function type and arg
  -- Right now this causes a crash in `ita`. I assumed that the first term in
  -- application was always a `ref`. This is of course wrong. Maybe it should be
  -- fixed in `itaApp`?
tttPTerm (TT.App appstatus tt1 tt2) = PApp TT.NoFC (tttPTerm tt1) [tttPArg tt2]
tttPTerm (TT.Constant const) = undefined
tttPTerm (TT.Proj tt i) = undefined
tttPTerm (TT.Erased) = undefined
tttPTerm (TT.Impossible) = undefined
tttPTerm (TT.Inferred tt) = undefined
  -- The type of types at some level `u`
  -- TODO Translate universe levels here!
  -- TODO This is it!
  -- u : UExp is eiter an explicit level or a variable with source file
tttPTerm (TT.TType u) = trace ("###tttPTerm TType of level: " ++ showU u) (tttU u)
tttPTerm (TT.UType universe) = undefined


  -- TODO There is a bug where i put things in the wrong order.
  -- `add : N -> N -> N`
  -- becomes `__pi_arg : Main.N -> (__pi_arg : Main.N) -> Main.N`
  
-- In parsed Idris this is always `PExp` for function application. Which is the
-- case here. This is not a general Type -> PArg. It only works for function app
tttPArg :: TT.Type -> PArg
tttPArg (TT.P nametype name term) = 
    PExp { priority = 1 , argopts = [] , pname = name , getTm = tttPTerm term }
  -- How to transform de Bruijn-indices back to names?
  -- I need more context.
  -- Look at the function `vToP` in TT.hs:120
  -- It should work now I think with `vToP`
tttPArg (TT.V v) = undefined
tttPArg (TT.Bind name binder term) = 
    PExp { priority = 1 , argopts = [] , pname = name , getTm = tttPTerm term }
  -- TODO
  -- This wont work. In TT The bound/implicit/explicit arg definition is
  -- separate from the function application. In PTerm they are connected.
  -- I probably need to do something else. Just looking a bit in to TT when
  -- transforming tydecls.
tttPArg (TT.App appstatus tt1 tt2) = PExp
  { priority = 1
  , argopts = []
  , pname = undefined -- This is probably a machinegenerated name
                      -- It seems to not be used.
  , getTm = PApp TT.NoFC (tttPTerm tt1) [tttPArg tt2]
  }
tttPArg (TT.Constant const) = undefined
tttPArg (TT.Proj tt i) = undefined
tttPArg (TT.Erased) = undefined
tttPArg (TT.Impossible) = undefined
tttPArg (TT.Inferred tt) = undefined
tttPArg (TT.TType u) = undefined
tttPArg (TT.UType universe) = undefined

tttU :: TT.UExp -> PTerm
tttU _ = PType TT.NoFC

  -- | HiddenArg Range (Named_ Expr)              -- ^ ex: @{e}@ or @{x=e}@
tttPBind :: TT.Name -> TT.Binder (TT.Term) -> TT.Term -> PTerm
  -- PPi _ n fc t1 t2 = (n : t1) -> t2
tttPBind name b@(TT.Pi rigCount implI ty kind) term = PPi plicity name TT.NoFC t1 t2
-- itaPi :: Plicity -> TT.Name -> PTerm -> PTerm -> Expr
-- itaPi (Exp pargopts pstatic pparam pcount) name term1 term2 =
-- itaPi q@(Imp pargopts pstatic pparam pscoped pinsource pcount) name term1 term2 =
  where plicity :: Plicity
        plicity = case implI of
            Just _ -> (Imp undefined undefined undefined undefined undefined undefined)
            Nothing -> (Exp undefined undefined undefined undefined)
        t1 = tttPTerm ty
        t2 = tttPTerm term
tttPBind _ _ _ = undefined


-- This below converts to Agda, which is not what I want.
tttExpr :: TT.Type -> IO Expr
tttExpr (TT.P nametype n tt) = do putStrLn $ "P " ++ show n
                                  tttExpr tt
tttExpr (TT.V i) = do putStrLn $ "V " ++ (show i)
                      return (lit $ fromIntegral i)
tttExpr (TT.Bind n binder tt) = do putStrLn $ "Bind " ++ show n
                                   tttaBind n binder tt
                                   -- tttPDecl tt
tttExpr (TT.App appstatus tt1 tt2) = do putStrLn "App"
                                        a <- tttExpr tt1
                                        b <- tttExpr tt2
                                        return $ application a [b]

tttExpr (TT.Constant const) = undefined
tttExpr (TT.Proj tt i) = undefined
tttExpr (TT.Erased) = undefined
tttExpr (TT.Impossible) = undefined
tttExpr (TT.Inferred tt) = undefined
tttExpr t@(TT.TType uexp) = do putStrLn $ "TType" ++ (showU uexp)
                               return (iden $ showU uexp)
tttExpr (TT.UType universe) = undefined

itaTTNameType :: TT.NameType -> Name
itaTTNameType (TT.Ref) = undefined
itaTTNameType _ = undefined

impl :: TT.ImplicitInfo -> Bool
impl (TT.Impl tc toplevel machine_gen) = toplevel

  -- | HiddenArg Range (Named_ Expr)              -- ^ ex: @{e}@ or @{x=e}@
tttaBind :: TT.Name -> TT.Binder (TT.Term) -> TT.Term -> IO Expr
tttaBind name b@(TT.Pi rigCount implI ty kind) term =
  do putStrLn $ "Binder: "  ++ (show b)
     putStrLn $ show (itaName name)
     putStrLn $ show (name)
     -- putStrLn $ show ty
     -- putStrLn "Binder term:"
     -- putStrLn $ show term
     -- putStrLn "End Binder"
     t <- tttExpr ty
     ter <- tttExpr term
     case implI of
       Just _ -> return (Fun NoRange (Arg argInfo (hiddenArg (itaName name) t)) ter)
       Nothing -> return (Fun NoRange (Arg argInfo t) ter)
  -- Fun NoRange (Arg argInfo (itaTerm term1)) (itaTerm term2)
  where argInfo = (ArgInfo NotHidden defaultModality UserWritten UnknownFVs)
tttaBind _ _ _ = undefined

-- data TT n = P NameType n (TT n) -- ^ named references with type
--             -- (P for "Parameter", motivated by McKinna and Pollack's
--             -- Pure Type Systems Formalized)
--           | V !Int -- ^ a resolved de Bruijn-indexed variable
--           | Bind n !(Binder (TT n)) (TT n) -- ^ a binding
--           | App (AppStatus n) !(TT n) (TT n) -- ^ function, function type, arg
--           | Constant Const -- ^ constant
--           | Proj (TT n) !Int -- ^ argument projection; runtime only
--                              -- (-1) is a special case for 'subtract one from BI'
--           | Erased -- ^ an erased term
--           | Impossible -- ^ special case for totality checking
--           | Inferred (TT n) -- ^ For building case trees when coverage checkimg only.
--                             -- Marks a term as being inferred by the machine, rather than
--                             -- given by the programmer
--           | TType UExp -- ^ the type of types at some level
  -- TODO start with universe here!
--           | UType Universe -- ^ Uniqueness type universe (disjoint from TType)
--   deriving (Ord, Functor, Data, Generic, Typeable)

-- data Def = Function !Type !Term
--          | TyDecl NameType !Type
--          | Operator Type Int ([Value] -> Maybe Value)
--          | CaseOp CaseInfo
--                   !Type
--                   ![(Type, Bool)] -- argument types, whether canonical
--                   ![Either Term (Term, Term)] -- original definition
--                   ![([Name], Term, Term)] -- simplified for totality check definition
--                   !CaseDefs
-- data CaseDefs = CaseDefs {
--                   cases_compiletime :: !([Name], SC),
--                   cases_runtime :: !([Name], SC)

-- type Term = TT Name
-- type Type = Term
-- -- | Terms in the core language. The type parameter is the type of
-- -- identifiers used for bindings and explicit named references;
-- -- usually we use @TT 'Name'@.

--------------------------------------------------------------------------------
-- Agda constructors

mkName :: String -> Name
mkName n = Name NoRange InScope [(Id n)]
qname :: String -> QName
qname n = QName $ mkName n

iden n = Ident $ qname n

lit :: Integer -> Expr
lit i = Lit $ LitNat NoRange i

modality :: Modality
modality = Modality Relevant defaultQuantity
-- modality = Modality Relevant Quantity0
-- defaultModality = Modality defaultRelevance defaultQuantity

arg :: String -> a -> NamedArg a
arg name expr
  -- TODO Should there be a special case for "__pi_arg"?
  | name == "__pi_arg" = defaultNamedArg expr
  | otherwise = Arg (ArgInfo NotHidden modality UserWritten UnknownFVs) $
  Named (Just (Ranged NoRange name)) expr

-- TODO Fix the mess of several functions called hiddenArg/hArg.
-- I'm sure I fixed this but it has been lost somehow.
hArg :: Name -> a -> NamedArg a
hArg name expr = Arg (ArgInfo Hidden modality UserWritten UnknownFVs) $
  Named (Just (Ranged NoRange (prettyShow name))) expr
-- data ArgInfo = ArgInfo
--   { argInfoHiding        :: Hiding
--   , argInfoModality      :: Modality
--   , argInfoOrigin        :: Origin
--   , argInfoFreeVariables :: FreeVariables
--   } deriving (Data, Eq, Ord, Show)

-- defaultArgInfo =  ArgInfo
--   { argInfoHiding        = NotHidden
--   , argInfoModality      = defaultModality
--   , argInfoOrigin        = UserWritten
--   , argInfoFreeVariables = UnknownFVs


  -- Only hidden arguments can have names in Agda
-- hiddenArg :: String -> a -> NamedArg a
-- hiddenArg name expr = Arg (ArgInfo Hidden modality UserWritten UnknownFVs) $
--   Named (Just (Ranged NoRange name)) expr

-- type Named_ = Named RString
  -- | HiddenArg Range (Named_ Expr)              -- ^ ex: @{e}@ or @{x=e}@
hiddenArg :: Name -> Expr -> Expr
hiddenArg n e = HiddenArg NoRange name
  where name = Named (Just (Ranged NoRange (prettyShow n))) e

funcExpr :: String -> Expr -> Expr
funcExpr name body = Fun NoRange (Arg argInfo (iden name)) body
  where argInfo = (ArgInfo NotHidden modality UserWritten UnknownFVs)

hole :: String -> Expr
hole comment = QuestionMark NoRange Nothing

typesig :: Name -> Expr -> TypeSignature
typesig name body = TypeSig argInfo name body
  where argInfo = ArgInfo NotHidden modality UserWritten UnknownFVs
        expr = funcExpr "argTest" (iden "FunctionTest")

mkFixity :: Fixity'
mkFixity = Fixity' f not NoRange
  where f = Fixity NoRange Unrelated NonAssoc
        rstring = Ranged NoRange "RawName"
        not = [IdPart rstring]
