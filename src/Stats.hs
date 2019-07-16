module Stats where

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
import Idris.ElabDecls (elabDecls)
import Idris.AbsSyntax
import Idris.AbsSyntaxTree
import Idris.Docstrings
import Idris.Unlit
import Idris.Error (tclift)
import Idris.IBC
import qualified Idris.Core.TT as TT

import Util.System (readSource)

import Data.List (intersperse)
import Data.Either (fromLeft, fromRight)
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Data as Data
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State.Strict (evalStateT, execStateT, runStateT)
import Control.Monad.Trans (lift, liftIO)

import System.Environment
import System.Exit

main :: IO ()
main = getArgs >>= parseCLIOpts >>= parseIdr

parseCLIOpts :: [String] -> IO (FilePath)
parseCLIOpts ["-h"] = usage >> exit
parseCLIOpts ["-v"] = version >> exit
parseCLIOpts [] = usage >> exit
parseCLIOpts [infile] = return infile

usage = putStrLn "Usage: stats [-vh] [infile.idr]"
version = putStrLn "IdrisToAgda Stats tool 0.1"
exit = exitWith ExitSuccess
die = exitWith (ExitFailure 1)
success outfile =
  putStrLn ("Successfuly compiled. Output written to: " ++ outfile) >> exit
error :: FilePath -> ParseError -> IO ()
error infile err =
  putStrLn ("Error while compiling file: " ++ infile ++ "\n\n" ++
            (prettyError err)) >> Stats.die



testParse :: FilePath -> String -> Either ParseError [PDecl]
testParse filepath file = runparser (prog defaultSyntax) idrisInit filepath file

-- testParse' :: FilePath -> String -> Either ParseError [PDecl]
-- testParse' filepath file = parseProg defaultSyntax filepath file Nothing

showErr :: (Show a) => IO (Either TT.Err a) -> IO ()
showErr a = do q <- a
               case q of
                 Right q -> putStrLn "Success"
                 Left w -> print w

tk a = showErr $ runExceptT (evalStateT a idrisInit)

iPrint a = liftIO $ print a

tl = do (file :: String) <- liftIO $ readSource f
  -- case P.parse (runWriterT (evalStateT p i)) inputname s of
        -- let fileR = Regex.subRegex "import.*$" file ""

        -- q <- parseImports f file
                  -- file <- if lidr then tclift $ unlit f file_in else return file_in
        -- let (c, d, e, f) = q
        -- let res = tp

        -- let a = res file
        -- b <- runExceptT a
        -- let (c, d, e, f) = fromRight undefined b
        -- iPrint c
-- TODO This fails because idrisDataDir env var is not set. Probably since I'm
-- not building Idris in the normal way. I should write something about this in
-- the report. But it's more about the SE side of things.
--
-- I'm trying to run the Idris/Setup.hs script in the new root folder. It may work
-- I should talk about this, in the report.
--
-- It fails in idris/src/IRTS/System.hs
        loadModule f IBC_Building
        -- loadSource False f Nothing
        return ()
  -- where f = "Blodwen/src/Core/Primitives.idr"
  -- where f = "../IdrisLibs/SequentialDecisionProblems/CoreTheory.lidr"
  where f = "Idris-dev/test/basic001/basic001a.idr"
        lidr = False
        mark = Nothing
        res i = evalStateT (parseImports f i) init
        init = idrisInit
        parser = (prog defaultSyntax)
  
-- Test functions while developing

-- TODO Parse or somehow strip imports from the source before running stats.

-- (mdocs, mname, imports_in, pos) <- parseImports f file
-- parseProg :: SyntaxInfo -> FilePath -> String -> Maybe Mark -> Idris [PDecl]
-- loadSource :: Bool -> FilePath -> Maybe Int -> Idris ()

-- TODO Handle literate Idris because the IdrisLibs SDP uses it exclusivly.
                    -- IDR fn  -> loadSource False fn Nothing
                    -- LIDR fn -> loadSource True  fn Nothing

                  -- file <- if lidr then tclift $ unlit f file_in else return file_in

  -- [Unrelated] Maybe I need to use this function to get a better AST to work with?
-- -- | Collect 'PClauses' with the same function name
-- collect :: [PDecl] -> [PDecl]

tp = do (file_in :: String) <- readSource f
        -- file <- if lidr
        --     then tclift $ unlit f file_in
        --     else return file_in
        -- file <- if lidr then tclift $ unlit f file_in else return file_in
        let res = testParse f file_in
        case res of
          Left err -> putStrLn $ prettyError err
          Right pd -> putStrLn $ showStats $ countD pd
  -- where f = "simpleIdris.idr"
  -- where f = "patrik.idr"
  where f = "Blodwen/src/Core/Primitives.idr"
  -- where f = "../IdrisLibs/SequentialDecisionProblems/CoreTheory.lidr"
        lidr = True


parseIdr :: FilePath -> IO ()
parseIdr infile = do
  file <- readSource infile
  let res = testParse infile file
  case res of
        Left err -> putStrLn $ prettyError err
        Right pd -> putStrLn $ showStats $ countD pd


type Stats = Map.Map String Int

countD decls = foldl (flip countD') Map.empty decls

addD :: String -> Stats -> Stats
addD name stats = Map.insertWith (+) ("PDecl: " ++ name) 1 stats

addT :: String -> Stats -> Stats
addT name stats = Map.insertWith (+) ("PTerm: " ++ name) 1 stats


recf :: String -> [PDecl] -> Stats -> Stats
recf name decls m = (addD name) (foldl (flip countD') m decls)

recdt :: String -> PTerm -> Stats -> Stats
recdt name term m = (addD name) (countT' term m)

rect :: String -> [PTerm] -> Stats -> Stats
rect name terms m = (addD name) (foldl (flip countT') m terms)

recpc :: String -> [PClause] -> Stats -> Stats
recpc name clauses m = (addD name) (foldl (flip pclfn) m clauses)

pclfn :: PClause -> Stats -> Stats
                                            -- Same as `rect`
pclfn (PClause _ _ whole withs rhs whr) m = (foldl (flip countT') m' pterms)
  where pterms = whole : rhs : withs
        -- Same as `recf`
        m' = foldl (flip countD') m whr
pclfn (PWith _ name whole withs rhs _ wher) m = undefined
-- Constructors below are within a `with`-statement.
pclfn (PClauseR _ withs rhs wher) m = undefined
pclfn (PWithR _ withs rhs _ wher) m = undefined

-- Determine if I want to save the stats as a tree shape, och a flat list.
-- flat list will do for now.

countD' :: PDecl -> Stats -> Stats
countD' (PFix _ _ _) = addD "PFix"
   -- | Type declaration (last FC is precise name location)
countD' (PTy _ _ _ _ _ _ _ t) = recdt "PTy" t
   -- | Postulate, second FC is precise name location
countD' (PPostulate _ _ _ _ _ _ _ t) = recdt "PPostulate" t
   -- | Pattern clause
countD' (PClauses _ _ _ clauses) = recpc "PClauses" clauses -- TODO recurse on clauses.
   -- | Top level constant
countD' (PCAF _ _ t) = recdt "PCAF" t
   -- | Data declaration.
countD' (PData _ _ _ _ _ pdataterms) = addD "PData" -- TODO Fix this
   -- | Params block
countD' (PParams _ nameTermPairs decls) = recf "PParams" decls
   -- | Open block/declaration
countD' (POpenInterfaces _ _ decls) = recf "POpenInterfaces" decls
   -- | New namespace, where FC is accurate location of the namespace
   -- in the file
countD' (PNamespace _ _ decls) = recf "PNamespace" decls
   -- | Record name.
countD' (PRecord _ _ _ _ _ _ _ _ _ _ _ _) = addD "PRecord" -- TODO Add records counting
   -- | Interface: arguments are documentation, syntax info, source
   -- location, constraints, interface name, interface name location,
   -- parameters, method declarations, optional constructor name
countD' (PInterface _ _ _ constraints _ _ parameters _ _ decls _ _) = recf "PInterface" decls
   -- | Implementation declaration: arguments are documentation, syntax
   -- info, source location, constraints, interface name, parameters, full
   -- Implementation type, optional explicit name, and definitions
countD' (PImplementation _ _ _ _ constraints _ _ _ _ _ parameters extraNames implementationType _ decls) = recf "PImplementation" decls
countD' (PDSL     _ dsl) = addD "PDSL" -- TODO Check if I need to add this?
   -- ^ DSL declaration
countD' (PSyntax  _ _) = addD "PSyntax"
   -- ^ Syntax definition
countD' (PMutual  _ decls) = recf "PMutual" decls
   -- ^ Mutual block
countD' (PDirective _) = addD "PDirective"
   -- ^ Compiler directive.
   -- | Type provider. The first t is the type, the second is the
   -- term. The second FC is precise highlighting location.
countD' (PProvider _ _ _ _ providewhat _) = addD "PProvider" -- TODO Fix this
   -- | Source-to-source transformation rule. If bool is True, lhs and
   -- rhs must be convertible.
countD' (PTransform _ _ t1 t2) = rect "PTransform" [t1, t2]
   -- | FC is decl-level, for errors, and Strings represent the
   -- namespace
countD' (PRunElabDecl _ t _) = recdt "PRunElabDecl" t

countT' :: PTerm -> Stats -> Stats
countT' (PQuote raw) = addT "term"
--  Inclusion of a core term into the
countT' (PRef _ _ _) = addT "PRef"
--  A reference to a variable. The _ is its precise
-- source location for highlighting. The list of _s is a
-- collection of additional highlighting locations.
countT' (PInferRef _ _ _) = addT "PInferRef"
--  A name to be defined later
countT' (PPatvar _ _) = addT "PPatvar"
--  A pattern variable
countT' (PLam _ _ _ term1 term2) = rect "PLam" [term1, term2]
--  A lambda abstraction. Second _ is name span.
countT' (PPi  _ _ _ term1 term2) = rect"PPi" [term1, term2]
--  (n : t1) -> t2, where the _ is for the precise location of the variable
countT' (PLet _ _ _ _ term1 term2 term3) = rect"PLet" [term1, term2, term3]
--  A let binding (second _ is precise name location)
countT' (PTyped term1 term2) = rect "PTyped" [term1, term2]
--  Term with explicit type
countT' (PApp _ term args) = rect "PApp" [term]
--  e.g. IO (), List Char, length x
countT' (PWithApp _ term1 term2) = rect "PWithApp" [term1, term2]
--  Application plus a 'with' argument
countT' (PAppImpl term _) = rect "PAppImpl" [term]
--  Implicit argument application (introduced during elaboration only)
countT' (PAppBind _ term args) = rect "PAppBind" [term]
--  implicitly bound application
countT' (PMatchApp _ _) = addT "PMatchApp"
--  Make an application by type matching
countT' (PIfThenElse _ term1 term2 term3) = rect "PIfThenElse" [term1, term2, term3]
--  Conditional expressions - elaborated to an overloading of ifThenElse
countT' (PCase _ term cases) = rect "PCase" [term]
--  A case expression. Args are source location, scrutinee, and a list of pattern/RHS pairs
countT' (PTrue _ _) = addT "PTrue"
--  Unit type..?
countT' (PResolveTC _) = addT "PResolveTC"
--  Solve this dictionary by interface resolution
countT' (PRewrite _ _ term1 term2 res) = rect "PRewrite" [term1, term2]
--  "rewrite" syntax, with optional rewriting function and
-- optional result type
countT' (PPair _ _ _ term1 term2) = rect "PPair" [term1, term2]
--  A pair (a, b) and whether it's a product type or a
-- pair (solved by elaboration). The list of _s is its
-- punctuation.
countT' (PDPair _ _ _ term1 term2 term3) = rect "PDPair" [term1, term2, term3]
--  A dependent pair (tm : a ** b) and whether it's a
-- sigma type or a pair that inhabits one (solved by
-- elaboration). The [_] is its punctuation.
countT' (PAs _ _ term) = rect "PAs" [term]
--  @-pattern, valid LHS only
countT' (PAlternative _ _ terms) = rect "PAlternative" terms
--  (| A, B, C|). Includes unapplied unique name mappings for mkUnique_s.
countT' (PHidden term) = rect "PHidden" [term]
--  Irrelevant or hidden pattern
countT' (PType _) = addT "PType"
--  'Type' type
countT' (PUniverse _ _) = addT "PUniverse"
--  Some universe
countT' (PGoal _ term1 _ term2) = rect "PGoal" [term1, term2]
--  quoteGoal, used for %reflection functions
countT' (PConstant _ _) = addT "PConstant"
--  Builtin types
countT' Idris.AbsSyntaxTree.Placeholder = addT "Placeholder"
--  Underscore
countT' (PDoBlock pdo) = addT "PDoBlock"
--  Do notation
countT' (PIdiom _ term) = rect "PIdiom" [term]
--  Idiom brackets
countT' (PMetavar _ _) = addT "PMetavar"
--  A metavariable, ?name, and its precise location
countT' (PProof tactics) = addT "PProof"
--  Proof script
countT' (PTactics tactics) = addT "PTactics"
--  As PProof, but no auto solving
countT' (PElabError _) = addT "PElabError"
--  Error to report on elaboration
countT' PImpossible = addT "PImpossible"
--  Special case for declaring when an LHS can't typecheck
countT' (PCoerced term) = rect "PCoerced" [term]
--  To mark a coerced argument, so as not to coerce twice
countT' (PDisamb _ term) = rect "PDisamb" [term]
--  Preferences for explicit namespaces
countT' (PUnifyLog term) = rect "PUnifyLog" [term]
--  dump a trace of unifications when building term
countT' (PNoImplicits term) = rect "PNoImplicits" [term]
--  never run implicit converions on the term
countT' (PQuasiquote term maybeTerm) = rect "PQuasiquote" [term]
--  `(Term [: Term])
countT' (PUnquote term) = rect "PUnquote" [term]
--  ~Term
countT' (PQuoteName _ _ _) = addT "PQuote_"
    --  `{n} where the _ is the precise highlighting for
    -- the name in particular. If the Bool is False, then
    -- it's `{{n}} and the name won't be resolved.
countT' (PRunElab _ term _) = rect "PRunElab" [term]
    --  %runElab tm - New-style proof script. Args are
    -- location, script, enclosing namespace.
countT' (PConstSugar _ term) = rect "PConstSugar" [term]
    --  A desugared constant. The _ is a precise source
    -- location that will be used to highlight it later.


showStats :: Stats -> String
showStats m = Map.foldlWithKey showRec "" m

showRec :: String -> String -> Int -> String
showRec pre constructor count = pre ++ "\n" ++ (padRight constructor) ++ (show count)

-- Add amount of tabs to line things up.
padRight :: String -> String
padRight s = if (length s) > 8
  then s ++ "\t\t"
  else s ++ "\t\t\t"

-- This function is probably a bad idea. It won't work probably
-- declToTerm :: PDecl -> Maybe PTerm
-- declToTerm (PData _ _ _ _ _ (PDatadecl _ _ typeconstructor _)) = typeconstructor
-- declToTerm (PTy _ _ _ _ _ _ _ terms) = terms

-- constStr :: PDecl -> String
-- constStr = undefined
-- constStr (PFix _ _ _) = "PFix"

-- constStr :: PTerm -> String
-- constStr = undefined


-- The idea is to count recursivly decls and terms and join the maps att each stage

-- Map.unionWith (+) is what I want
