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

import System.Environment
import System.Exit

mkName :: String -> Name
mkName n = Name NoRange InScope [(Id n)]
qname :: String -> QName
qname n = QName $ Main.mkName n

iden n = Ident $ qname n

lit :: Integer -> Expr
lit i = Lit $ LitNat NoRange i

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

-- TODO Proporly display infix operators in the Agda AST.
itaFixity :: Idris.AbsSyntax.Fixity -> Agda.Syntax.Fixity.Fixity
itaFixity (Infixl prec) = Fixity NoRange (Related (toInteger prec)) LeftAssoc
itaFixity (Infixr prec) = Fixity NoRange (Related (toInteger prec)) RightAssoc
itaFixity (InfixN prec) = Fixity NoRange (Related (toInteger prec)) NonAssoc
itaFixity (PrefixN prec) = undefined -- I don't know that this is.

itaClauses :: [PClause] -> Declaration
itaClauses clauses = case length clauses of
  1 -> itaClause $ head clauses -- Safe because of the case stmt.
  _ -> undefined -- I don't know when this case happen. But it's not common.

itaClause :: PClause -> Declaration
itaClause (PClause fc name whole with rhsIdr whrIdr) =
    FunClause lhs rhs whr bool
  where lhs = LHS ptn rewriteExpr withExpr
        rhs = RHS rhsexp -- Can also be 'AbsurdRHS'
        whr = NoWhere -- Can also be 'AnyWhere [Decls]'
        bool = False
        rhsexp = itaTerm rhsIdr
  -- TODO Research Problem
  -- This does not work for general patterns
  -- But I talked about skipping dependent patterns in the planning report.
  -- The LHS in Agda is represented as a 'Pattern'-type while it's a simple
  -- 'PTerm' in Idris. It's not obvious how to convert betwen them.
        ptn = itaPattern whole
        rewriteExpr = []
        withExpr = []

itaPattern :: PTerm -> Pattern
itaPattern (PRef _ _ name) = IdentP $ qname $ Main.prettyName name
itaPattern (PApp range fst args) =
  RawAppP NoRange ((itaPattern fst) : (map (itaPattern . itaArgsToTerm) args))

itaArgsToTerm :: PArg -> PTerm
itaArgsToTerm (PExp prio argopts pname getTm) = getTm
itaArgsToTerm _ = undefined

application :: String -> [Expr] -> Expr
application name args = RawApp NoRange ((iden name) : args)

-- TODO START HERE
-- Parenthesis are explicit in the concrete Agda AST but are not represented in
-- the Idris PDecl lang. So that information is lost. I need to reconstruct that
-- somehow to get the output to typecheck. It is certainly possible to do with a
-- post-processing pass. But the output won't match the input. And how hard is
-- it to do correctly?
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
-- type PArg = PArg' PTerm -- getTm :: PTerm
itaArgs :: PArg -> Expr
itaArgs (PExp prio argopts pname getTm) = itaTerm getTm
itaArgs _ = undefined

addComment = (++)
  -- There is also AbsStyntaxTree.prettyName but it's harder to use.
prettyName :: TT.Name -> String
prettyName (TT.UN name) =  Text.unpack name
prettyName (TT.NS ns names) = concat $ intersperse "." $
  (map Text.unpack names) ++ [Main.prettyName ns]
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
main :: IO ()
main = getArgs >>= parse

parse ["-h"] = usage >> exit
parse ["-v"] = version >> exit
parse [] = usage >> exit
parse ["-o", outfile, infile] = runITA infile (Just outfile)
parse [infile] = runITA infile Nothing

usage = putStrLn "Usage: ita [-vh] [-o outfile.agda] [infile.idr]"
version = putStrLn "IdrisToAgda 0.1"
exit = exitWith ExitSuccess
die = exitWith (ExitFailure 1)
success outfile =
  putStrLn ("Successfuly compiled. Output written to: " ++ outfile) >> exit
error :: FilePath -> ParseError -> IO ()
error infile err =
  putStrLn ("Error while compiling file: " ++ infile ++ "\n\n" ++
            (prettyError err)) >> Main.die
  
runITA :: FilePath -> Maybe FilePath -> IO ()
runITA infile outfile =
  do (file :: String) <- readSource infile
     let out = tryCompile file infile
     case out of
       Left err -> Main.error infile err
       Right res -> case outfile of
         Just outfilename -> writeFile outfilename res >> success outfilename
         Nothing -> putStrLn res
 
-- 'itaDecl' is the function which does the translation.
tryCompile :: String -> FilePath -> Either ParseError String
tryCompile infile filename =
  let parseRes = runparser (prog defaultSyntax) idrisInit filename infile in
    case parseRes of
  Left err -> Left err
  Right pd -> Right $ prettyShow $ map itaDecl pd
  
-- Test function while developing
tp = do (file :: String) <- readSource f
        let res = testParse file
        case res of
          Left err -> putStrLn $ prettyError err
          Right pd -> putStrLn $ prettyShow $ map itaDecl pd
          -- Right pd -> putStrLn $ prettyShow $ itaDecl $ head pd
          -- Right pd -> putPDecls pd
  where f = "../simpleIdris.idr"
        putPDecls lst = putStrLn $ concat $ intersperse "\n\n" $ map show lst

testParse p = runparser (prog defaultSyntax) idrisInit "(test)" p


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
