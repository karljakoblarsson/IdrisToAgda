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
import Idris.Docstrings
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

parseCLIOpts :: [String] -> IO (FilePath, Maybe FilePath)
parseCLIOpts ["-h"] = usage >> exit
parseCLIOpts ["-v"] = version >> exit
parseCLIOpts [] = usage >> exit
parseCLIOpts ["-o", outfile, infile] = return (infile, Just outfile)
parseCLIOpts [infile] = return (infile, Nothing)

usage = putStrLn "Usage: ita [-vh] [-o outfile.agda] [infile.idr]"
version = putStrLn "IdrisToAgda 0.1"
exit = exitWith ExitSuccess
die = exitWith (ExitFailure 1)
success outfile =
  putStrLn ("Successfuly compiled. Output written to: " ++ outfile) >> exit
error :: FilePath -> ParseError -> IO ()
error infile err =
  putStrLn ("Error while compiling file: " ++ infile ++ "\n\n" ++
            (prettyError err)) >> Stats.die

testParse p = runparser (prog defaultSyntax) idrisInit "(test)" p
-- Test functions while developing

parseIdr :: (FilePath, Maybe FilePath) -> IO ()
parseIdr _ = tp

tp = do (file :: String) <- readSource f
        let res = testParse file
        case res of
          Left err -> putStrLn $ prettyError err
          Right pd -> putStrLn $ showStats $ countDecl pd
  where f = "simpleIdris.idr"
        putPDecls lst = putStrLn $ concat $ intersperse "\n\n" $ map show lst

countDecl :: [PDecl] -> Map.Map String Int
countDecl decls = foldl acc init decls
  where init = Map.empty
        acc m decl = Map.insertWith (+) (constStr decl) 1 m
        constStr d = show $ Data.toConstr d

showStats :: Map.Map String Int -> String
showStats = show

countTerm :: PTerm -> Map.Map String Int
countTerm = undefined

declToTerm :: PDecl -> Maybe PTerm
declToTerm (PData _ _ _ _ _ (PDatadecl _ _ typeconstructor _)) = typeconstructor
declToTerm (PTy _ _ _ _ _ _ _ terms) = terms
