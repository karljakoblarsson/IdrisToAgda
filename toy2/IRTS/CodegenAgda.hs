{-|
Module      : IRTS.CodegenAgda
Description : The JavaScript code generator.

License     : BSD3
Maintainer  : The Idris Community.
-}
{-# LANGUAGE OverloadedStrings, PatternGuards #-}
module IRTS.CodegenAgda (codegenAgda
                             ) where

import Data.Char
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import IRTS.CodegenCommon
import System.Directory
import System.FilePath

import Idris.Core.Evaluate
import Idris.Core.TT
import IRTS.Defunctionalise
import IRTS.Simplified

-- codegenJs :: CGConf -> CodeGenerator

{- data CodegenInfo = CodegenInfo { -}
{-     outputFile    :: String -}
{-   , outputType    :: OutputType -}
{-   , targetTriple  :: String -}
{-   , targetCPU     :: String -}
{-   , includes      :: [FilePath] -}
{-   , importDirs    :: [FilePath] -}
{-   , compileObjs   :: [String] -}
{-   , compileLibs   :: [String] -}
{-   , compilerFlags :: [String] -}
{-   , debugLevel    :: DbgLevel -}
{-   , simpleDecls   :: [(Name, SDecl)] -}
{-   , defunDecls    :: [(Name, DDecl)] -}
{-   , liftDecls     :: [(Name, LDecl)] -}
{-   , interfaces    :: Bool -}
{-   , exportDecls   :: [ExportIFace] -}
{-   , ttDecls       :: [(Name, TTDecl)] -}
{-   } -}

{- type CodeGenerator = CodegenInfo -> IO () -}
codegenAgda :: CodeGenerator
codegenAgda ci =
    TIO.writeFile (outputFile ci) $ T.intercalate "\n" $
        [ "-- Start translated Agda code\n" ] ++
        map (fn) ldecls
        ++ [ "\n-- End translated Agda code"
        ]
  where ldecls = liftDecls ci
        fn :: (Name, LDecl) -> Text
        fn (name, ldecl) = T.concat
          [ "\n{-"
          , prettyName name
          , "-}\n"
          , printLDecl ldecl
          ]


printLDecl :: LDecl -> Text
printLDecl (LFun opts name args def) = addComment "LFun" $ T.intercalate " "
  [ prettyName name
  , "args TODO"
  , prettyLExp def
  ]
printLDecl (LConstructor name tag arity) = addComment "LConstructor" $ prettyName name
-- data LDecl = LFun [LOpt] Name [Name] LExp -- options, name, arg names, def
           -- LConstructor Name Int Int -- constructor name, tag, arity
-- data LExp = LV Name
--           | LApp Bool LExp [LExp]    -- True = tail call
--           | LLazyApp Name [LExp]     -- True = tail call
--           | LLazyExp LExp            -- lifted out before compiling
--           | LForce LExp              -- make sure Exp is evaluted
--           | LLet Name LExp LExp      -- name just for pretty printing
--           | LLam [Name] LExp         -- lambda, lifted out before compiling
--           | LProj LExp Int           -- projection
--           | LCon (Maybe Name)        -- Location to reallocate, if available
--                  Int Name [LExp]
--           | LCase CaseType LExp [LAlt]
--           | LConst Const
--           | LForeign FDesc           -- Function descriptor (usually name as string)
--                      FDesc           -- Return type descriptor
--                      [(FDesc, LExp)] -- first LExp is the FFI type description
--           | LOp PrimFn [LExp]
--           | LNothing
--           | LError String
-- data LOpt = Inline | NoInline

prettyLExp :: LExp -> Text
prettyLExp _ = "LExp Undefined"
-- prettyLExp (LV Name) = undefined
-- prettyLExp (LApp Bool LExp [LExp]) = undefined -- True = tail call
-- prettyLExp (LLazyApp Name [LExp]) = undefined -- True = tail call
-- prettyLExp (LLazyExp LExp) = undefined -- lifted out before compiling
-- prettyLExp (LForce LExp) = undefined -- make sure Exp is evaluted
-- prettyLExp (LLet Name LExp LExp) = undefined -- name just for pretty printing
-- prettyLExp (LLam [Name] LExp) = undefined -- lambda, lifted out before compiling
-- prettyLExp (LProj LExp Int) = undefined -- projection
-- prettyLExp (LCon (Maybe Name
--                  Int Name [LExp])) = undefined -- Location to reallocate, if available
-- prettyLExp (LCase CaseType LExp [LAlt]) = undefined
-- prettyLExp (LConst Const) = undefined
-- prettyLExp (LForeign FDesc
--                      FDesc
--                      [(FDesc, LExp)]) = undefined -- first LExp is the FFI type description
-- -- = undefined -- Function descriptor (usually name as string)
-- -- = undefined -- Return type descriptor
-- prettyLExp (LOp PrimFn [LExp]) = undefined
-- prettyLExp (LNothing) = undefined
-- prettyLExp (LError String) = undefined                                                            

-- data Name = UN !T.Text -- ^ User-provided name
--           | NS !Name [T.Text] -- ^ Root, namespaces
--           | MN !Int !T.Text -- ^ Machine chosen names
--           | SN !SpecialName -- ^ Decorated function names
--           | SymRef Int -- ^ Reference to IBC file symbol table (used during serialisation)
-- data SpecialName = WhereN !Int !Name !Name
--                  | WithN !Int !Name
--                  | ImplementationN !Name [T.Text]
--                  | ParentN !Name !T.Text
--                  | MethodN !Name
--                  | CaseN !FC' !Name
--                  | ImplementationCtorN !Name
--                  | MetaN !Name !Name
prettyName :: Name -> Text
prettyName (UN name) = addComment "User-provided name" name
prettyName (NS ns names) = T.concat $ [prettyName ns] ++ names
prettyName (MN id name) = addComment
  (T.append "Machine chosen name with id: " $ T.pack $ show id) name
prettyName (SN sn) = addComment "Decorated function name" $ prettySN sn
prettyName (SymRef id) = addComment "Reference to IBC" $ T.pack $ show id

addComment :: Text -> Text -> Text
addComment c rest = T.intercalate " " ["{-", c, "-}", rest]

prettySN :: SpecialName -> Text
prettySN (WhereN a b c) = "WhereN"
prettySN (WithN a b) = "WithN"
prettySN (ImplementationN a b) = "ImplementationN"
prettySN (ParentN a b) = "ParentN"
prettySN (MethodN a) = "MethodN"
prettySN (CaseN a b) = "CaseN"
prettySN (ImplementationCtorN a) = "ImplementationCtorN"
prettySN (MetaN a b) = "MetaN"
