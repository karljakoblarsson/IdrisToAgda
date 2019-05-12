{-|
Module      : IRTS.Lang
Description : Internal representation of Idris' constructs.

License     : BSD3
Maintainer  : The Idris Community.
-}
{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, DeriveGeneric, FlexibleContexts,
             PatternGuards #-}

module IRTS.Lang where

import Idris.Core.CaseTree
import Idris.Core.TT

import Control.Monad.State hiding (lift)
import Data.Data (Data)
import Data.List
import qualified Data.Map.Strict as Map
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

data Endianness = Native | BE | LE deriving (Show, Eq)

data LVar = Loc Int | Glob Name
  deriving (Show, Eq)

-- ASSUMPTION: All variable bindings have unique names here
-- Constructors commented as lifted are not present in the LIR provided to the different backends.
data LExp = LV Name
          | LApp Bool LExp [LExp]    -- True = tail call
          | LLazyApp Name [LExp]     -- True = tail call
          | LLazyExp LExp            -- lifted out before compiling
          | LForce LExp              -- make sure Exp is evaluted
          | LLet Name LExp LExp      -- name just for pretty printing
          | LLam [Name] LExp         -- lambda, lifted out before compiling
          | LProj LExp Int           -- projection
          | LCon (Maybe Name)        -- Location to reallocate, if available
                 Int Name [LExp]
          | LCase CaseType LExp [LAlt]
          | LConst Const
          | LForeign FDesc           -- Function descriptor (usually name as string)
                     FDesc           -- Return type descriptor
                     [(FDesc, LExp)] -- first LExp is the FFI type description
          | LOp PrimFn [LExp]
          | LNothing
          | LError String
  deriving (Eq, Ord)

data FDesc = FCon Name
           | FStr String
           | FUnknown
           | FIO FDesc
           | FApp Name [FDesc]
  deriving (Show, Eq, Ord)

data Export = ExportData FDesc -- Exported data descriptor (usually string)
            | ExportFun Name -- Idris name
                        FDesc -- Exported function descriptor
                        FDesc -- Return type descriptor
                        [FDesc] -- Argument types
  deriving (Show, Eq, Ord)

data ExportIFace = Export Name -- FFI descriptor
                          String -- interface file
                          [Export]
  deriving (Show, Eq, Ord)

-- Primitive operators. Backends are not *required* to implement all
-- of these, but should report an error if they are unable

data PrimFn = LPlus ArithTy | LMinus ArithTy | LTimes ArithTy
            | LUDiv IntTy | LSDiv ArithTy | LURem IntTy | LSRem ArithTy
            | LAnd IntTy | LOr IntTy | LXOr IntTy | LCompl IntTy
            | LSHL IntTy | LLSHR IntTy | LASHR IntTy
            | LEq ArithTy | LLt IntTy | LLe IntTy | LGt IntTy | LGe IntTy
            | LSLt ArithTy | LSLe ArithTy | LSGt ArithTy | LSGe ArithTy
            | LSExt IntTy IntTy | LZExt IntTy IntTy | LTrunc IntTy IntTy
            | LStrConcat | LStrLt | LStrEq | LStrLen
            | LIntFloat IntTy | LFloatInt IntTy | LIntStr IntTy | LStrInt IntTy
            | LFloatStr | LStrFloat | LChInt IntTy | LIntCh IntTy
            | LBitCast ArithTy ArithTy -- Only for values of equal width

            | LFExp | LFLog | LFSin | LFCos | LFTan | LFASin | LFACos | LFATan
            | LFATan2 | LFSqrt | LFFloor | LFCeil | LFNegate

            | LStrHead | LStrTail | LStrCons | LStrIndex | LStrRev | LStrSubstr
            | LReadStr | LWriteStr

            -- system info
            | LSystemInfo

            | LFork
            | LPar -- evaluate argument anywhere, possibly on another
                   -- core or another machine. 'id' is a valid implementation
            | LExternal Name
            | LCrash

            | LNoOp
  deriving (Show, Eq, Ord, Generic)

-- Supported target languages for foreign calls

data FCallType = FStatic | FObject | FConstructor
  deriving (Show, Eq, Ord)

data FType = FArith ArithTy
           | FFunction
           | FFunctionIO
           | FString
           | FUnit
           | FPtr
           | FManagedPtr
           | FCData
           | FAny
  deriving (Show, Eq, Ord)

-- FIXME: Why not use this for all the IRs now?
data LAlt' e = LConCase Int Name [Name] e
             | LConstCase Const e
             | LDefaultCase e
  deriving (Show, Eq, Ord, Functor, Data, Typeable)

type LAlt = LAlt' LExp

data LDecl = LFun [LOpt] Name [Name] LExp -- options, name, arg names, def
           | LConstructor Name Int Int -- constructor name, tag, arity
  deriving (Show, Eq, Ord)

type LDefs = Ctxt LDecl

data LOpt = Inline | NoInline
  deriving (Show, Eq, Ord)

addTags :: Int -> [(Name, LDecl)] -> (Int, [(Name, LDecl)])
addTags i ds = tag i ds []
  where tag i ((n, LConstructor n' (-1) a) : as) acc
            = tag (i + 1) as ((n, LConstructor n' i a) : acc)
        tag i ((n, LConstructor n' t a) : as) acc
            = tag i as ((n, LConstructor n' t a) : acc)
        tag i (x : as) acc = tag i as (x : acc)
        tag i [] acc  = (i, reverse acc)

data LiftState = LS (Maybe Name) Int [(Name, LDecl)]
                    (Map.Map ([Name], LExp) Name) -- map from args/expressions
                          -- to names, so we don't create the same function
                          -- multiple times

setBaseName :: Name -> State LiftState ()
setBaseName n
    = do LS _ i ds done <- get
         put (LS (Just n) i ds done)

lname (NS n x) i = NS (lname n i) x
lname (UN n) i = MN i n
lname x i = sMN i (showCG x ++ "_lam")

getNextName :: State LiftState Name
getNextName
    = do LS mn i ds done <- get
         let newn = case mn of
                         Nothing -> lname (sUN "_") i
                         Just n -> lname n i
         put (LS mn (i + 1) ds done)
         return newn

renameArgs :: [Name] -> LExp -> ([Name], LExp)
renameArgs args e
   = let newargNames = map (\i -> sMN i "lift") [0..]
         newargs = zip args newargNames in
         (map snd newargs, rename newargs e)

addFn :: Name -> LDecl -> State LiftState ()
addFn fn d
    = do LS n i ds done <- get
         put (LS n i ((fn, d) : ds) done)

makeFn :: [Name] -> LExp -> State LiftState Name
makeFn args exp
    = do fn <- getNextName
         let (args', exp') = renameArgs args exp
         LS n i ds done <- get
         case Map.lookup (args', exp') done of
              Just fn -> return fn
              Nothing ->
                do addFn fn (LFun [Inline] fn args' exp')
                   LS n i ds done <- get
                   put (LS n i ds (Map.insert (args', exp') fn done))
                   return fn

liftAll :: [(Name, LDecl)] -> [(Name, LDecl)]
liftAll xs =
  let (LS _ _ decls _) = execState (mapM_ liftDef xs) (LS Nothing 0 [] Map.empty) in
      decls

liftDef :: (Name, LDecl) -> State LiftState ()
liftDef (n, LFun opts _ args e) =
    do setBaseName n
       e' <- lift args e
       addFn n (LFun opts n args e')
liftDef (n, x) = addFn n x

lift :: [Name] -> LExp -> State LiftState LExp
lift env (LV v) = return (LV v) -- Lifting happens before these can exist...
lift env (LApp tc (LV n) args) = do args' <- mapM (lift env) args
                                    return (LApp tc (LV n) args')
lift env (LApp tc f args) = do f' <- lift env f
                               fn <- makeFn env f'
                               args' <- mapM (lift env) args
                               return (LApp tc (LV fn) (map LV env ++ args'))
lift env (LLazyApp n args) = do args' <- mapM (lift env) args
                                return (LLazyApp n args')
lift env (LLazyExp (LConst c)) = return (LConst c)
-- lift env (LLazyExp (LApp tc (LV (Glob f)) args))
--                       = lift env (LLazyApp f args)
lift env (LLazyExp e) = do e' <- lift env e
                           let usedArgs = nub $ usedIn env e'
                           fn <- makeFn usedArgs e'
                           return (LLazyApp fn (map LV usedArgs))
lift env (LForce e) = do e' <- lift env e
                         return (LForce e')
lift env (LLet n v e) = do v' <- lift env v
                           e' <- lift (env ++ [n]) e
                           return (LLet n v' e')
lift env (LLam args (LLam args' e)) = lift env (LLam (args ++ args') e)
lift env (LLam args e) = do e' <- lift (env ++ args) e
                            let usedArgs = nub $ usedIn env e'
                            fn <- makeFn (usedArgs ++ args) e'
                            return (LApp False (LV fn) (map LV usedArgs))
lift env (LProj t i) = do t' <- lift env t
                          return (LProj t' i)
lift env (LCon loc i n args) = do args' <- mapM (lift env) args
                                  return (LCon loc i n args')
lift env (LCase up e alts) = do alts' <- mapM liftA alts
                                e' <- lift env e
                                return (LCase up e' alts')
  where
    liftA (LConCase i n args e) = do e' <- lift (env ++ args) e
                                     return (LConCase i n args e')
    liftA (LConstCase c e) = do e' <- lift env e
                                return (LConstCase c e')
    liftA (LDefaultCase e) = do e' <- lift env e
                                return (LDefaultCase e')
lift env (LConst c) = return (LConst c)
lift env (LForeign t s args) = do args' <- mapM (liftF env) args
                                  return (LForeign t s args')
  where
    liftF env (t, e) = do e' <- lift env e
                          return (t, e')
lift env (LOp f args) = do args' <- mapM (lift env) args
                           return (LOp f args')
lift env (LError str) = return $ LError str
lift env LNothing = return LNothing

allocUnique :: LDefs -> (Name, LDecl) -> (Name, LDecl)
allocUnique defs p@(n, LConstructor _ _ _) = p
allocUnique defs (n, LFun opts fn args e)
    = let e' = evalState (findUp e) [] in
          (n, LFun opts fn args e')
  where
    -- Keep track of 'updatable' names in the state, i.e. names whose heap
    -- entry may be reused, along with the arity which was there
    findUp :: LExp -> State [(Name, Int)] LExp
    findUp (LApp t (LV n) as)
       | Just (LConstructor _ i ar) <- lookupCtxtExact n defs,
         ar == length as
          = findUp (LCon Nothing i n as)
    findUp (LV n)
       | Just (LConstructor _ i 0) <- lookupCtxtExact n defs
          = return $ LCon Nothing i n [] -- nullary cons are global, no need to update
    findUp (LApp t f as) = LApp t <$> findUp f <*> mapM findUp as
    findUp (LLazyApp n as) = LLazyApp n <$> mapM findUp as
    findUp (LLazyExp e) = LLazyExp <$> findUp e
    findUp (LForce e) = LForce <$> findUp e
    -- use assumption that names are unique!
    findUp (LLet n val sc) = LLet n <$> findUp val <*> findUp sc
    findUp (LLam ns sc) = LLam ns <$> findUp sc
    findUp (LProj e i) = LProj <$> findUp e <*> return i
    findUp (LCon (Just l) i n es) = LCon (Just l) i n <$> mapM findUp es
    findUp (LCon Nothing i n es)
           = do avail <- get
                v <- findVar [] avail (length es)
                LCon v i n <$> mapM findUp es
    findUp (LForeign t s es)
           = LForeign t s <$> mapM (\ (t, e) -> do e' <- findUp e
                                                   return (t, e')) es
    findUp (LOp o es) = LOp o <$> mapM findUp es
    findUp (LCase Updatable e@(LV n) as)
           = LCase Updatable e <$> mapM (doUpAlt n) as
    findUp (LCase t e as)
           = LCase t <$> findUp e <*> mapM findUpAlt as
    findUp t = return t

    findUpAlt (LConCase i t args rhs) = do avail <- get
                                           rhs' <- findUp rhs
                                           put avail
                                           return $ LConCase i t args rhs'
    findUpAlt (LConstCase i rhs) = LConstCase i <$> findUp rhs
    findUpAlt (LDefaultCase rhs) = LDefaultCase <$> findUp rhs

    doUpAlt n (LConCase i t args rhs)
           = do avail <- get
                put ((n, length args) : avail)
                rhs' <- findUp rhs
                put avail
                return $ LConCase i t args rhs'
    doUpAlt n (LConstCase i rhs) = LConstCase i <$> findUp rhs
    doUpAlt n (LDefaultCase rhs) = LDefaultCase <$> findUp rhs

    findVar _ [] i = return Nothing
    findVar acc ((n, l) : ns) i | l == i = do put (reverse acc ++ ns)
                                              return (Just n)
    findVar acc (n : ns) i = findVar (n : acc) ns i


-- Return variables in list which are used in the expression

usedArg env n | n `elem` env = [n]
              | otherwise = []

usedIn :: [Name] -> LExp -> [Name]
usedIn env (LV n) = usedArg env n
usedIn env (LApp _ e args) = usedIn env e ++ concatMap (usedIn env) args
usedIn env (LLazyApp n args) = concatMap (usedIn env) args ++ usedArg env n
usedIn env (LLazyExp e) = usedIn env e
usedIn env (LForce e) = usedIn env e
usedIn env (LLet n v e) = usedIn env v ++ usedIn (env \\ [n]) e
usedIn env (LLam ns e) = usedIn (env \\ ns) e
usedIn env (LCon v i n args) = let rest = concatMap (usedIn env) args in
                                   case v of
                                      Nothing -> rest
                                      Just n -> usedArg env n ++ rest
usedIn env (LProj t i) = usedIn env t
usedIn env (LCase up e alts) = usedIn env e ++ concatMap (usedInA env) alts
  where usedInA env (LConCase i n ns e) = usedIn env e
        usedInA env (LConstCase c e) = usedIn env e
        usedInA env (LDefaultCase e) = usedIn env e
usedIn env (LForeign _ _ args) = concatMap (usedIn env) (map snd args)
usedIn env (LOp f args) = concatMap (usedIn env) args
usedIn env _ = []

lsubst :: Name -> LExp -> LExp -> LExp
lsubst n new (LV x) | n == x = new
lsubst n new (LApp t e args) = let e' = lsubst n new e
                                   args' = map (lsubst n new) args in
                                   LApp t e' args'
lsubst n new (LLazyApp fn args) = let args' = map (lsubst n new) args in
                                      LLazyApp fn args'
lsubst n new (LLazyExp e) = LLazyExp (lsubst n new e)
lsubst n new (LForce e) = LForce (lsubst n new e)
lsubst n new (LLet v val sc) = LLet v (lsubst n new val) (lsubst n new sc)
lsubst n new (LLam ns sc) = LLam ns (lsubst n new sc)
lsubst n new (LProj e i) = LProj (lsubst n new e) i
lsubst n new (LCon lv t cn args) = let args' = map (lsubst n new) args in
                                       LCon lv t cn args'
lsubst n new (LOp op args) = let args' = map (lsubst n new) args in
                                 LOp op args'
lsubst n new (LForeign fd rd args)
     = let args' = map (\(d, a) -> (d, lsubst n new a)) args in
           LForeign fd rd args'
lsubst n new (LCase t e alts) = let e' = lsubst n new e
                                    alts' = map (fmap (lsubst n new)) alts in
                                    LCase t e' alts'
lsubst n new tm = tm

rename :: [(Name, Name)] -> LExp -> LExp
rename ns tm@(LV x)
   = case lookup x ns of
          Just n -> LV n
          _ -> tm
rename ns (LApp t e args)
    = let e' = rename ns e
          args' = map (rename ns) args in
          LApp t e' args'
rename ns (LLazyApp fn args)
    = let args' = map (rename ns) args in
          LLazyApp fn args'
rename ns (LLazyExp e) = LLazyExp (rename ns e)
rename ns (LForce e) = LForce (rename ns e)
rename ns (LLet v val sc) = LLet v (rename ns val) (rename ns sc)
rename ns (LLam args sc) = LLam args (rename ns sc)
rename ns (LProj e i) = LProj (rename ns e) i
rename ns (LCon lv t cn args) = let args' = map (rename ns) args in
                                    LCon lv t cn args'
rename ns (LOp op args) = let args' = map (rename ns) args in
                              LOp op args'
rename ns (LForeign fd rd args)
     = let args' = map (\(d, a) -> (d, rename ns a)) args in
           LForeign fd rd args'
rename ns (LCase t e alts) = let e' = rename ns e
                                 alts' = map (fmap (rename ns)) alts in
                                 LCase t e' alts'
rename ns tm = tm

instance Show LExp where
   show e = show' [] "" e where
     show' env ind (LV n) = show n

     show' env ind (LLazyApp e args)
        = show e ++ "|(" ++ showSep ", " (map (show' env ind) args) ++")"

     show' env ind (LApp _ e args)
        = show' env ind e ++ "(" ++ showSep ", " (map (show' env ind) args) ++")"

     show' env ind (LLazyExp e) = "lazy{ "  ++ show' env ind e ++ " }"
     show' env ind (LForce   e) = "force{ " ++ show' env ind e ++ " }"
     show' env ind (LLet n v e)
        = "let " ++ show n ++ " = " ++ show' env ind v
            ++ " in " ++ show' (env ++ [show n]) ind e

     show' env ind (LLam args e)
        = "(\\ " ++ showSep "," (map show args)
            ++ " => " ++ show' (env ++ (map show args)) ind e ++ ") "

     show' env ind (LProj t i) = show t ++ "!" ++ show i

     show' env ind (LCon loc i n args)
        = atloc loc ++ show n ++ "(" ++ showSep ", " (map (show' env ind) args) ++ ")"
       where atloc Nothing = ""
             atloc (Just l) = "@" ++ show (LV l) ++ ":"

     show' env ind (LCase up e alts)
        = "case" ++ update ++ "(" ++ show' env ind e ++ ") of \n" ++ fmt alts
       where
         update = case up of
                       Shared -> " "
                       Updatable -> "! "
         fmt [] = ""
         fmt [alt]
            = "\t" ++ ind ++ "| " ++ showAlt env (ind ++ "    ") alt
         fmt (alt:as)
            = "\t" ++ ind ++ "| " ++ showAlt env (ind ++ ".   ") alt
                ++ "\n" ++ fmt as

     show' env ind (LConst c) = show c

     show' env ind (LForeign ty n args) = concat
            [ "foreign{ "
            ,       show n ++ "("
            ,           showSep ", " (map (\(ty,x) -> show' env ind x ++ " : " ++ show ty) args)
            ,       ") : "
            ,       show ty
            , " }"
            ]

     show' env ind (LOp f args)
        = show f ++ "(" ++ showSep ", " (map (show' env ind) args) ++ ")"

     show' env ind (LError str) = "error " ++ show str
     show' env ind LNothing = "____"

     showAlt env ind (LConCase _ n args e)
          = show n ++ "(" ++ showSep ", " (map show args) ++ ") => "
             ++ show' env ind e

     showAlt env ind (LConstCase c e) = show c ++ " => " ++ show' env ind e
     showAlt env ind (LDefaultCase e) = "_ => " ++ show' env ind e

occName :: Name -> LExp -> Int
occName n (LV x) = if n == x then 1 else 0
occName n (LApp t e es) = occName n e + sum (map (occName n) es)
occName n (LLazyApp x es)
    = if n == x then 1 + sum (map (occName n) es)
                else sum (map (occName n) es)
occName n (LForce e) = occName n e
occName n (LLet x v sc)
    = if n == x then occName n v
                else occName n v + occName n sc
occName n (LLam ns sc)
    = if n `elem` ns then 0 else occName n sc
occName n (LProj e i) = occName n e
occName n (LCon _ _ _ es) = sum (map (occName n) es)
occName n (LCase t e alts) = occName n e + maximum (map occAlt alts)
  where
    occAlt (LConCase _ _ ns e)
        = if n `elem` ns then 0 else occName n e
    occAlt (LConstCase _ e) = occName n e
    occAlt (LDefaultCase e) = occName n e
occName n (LForeign _ _ es) = sum (map (occName n . snd) es)
occName n (LOp _ es) = sum (map (occName n) es)
occName n _ = 0

