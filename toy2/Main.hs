module Main where

import Agda.Syntax.Concrete
import Agda.Syntax.Concrete.Pretty
import Agda.Utils.Pretty
import Agda.Syntax.Position
import Agda.Syntax.Literal
import Agda.Syntax.Common
import Agda.Syntax.Fixity
import Agda.Syntax.Notation

name :: String -> Name
name n = Name NoRange InScope [(Id n)]
qname :: String -> QName
qname n = QName $ name n

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
 
pie :: Expr
pie = Pi tl (Ident id1)
  -- where tl = [TBind NoRange [arg "ArgName" (mkBoundName (name "Hej") mkFixity)] (lit 13)]
  where tl = [TLet NoRange [typesig]]
--   | TLet  Range [Declaration]           -- ^ Let binding @(let Ds)@ or @(open M args)@.

mkFixity :: Fixity'
mkFixity = Fixity' f not NoRange
  where f = Fixity NoRange Unrelated NonAssoc
        rstring = Ranged NoRange "RawName"
        not = [IdPart rstring]

main = putStrLn $ prettyShow rapp
 
modality :: Modality
modality = Modality Relevant Quantity0

arg :: String -> a -> NamedArg a
arg name expr = Arg (ArgInfo NotHidden modality UserWritten UnknownFVs) $
  Named (Just (Ranged NoRange name)) expr

typesig :: TypeSignature
typesig = TypeSig argInfo name expr
  where argInfo = ArgInfo NotHidden modality UserWritten UnknownFVs
        name = Name NoRange InScope [(Id "addOne")]
        expr = pie
        
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
