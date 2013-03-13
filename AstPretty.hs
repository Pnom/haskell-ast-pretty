module AstPretty ( AstPretty(astPretty),
  DocState(..),
  DocM,
  format, line, space, nest,
  renderWithMode, renderWithDefMode,
  PrettyMode(..), defPrettyMode,
  parenList, braceList
  ) where

import Language.Haskell.Exts.Annotated
import qualified Language.Haskell.Exts.Pretty as PR
import Control.Monad.State
import Control.Monad.Reader

import Data.Maybe
import Data.List

data DocState = DocState {
  pos :: !SrcLoc,
  nestSize :: !Int
  } deriving Show

data PrettyMode = PrettyMode PR.PPHsMode PR.Style

defPrettyMode = PrettyMode PR.defaultMode PR.style

type DocM = ReaderT PrettyMode (State DocState)

-- --------------------------------------------------------------------------
-- | render the document with a given mode.

renderWithMode :: PrettyMode -> DocState -> DocM a -> a
renderWithMode mode state doc = evalState (runReaderT doc mode) state

-- | render the document with 'defaultMode'.
renderWithDefMode :: DocState -> DocM a -> a
renderWithDefMode = renderWithMode defPrettyMode

-- --------------------------------------------------------------------------

format :: String -> DocM SrcSpan
format s = do
  SrcLoc f l c <- getPos
  let newColumn = c + length s
  putPos $ SrcLoc f l newColumn
  return $ SrcSpan f l c l newColumn

-- --------------------------------------------------------------------------

getPos :: DocM SrcLoc
getPos = gets pos

-- --------------------------------------------------------------------------

putPos :: SrcLoc -> DocM SrcLoc
putPos l = do
  DocState _ n <- get
  put $! DocState l n
  return l

-- --------------------------------------------------------------------------

line :: DocM ()
line = do
  DocState (SrcLoc f l c) n <- get
  putPos $! SrcLoc f (l + 1) (if n > 0 then n else 1)
  return ()

-- --------------------------------------------------------------------------

space :: Int -> DocM ()
space x = do
  SrcLoc f l c <- getPos
  putPos $! SrcLoc f l (c + x)
  return ()

-- --------------------------------------------------------------------------

nest :: Int -> DocM ()
nest x = do
  DocState l n <- get
  put $! DocState l (1 + n + x)
  return ()

-- --------------------------------------------------------------------------

empty :: DocM ()
empty = return ()

-- --------------------------------------------------------------------------

data AstElement a = Ast (DocM (a, [SrcSpan]))
  | InfoPoint (DocM [SrcSpan])

(<>) :: AstElement a -> AstElement a -> AstElement a
Ast a <> InfoPoint p = Ast $ do
  (a', ps) <- a
  p' <- p
  return (a', ps ++ p')
InfoPoint p <> Ast a = Ast $ do
  p' <- p
  (a', ps) <- a
  return (a', p' ++ ps)
InfoPoint l <> InfoPoint r = InfoPoint $ do
  l' <- l
  r' <- r
  return $ l' ++ r'

(<\/>) :: AstElement (a -> b) -> AstElement a -> AstElement b
(<\/>) (Ast f) (Ast a) = Ast $ do
  (a', p) <- a
  (f', ps) <- f
  return (f' a', ps ++ p)


(|++|) :: AstElement [a] -> AstElement [a] -> AstElement [a]
Ast l |++| Ast r = Ast $ do
-- what if l and r are lists
  (l', ls) <- l
  (r', rs) <- r
  return (l' ++ r', ls ++ rs)

(|+|) :: AstElement a -> AstElement [a] -> AstElement [a]
Ast l |+| Ast r = Ast $ do
-- what if l and r are lists
  (l', ls) <- l
  (r', rs) <- r
  return (l':r', ls ++ rs)


-- --------------------------------------------------------------------------

ast :: (Annotated ast, AstPretty ast) => ast a -> AstElement (ast SrcSpanInfo)
ast a = Ast $ do
  a' <- astPretty a
  return (a', [])

astInfoPoint :: (Annotated ast, AstPretty ast) => ast a -> AstElement (ast SrcSpanInfo)
astInfoPoint a = Ast $ do
  a' <- astPretty a
  return (a', [srcInfoSpan $ ann a'])

astArr :: (Annotated ast, AstPretty ast) => ast a -> AstElement [ast SrcSpanInfo]
astArr a = Ast $ do
  a' <- astPretty a
  return ([a'], [])

raw :: String -> a -> AstElement a
raw "" a = Ast $ return (a, [])
raw s a = Ast $ do
  s' <- format s
  return (a, [s'])

rawS :: String -> AstElement String
rawS s = raw s s

sepPoint :: DocM a -> AstElement b
sepPoint p = InfoPoint $ do
  _ <- p
  return []

infoPoint :: String -> AstElement a
infoPoint s = InfoPoint $ do
  p <- format s
  return [p]

may :: (a -> AstElement b) -> Maybe a -> AstElement (Maybe b)
may f (Just a) = Ast $ do
  let Ast f' = f a
  (a', p) <- f'
  return (Just a', p)
may _ Nothing = Ast $ return (Nothing, [])

-- --------------------------------------------------------------------------

list:: (Annotated ast, AstPretty ast) => AstElement [ast SrcSpanInfo] -> [ast a] -> AstElement [ast SrcSpanInfo]
list  _ [] = Ast $ return ([], [])
list sep es = let (e:es') = reverse es in foldl' (\ ac i -> (ast i) |+| (sep <> ac) ) (astArr e) es'

-- --------------------------------------------------------------------------

startPretty :: (a -> b) -> AstElement b
startPretty f = Ast $ return (f undefined, [])

-- --------------------------------------------------------------------------

resultPretty :: Annotated ast => AstElement (ast SrcSpanInfo) -> DocM (ast SrcSpanInfo)
resultPretty (Ast res) = do
  sp <- getPos
  (m, ps) <- res
  ep <- getPos
  let span = SrcSpanInfo (mkSrcSpan sp ep) ps
  return $ amap (const span) m

-- --------------------------------------------------------------------------

class AstPretty ast where
  astPretty :: ast a -> DocM (ast SrcSpanInfo)

---------------------------------------------------------------------
-- Annotated version

-------------------------  Pretty-Print a Module --------------------

instance AstPretty Module where
  astPretty (Module _ mbHead os imp decls) =
    resultPretty $ startPretty impl
      <\/> vcatList os
      <> sepPoint myVcat
      <\/> may ast mbHead
      <> sepPoint myVcat
      <\/> prettyLs imp
      <> sepPoint myVcat
      <\/> prettyLs decls
      where
        impl _ os h i d = Module undefined h os i d
        vcatList dl = list (sepPoint myVcat) dl
        prettyLs dl = (if isJust mbHead then topLevel else vcatList) dl
  astPretty (XmlPage pos _mn os n attrs mattr cs) = undefined
  astPretty (XmlHybrid pos mbHead os imp decls n attrs mattr cs) = undefined

--------------------------  Module Header ------------------------------

instance AstPretty ModuleHead where
 -- mySep
  astPretty (ModuleHead _ m mbWarn mbExportList) =
    resultPretty $ startPretty ModuleHead <> infoPoint "module"
        <> sepPoint hsep
        <\/> ast m
        <> sepPoint fsep
        <\/>  may ast mbWarn
        <> sepPoint fsep
        <\/>  may ast mbExportList
        <> sepPoint fsep
        <> infoPoint "where"

-- --------------------------------------------------------------------------

instance AstPretty WarningText where
    astPretty w = case w of
      (DeprText _ s) -> impl DeprText "{-# DEPRECATED" s
      (WarnText _ s) -> impl WarnText "{-# WARNING"    s
      where
        -- mySep
      impl f c s = resultPretty $ startPretty f <> infoPoint c <> sepPoint hsep <\/> rawS s <> sepPoint fsep <> infoPoint "#}"


-- --------------------------------------------------------------------------

instance AstPretty ModuleName where
  astPretty (ModuleName _ s) = resultPretty $ startPretty ModuleName <\/> rawS s

-- --------------------------------------------------------------------------

instance AstPretty ExportSpecList where
  astPretty (ExportSpecList _ especs) =
    resultPretty $ startPretty ExportSpecList <\/> parenList especs

-- --------------------------------------------------------------------------

instance AstPretty ExportSpec where

  astPretty (EVar _ name) = resultPretty $ startPretty EVar <\/> astInfoPoint name

  astPretty (EAbs _ name) = resultPretty $ startPretty EAbs <\/> astInfoPoint name

  astPretty (EThingAll _ name) =
    resultPretty $ startPretty EThingAll <\/> ast name <> infoPoint "(..)"

  astPretty (EThingWith _ name nameList) =
    resultPretty $ startPretty EThingWith
      <\/> ast name
      <\/> parenList nameList

  astPretty (EModuleContents _ m) = resultPretty $ startPretty EModuleContents <\/> astInfoPoint m

-- --------------------------------------------------------------------------

instance AstPretty ImportDecl where
  astPretty (ImportDecl _ mod qual src mbPkg mbName mbSpecs) =
    resultPretty $ startPretty impl
      -- markLine
      -- mySep
      <> infoPoint "import"
      <> sepPoint hsep
      <\/> raw (if src then "{-# SOURCE #-}" else "") src
      <> sepPoint fsep
      <\/>  raw (if qual then "qualified" else "") qual
      <> sepPoint fsep
      <\/>  may rawS mbPkg
      <> sepPoint fsep
      <\/>  ast mod
      <> sepPoint fsep
      <\/>  may ast mbName
      <> sepPoint fsep
      <\/>  may ast mbSpecs
    where impl l s q p m n sp = ImportDecl undefined m q s p n sp

instance AstPretty ImportSpecList where
  astPretty (ImportSpecList _ b ispecs) =
    resultPretty $ startPretty ImportSpecList
      <\/> raw (if b then "hiding" else "") b
      <>   sepPoint hsep
      <\/> parenList ispecs

instance AstPretty ImportSpec where
  astPretty (IVar _ name)                = resultPretty $ startPretty IVar <\/> ast name
  astPretty (IAbs _ name)                = resultPretty $ startPretty IAbs <\/> ast name
  astPretty (IThingAll _ name)           =
    resultPretty $ startPretty IThingAll <\/> ast name <> infoPoint "(..)"
  astPretty (IThingWith _ name nameList) =
    resultPretty $ startPretty IThingWith <\/> ast name <\/> parenList nameList


-------------------------  Declarations ------------------------------

instance AstPretty Decl where astPretty = undefined

------------------------- Pragmas ---------------------------------------

instance AstPretty ModulePragma where

  astPretty (LanguagePragma _ ns) =
    resultPretty $ startPretty LanguagePragma
    -- myFsep
      <> infoPoint "{-# LANGUAGE"
      <> sepPoint myFsep
      <\/> list (infoPoint "," <> sepPoint myFsep) ns
      <> sepPoint myFsep
      <> infoPoint "#-}"

  astPretty (OptionsPragma _ mbTool s) = do
    -- myFsep
    let
      opt = "{-# OPTIONS_" ++ case mbTool of
        Nothing -> ""
        Just (UnknownTool u) -> show u
        Just tool -> show tool in
      resultPretty $ startPretty OptionsPragma
        <\/> raw "" mbTool
        <>   infoPoint opt
        <> sepPoint myFsep
        <\/> rawS s
        <> sepPoint myFsep
        <>   infoPoint "#-}"

  astPretty (AnnModulePragma _ ann) =
    resultPretty $ startPretty AnnModulePragma
      -- myFsep
      <>   infoPoint "{-# ANN"
      <>   sepPoint myFsep
      <\/> astInfoPoint ann
      <>   sepPoint myFsep
      <>   infoPoint "#-}"

-- --------------------------------------------------------------------------

instance AstPretty Annotation where
  astPretty (Ann _ n e) =
    resultPretty $ startPretty Ann
      -- myFsep
      <\/> astInfoPoint n
      <> sepPoint myFsep
      <\/> astInfoPoint e

  astPretty (TypeAnn _ n e) =
    resultPretty $ startPretty TypeAnn
      -- myFsep
      <> infoPoint "type"
      <> sepPoint myFsep
      <\/> astInfoPoint n
      <> sepPoint myFsep
      <\/> astInfoPoint e

  astPretty (ModuleAnn _ e) =
    resultPretty $ startPretty ModuleAnn
      -- myFsep
      <> infoPoint "module"
      <> sepPoint myFsep
      <\/> astInfoPoint e

------------------------- Data & Newtype Bodies -------------------------
instance AstPretty QualConDecl where astPretty = undefined

instance AstPretty GadtDecl where astPretty = undefined

instance AstPretty ConDecl where astPretty = undefined

instance AstPretty FieldDecl where astPretty = undefined

instance AstPretty BangType where astPretty = undefined

instance AstPretty Deriving where astPretty = undefined

------------------------- Types -------------------------
instance AstPretty Type where astPretty = undefined

instance AstPretty TyVarBind where astPretty = undefined


---------------------------- Kinds ----------------------------

instance AstPretty Kind where astPretty = undefined

------------------- Functional Dependencies -------------------
instance AstPretty FunDep where astPretty = undefined

------------------------- Expressions -------------------------

instance AstPretty Exp where astPretty = undefined

-- --------------------------------------------------------------------------

instance AstPretty  CName where
  astPretty (VarName _ name) = resultPretty $ startPretty VarName <\/> astInfoPoint name
  astPretty (ConName _ name) = resultPretty $ startPretty ConName <\/> astInfoPoint name

-- --------------------------------------------------------------------------

instance AstPretty SpecialCon where

  astPretty (UnitCon _) = resultPretty $ startPretty UnitCon <> infoPoint "()"
  astPretty (ListCon _) = resultPretty $ startPretty ListCon <> infoPoint "[]"
  astPretty (FunCon _) = resultPretty $ startPretty FunCon <> infoPoint "->"
  astPretty (TupleCon _ b n) =
    let
      hash = if b == Unboxed then "#" else ""
      point = "(" ++ hash ++ replicate (n-1) ',' ++ hash ++ ")" in
    resultPretty $ startPretty TupleCon
      <> infoPoint point
      <\/> raw "" b
      <\/> raw "" n

  astPretty (Cons _) = resultPretty $ startPretty Cons <> infoPoint ":"
  astPretty (UnboxedSingleCon _) = resultPretty $ startPretty UnboxedSingleCon <> infoPoint "(# #)"

-- --------------------------------------------------------------------------

instance AstPretty QName where
  astPretty qn
    | needParens = resultPretty $ enclose (infoPoint "(" <> sepPoint hsep) (infoPoint ")") (rawQName qn)
    | otherwise =  resultPretty $ rawQName qn
    where
      needParens = case qn of
        UnQual _    (Symbol _ _) -> True
        Qual   _  _ (Symbol _ _) -> True
        Special _ (Cons _)    -> True
        Special _ (FunCon _)  -> True
        _ -> False

-- --------------------------------------------------------------------------
-- QName utils
rawQName :: QName a -> AstElement (QName SrcSpanInfo)
rawQName (Qual _ mn n)  =
  startPretty Qual
    <\/> astInfoPoint mn
    <>   infoPoint "."
    <\/> rawName n
rawQName (UnQual _ n) =
  startPretty UnQual <\/> rawName n
rawQName (Special _ sc) = startPretty Special <\/> astInfoPoint sc

-- --------------------------------------------------------------------------

instance AstPretty Name where
  astPretty n@(Ident _ _) = resultPretty $ rawName n
  astPretty (Symbol _ s) =
    resultPretty $ startPretty Symbol
      <> infoPoint "("
      <> sepPoint hsep
      <\/> rawS s
      <> infoPoint ")"

-- --------------------------------------------------------------------------

isSymbol :: Name l -> Bool
isSymbol (Symbol _ _) = True
isSymbol _ = False

-- --------------------------------------------------------------------------

rawName :: Name t -> AstElement (Name a)
rawName (Symbol _ s) = startPretty Symbol <\/> rawS s
rawName (Ident _ s)  = startPretty Ident  <\/> rawS s

-- --------------------------------------------------------------------------

vcat :: DocM ()
vcat = do
  DocState (SrcLoc f l c) n <- get
  let s = if n < c then line else space $ n - c
  _ <- s
  return ()

-- --------------------------------------------------------------------------

hsep :: DocM ()
hsep = space 1

-- --------------------------------------------------------------------------
-- fsep prototype
fsep :: DocM ()
fsep  = do
  PrettyMode _ style  <- ask
  c <- getPos
  case mode style of
    PageMode ->
      if srcColumn c >= lineLength style then line else empty
    _ -> undefined

------------------------- pp utils -------------------------

parenList xs = infoPoint "(" <> list (infoPoint "," <> sepPoint myFsepSimple) xs <> infoPoint ")"

hashParenList xs = infoPoint "(#" <> list (infoPoint "," <> sepPoint myFsepSimple) xs <> infoPoint "#)"

braceList xs = infoPoint "{" <> list (infoPoint "," <> sepPoint myFsepSimple) xs <> infoPoint "}"

bracketList xs = infoPoint "[" <> list (sepPoint myFsepSimple) xs <> infoPoint "]"

enclose ob cb x = ob <> x <> cb

-- --------------------------------------------------------------------------
-- Wrap in braces and semicolons, with an extra space at the start in
-- case the first doc begins with "-", which would be scanned as {-

flatBlock :: (Annotated ast, AstPretty ast) => [ast a] -> AstElement [ast SrcSpanInfo]
flatBlock xs = infoPoint "{" <> sepPoint hsep <> list (infoPoint ";" <> sepPoint hsep) xs <> infoPoint "}"

-- Same, but put each thing on a separate line
prettyBlock :: (Annotated ast, AstPretty ast) => [ast a] -> AstElement [ast SrcSpanInfo]
prettyBlock xs = infoPoint "{" <> sepPoint hsep <> list (infoPoint ";" <> sepPoint vcat) xs <> infoPoint "}"

-- --------------------------------------------------------------------------

blankline = InfoPoint $ do
  PrettyMode mode _ <- ask
  let InfoPoint x = if spacing mode && layout mode /= PPNoLayout
      then
        sepPoint hsep <> sepPoint vcat
      else
        sepPoint empty
  x

topLevel dl = Ast $ do
  PrettyMode mode _ <- ask
  case layout mode of
    PPOffsideRule -> do
      let Ast x = sepPoint vcat <> list (sepPoint myVcat) dl
      x
    PPSemiColon -> do
      let Ast x = sepPoint vcat <> prettyBlock dl
      x
    PPInLine -> do
      let Ast x = sepPoint vcat <> prettyBlock dl
      x
    PPNoLayout -> do
      let Ast x = sepPoint hsep <> flatBlock dl
      x

-- --------------------------------------------------------------------------
{-
a $$$ b = layoutChoice (a vcat) (a <+>) b

mySep = layoutChoice mySep' hsep
  where
    -- ensure paragraph fills with indentation.
    mySep' [x]    = x
    mySep' (x:xs) = x <+> fsep xs
    mySep' []     = error "Internal error: mySep"
-}

-- --------------------------------------------------------------------------

myVcat = layoutChoice vcat hsep

-- --------------------------------------------------------------------------

myFsepSimple = layoutChoice fsep hsep

-- --------------------------------------------------------------------------
-- same, except that continuation lines are indented,
-- which is necessary to avoid triggering the offside rule.
-- myFsep prototype
myFsep  = layoutChoice fsep' hsep
  where
    fsep' = do
      PrettyMode m style  <- ask
      let n = onsideIndent m
      c <- getPos
      case mode style of
        PageMode ->
          if srcColumn c >= lineLength style - n then line else empty
        _ -> undefined

-- --------------------------------------------------------------------------

layoutChoice a b  = do
  PrettyMode mode _ <- ask
  if layout mode == PPOffsideRule || layout mode == PPSemiColon
  then a
  else b

-- --------------------------------------------------------------------------
-- Prefix something with a LINE pragma, if requested.
-- GHC's LINE pragma actually sets the current line number to n-1, so
-- that the following line is line n.  But if there's no newline before
-- the line we're talking about, we need to compensate by adding 1.

markLine :: DocM ()
markLine = return () -- not implemented yet

zeroSt = DocState (SrcLoc "unknown.hs"  1  1) 0

-- -----------------------------------------------------------------------------

ilist = [Ident undefined "fst", Ident undefined "second"]-- , Ident undefined "third", Ident undefined "four"]

rZero = renderWithMode (PrettyMode PR.defaultMode (Style PageMode 10 1.5)) zeroSt

ftst = let Ast x = list (infoPoint "," <> sepPoint hsep ) ilist in rZero x
