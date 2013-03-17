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
import Control.Applicative

import Data.Maybe
import Data.List hiding (intersperse)
import Data.Traversable

import Debug.Trace

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
  putPos $! SrcLoc f l $! c + x
  return ()

-- --------------------------------------------------------------------------

nest :: Int -> DocM ()
nest x = do
  DocState l n <- get
  put $! DocState l $ 1 + n + x
  return ()

-- --------------------------------------------------------------------------

data AstElem a = AstElem (DocM (a, [SrcSpan]))

instance Functor AstElem where
  fmap f (AstElem a) = AstElem $ do
    (a', ps) <- a
    return $ (f a', ps)

instance Applicative AstElem where
  pure a = AstElem $ return (a, [])

  (<*>) (AstElem f) (AstElem a) = AstElem $ do
    (a', p) <- a
    (f', ps) <- f
    return (f' a', ps ++ p)

  (*>) (AstElem a) (AstElem b) = AstElem $ do
    (_,  ap) <- a
    (b', bp) <- b
    return (b', ap ++ bp)

  (<*) (AstElem a) (AstElem b) = AstElem $ do
    (a', ap) <- a
    (_,  bp) <- b
    return (a', ap ++ bp)

-- --------------------------------------------------------------------------

infoElem :: String -> AstElem ()
infoElem s = AstElem $ do
  s' <- format s
  return ((), [s'])

sepElem :: DocM() -> AstElem ()
sepElem s = AstElem $ do
  _ <- s
  return ((), [])

astElem :: (AstPretty ast) => ast a -> AstElem (ast SrcSpanInfo)
astElem a = AstElem $ do
  a' <- astPretty a
  return (a', [])

strElem s = AstElem $ do
  s' <- format s
  return (s, [])

rawElem a s = undefined

-- --------------------------------------------------------------------------

intersperse :: Applicative f => f a1 -> [f a] -> f [a]
intersperse _ [] = pure []
intersperse sep (e:es) = (:) <$> e <*> (sequenceA $ map (sep *>) es)

-- --------------------------------------------------------------------------

--resultPretty :: Annotated ast => AstElem (ast SrcSpanInfo) -> DocM (ast SrcSpanInfo)
resultPretty (AstElem a) = do
  sp <- getPos
  (a', ps) <- a
  ep <- getPos
  let span = SrcSpanInfo (mkSrcSpan sp ep) ps
  return $ amap (const span) a'

-- --------------------------------------------------------------------------

class AstPretty ast where
  astPretty :: ast a -> DocM (ast SrcSpanInfo)

---------------------------------------------------------------------
-- Annotated version

-------------------------  Pretty-Print a Module --------------------

instance AstPretty Module where
  astPretty (Module _ mbHead os imp decls) =
    resultPretty $ pure impl
      <*> vcatList os
      <*  sepElem myVcat
      <*> traverse astElem mbHead
      <*  sepElem myVcat
      <*> prettyLs imp
      <*  sepElem myVcat
      <*> prettyLs decls
      where
        impl os h i d = Module undefined h os i d
        vcatList dl = intersperse (sepElem myVcat) $ map astElem dl
        prettyLs dl = (if isJust mbHead then topLevel else vcatList) dl
  astPretty (XmlPage pos _mn os n attrs mattr cs) = undefined
  astPretty (XmlHybrid pos mbHead os imp decls n attrs mattr cs) = undefined

--------------------------  Module Header ------------------------------

instance AstPretty ModuleHead where
 -- mySep
  astPretty (ModuleHead _ m mbWarn mbExportList) =
    resultPretty $ pure (ModuleHead undefined)
      <*  infoElem "module"
      <*  sepElem hsep
      <*> astElem m
      <*  sepElem fsep
      <*> traverse astElem mbWarn
      <*  sepElem fsep
      <*> traverse astElem mbExportList
      <*  sepElem fsep
      <*  infoElem "where"

-- --------------------------------------------------------------------------

instance AstPretty WarningText where
    astPretty w = case w of
      (DeprText _ s) -> impl DeprText "{-# DEPRECATED" s
      (WarnText _ s) -> impl WarnText "{-# WARNING"    s
      where
        -- mySep
      impl f c s = resultPretty $ pure (f undefined) <* infoElem c <* sepElem hsep <*> strElem s <* sepElem fsep <* infoElem "#}"


-- --------------------------------------------------------------------------

instance AstPretty ModuleName where
  astPretty (ModuleName _ s) = resultPretty $ pure (ModuleName undefined) <*> strElem s

-- --------------------------------------------------------------------------

instance AstPretty ExportSpecList where
  astPretty (ExportSpecList _ especs) =
    resultPretty $ pure (ExportSpecList undefined) <*> parenList especs

-- --------------------------------------------------------------------------

instance AstPretty ExportSpec where

  astPretty (EVar _ name) = resultPretty $ pure (EVar undefined) <*> astElem name

  astPretty (EAbs _ name) = resultPretty $ pure (EAbs undefined) <*> astElem name

  astPretty (EThingAll _ name) =
    resultPretty $ pure (EThingAll undefined) <*> astElem name <* infoElem "(..)"

  astPretty (EThingWith _ name nameList) =
    resultPretty $ pure (EThingWith undefined)
      <*> astElem name
      <*> parenList nameList

  astPretty (EModuleContents _ m) = resultPretty $ pure (EModuleContents undefined) <*> astElem m

-- --------------------------------------------------------------------------

instance AstPretty ImportDecl where
  astPretty (ImportDecl _ mod qual src mbPkg mbName mbSpecs) =
    resultPretty $ pure impl
      -- markLine
      -- mySep
      <* infoElem "import"
      <* sepElem hsep
      <*> rawElem  src (if src then "{-# SOURCE #-}" else "")
      <* sepElem fsep
      <*>  rawElem qual (if qual then "qualified" else "")
      <* sepElem fsep
      <*>  traverse strElem mbPkg
      <* sepElem fsep
      <*>  astElem mod
      <* sepElem fsep
      <*>  traverse astElem mbName
      <* sepElem fsep
      <*>  traverse astElem mbSpecs
    where impl s q p m n sp = ImportDecl undefined m q s p n sp

instance AstPretty ImportSpecList where
  astPretty (ImportSpecList _ b ispecs) =
    resultPretty $ pure (ImportSpecList undefined)
      <*> rawElem b (if b then "hiding" else "")
      <*  sepElem hsep
      <*> parenList ispecs

instance AstPretty ImportSpec where
  astPretty (IVar _ name)                = resultPretty $ pure (IVar undefined) <*> astElem name
  astPretty (IAbs _ name)                = resultPretty $ pure (IAbs undefined) <*> astElem name
  astPretty (IThingAll _ name)           =
    resultPretty $ pure (IThingAll undefined) <*> astElem name <* infoElem "(..)"
  astPretty (IThingWith _ name nameList) =
    resultPretty $ pure (IThingWith undefined) <*> astElem name <*> parenList nameList


-------------------------  Declarations ------------------------------

instance AstPretty Decl where
  astPretty (TypeDecl _ head htype) =
    resultPretty $ pure (TypeDecl undefined)
      <* blankline
      -- markLine
      -- mySep
      <* infoElem "type"
      <* sepElem hsep
      <*> astElem head
      <* sepElem fsep
      <* infoElem "="
      <* sepElem fsep
      <*> astElem htype

  astPretty _ = undefined

instance AstPretty DeclHead where astPretty = undefined

------------------------- Pragmas ---------------------------------------

instance AstPretty ModulePragma where

  astPretty (LanguagePragma _ ns) =
    resultPretty $ pure (LanguagePragma undefined)
    -- myFsep
      <* infoElem "{-# LANGUAGE"
      <* sepElem myFsep
      <*> undefined --  intersperse (infoElem "," <* sepElem myFsep) ns
      <* sepElem myFsep
      <* infoElem "#-}"

  astPretty (OptionsPragma _ mbTool s) = do
    -- myFsep
    let
      opt = "{-# OPTIONS_" ++ case mbTool of
        Nothing -> ""
        Just (UnknownTool u) -> show u
        Just tool -> show tool in
      resultPretty $ pure (OptionsPragma undefined)
        <*> pure mbTool
        <*  infoElem opt
        <*  sepElem myFsep
        <*> strElem s
        <*  sepElem myFsep
        <*  infoElem "#-}"

  astPretty (AnnModulePragma _ ann) =
    resultPretty $ pure (AnnModulePragma undefined)
      -- myFsep
      <*   infoElem "{-# ANN"
      <*   sepElem myFsep
      <*>  astElem ann
      <*   sepElem myFsep
      <*   infoElem "#-}"

-- --------------------------------------------------------------------------

instance AstPretty Annotation where
  astPretty (Ann _ n e) =
    resultPretty $ pure (Ann undefined)
      -- myFsep
      <*> astElem n
      <*  sepElem myFsep
      <*> astElem e

  astPretty (TypeAnn _ n e) =
    resultPretty $ pure (TypeAnn undefined)
      -- myFsep
      <* infoElem "type"
      <* sepElem myFsep
      <*> astElem n
      <* sepElem myFsep
      <*> astElem e

  astPretty (ModuleAnn _ e) =
    resultPretty $ pure (ModuleAnn undefined)
      -- myFsep
      <* infoElem "module"
      <* sepElem myFsep
      <*> astElem e

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
  astPretty (VarName _ name) = resultPretty $ pure (VarName undefined) <*> astElem name
  astPretty (ConName _ name) = resultPretty $ pure (ConName undefined) <*> astElem name

-- --------------------------------------------------------------------------

instance AstPretty SpecialCon where

  astPretty (UnitCon _) = resultPretty $ pure (UnitCon undefined) <* infoElem "()"
  astPretty (ListCon _) = resultPretty $ pure (ListCon undefined) <* infoElem "[]"
  astPretty (FunCon _) = resultPretty $ pure  (FunCon undefined)  <* infoElem "->"
  astPretty (TupleCon _ b n) =
    let
      hash = if b == Unboxed then "#" else ""
      point = "(" ++ hash ++ replicate (n-1) ',' ++ hash ++ ")" in
    resultPretty $ pure (TupleCon undefined)
      <* infoElem point
      <*> pure b
      <*> pure n

  astPretty (Cons _) = resultPretty $ pure (Cons undefined) <* infoElem ":"
  astPretty (UnboxedSingleCon _) = resultPretty $ pure (UnboxedSingleCon undefined) <* infoElem "(# #)"

-- --------------------------------------------------------------------------

instance AstPretty QName where
  astPretty qn
    | needParens = resultPretty $ enclose (infoElem "(" <* sepElem hsep) (infoElem ")") (rawQName qn)
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
rawQName :: QName a -> AstElem (QName SrcSpanInfo)
rawQName (Qual _ mn n)  =
  pure (Qual undefined)
    <*> astElem mn
    <*   infoElem "."
    <*> rawName n
rawQName (UnQual _ n) =
  pure (UnQual undefined) <*> rawName n
rawQName (Special _ sc) = pure (Special undefined) <*> astElem sc

-- --------------------------------------------------------------------------

instance AstPretty Name where
  astPretty n@(Ident _ _) = resultPretty $ rawName n
  astPretty (Symbol _ s) =
    resultPretty $ pure (Symbol undefined)
      <* infoElem "("
      <* sepElem hsep
      <*> strElem s
      <* infoElem ")"

-- --------------------------------------------------------------------------

isSymbol :: Name l -> Bool
isSymbol (Symbol _ _) = True
isSymbol _ = False

-- --------------------------------------------------------------------------

rawName :: Name t -> AstElem (Name a)
rawName (Symbol _ s) = pure (Symbol undefined) <*> strElem s
rawName (Ident _ s)  = pure (Ident  undefined) <*> strElem s

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
      if srcColumn c >= lineLength style then line else (pure ())
    _ -> undefined

------------------------- pp utils -------------------------

parenList xs =  infoElem "(" *> (intersperse (infoElem "," <* sepElem myFsepSimple) $ map astElem xs) <* infoElem ")"

hashParenList xs = infoElem "(#" *> (intersperse (infoElem "," <* sepElem myFsepSimple) $ map astElem xs) <* infoElem "#)"

braceList xs = infoElem "{" *> (intersperse (infoElem "," <* sepElem myFsepSimple) $ map astElem xs) <* infoElem "}"

bracketList xs = infoElem "[" *> (intersperse (sepElem myFsepSimple) $ map astElem xs) <* infoElem "]"

enclose ob cb x = ob *> x <* cb

-- --------------------------------------------------------------------------
-- Wrap in braces and semicolons, with an extra space at the start in
-- case the first doc begins with "-", which would be scanned as {-

flatBlock :: (Annotated astElem, AstPretty astElem) => [astElem a] -> AstElem [astElem SrcSpanInfo]
flatBlock xs = infoElem "{" *> sepElem hsep *> (intersperse (infoElem ";" <* sepElem hsep) $ map astElem xs) <* infoElem "}"

-- Same, but put each thing on a separate line
prettyBlock :: (Annotated astElem, AstPretty astElem) => [astElem a] -> AstElem [astElem SrcSpanInfo]
prettyBlock xs = infoElem "{" *> sepElem hsep *> (intersperse (infoElem ";" <* sepElem vcat) $ map astElem xs) <* infoElem "}"

-- --------------------------------------------------------------------------

blankline = AstElem $ do
  PrettyMode mode _ <- ask
  let AstElem x = if spacing mode && layout mode /= PPNoLayout
      then
        sepElem hsep <* sepElem vcat
      else
        sepElem (pure ())
  x

topLevel :: (Annotated astElem, AstPretty astElem) => [astElem a] -> AstElem [astElem SrcSpanInfo]
topLevel dl = AstElem $ do
  PrettyMode mode _ <- ask
  case layout mode of
    PPOffsideRule -> do
      let AstElem x = sepElem vcat *> (intersperse (sepElem myVcat) $ map astElem dl)
      x
    PPSemiColon -> do
      let AstElem x = sepElem vcat *> (prettyBlock dl)
      x
    PPInLine -> do
      let AstElem x = sepElem vcat *> (prettyBlock dl)
      x
    PPNoLayout -> do
      let AstElem x = sepElem hsep *> (flatBlock dl)
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
          if srcColumn c >= lineLength style - n then line else (pure ())
        _ -> undefined

-- --------------------------------------------------------------------------

layoutChoice a b  = do
  PrettyMode mode _ <- ask
  if layout mode == PPOffsideRule || layout mode == PPSemiColon
  then a
  else b

-- --------------------------------------------------------------------------

zeroSt = DocState (SrcLoc "unknown.hs"  1  1) 0

-- -----------------------------------------------------------------------------

ilist = [Ident undefined "fst", Ident undefined "second"]-- , Ident undefined "third", Ident undefined "four"]

renderAst (AstElem x) = renderWithMode (PrettyMode PR.defaultMode (Style PageMode 10 1.5)) zeroSt x

exampleList = renderAst $ intersperse (pure ()) (map astElem ilist)
