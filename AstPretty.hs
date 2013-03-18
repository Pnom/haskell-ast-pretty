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

infoElem :: String -> AstElem String
infoElem "" = AstElem $ return ("", [])
infoElem s = AstElem $ do
  s' <- format s
  return (s, [s'])

sepElem :: DocM() -> AstElem ()
sepElem s = AstElem $ do
  _ <- s
  return ((), [])

prettyNoInfoElem :: (AstPretty ast) => ast a -> AstElem (ast SrcSpanInfo)
prettyNoInfoElem a = annNoInfoElem $ astPretty a

prettyInfoElem :: (Annotated ast, AstPretty ast) => ast a -> AstElem (ast SrcSpanInfo)
prettyInfoElem a = annInfoElem $ astPretty a

annNoInfoElem :: DocM a -> AstElem a
annNoInfoElem a = AstElem $ do
  a' <- a
  return (a', [])

annInfoElem :: DocM a -> AstElem a
annInfoElem a = AstElem $ do
  sp <- getPos
  a' <- a
  ep <- getPos
  let ps = if sp == ep then [] else [mkSrcSpan sp ep]
  return $ (a', ps)

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
 -- | Pretty-print something in isolation.
  astPretty :: ast a -> DocM (ast SrcSpanInfo)

  -- | Pretty-print something in a precedence context.
  astPrettyPrec :: Int -> ast a -> DocM (ast SrcSpanInfo)
  astPretty = astPrettyPrec 0
  astPrettyPrec _ = astPretty

---------------------------------------------------------------------
-- Annotated version

-------------------------  Pretty-Print a Module --------------------

instance AstPretty Module where
  astPretty (Module _ mbHead os imp decls) =
    resultPretty $ pure impl
      <*> vcatList os
      <*  sepElem myVcat
      <*> traverse prettyNoInfoElem mbHead
      <*  sepElem myVcat
      <*> prettyLs imp
      <*  sepElem myVcat
      <*> prettyLs decls
      where
        impl os h i d = Module undefined h os i d
        vcatList dl = intersperse (sepElem myVcat) $ map prettyNoInfoElem dl
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
      <*> prettyNoInfoElem m
      <*  sepElem fsep
      <*> traverse prettyNoInfoElem mbWarn
      <*  sepElem fsep
      <*> traverse prettyNoInfoElem mbExportList
      <*  sepElem fsep
      <*  infoElem "where"

-- --------------------------------------------------------------------------

instance AstPretty WarningText where
    astPretty w = case w of
      (DeprText _ s) -> impl DeprText "{-# DEPRECATED" s
      (WarnText _ s) -> impl WarnText "{-# WARNING"    s
      where
        -- mySep
      impl f c s = resultPretty $ pure (f undefined) <* infoElem c <* sepElem hsep <*> infoElem s <* sepElem fsep <* infoElem "#}"

-- --------------------------------------------------------------------------

instance AstPretty ModuleName where
  astPretty (ModuleName _ s) = resultPretty $ pure (ModuleName undefined) <*> infoElem s

-- --------------------------------------------------------------------------

instance AstPretty ExportSpecList where
  astPretty (ExportSpecList _ especs) =
    resultPretty $ pure (ExportSpecList undefined) <*> parenList especs

-- --------------------------------------------------------------------------

instance AstPretty ExportSpec where

  astPretty (EVar _ name) = resultPretty $ pure (EVar undefined) <*> prettyNoInfoElem name

  astPretty (EAbs _ name) = resultPretty $ pure (EAbs undefined) <*> prettyNoInfoElem name

  astPretty (EThingAll _ name) =
    resultPretty $ pure (EThingAll undefined) <*> prettyNoInfoElem name <* infoElem "(..)"

  astPretty (EThingWith _ name nameList) =
    resultPretty $ pure (EThingWith undefined)
      <*> prettyNoInfoElem name
      <*> parenList nameList

  astPretty (EModuleContents _ m) = resultPretty $ pure (EModuleContents undefined) <*> prettyNoInfoElem m

-- --------------------------------------------------------------------------

instance AstPretty ImportDecl where
  astPretty (ImportDecl _ mod qual src mbPkg mbName mbSpecs) =
    resultPretty $ pure impl
      -- markLine
      -- mySep
      <*  infoElem "import"
      <*  sepElem hsep
      <*> pure src <* (infoElem $ if src then "{-# SOURCE #-}" else "")
      <*  sepElem fsep
      <*> pure qual <* (infoElem $ if qual then "qualified" else "")
      <*  sepElem fsep
      <*> traverse infoElem mbPkg
      <*  sepElem fsep
      <*> prettyNoInfoElem mod
      <*  sepElem fsep
      <*> traverse prettyNoInfoElem mbName
      <*  sepElem fsep
      <*> traverse prettyNoInfoElem mbSpecs
    where impl s q p m n sp = ImportDecl undefined m q s p n sp

instance AstPretty ImportSpecList where
  astPretty (ImportSpecList _ b ispecs) =
    resultPretty $ pure (ImportSpecList undefined)
      <*> pure b <* (infoElem $ if b then "hiding" else "")
      <*  sepElem hsep
      <*> parenList ispecs

instance AstPretty ImportSpec where
  astPretty (IVar _ name)                = resultPretty $ pure (IVar undefined) <*> prettyNoInfoElem name
  astPretty (IAbs _ name)                = resultPretty $ pure (IAbs undefined) <*> prettyNoInfoElem name
  astPretty (IThingAll _ name)           =
    resultPretty $ pure (IThingAll undefined) <*> prettyNoInfoElem name <* infoElem "(..)"
  astPretty (IThingWith _ name nameList) =
    resultPretty $ pure (IThingWith undefined) <*> prettyNoInfoElem name <*> parenList nameList


-------------------------  Declarations ------------------------------

instance AstPretty Decl where
  astPretty (TypeDecl _ head htype) =
    resultPretty $ pure (TypeDecl undefined)
      <* blankline
      -- markLine
      -- mySep
      <* infoElem "type"
      <* sepElem hsep
      <*> prettyNoInfoElem head
      <* sepElem fsep
      <* infoElem "="
      <* sepElem fsep
      <*> prettyNoInfoElem htype

  astPretty _ = undefined

instance AstPretty DeclHead where astPretty = undefined

------------------------- Pragmas ---------------------------------------

instance AstPretty ModulePragma where

  astPretty (LanguagePragma _ ns) =
    resultPretty $ pure (LanguagePragma undefined)
    -- myFsep
      <* infoElem "{-# LANGUAGE"
      <* sepElem myFsep
      <*> intersperse (infoElem "," <* sepElem myFsep) (map prettyNoInfoElem ns)
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
        <*> infoElem s
        <*  sepElem myFsep
        <*  infoElem "#-}"

  astPretty (AnnModulePragma _ ann) =
    resultPretty $ pure (AnnModulePragma undefined)
      -- myFsep
      <*   infoElem "{-# ANN"
      <*   sepElem myFsep
      <*>  prettyNoInfoElem ann
      <*   sepElem myFsep
      <*   infoElem "#-}"

-- --------------------------------------------------------------------------

instance AstPretty Annotation where
  astPretty (Ann _ n e) =
    resultPretty $ pure (Ann undefined)
      -- myFsep
      <*> prettyNoInfoElem n
      <*  sepElem myFsep
      <*> prettyNoInfoElem e

  astPretty (TypeAnn _ n e) =
    resultPretty $ pure (TypeAnn undefined)
      -- myFsep
      <* infoElem "type"
      <* sepElem myFsep
      <*> prettyNoInfoElem n
      <* sepElem myFsep
      <*> prettyNoInfoElem e

  astPretty (ModuleAnn _ e) =
    resultPretty $ pure (ModuleAnn undefined)
      -- myFsep
      <* infoElem "module"
      <* sepElem myFsep
      <*> prettyNoInfoElem e

------------------------- Data & Newtype Bodies -------------------------
instance AstPretty QualConDecl where astPretty = undefined

instance AstPretty GadtDecl where astPretty = undefined

instance AstPretty ConDecl where astPretty = undefined

instance AstPretty FieldDecl where astPretty = undefined

instance AstPretty BangType where astPretty = undefined

instance AstPretty Deriving where astPretty = undefined

------------------------- Types -------------------------
ppBType :: Type a -> DocM (Type SrcSpanInfo)
ppBType = astPrettyPrec prec_btype

ppAType :: Type a -> DocM (Type SrcSpanInfo)
ppAType = astPrettyPrec prec_atype

-- precedences for types
prec_btype, prec_atype :: Int
prec_btype = 1  -- left argument of ->,
                -- or either argument of an infix data constructor
prec_atype = 2  -- argument of type or data constructor, or of a class

instance AstPretty Type where
  astPrettyPrec p (TyForall _ mtvs ctxt htype) = resultPretty t
    -- parensIf (p > 0) $ myFsep [ppForall mtvs, ppContext ctxt, pretty htype]
    where
      t = parensIf (p > 0) $ pure (TyForall undefined)
        -- myFsep
        <*> (traverse (ppForall . map prettyNoInfoElem) mtvs)
        <*  sepElem myFsep
        <*> traverse prettyNoInfoElem ctxt
        <* sepElem myFsep
        <*> prettyNoInfoElem htype
  astPrettyPrec p (TyFun _ a b) = resultPretty t
    -- myFsep [ppBType a, text "->", pretty b]
    where
      t = parensIf (p > 0) $ pure (TyFun undefined)
        <*> (annNoInfoElem $ ppBType a)
        <*  sepElem myFsep
        <*  infoElem "->"
        <*  sepElem myFsep
        <*> prettyNoInfoElem b
  astPrettyPrec _ (TyTuple _ bxd l) =
    resultPretty $ pure (TyTuple undefined)
      <*> pure bxd
      <*> l'
    where
      l' = case bxd of
        Boxed   -> parenList l
        Unboxed -> hashParenList l
  astPrettyPrec _ (TyList _ t)  = resultPretty $ pure (TyList undefined) <*> t'
    where t' = enclose (infoElem "[") (infoElem "]") (prettyNoInfoElem t)
  astPrettyPrec  p (TyApp _ a b) = resultPretty t
    -- parensIf (p > prec_btype) $ myFsep [pretty a, ppAType b]
    where
      t = parensIf (p > prec_btype) $
        pure (TyApp undefined)
        <*  sepElem myFsep
        <*> prettyNoInfoElem a
        <*  sepElem myFsep
        <*> (annNoInfoElem $ ppAType a)
        <*   sepElem myFsep
  astPrettyPrec _ (TyVar _ t)  = resultPretty $ pure (TyVar undefined)  <*> prettyNoInfoElem t
  astPrettyPrec _ (TyCon _ t)  = resultPretty $ pure (TyCon  undefined) <*> prettyNoInfoElem t
  astPrettyPrec _ (TyParen _ t)  = resultPretty $ pure (TyParen undefined) <*> t'
    where t' = enclose (infoElem "(") (infoElem ")") (prettyNoInfoElem t)
  astPrettyPrec _ (TyInfix _ a op b)  = resultPretty $ pure (TyInfix undefined)
    -- myFsep [pretty a, ppQNameInfix op, pretty b]
    <*> prettyNoInfoElem a
    <* sepElem myFsep
    <*> ppQNameInfix op
    <* sepElem myFsep
    <*> prettyNoInfoElem b
  astPrettyPrec  _ (TyKind _ t k) = resultPretty t'
    -- parens (myFsep [pretty t, text "::", pretty k])
    where
      t' = pure (TyKind undefined)
        <*  infoElem "("
        <*  sepElem myFsep
        <*> prettyNoInfoElem t
        <*   sepElem myFsep
        <*   infoElem "::"
        <*> prettyNoInfoElem k
        <*   sepElem myFsep
        <*  infoElem ")"

-- --------------------------------------------------------------------------

instance AstPretty TyVarBind where astPretty = undefined

ppForall :: [AstElem a] -> AstElem [a]
ppForall [] = pure []
ppForall vs = infoElem "forall" *> sepElem myFsep *> intersperse (sepElem myFsep) vs <* infoElem "."

---------------------------- Kinds ----------------------------

instance AstPretty Kind where astPretty = undefined

------------------- Functional Dependencies -------------------
instance AstPretty FunDep where astPretty = undefined

------------------------- Expressions -------------------------

instance AstPretty Exp where astPretty = undefined

-- --------------------------------------------------------------------------

instance AstPretty IPName where
  astPretty = undefined

-- --------------------------------------------------------------------------

instance AstPretty IPBind where
  astPretty = undefined

-- --------------------------------------------------------------------------

instance AstPretty  CName where
  astPretty (VarName _ name) = resultPretty $ pure (VarName undefined) <*> prettyNoInfoElem name
  astPretty (ConName _ name) = resultPretty $ pure (ConName undefined) <*> prettyNoInfoElem name

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
    <*> prettyNoInfoElem mn
    <*  infoElem "."
    <*> rawName n
rawQName (UnQual _ n) =
  pure (UnQual undefined) <*> rawName n
rawQName (Special _ sc) = pure (Special undefined) <*> prettyNoInfoElem sc

ppQNameInfix :: QName a -> AstElem (QName SrcSpanInfo)
ppQNameInfix name
  | isSymbolName (getName name) = rawQName name
  | otherwise = infoElem "`" *> rawQName name <* infoElem "`"

-- --------------------------------------------------------------------------

instance AstPretty Name where
  astPretty n@(Ident _ _) = resultPretty $ rawName n
  astPretty (Symbol _ s) =
    resultPretty $ pure (Symbol undefined)
      <* infoElem "("
      <* sepElem hsep
      <*> infoElem s
      <* infoElem ")"

-- --------------------------------------------------------------------------

isSymbolName :: Name l -> Bool
isSymbolName (Symbol _ _) = True
isSymbolName _ = False

getName :: QName l -> Name l
getName (UnQual _ s) = s
getName (Qual _ _ s) = s
getName (Special _ (Cons _)) = Symbol undefined ":"
getName (Special _ (FunCon _)) = Symbol undefined  "->"
getName (Special _ s) = Ident undefined (specialName s)

specialName :: SpecialCon l -> String
specialName (UnitCon _) = "()"
specialName (ListCon _) = "[]"
specialName (FunCon  _) = "->"
specialName (TupleCon _ b n) = "(" ++ hash ++ replicate (n-1) ',' ++ hash ++ ")"
    where hash = if b == Unboxed then "#" else ""
specialName (Cons _) = ":"
specialName (UnboxedSingleCon _) = "(# #)"

rawName :: Name t -> AstElem (Name a)
rawName (Symbol _ s) = pure (Symbol undefined) <*> infoElem s
rawName (Ident  _ s) = pure (Ident  undefined) <*> infoElem s

-- --------------------------------------------------------------------------

instance AstPretty Context where
  astPretty (CxEmpty _) = resultPretty $ pure (CxEmpty undefined) <* infoElem "()" <* sepElem hsep <* infoElem "=>"
  astPretty (CxSingle _ asst) =
    resultPretty $ pure (CxSingle undefined)
      <*> prettyNoInfoElem asst <* sepElem hsep <* infoElem "=>"
  astPretty (CxTuple _ assts) =
    resultPretty $ pure (CxTuple undefined)
      <*> parenList assts -- myFsep and parenList -> myFsep and myFsepSimple ???
      <* sepElem myFsep
      <* infoElem "=>"
  astPretty (CxParen _ asst) = resultPretty $ pure (CxParen undefined) <*> enclose (infoElem "(") (infoElem ")") (prettyNoInfoElem asst)

-- --------------------------------------------------------------------------
-- hacked for multi-parameter type classes
instance AstPretty Asst where
  astPretty (ClassA _ a ts)   = -- myFsep $ ppQName a : map ppAType ts
    resultPretty $ pure (ClassA undefined)
      <*> prettyNoInfoElem a
      <*  sepElem myFsep
      <*> undefined -- (intersperse (sepElem myFsep) $ map (prettyNoInfoElem . ppAType) ts)
  astPretty (InfixA _ a op b) =  -- myFsep $ [pretty a, ppQNameInfix op, pretty b]
    resultPretty $ pure (InfixA undefined)
      <*> prettyNoInfoElem a
      <*   sepElem myFsep
      <*> ppQNameInfix op
      <*   sepElem myFsep
      <*> prettyNoInfoElem b
  astPretty (IParam _ i t)    = -- myFsep $ [pretty i, text "::", pretty t]
    resultPretty $ pure (IParam undefined)
      <*> prettyInfoElem i
      <*  sepElem myFsep
      <*  infoElem "::"
      <*> prettyInfoElem t

  astPretty (EqualP _ t1 t2)  = -- myFsep $ [pretty t1, text "~", pretty t2]
    resultPretty $ pure (EqualP undefined)
      <*> prettyInfoElem t1
      <*   sepElem myFsep
      <*   infoElem "~"
      <*> prettyInfoElem t2

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

parenList xs =  infoElem "(" *> (intersperse (infoElem "," <* sepElem myFsepSimple) $ map prettyNoInfoElem xs) <* infoElem ")"

hashParenList xs = infoElem "(#" *> (intersperse (infoElem "," <* sepElem myFsepSimple) $ map prettyNoInfoElem xs) <* infoElem "#)"

braceList xs = infoElem "{" *> (intersperse (infoElem "," <* sepElem myFsepSimple) $ map prettyNoInfoElem xs) <* infoElem "}"

bracketList xs = infoElem "[" *> (intersperse (sepElem myFsepSimple) $ map prettyNoInfoElem xs) <* infoElem "]"

enclose ob cb x = ob *> x <* cb

parensIf :: Bool -> AstElem a -> AstElem a
parensIf p d = if p then infoElem "(" *> d <* infoElem ")" else d


-- --------------------------------------------------------------------------
-- Wrap in braces and semicolons, with an extra space at the start in
-- case the first doc begins with "-", which would be scanned as {-

flatBlock :: (Annotated prettyNoInfoElem, AstPretty prettyNoInfoElem) => [prettyNoInfoElem a] -> AstElem [prettyNoInfoElem SrcSpanInfo]
flatBlock xs = infoElem "{" *> sepElem hsep *> (intersperse (infoElem ";" <* sepElem hsep) $ map prettyNoInfoElem xs) <* infoElem "}"

-- Same, but put each thing on a separate line
prettyBlock :: (Annotated prettyNoInfoElem, AstPretty prettyNoInfoElem) => [prettyNoInfoElem a] -> AstElem [prettyNoInfoElem SrcSpanInfo]
prettyBlock xs = infoElem "{" *> sepElem hsep *> (intersperse (infoElem ";" <* sepElem vcat) $ map prettyNoInfoElem xs) <* infoElem "}"

-- --------------------------------------------------------------------------

blankline = AstElem $ do
  PrettyMode mode _ <- ask
  let AstElem x = if spacing mode && layout mode /= PPNoLayout
      then
        sepElem hsep <* sepElem vcat
      else
        sepElem (pure ())
  x

topLevel :: (Annotated prettyNoInfoElem, AstPretty prettyNoInfoElem) => [prettyNoInfoElem a] -> AstElem [prettyNoInfoElem SrcSpanInfo]
topLevel dl = AstElem $ do
  PrettyMode mode _ <- ask
  case layout mode of
    PPOffsideRule -> do
      let AstElem x = sepElem vcat *> (intersperse (sepElem myVcat) $ map prettyNoInfoElem dl)
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

exampleList = renderAst $ intersperse (pure ()) (map prettyNoInfoElem ilist)

