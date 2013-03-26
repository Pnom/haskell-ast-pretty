module AstPretty ( AstPretty(astPretty),
  DocState(..),
  DocM,
  format, line, space, nest,
  renderWithMode, renderWithDefMode,
  PrettyMode(..), defPrettyMode,
  parenList, braceList
  ) where

import Language.Haskell.Exts.Annotated hiding (paren)
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

getPos :: DocM SrcLoc
getPos = gets pos

putPos :: SrcLoc -> DocM SrcLoc
putPos l = do
  DocState _ n <- get
  put $! DocState l n
  return l

line :: DocM ()
line = do
  DocState (SrcLoc f l c) n <- get
  putPos $! SrcLoc f (l + 1) (if n > 0 then n else 1)
  return ()

space :: Int -> DocM ()
space x = do
  SrcLoc f l c <- getPos
  putPos $! SrcLoc f l $! c + x
  return ()

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
    (f', ps) <- f
    (a', p) <- a
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

noInfoElem :: String -> AstElem String
noInfoElem "" = AstElem $ return ("", [])
noInfoElem s = AstElem $ do
  s' <- format s
  return (s, [])

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

constrElem f = pure $ f undefined

infoList xs = map prettyInfoElem xs
noInfoList xs = map prettyNoInfoElem xs

-- --------------------------------------------------------------------------

intersperse :: Applicative f => f a1 -> [f a] -> f [a]
intersperse _ [] = pure []
intersperse sep (e:es) = sequenceA $ e : (map (sep *>) es)

-- --------------------------------------------------------------------------

intersperse1 :: Applicative f => f a1 -> f a2 -> [f a] -> f [a]
intersperse1 _ _ [] = pure []
intersperse1 sFrst sep [e] = sequenceA [e]
intersperse1 sFrst sep (e1: e2 : es) = sequenceA $ (e1 <* sFrst) : e2 : (map (sep *>) es)

traverseSep sep f m = traverse (\e -> f e <* sep) m

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
      <*> traverseSep (sepElem myVcat) prettyNoInfoElem mbHead
      <*> prettyLs imp
      <*  sepElem myVcat
      <*> prettyLs decls
      where
        impl os h i d = Module undefined h os i d
        vcatList dl = intersperse (sepElem myVcat) $ noInfoList dl
        prettyLs dl = (if isJust mbHead then topLevel else vcatList) dl
  astPretty (XmlPage pos _mn os n attrs mattr cs) = undefined
  astPretty (XmlHybrid pos mbHead os imp decls n attrs mattr cs) = undefined

--------------------------  Module Header ------------------------------

instance AstPretty ModuleHead where
 -- mySep
  astPretty (ModuleHead _ m mbWarn mbExportList) =
    resultPretty $ constrElem ModuleHead
      <*  infoElem "module"
      <*  sepElem hsep
      <*> prettyNoInfoElem m
      <*  sepElem fsep
      <*> traverseSep (sepElem fsep) prettyNoInfoElem mbWarn
      <*> traverseSep (sepElem fsep) prettyNoInfoElem mbExportList
      <*  infoElem "where"

-- --------------------------------------------------------------------------

instance AstPretty WarningText where
    astPretty w = case w of
      (DeprText _ s) -> impl DeprText "{-# DEPRECATED" s
      (WarnText _ s) -> impl WarnText "{-# WARNING"    s
      where
        -- mySep
      impl f c s = resultPretty $ constrElem f <* infoElem c <* sepElem hsep <*> infoElem s <* sepElem fsep <* infoElem "#}"

-- --------------------------------------------------------------------------

instance AstPretty ModuleName where
  astPretty (ModuleName _ s) = resultPretty $ constrElem ModuleName <*> infoElem s

-- --------------------------------------------------------------------------

instance AstPretty ExportSpecList where
  astPretty (ExportSpecList _ especs) =
    resultPretty $ constrElem ExportSpecList <*> parenList (noInfoList especs)

-- --------------------------------------------------------------------------

instance AstPretty ExportSpec where

  astPretty (EVar _ name) = resultPretty $ constrElem EVar <*> prettyNoInfoElem name

  astPretty (EAbs _ name) = resultPretty $ constrElem EAbs <*> prettyNoInfoElem name

  astPretty (EThingAll _ name) =
    resultPretty $ constrElem EThingAll <*> prettyNoInfoElem name <* infoElem "(..)"

  astPretty (EThingWith _ name nameList) =
    resultPretty $ constrElem EThingWith
      <*> prettyNoInfoElem name
      <*> parenList (noInfoList nameList)

  astPretty (EModuleContents _ m) = resultPretty $ constrElem EModuleContents <*> prettyNoInfoElem m

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
      <*> traverseSep (sepElem fsep) infoElem mbPkg
      <*> prettyNoInfoElem mod
      <*  sepElem fsep
      <*> traverseSep (sepElem fsep) prettyNoInfoElem mbName
      <*> traverseSep (sepElem fsep) prettyNoInfoElem mbSpecs
    where impl s q p m n sp = ImportDecl undefined m q s p n sp

instance AstPretty ImportSpecList where
  astPretty (ImportSpecList _ b ispecs) =
    resultPretty $ constrElem ImportSpecList
      <*> pure b <* (infoElem $ if b then "hiding" else "")
      <*  sepElem hsep
      <*> parenList (noInfoList ispecs)

instance AstPretty ImportSpec where
  astPretty (IVar _ name)                = resultPretty $ constrElem IVar <*> prettyNoInfoElem name
  astPretty (IAbs _ name)                = resultPretty $ constrElem IAbs <*> prettyNoInfoElem name
  astPretty (IThingAll _ name)           =
    resultPretty $ constrElem IThingAll <*> prettyNoInfoElem name <* infoElem "(..)"
  astPretty (IThingWith _ name nameList) =
    resultPretty $ constrElem IThingWith <*> prettyNoInfoElem name <*> parenList (noInfoList nameList)


-------------------------  Declarations ------------------------------

instance AstPretty Decl where
  astPretty (TypeDecl _ head htype) =
    resultPretty $ constrElem TypeDecl
      <* blankline
      -- mySep
      <* infoElem "type"
      <* sepElem hsep
      <*> prettyInfoElem head
      <* sepElem fsep
      <* infoElem "="
      <* sepElem fsep
      <*> prettyInfoElem htype

  astPretty (TypeFamDecl _ head mkind) =
    resultPretty $ constrElem TypeFamDecl
      <* blankline
      -- mySep
      <* infoElem "type"
    <*  sepElem hsep
      <* infoElem "family"
      <* sepElem fsep
      <*> ppDheadInDecl head
      <* sepElem fsep
      <*> ppOptKind mkind

  astPretty (DataDecl _ don mContext head constrList mDeriving) =
    resultPretty $ constrElem DataDecl
    <* blankline
    -- mySep
    <*> prettyInfoElem don
    <*  sepElem hsep
    <*> ppContext mContext
    <*  sepElem fsep
    <*> ppDheadInDecl head
    <*  sepElem hsep
    <*> ppConstrList constrList
    -- $$$ is the same as myVcat
    <*  sepElem myVcat
    <*> traverse ppDeriving mDeriving

  astPretty (GDataDecl _ don mContext head mkind gadtDecl mDeriving) =
    resultPretty $ constrElem GDataDecl
      <* blankline
      -- mySep
      <*> prettyInfoElem don
      <*  sepElem hsep
      <*> ppContext mContext
      <*  sepElem fsep
      <*> ppDheadInDecl head
      <*  sepElem fsep
      <*> ppOptKind mkind
      <*  sepElem fsep
      <*  infoElem "where"
      -- $$$ is the same as myVcat
      <*  sepElem myVcat
      <*> ppBody classIndent (noInfoList gadtDecl)
      <*  sepElem myVcat
      <*> traverse identDeriving mDeriving
    where
      identDeriving d = AstElem $ do
        let AstElem xs = ppBody letIndent [ppDeriving d]
        ([x], ps) <- xs
        return (x, ps)

  astPretty (DataFamDecl _ mContext head mKind) =
    resultPretty $ constrElem DataFamDecl
      <* blankline
      -- mySep
      <*  infoElem "data"
      <*  sepElem hsep
      <*  infoElem "family"
      <*  sepElem fsep
      <*> ppContext mContext
      <*  sepElem fsep
      <*> ppDheadInDecl head
      <*  sepElem fsep
      <*> ppOptKind mKind

  astPretty (TypeInsDecl _ tl tr) =
    resultPretty $ constrElem TypeInsDecl
      <* blankline
      -- mySep
      <*  infoElem "type"
      <*  sepElem hsep
      <*  infoElem "instance"
      <*  sepElem fsep
      <*> prettyInfoElem tl
      <*  sepElem fsep
      <*  infoElem "="
      <*  sepElem fsep
      <*> prettyInfoElem tr

  astPretty (DataInsDecl _ don t qConDecl mDeriving) =
    resultPretty $ constrElem DataInsDecl
      <* blankline
      -- mySep
      <*> prettyInfoElem don
      <*  sepElem hsep
      <*  infoElem "instance"
      <*  sepElem fsep
      <*> prettyInfoElem t
      <*  sepElem hsep
      <*> ppConstrList qConDecl
      -- $$$ is the same as myVcat
      <*  sepElem myVcat
      <*> traverse ppDeriving mDeriving

  astPretty (GDataInsDecl _ don t mKind gadtDecl mDeriving) =
    resultPretty $ constrElem GDataInsDecl
      <* blankline
      -- mySep
      <*> prettyInfoElem don
      <*  sepElem hsep
      <*  infoElem "instance"
      <*  sepElem fsep
      <*> prettyInfoElem t
      <*  sepElem fsep
      <*> ppOptKind mKind
      -- $$$ is the same as myVcat
      <*  sepElem myVcat
      <*> ppBody classIndent (noInfoList gadtDecl)
      <*  sepElem myVcat
      <*> traverse ppDeriving mDeriving

  astPretty (ClassDecl _ mContext head funDep mClassDecl) =
    resultPretty $ constrElem ClassDecl
      <* blankline
      -- mySep
      <*  infoElem "class"
      <*  sepElem hsep
      <*> ppContext mContext
      <*  sepElem fsep
      <*> ppDheadInDecl head
      <*  sepElem fsep
      <*> ppFunDeps funDep
      <*> traverse cDecl mClassDecl
    where
      cDecl cd = case cd of
        [] -> pure []
        cs -> sepElem fsep *> infoElem "where" *> sepElem myVcat *> ppBody classIndent (noInfoList cs)

  astPretty (InstDecl _ mContext instHead mInstDecl) =
    resultPretty $ constrElem InstDecl
      <* blankline
      -- mySep
      <*  infoElem "instance"
      <*  sepElem hsep
      <*> ppContext mContext
      <*  sepElem fsep
      <*> ppInstHeadInDecl instHead
      <*  sepElem fsep
      <*> traverse instDecl mInstDecl
    where
      instDecl i =  case i of
        [] -> pure []
        is -> sepElem fsep *> infoElem "where" *> sepElem myVcat *> ppBody classIndent (noInfoList is)

  astPretty (DerivDecl _ mContext instHead) =
    resultPretty $ constrElem DerivDecl
      <* blankline
      -- mySep
      <*  infoElem "deriving"
      <*  sepElem hsep
      <*  infoElem "instance"
      <* sepElem fsep
      <*> ppContext mContext
      <* sepElem fsep
      <*> ppInstHeadInDecl instHead

  astPretty (InfixDecl _ assoc mInt op) =
    resultPretty $ constrElem InfixDecl
      <* blankline
      -- mySep
      <*> prettyInfoElem assoc
      <*  sepElem hsep
      <*> traverseSep (sepElem fsep) (\i -> (infoElem $ show i) *> pure i) mInt
      <*> intersperse (sepElem fsep) (infoList op)

  astPretty (DefaultDecl _ t) =
    resultPretty $ constrElem DefaultDecl
      <*  blankline
      <*  infoElem "default"
      <*  sepElem hsep
      <*> parenList (noInfoList t)

  astPretty (SpliceDecl _ e) =
    resultPretty $ constrElem SpliceDecl
      <*  blankline
      <*> prettyInfoElem e

  astPretty (TypeSig _ ns t) =
    resultPretty $ constrElem TypeSig
      <*  blankline
      -- mySep
      <*> intersperse1 (sepElem hsep) (infoElem "," <* sepElem fsep) (noInfoList ns)
      <*  (sepElem $ case ns of [n] -> hsep; _ -> fsep)
      <*  infoElem "::"
      <*> prettyInfoElem t

  astPretty (FunBind _ ms) =
    resultPretty $ constrElem FunBind
      <*> intersperse sep (noInfoList ms)
    where
      sep = AstElem $ do
        PrettyMode mode _ <- ask
        let AstElem x = if layout mode == PPOffsideRule then (infoElem "" <* sepElem myVcat) else infoElem ";"
        x
  astPretty (PatBind _ pat mType rhs mBinds) =
    resultPretty $ constrElem PatBind
      -- myFsep
      <*> prettyInfoElem pat
      <*  sepElem myFsep
      <*> traverseSep  (sepElem myFsep) (\t -> infoElem "::" *> sepElem hsep *> prettyInfoElem t) mType
      <*> prettyInfoElem rhs
      <*  sepElem myVcat
      <*> traverse ppWhere mBinds
  astPretty (ForImp _ callConv mSafety mStr n t) =
    resultPretty $ constrElem ForImp
      <*  blankline
      -- mySep
      <*  infoElem "foreign import"
      <*  sepElem hsep
      <*> prettyInfoElem callConv
      <*  sepElem fsep
      <*> traverseSep (sepElem fsep) prettyInfoElem mSafety
      <*> traverseSep (sepElem fsep) infoElem mStr
      <*> prettyInfoElem n
      <*  sepElem fsep
      <*  infoElem "::"
      <*> prettyInfoElem t
  astPretty (ForExp _ callConv mStr n t) =
    resultPretty $ constrElem ForExp
      <*  blankline
      -- mySep
      <*  infoElem "foreign export"
      <*  sepElem hsep
      <*> prettyInfoElem callConv
      <*  sepElem fsep
      <*> traverseSep (sepElem fsep) infoElem mStr
      <*> prettyInfoElem n
      <*  sepElem fsep
      <*  infoElem "::"
      <*> prettyInfoElem t

  astPretty (RulePragmaDecl _ rs) =
    resultPretty $ constrElem RulePragmaDecl
      <*  blankline
      -- myVcat
      <*  infoElem "{-# RULES"
      <*  sepElem myVcat
      <*> intersperse (sepElem myVcat) (noInfoList rs)
      <*  sepElem myVcat
      <*  sepElem hsep
      <*  infoElem "#-}"

  astPretty (DeprPragmaDecl _ ds) =
    resultPretty $ constrElem DeprPragmaDecl
      <*  blankline
      -- myVcat
      <*  infoElem "{-# DEPRECATED"
      <*  sepElem myVcat
      <*> intersperse (sepElem myVcat) (map ppWarnDepr ds)
      <*  sepElem myVcat
      <*  sepElem hsep
      <*  infoElem "#-}"
  astPretty (WarnPragmaDecl _ ws) =
    resultPretty $ constrElem WarnPragmaDecl
      <*  blankline
      -- myVcat
      <*  infoElem "{-# WARNING"
      <*  sepElem myVcat
      <*> intersperse (sepElem myVcat) (map ppWarnDepr ws)
      <*  sepElem myVcat
      <*  sepElem hsep
      <*  infoElem "#-}"
  astPretty (InlineSig _ b mActivation qName) =
    resultPretty $ constrElem InlineSig
      <*  blankline
      -- mySep
      <*> pure b <* (infoElem $ if b then "{-# INLINE" else "{-# NOINLINE")
      <*  sepElem hsep
      <*> traverseSep (sepElem fsep) prettyInfoElem mActivation
      <*> prettyInfoElem qName
      <*  infoElem "#-}"

  astPretty (InlineConlikeSig _ mActivation qName) =
    resultPretty $ constrElem InlineConlikeSig
      <*  blankline
      -- mySep
      <* infoElem "{-# INLINE_CONLIKE"
      <*  sepElem hsep
      <*> traverseSep (sepElem fsep) prettyInfoElem mActivation
      <*> prettyInfoElem qName
      <*  infoElem "#-}"

  astPretty (SpecSig _ qName ts) =
    resultPretty $ constrElem SpecSig
      <*  blankline
      -- mySep
      <*  infoElem "{-# SPECIALISE"
      <*  sepElem hsep
      <*> prettyInfoElem qName
      <*  sepElem fsep
      <*  infoElem "::"
      <*  sepElem fsep
      <*> intersperse (infoElem "," <* sepElem fsep) (noInfoList ts)
      <*  sepElem fsep
      <*  infoElem "#-}"

  astPretty (SpecInlineSig _ b mActivation qName ts) =
    resultPretty $ constrElem SpecInlineSig
      <*  blankline
      -- mySep
      <*  infoElem "{-# SPECIALISE"
      <*  sepElem hsep
      <*> pure b <* (infoElem $ if b then "INLINE" else "NOINLINE")
      <*  sepElem fsep
      <*> traverseSep (sepElem fsep) prettyInfoElem mActivation
      <*> prettyInfoElem qName
      <*  sepElem fsep
      <*  infoElem "::"
      <*  sepElem fsep
      <*> intersperse (infoElem "," <* sepElem fsep) (noInfoList ts)
      <*  sepElem fsep
      <*  infoElem "#-}"

  astPretty (InstSig _ mContext ih ) =
    resultPretty $ constrElem InstSig
      <*  blankline
      -- mySep
      <*  infoElem "{-# SPECIALISE"
      <*  sepElem hsep
      <*  infoElem "instance"
      <*  sepElem fsep
      <*> traverseSep (sepElem fsep) prettyInfoElem mContext
      <*> ppInstHeadInDecl ih
      <*  sepElem fsep
      <*  infoElem "#-}"

  astPretty (AnnPragma _ annotation) =
    resultPretty $ constrElem AnnPragma
      <*  blankline
      -- mySep
      <*  infoElem "{-# ANN"
      <*  sepElem hsep
      <*> prettyInfoElem annotation
      <*  sepElem fsep
      <*  infoElem "#-}"

ppConstrList [] = sequenceA []
ppConstrList cs = sepElem hsep
  *> infoElem "="
  *> intersperse (sepElem hsep *> infoElem "|" *> sepElem myVcat) (noInfoList cs)

ppDheadInDecl dh = constrElem DHead
  <*> prettyInfoElem name
  <*  sepElem fsep
  <*> intersperse (sepElem fsep) (noInfoList tvs)
  where
    (name, tvs) = sDeclHead dh

ppInstHeadInDecl ih = constrElem IHead
  <*> prettyInfoElem qn
  <*  sepElem fsep
  <*> intersperse (sepElem fsep) (map (annNoInfoElem.ppAType) ts)
  where
    (qn, ts) = sInstHead ih

-- --------------------------------------------------------------------------

instance AstPretty DeclHead where
  astPretty (DHead _ n tvs) =
    -- mySep (pretty n : map pretty tvs)
    resultPretty $ constrElem DHead
      <*> prettyInfoElem n
      <*  sepElem hsep
      <*> intersperse (sepElem fsep) (noInfoList tvs)

  astPretty (DHInfix _ tva n tvb) =
    -- mySep [pretty tva, pretty n, pretty tvb]
    resultPretty $ constrElem DHInfix
      <*> prettyInfoElem tva
      <* sepElem hsep
      <*> prettyInfoElem n
      <* sepElem fsep
      <*> prettyInfoElem tvb

  astPretty (DHParen _ dh)        =
    -- parens (pretty dh)
    resultPretty.paren $ constrElem DHParen <*> prettyNoInfoElem dh

-- --------------------------------------------------------------------------

instance AstPretty InstHead where
  astPretty (IHead _ qn ts) =
    resultPretty $ constrElem IHead
      -- mySep
      <*> prettyInfoElem qn
      <*  sepElem hsep
      <*> intersperse (sepElem fsep) (infoList ts)
  astPretty (IHInfix _ ta qn tb) =
    resultPretty $ constrElem IHInfix
      -- mySep
      <*> prettyInfoElem ta
      <*  sepElem hsep
      <*> prettyInfoElem qn
      <*  sepElem fsep
      <*> prettyInfoElem tb
  astPretty (IHParen _ ih) =
    resultPretty.paren $ constrElem IHParen <*> prettyInfoElem ih

------------------------- Pragmas ---------------------------------------

instance AstPretty ModulePragma where

  astPretty (LanguagePragma _ ns) =
    resultPretty $ constrElem LanguagePragma
    -- myFsep
      <* infoElem "{-# LANGUAGE"
      <* sepElem myFsep
      <*> intersperse (infoElem "," <* sepElem myFsep) (noInfoList ns)
      <* sepElem myFsep
      <* infoElem "#-}"

  astPretty (OptionsPragma _ mbTool s) = do
    -- myFsep
    let
      opt = "{-# OPTIONS_" ++ case mbTool of
        Nothing -> ""
        Just (UnknownTool u) -> show u
        Just tool -> show tool in
      resultPretty $ constrElem OptionsPragma
        <*> pure mbTool
        <*  infoElem opt
        <*  sepElem myFsep
        <*> infoElem s
        <*  sepElem myFsep
        <*  infoElem "#-}"

  astPretty (AnnModulePragma _ ann) =
    resultPretty $ constrElem AnnModulePragma
      -- myFsep
      <*   infoElem "{-# ANN"
      <*   sepElem myFsep
      <*>  prettyNoInfoElem ann
      <*   sepElem myFsep
      <*   infoElem "#-}"

-- --------------------------------------------------------------------------

instance AstPretty Annotation where
  astPretty (Ann _ n e) =
    resultPretty $ constrElem Ann
      -- myFsep
      <*> prettyNoInfoElem n
      <*  sepElem myFsep
      <*> prettyNoInfoElem e

  astPretty (TypeAnn _ n e) =
    resultPretty $ constrElem TypeAnn
      -- myFsep
      <* infoElem "type"
      <* sepElem myFsep
      <*> prettyNoInfoElem n
      <* sepElem myFsep
      <*> prettyNoInfoElem e

  astPretty (ModuleAnn _ e) =
    resultPretty $ constrElem ModuleAnn
      -- myFsep
      <* infoElem "module"
      <* sepElem myFsep
      <*> prettyNoInfoElem e

-- --------------------------------------------------------------------------

instance AstPretty DataOrNew where
  astPretty (DataType _) = resultPretty $ constrElem DataType <* infoElem "data"
  astPretty (NewType  _) = resultPretty $ constrElem NewType <* infoElem "newtype"

-- --------------------------------------------------------------------------

instance AstPretty Assoc where
  astPretty (AssocNone  _) = resultPretty $ constrElem AssocNone  <* infoElem "infix"
  astPretty (AssocLeft  _) = resultPretty $ constrElem AssocLeft  <* infoElem "infixl"
  astPretty (AssocRight _) = resultPretty $ constrElem AssocRight <* infoElem "infixr"

-- --------------------------------------------------------------------------

instance AstPretty Match where

  astPretty m =
    case m of
      (InfixMatch _ pa n pbs rhs mWhere) -> res hd rhs mWhere
        where
          (n', l':pbs') = lhs n (pa:pbs)
          hd = constrElem InfixMatch
            <*> l'
            <*  sepElem myFsep
            <*> n'
            <*  sepElem myFsep
            <*> intersperse (sepElem myFsep) pbs'

      (Match _ n pbs rhs mWhere) -> res hd rhs mWhere
        where
          (n', pbs') = lhs n pbs
          hd = constrElem Match
            <*> n'
            <*  sepElem myFsep
            <*> intersperse (sepElem myFsep) pbs'
    where
      lhs n pbs = case pbs of
        l:r:pbs' | isSymbolName n ->
          let
            op  = infoElem $ if null pbs' then "" else "("
            cp  = infoElem $ if null pbs' then "" else ")"
            l'   = op *> prettyInfoElem l
            r'   = cp *> prettyInfoElem r
            n'   = ppName n
          in l' `seq` n' `seq` r' `seq` (n', l' : r' : map (annNoInfoElem.astPrettyPrec 2) pbs')
        _ -> let
          n'   = prettyInfoElem n
          pbs' = map (annNoInfoElem.astPrettyPrec 2) pbs
          in n' `seq` pbs' `seq` (n', pbs')
      res f rhs mWhere = resultPretty $ f
        <*  sepElem myFsep
        <*> prettyInfoElem rhs
        <*  sepElem myVcat -- same as $$$ in original pretty
        <*> traverse ppWhere mWhere




ppWhere (BDecls  _ []) = constrElem BDecls  <*> pure []
ppWhere (BDecls  _ l)  = constrElem BDecls  <* (sepElem $ nest 2) <* infoElem "where" <* sepElem myVcat <*> ppBody whereIndent (noInfoList l)
ppWhere (IPBinds _ b)  = constrElem IPBinds <* (sepElem $ nest 2) <* infoElem "where" <* sepElem myVcat <*> ppBody whereIndent (noInfoList b)

-- --------------------------------------------------------------------------

instance AstPretty ClassDecl where astPretty = undefined

instance AstPretty InstDecl where astPretty = undefined

instance AstPretty Rule where astPretty = undefined

ppWarnDepr :: AstPretty ast => ([ast a], String) -> AstElem ([ast SrcSpanInfo], String)
ppWarnDepr ([], txt) = pure (,) <*> pure []  <*> infoElem txt
ppWarnDepr (ns, txt) = pure (,)
  <*> intersperse1 (sepElem hsep) (infoElem "," <* sepElem fsep) (noInfoList ns)
  <*  sep ns
  <*> infoElem txt
  where
    sep [n] = sepElem hsep
    sep _   = sepElem fsep
-- --------------------------------------------------------------------------
instance AstPretty Activation where
  astPretty (ActiveFrom  _ i) = resultPretty $ constrElem ActiveFrom  <* infoElem "["  <*> pure i <* (infoElem $ show i) <* infoElem "]"
  astPretty (ActiveUntil _ i) = resultPretty $ constrElem ActiveUntil <* infoElem "[~" <*> pure i <* (infoElem $ show i) <* infoElem "]"

-- --------------------------------------------------------------------------

instance AstPretty RuleVar where
    astPretty (RuleVar _ n) = resultPretty $ constrElem RuleVar <*> prettyInfoElem n
    astPretty (TypedRuleVar _ n t) =
      resultPretty.paren $ constrElem TypedRuleVar
        <*  sepElem hsep
        <*> prettyInfoElem n
        <*  sepElem fsep
        <*  infoElem "::"
        <*  sepElem fsep
        <*> prettyInfoElem t
        <*  sepElem fsep

------------------------- FFI stuff -------------------------------------
instance AstPretty Safety where
  astPretty (PlayRisky _)  = resultPretty $ constrElem PlayRisky <* infoElem "unsafe"
  astPretty (PlaySafe _ b) = resultPretty $ constrElem PlaySafe  <*> pure b <* (infoElem $ if b then "threadsafe" else "safe")

-- --------------------------------------------------------------------------

instance AstPretty CallConv where
  astPretty (StdCall   _) = resultPretty $ constrElem StdCall   <* infoElem "stdcall"
  astPretty (CCall     _) = resultPretty $ constrElem CCall     <* infoElem "ccall"
  astPretty (CPlusPlus _) = resultPretty $ constrElem CPlusPlus <* infoElem "cplusplus"
  astPretty (DotNet    _) = resultPretty $ constrElem DotNet    <* infoElem "dotnet"
  astPretty (Jvm       _) = resultPretty $ constrElem Jvm       <* infoElem "jvm"
  astPretty (Js        _) = resultPretty $ constrElem Js        <* infoElem "js"

------------------------- Data & Newtype Bodies -------------------------

instance AstPretty QualConDecl where
  astPretty (QualConDecl _ mtvs mctxt con) =
    resultPretty $ constrElem QualConDecl
      <*> traverseSep (sepElem myFsep) (ppForall . noInfoList) mtvs
      <*  sepElem myFsep
      <*> ppContext mctxt
      <*  sepElem myFsep
      <*> prettyInfoElem con

-- --------------------------------------------------------------------------

instance AstPretty GadtDecl where
  astPretty (GadtDecl _ name ty) =
    resultPretty $ constrElem GadtDecl
      <*> prettyInfoElem name
      <*  sepElem myFsep
      <*  infoElem "::"
      <*  sepElem myFsep
      <*> prettyInfoElem ty

-- --------------------------------------------------------------------------

instance AstPretty ConDecl where astPretty = undefined

instance AstPretty FieldDecl where astPretty = undefined

-- --------------------------------------------------------------------------

instance AstPretty BangType where
  astPrettyPrec _ (BangedTy _ ty) =
    resultPretty $ constrElem BangedTy
      <*  infoElem "!"
      <*> (annInfoElem $ ppAType ty)

  astPrettyPrec p (UnBangedTy _ ty) = resultPretty $ constrElem UnBangedTy <*> (annInfoElem $ ppType p ty)

  astPrettyPrec p (UnpackedTy _ ty) =
    resultPretty $ constrElem UnpackedTy
      <*  infoElem "{-# UNPACK #-}"
      <*  sepElem hsep
      <*  infoElem "!"
      <*> (annInfoElem $ ppType p ty)

-- --------------------------------------------------------------------------

instance AstPretty Deriving where astPretty = undefined
{-
instance Pretty (A.Deriving l) where
        pretty (A.Deriving _ []) = text "deriving" <+> parenList []
        pretty (A.Deriving _ [A.IHead _ d []]) = text "deriving" <+> pretty d
        pretty (A.Deriving _ ihs) = text "deriving" <+> parenList (map pretty ihs)
-}

ppDeriving :: Deriving a -> AstElem (Deriving SrcSpanInfo)
ppDeriving (Deriving _ []) = constrElem Deriving <*> pure []
ppDeriving (Deriving _ is) = infoElem "deriving"
  *> sepElem hsep
  *> (parensIf useParen $ constrElem Deriving <*> sequenceA ppIheads)
  where
    iheads = map sInstHead is

    useParen = case iheads of
      [(qn, [])] -> False
      _ -> True

    ppIheads = case iheads of
      [(qn, [])] -> [constrElem IHead <*> prettyNoInfoElem qn <*> pure []]
      _ -> map ppDer iheads

    ppDer (qn, ts) = constrElem IHead
      -- mySep
      <*> prettyNoInfoElem qn
      <*  sepElem hsep
      <*> intersperse parenListSep (noInfoList ts)

------------------------- Types -------------------------

ppType :: Int -> Type a -> DocM (Type SrcSpanInfo)
ppType p a = astPrettyPrec p a


ppBType :: Type a -> DocM (Type SrcSpanInfo)
ppBType = ppType prec_btype

ppAType :: Type a -> DocM (Type SrcSpanInfo)
ppAType = ppType prec_atype

-- precedences for types
prec_btype, prec_atype :: Int
prec_btype = 1  -- left argument of ->,
                -- or either argument of an infix data constructor
prec_atype = 2  -- argument of type or data constructor, or of a class

instance AstPretty Type where
  astPrettyPrec p (TyForall _ mtvs ctxt htype) = resultPretty t
    -- parensIf (p > 0) $ myFsep [ppForall mtvs, ppContext ctxt, pretty htype]
    where
      t = parensIf (p > 0) $ constrElem TyForall
        -- myFsep
        <*> traverseSep (sepElem myFsep) (ppForall . noInfoList) mtvs
        <*> ppContext ctxt
        <* sepElem myFsep
        <*> prettyNoInfoElem htype
  astPrettyPrec p (TyFun _ a b) = resultPretty t
    -- myFsep [ppBType a, text "->", pretty b]
    where
      t = parensIf (p > 0) $ constrElem TyFun
        <*> (annNoInfoElem $ ppBType a)
        <*  sepElem myFsep
        <*  infoElem "->"
        <*  sepElem myFsep
        <*> prettyNoInfoElem b
  astPrettyPrec _ (TyTuple _ bxd l) =
    resultPretty $ constrElem TyTuple
      <*> pure bxd
      <*> l'
    where
      l' = case bxd of
        Boxed   -> parenList (noInfoList l)
        Unboxed -> hashParenList (noInfoList l)
  astPrettyPrec _ (TyList _ t)  = resultPretty $ constrElem TyList <*> t'
    where t' = enclose (infoElem "[") (infoElem "]") (prettyNoInfoElem t)
  astPrettyPrec  p (TyApp _ a b) = resultPretty t
    -- parensIf (p > prec_btype) $ myFsep [pretty a, ppAType b]
    where
      t = parensIf (p > prec_btype) $
        constrElem TyApp
        <*  sepElem myFsep
        <*> prettyNoInfoElem a
        <*  sepElem myFsep
        <*> (annNoInfoElem $ ppAType a)
        <*   sepElem myFsep
  astPrettyPrec _ (TyVar _ t)  = resultPretty $ constrElem TyVar  <*> prettyNoInfoElem t
  astPrettyPrec _ (TyCon _ t)  = resultPretty $ constrElem TyCon <*> prettyNoInfoElem t
  astPrettyPrec _ (TyParen _ t)  = resultPretty $ constrElem TyParen <*> t'
    where t' = enclose (infoElem "(") (infoElem ")") (prettyNoInfoElem t)
  astPrettyPrec _ (TyInfix _ a op b)  = resultPretty $ constrElem TyInfix
    -- myFsep [pretty a, ppQNameInfix op, pretty b]
    <*> prettyNoInfoElem a
    <* sepElem myFsep
    <*> ppQNameInfix op
    <* sepElem myFsep
    <*> prettyNoInfoElem b
  astPrettyPrec  _ (TyKind _ t k) = resultPretty t'
    -- parens (myFsep [pretty t, text "::", pretty k])
    where
      t' = paren $ constrElem TyKind
        -- myFsep
        <*  sepElem myFsep
        <*> prettyNoInfoElem t
        <*   sepElem myFsep
        <*   infoElem "::"
        <*> prettyNoInfoElem k
        <*   sepElem myFsep

-- --------------------------------------------------------------------------

instance AstPretty TyVarBind where
  astPretty (KindedVar _ var kind) =
    resultPretty.paren $ constrElem KindedVar
      -- myFsep
      <*  sepElem myFsep
      <*> prettyInfoElem var
      <*  sepElem myFsep
      <*  infoElem "::"
      <*  sepElem myFsep
      <*> prettyInfoElem kind
      <*  sepElem myFsep
  astPretty (UnkindedVar _ var) = resultPretty $ constrElem UnkindedVar <*> prettyInfoElem var

ppForall :: [AstElem a] -> AstElem [a]
ppForall [] = pure []
ppForall vs = infoElem "forall" *> sepElem myFsep *> intersperse (sepElem myFsep) vs <* infoElem "."

---------------------------- Kinds ----------------------------

instance AstPretty Kind where
  astPrettyPrec _ (KindStar _) = resultPretty $ constrElem KindStar <* infoElem "*"
  astPrettyPrec _ (KindBang _) = resultPretty $ constrElem KindBang <* infoElem "!"
  astPrettyPrec n (KindFn _ a b)  =
    resultPretty.parensIf (n > 0) $ constrElem KindFn
      -- myFsep
      <*> (annInfoElem $ astPrettyPrec 1 a)
      <*  sepElem myFsep
      <*  infoElem "->"
      <*  sepElem myFsep
      <*> prettyInfoElem b

  astPrettyPrec _ (KindParen _ k) =
    resultPretty.paren $ constrElem KindParen <*> prettyInfoElem k
  astPrettyPrec _ (KindVar _ n) = resultPretty $ constrElem KindVar <*> prettyInfoElem n

ppOptKind :: Maybe (Kind a) -> AstElem (Maybe (Kind SrcSpanInfo))
ppOptKind k = traverse (\ a -> infoElem "::" *> prettyInfoElem a) k

------------------- Functional Dependencies -------------------

instance AstPretty FunDep where
  astPretty (FunDep _ from to) =
    resultPretty $ constrElem FunDep
      <*> intersperse (sepElem myFsep) (noInfoList from)
      <*  sepElem myFsep
      <*  infoElem "->"
      <*> intersperse (sepElem myFsep) (noInfoList to)

ppFunDeps :: AstPretty ast => [ast a] -> AstElem [ast SrcSpanInfo]
ppFunDeps []  = sequenceA []
ppFunDeps fds = infoElem "|" *> sepElem myFsep *> intersperse (infoElem "," <* sepElem myFsep) (noInfoList fds)

------------------------- Expressions -------------------------

instance AstPretty Rhs        where
  astPretty (UnGuardedRhs _ e) =
    resultPretty $ constrElem UnGuardedRhs
      <*  infoElem "="
      <*> prettyInfoElem e
  astPretty (GuardedRhss _ guardList) =
    resultPretty $ constrElem GuardedRhss
      <*> intersperse (sepElem myVcat) (noInfoList guardList)

-- --------------------------------------------------------------------------

instance AstPretty GuardedRhs where astPretty = undefined

-- --------------------------------------------------------------------------

instance AstPretty Literal where
  astPretty (Int _ i s)  = resultPretty $ constrElem Int  <*> pure i <* (infoElem $ show i) <*> pure s
  astPretty (Char _ c s) = resultPretty $ constrElem Char <*> pure c <* (infoElem $ show c) <*> pure s
  astPretty (String _ s s') = resultPretty $ constrElem String <*> pure s <* (infoElem $ show s) <*> pure s'
  astPretty (Frac _ r s)    = resultPretty $ constrElem Frac <*> pure r <* (infoElem.show $ fromRational r) <*> pure s
  -- GHC unboxed literals:
  astPretty (PrimChar _ c s) = resultPretty $ constrElem PrimChar
    <*> pure c <* (infoElem $ show c ++ "#") <*> pure s
  astPretty (PrimString _ s s') = resultPretty $ constrElem PrimString
    <*> pure s <* (infoElem $ show s ++ "#") <*> pure s'
  astPretty (PrimInt _ i s)     = resultPretty $ constrElem PrimInt
    <*> pure i <* (infoElem $ show i ++ "#") <*> pure s
  astPretty (PrimWord _ w s)    = resultPretty $ constrElem PrimWord
    <*> pure w <* (infoElem $ show w ++ "##") <*> pure s
  astPretty (PrimFloat _ r s)   = resultPretty $ constrElem PrimFloat
    <*> pure r <* (infoElem $ (show $ fromRational r) ++ "#") <*> pure s
  astPretty (PrimDouble _ r s)  = resultPretty $ constrElem PrimFloat
    <*> pure r <* (infoElem $ (show $ fromRational r) ++ "##") <*> pure s

-- --------------------------------------------------------------------------

instance AstPretty Exp where

  astPrettyPrec _ (Lit _ l) = resultPretty $ constrElem Lit <*> prettyInfoElem l
  -- lambda stuff
  astPrettyPrec p (InfixApp _ a op b) = resultPretty $ constrElem InfixApp
    <*> prettyInfoElem a
    <*  sepElem myFsep
    <*> prettyInfoElem op
    <*  sepElem myFsep
    <*> prettyInfoElem b
  astPrettyPrec p (NegApp _ e) = resultPretty $ constrElem NegApp
    <*  infoElem "-"
    <*  sepElem myFsep
    <*> prettyInfoElem e
  astPrettyPrec p (App _ a b) = resultPretty $ constrElem App
    <*> prettyInfoElem a
    <*  sepElem myFsep
    <*> prettyInfoElem b
  astPrettyPrec p (Lambda _ expList body) = resultPretty $ constrElem Lambda
    <*  infoElem "\\"
    <*  sepElem myFsep
    <*> intersperse (sepElem myFsep) (noInfoList expList)
    <*  sepElem myFsep
    <*  infoElem "->"
    <*> prettyInfoElem body

  -- keywords
  -- two cases for lets
  astPrettyPrec p (Let _ (BDecls _ declList) letBody)  = resultPretty $ ppLetExp BDecls  declList letBody
  astPrettyPrec p (Let _ (IPBinds _ bindList) letBody) = resultPretty $ ppLetExp IPBinds bindList letBody
  astPrettyPrec p (If _ cond thenexp elsexp) = resultPretty $ constrElem If
    <*  infoElem "if"
    <*  sepElem myFsep
    <*> prettyInfoElem cond
    <*  sepElem myFsep
    <*  infoElem "then"
    <*  sepElem myFsep
    <*> prettyInfoElem thenexp
    <*  infoElem "else"
    <*  sepElem myFsep
    <*> prettyInfoElem elsexp
  astPrettyPrec p (Case _ cond altList) = resultPretty $ constrElem Case
    <*  infoElem "case"
    <*  sepElem myFsep
    <*> prettyInfoElem cond
    <*  sepElem myFsep
    <*  infoElem "of"
    <*  sepElem myVcat
    <*> ppBody caseIndent (noInfoList altList)
  astPrettyPrec p (Do _ stmtList) = resultPretty $ constrElem Do
    <*  infoElem "do"
    <*  sepElem myVcat
    <*> ppBody doIndent (noInfoList stmtList)
  astPrettyPrec p (MDo _ stmtList) = resultPretty $ constrElem MDo
    <*  infoElem "mdo"
    <*  sepElem myVcat
    <*> ppBody doIndent (noInfoList stmtList)
  -- Constructors & Vars
  astPrettyPrec _ (Var _ name) = resultPretty $ constrElem Var <*> prettyInfoElem name
  astPrettyPrec _ (IPVar _ ipname) = resultPretty $ constrElem IPVar <*> prettyInfoElem ipname
  astPrettyPrec _ (Con _ name) = resultPretty $ constrElem Con <*> prettyInfoElem name
--  astPrettyPrec _ (Tuple _ expList) = undefined
  astPrettyPrec _ (TupleSection _ mExpList) = resultPretty $ constrElem TupleSection
    <*> parenList (map (traverse prettyNoInfoElem) mExpList)

  -- weird stuff
  astPrettyPrec _ (Paren _ e) = resultPretty $ constrElem Paren <*> paren (prettyInfoElem e)

  astPrettyPrec _ (LeftSection _ e op) = undefined
  astPrettyPrec _ (RightSection _ op e) = undefined
  astPrettyPrec _ (RecConstr _ c fieldList) = undefined
  astPrettyPrec _ (RecUpdate _ e fieldList) = undefined
  -- Lists
  astPrettyPrec _ (List _ list) = undefined
  astPrettyPrec _ (EnumFrom _ e) = undefined
  astPrettyPrec _ (EnumFromTo _ from to) = undefined
  astPrettyPrec _ (EnumFromThen _ from thenE) = undefined
  astPrettyPrec _ (EnumFromThenTo _ from thenE to) = undefined
  astPrettyPrec _ (ListComp _ e qualList) = undefined
  astPrettyPrec _ (ParComp _ e qualLists) = undefined
  astPrettyPrec p (ExpTypeSig _ e ty) =  undefined
  -- Template Haskell
  astPrettyPrec _ (BracketExp _ b) = undefined
  astPrettyPrec _ (SpliceExp _ s) = undefined
  astPrettyPrec _ (TypQuote _ t)  = undefined
  astPrettyPrec _ (VarQuote _ x)  = undefined
  astPrettyPrec _ (QuasiQuote _ n qt) = undefined
  -- Hsx
  astPrettyPrec _ (XTag _ n attrs mattr cs) = undefined
  astPrettyPrec _ (XETag _ n attrs mattr) = undefined
  astPrettyPrec _ (XPcdata _ s) = undefined
  astPrettyPrec _ (XExpTag _ e) = undefined
  astPrettyPrec _ (XChildTag _ cs) = undefined

  -- Pragmas
  astPrettyPrec p (CorePragma _ s e) = undefined
  astPrettyPrec _ (SCCPragma  _ s e) = undefined
  astPrettyPrec _ (GenPragma  _ s (a,b) (c,d) e) = undefined
  -- Arrows
  astPrettyPrec p (Proc _ pat e) =  undefined
  astPrettyPrec p (LeftArrApp _ l r)      = undefined
  astPrettyPrec p (RightArrApp _ l r)     = undefined
  astPrettyPrec p (LeftArrHighApp _ l r)  = undefined
  astPrettyPrec p (RightArrHighApp _ l r) = undefined

instance AstPretty IPName where astPretty = undefined
instance AstPretty IPBind where astPretty = undefined

-- --------------------------------------------------------------------------

instance AstPretty XAttr where
  astPretty (XAttr _ n v) =
    resultPretty $ constrElem XAttr
    <*> prettyInfoElem n
    <*  sepElem myFsep
    <*  infoElem "="
    <*  sepElem myFsep
    <*> prettyInfoElem v

-- --------------------------------------------------------------------------

instance AstPretty XName where
  astPretty (XName _ n) = resultPretty $ constrElem XName <*> infoElem n
  astPretty (XDomName _ d n) =
    resultPretty $ constrElem XDomName
      <*> infoElem d
      <*  infoElem ":"
      <*> infoElem n

ppLetExp f l b = constrElem Let
  <*  infoElem "let"
  <*  sepElem hsep
  <*> (constrElem f <*> ppBody letIndent (noInfoList l))
  <*  sepElem myFsep
  <*  infoElem "in"
  <*> prettyInfoElem b

ppWith f binds = f
  <*  (sepElem $ nest 2)
  <*  infoElem "with"
  <*  sepElem myVcat
  <*> ppBody withIndent (noInfoList binds)
withIndent = whereIndent

--------------------- Template Haskell -------------------------

instance AstPretty Bracket where astPretty = undefined

-- --------------------------------------------------------------------------

instance AstPretty Splice where
  astPretty (IdSplice _ s) = resultPretty $ constrElem IdSplice <* infoElem "$" <*> infoElem s
  astPretty (ParenSplice _ e) =
    resultPretty $ constrElem ParenSplice
      <* infoElem "$(" <* sepElem myFsep <*> prettyInfoElem e <* sepElem myFsep <* infoElem ")"

------------------------- Patterns -----------------------------

instance AstPretty Pat where astPretty = undefined

-- --------------------------------------------------------------------------

instance AstPretty PXAttr where
  astPretty (PXAttr _ n p) =
    resultPretty $ constrElem PXAttr
      <*> prettyInfoElem n
      <*  sepElem myFsep
      <*  infoElem "="
      <*  sepElem myFsep
      <*> prettyInfoElem p

-- --------------------------------------------------------------------------

instance AstPretty PatField where
  astPretty (PFieldPat _ n p) =
    resultPretty $ constrElem PFieldPat
      <*> prettyInfoElem n
      <*  sepElem myFsep
      <*  infoElem "="
      <*  sepElem myFsep
      <*> prettyInfoElem p

  astPretty (PFieldPun _ name) = resultPretty $ constrElem PFieldPun <*> prettyInfoElem name
  astPretty (PFieldWildcard _) = resultPretty $ constrElem PFieldWildcard <* infoElem ".."

--------------------- Regular Patterns -------------------------

instance AstPretty RPat where astPretty = undefined

-- --------------------------------------------------------------------------

instance AstPretty RPatOp where
  astPretty (RPStar  _) = resultPretty $ constrElem RPStar  <* infoElem "*"
  astPretty (RPStarG _) = resultPretty $ constrElem RPStarG <* infoElem "*!"
  astPretty (RPPlus  _) = resultPretty $ constrElem RPPlus  <* infoElem "+"
  astPretty (RPPlusG _) = resultPretty $ constrElem RPPlusG <* infoElem "+!"
  astPretty (RPOpt   _) = resultPretty $ constrElem RPOpt   <* infoElem "?"
  astPretty (RPOptG  _) = resultPretty $ constrElem RPOptG  <* infoElem "?!"

------------------------- Case bodies  -------------------------
instance AstPretty Alt where astPretty = undefined
instance AstPretty GuardedAlts where astPretty = undefined
instance AstPretty GuardedAlt where astPretty = undefined

------------------------- Statements in monads, guards & list comprehensions -----

instance AstPretty Stmt where astPretty = undefined

-- --------------------------------------------------------------------------

instance AstPretty QualStmt where
  -- myFsep
  astPretty (QualStmt _ s) = resultPretty $ constrElem QualStmt <*> prettyInfoElem s
  astPretty (ThenTrans _ f) =
    resultPretty $ constrElem ThenTrans <* infoElem "then" <* sepElem myFsep <*> prettyInfoElem f
  astPretty (ThenBy _ f e) =
    resultPretty $ constrElem ThenBy
      <* infoElem "then"
      <* sepElem myFsep
      <*> prettyInfoElem f
      <* sepElem myFsep
      <* infoElem "by"
      <* sepElem myFsep
      <*> prettyInfoElem e
  astPretty (GroupBy _ e) =
    resultPretty $ constrElem GroupBy
      <* infoElem "then"
      <* sepElem myFsep
      <* infoElem "group"
      <* sepElem myFsep
      <* infoElem "by"
      <* sepElem myFsep
      <*> prettyInfoElem e
  astPretty (GroupUsing   _ f) =
    resultPretty $ constrElem GroupUsing
      <* infoElem "then"
      <* sepElem myFsep
      <* infoElem "group"
      <* sepElem myFsep
      <* infoElem "using"
      <* sepElem myFsep
      <*> prettyInfoElem f
  astPretty (GroupByUsing _ e f) =
    resultPretty $ constrElem GroupByUsing
      <* infoElem "then"
      <* sepElem myFsep
      <* infoElem "group"
      <* sepElem myFsep
      <* infoElem "by"
      <* sepElem myFsep
      <*> prettyInfoElem e
      <* sepElem myFsep
      <* infoElem "using"
      <* sepElem myFsep
      <*> prettyInfoElem f

------------------------- Record updates

instance AstPretty FieldUpdate where
  astPretty (FieldUpdate _ name e) =
    resultPretty $ constrElem FieldUpdate
      <*> prettyInfoElem name
      <* sepElem myFsep
      <* infoElem "="
      <* sepElem myFsep
      <*> prettyInfoElem e
  astPretty (FieldPun _ name) = resultPretty $ constrElem FieldPun <*> prettyInfoElem name
  astPretty (FieldWildcard _) = resultPretty $ constrElem FieldWildcard <* infoElem ".."

------------------------- Names -------------------------
instance AstPretty QOp where astPretty = undefined

instance AstPretty  CName where
  astPretty (VarName _ name) = resultPretty $ constrElem VarName <*> prettyNoInfoElem name
  astPretty (ConName _ name) = resultPretty $ constrElem ConName <*> prettyNoInfoElem name

-- --------------------------------------------------------------------------

instance AstPretty SpecialCon where

  astPretty (UnitCon _) = resultPretty $ constrElem UnitCon <* infoElem "()"
  astPretty (ListCon _) = resultPretty $ constrElem ListCon <* infoElem "[]"
  astPretty (FunCon _) = resultPretty $ constrElem FunCon  <* infoElem "->"
  astPretty (TupleCon _ b n) =
    let
      hash = if b == Unboxed then "#" else ""
      point = "(" ++ hash ++ replicate (n-1) ',' ++ hash ++ ")" in
    resultPretty $ constrElem TupleCon
      <* infoElem point
      <*> pure b
      <*> pure n

  astPretty (Cons _) = resultPretty $ constrElem Cons <* infoElem ":"
  astPretty (UnboxedSingleCon _) = resultPretty $ constrElem UnboxedSingleCon <* infoElem "(# #)"

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
    <*> ppName n
rawQName (UnQual _ n) =
  constrElem UnQual <*> ppName n
rawQName (Special _ sc) = constrElem Special <*> prettyNoInfoElem sc

ppQNameInfix :: QName a -> AstElem (QName SrcSpanInfo)
ppQNameInfix name
  | isSymbolName (getName name) = rawQName name
  | otherwise = infoElem "`" *> rawQName name <* infoElem "`"

-- --------------------------------------------------------------------------

instance AstPretty Op where astPretty = undefined

-- --------------------------------------------------------------------------

instance AstPretty Name where
  astPretty n@(Ident _ _) = resultPretty $ ppName n
  astPretty (Symbol _ s) =
    resultPretty.paren $ constrElem Symbol
      <*  sepElem hsep
      <*> infoElem s

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

ppName :: Name t -> AstElem (Name a)
ppName (Symbol _ s) = constrElem Symbol <*> noInfoElem s
ppName (Ident  _ s) = constrElem Ident <*> noInfoElem s

-- --------------------------------------------------------------------------

instance AstPretty Context where
  astPretty (CxEmpty _) = resultPretty $ constrElem CxEmpty <* infoElem "()" <* sepElem hsep <* infoElem "=>"
  astPretty (CxSingle _ asst) =
    resultPretty $ constrElem CxSingle
      <*> prettyNoInfoElem asst <* sepElem hsep <* infoElem "=>"
  astPretty (CxTuple _ assts) =
    resultPretty $ constrElem CxTuple
      <*> parenList (noInfoList assts) -- myFsep and parenList -> myFsep and myFsepSimple ???
      <* sepElem myFsep
      <* infoElem "=>"
  astPretty (CxParen _ asst) = resultPretty $ constrElem CxParen <*> enclose (infoElem "(") (infoElem ")") (prettyNoInfoElem asst)

ppContext :: Maybe (Context a) -> AstElem (Maybe (Context SrcSpanInfo))
ppContext context = traverse impl context
  where
    impl c = (paren $ prettyInfoElem c)
      <*  sepElem hsep
      <*  infoElem "=>"

-- --------------------------------------------------------------------------
-- hacked for multi-parameter type classes
instance AstPretty Asst where
  astPretty (ClassA _ a ts)   = -- myFsep $ ppQName a : map ppAType ts
    resultPretty $ constrElem ClassA
      <*> prettyNoInfoElem a
      <*  sepElem myFsep
      <*> intersperse (sepElem myFsep) (map (annNoInfoElem.ppAType) ts)
  astPretty (InfixA _ a op b) =  -- myFsep $ [pretty a, ppQNameInfix op, pretty b]
    resultPretty $ constrElem InfixA
      <*> prettyNoInfoElem a
      <*   sepElem myFsep
      <*> ppQNameInfix op
      <*   sepElem myFsep
      <*> prettyNoInfoElem b
  astPretty (IParam _ i t)    = -- myFsep $ [pretty i, text "::", pretty t]
    resultPretty $ constrElem IParam
      <*> prettyInfoElem i
      <*  sepElem myFsep
      <*  infoElem "::"
      <*> prettyInfoElem t

  astPretty (EqualP _ t1 t2)  = -- myFsep $ [pretty t1, text "~", pretty t2]
    resultPretty $ constrElem EqualP
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
parenListSep :: AstElem String
parenListSep = infoElem "," <* sepElem myFsepSimple

parenList :: [AstElem a] -> AstElem [a]
parenList xs =  paren $ intersperse parenListSep xs

hashParenList :: [AstElem a] -> AstElem [a]
hashParenList xs = infoElem "(#" *> intersperse parenListSep xs <* infoElem "#)"

braceList :: [AstElem a] -> AstElem [a]
braceList xs = brace $ intersperse parenListSep xs

bracketList :: [AstElem a] -> AstElem [a]
bracketList xs = bracket $ intersperse (sepElem myFsepSimple) xs

enclose ob cb x = ob *> x <* cb

paren :: AstElem a -> AstElem a
paren d = infoElem "(" *> d <* infoElem ")"

brace :: AstElem a -> AstElem a
brace d = infoElem "{" *> d <* infoElem "}"

bracket :: AstElem a -> AstElem a
bracket d = infoElem "[" *> d <* infoElem "]"

parensIf :: Bool -> AstElem a -> AstElem a
parensIf p d = if p then paren d else d

-- --------------------------------------------------------------------------
-- Wrap in braces and semicolons, with an extra space at the start in
-- case the first doc begins with "-", which would be scanned as {-

flatBlock  :: [AstElem a] -> AstElem [a]
flatBlock xs = infoElem "{" *> sepElem hsep *> intersperse (infoElem ";" <* sepElem hsep) xs <* infoElem "}"

-- Same, but put each thing on a separate line
prettyBlock :: [AstElem a] -> AstElem [a]
prettyBlock xs = infoElem "{" *> sepElem hsep *> intersperse (infoElem ";" <* sepElem vcat) xs <* infoElem "}"

-- --------------------------------------------------------------------------
blankline :: AstElem ()
blankline = AstElem $ do
  PrettyMode mode _ <- ask
  let AstElem x = if spacing mode && layout mode /= PPNoLayout
      then
        sepElem hsep <* sepElem vcat
      else
        sepElem (pure ())
  x

ppBody :: (PR.PPHsMode -> Int) -> [AstElem a] -> AstElem [a]
ppBody f dl =  AstElem $ do
  (PrettyMode mode _) <- ask
  let i = f mode
  case layout mode of
    PPOffsideRule -> do
      let AstElem x = indent i
      x
    PPSemiColon   -> do
      let AstElem x = indentExplicit i
      x
    _ -> do
      let AstElem x = flatBlock dl
      x
  where
    indent i = intersperse ((sepElem $ nest i) *> sepElem vcat) dl
    indentExplicit i = (sepElem $ nest i) *> prettyBlock dl

topLevel :: (Annotated ast, AstPretty ast) => [ast a] -> AstElem [ast SrcSpanInfo]
topLevel dl = AstElem $ do
  PrettyMode mode _ <- ask
  let dl' = noInfoList dl
  case layout mode of
    PPOffsideRule -> do
      let AstElem x = sepElem vcat *> intersperse (sepElem myVcat) dl'
      x
    PPSemiColon -> do
      let AstElem x = sepElem vcat *> prettyBlock dl'
      x
    PPInLine -> do
      let AstElem x = sepElem vcat *> prettyBlock dl'
      x
    PPNoLayout -> do
      let AstElem x = sepElem hsep *> flatBlock dl'
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
-- simplify utils

sDeclHead dh = case dh of
    DHead _ n tvs         -> (n, tvs)
    (DHInfix _ tva n tvb) -> (n, [tva, tvb])
    (DHParen _ dh)        -> sDeclHead dh

sInstHead ih = case ih of
  IHead _ qn ts      -> (qn, ts)
  IHInfix _ ta qn tb -> (qn, [ta,tb])
  IHParen _ ih       -> sInstHead ih

-- --------------------------------------------------------------------------

zeroSt = DocState (SrcLoc "unknown.hs"  1  1) 0

-- -----------------------------------------------------------------------------

ilist = [Ident undefined "fst", Ident undefined "second"]-- , Ident undefined "third", Ident undefined "four"]

renderAst (AstElem x) = renderWithMode (PrettyMode PR.defaultMode (Style PageMode 10 1.5)) zeroSt x

exampleList = renderAst $ intersperse (pure ()) (noInfoList ilist)

