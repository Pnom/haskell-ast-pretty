{-# LANGUAGE FlexibleContexts #-}
module Language.Haskell.Exts.PrettyAst
  ( PrettyAst(..)
  , renderWithMode
  , renderWithDefMode
  , PrettyMode(..)
  , defPrettyMode
  , PR.Style(..)
  , PR.style
  , PR.Mode(..)
  , PR.defaultMode
  , DocM
  ) where

import Language.Haskell.Exts.Annotated
import qualified Language.Haskell.Exts.Pretty as PR
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Applicative

import Data.Maybe
import Data.List hiding (intersperse)
import Data.Traversable

data DocState = DocState {
  pos :: !SrcLoc,
  nestSize :: !Int
  } deriving Show

defDocState fl = DocState (SrcLoc fl  1  1) 0

data PrettyMode = PrettyMode PR.PPHsMode PR.Style
defPrettyMode = PrettyMode PR.defaultMode PR.style

type DocM = ReaderT PrettyMode (State DocState)

-- | render the document with a given file name and mode.
renderWithMode :: String -> PrettyMode -> DocM a -> a
renderWithMode fl mode doc = evalState (runReaderT doc mode) (defDocState fl)

-- | render the document with a given file name and 'defaultMode'.
renderWithDefMode :: String -> DocM a -> a
renderWithDefMode fl a = renderWithMode fl defPrettyMode a

-- --------------------------------------------------------------------------

class PrettyAst ast where
  -- | Pretty-print something in isolation.
  astPretty :: ast a -> DocM (ast SrcSpanInfo)

  -- | Pretty-print something in a precedence context.
  astPrettyPrec :: Int -> ast a -> DocM (ast SrcSpanInfo)
  astPretty = astPrettyPrec 0
  astPrettyPrec _ = astPretty

-------------------------  Pretty-Print a Module --------------------

instance PrettyAst Module where
  astPretty (Module _ mbHead os imp decls) =
    resultPretty $ pure impl
      <*> vcatList os
      <*  sepElem myVcat
      <*> traverseSep (sepElem myVcat) (annInfoElem.astPretty) mbHead
      <*> prettyLs imp
      <*  sepElem myVcat
      <*> prettyLs decls
      where
        impl os h i d = Module annStub h os i d
        vcatList dl = intersperse (sepElem myVcat) $ noInfoList dl
        prettyLs dl = (if isJust mbHead then topLevel else vcatList) dl
  astPretty (XmlPage _ _mn os n attrs mattr cs) = unimplemented
  astPretty (XmlHybrid _ mbHead os imp decls n attrs mattr cs) = unimplemented

--------------------------  Module Header ------------------------------

instance PrettyAst ModuleHead where
  -- mySep
  astPretty (ModuleHead _ m mbWarn mbExportList) =
    resultPretty.onsideNest $ constrElem ModuleHead
      -- MySep
      <*  infoElem "module"
      <*  sepElem fsep
      <*> (annNoInfoElem $ astPretty m)
      <*  sepElem fsep
      <*> traverseSep (sepElem fsep) (annNoInfoElem.astPretty) mbWarn
      <*> traverseSep (sepElem fsep) (annNoInfoElem.astPretty) mbExportList
      <*  infoElem "where"

--cdd :: ModuleHead a -> WriterT [SrcSpan] DocM (ModuleHead SrcSpanInfo)
cdd :: ModuleHead a -> AstElem (ModuleHead SrcSpanInfo)
cdd (ModuleHead _ m mbWarn mbExportList) =
  constrElem ModuleHead
    <*  infoElem "module"
    <*  sepElem hsep
    <*> (annNoInfoElem $ astPretty m)
    <*  sepElem fsep
    <*> traverseSep (sepElem fsep) (annNoInfoElem.astPretty) mbWarn
    <*> traverseSep (sepElem fsep) (annNoInfoElem.astPretty) mbExportList
    <*  infoElem "where"

-- --------------------------------------------------------------------------

instance PrettyAst WarningText where
    astPretty w = case w of
      (DeprText _ s) -> impl DeprText "{-# DEPRECATED" s
      (WarnText _ s) -> impl WarnText "{-# WARNING"    s
      where
        -- mySep
      impl f c s = resultPretty.onsideNest $ constrElem f <* infoElem c <* sepElem fsep <*> infoElem s <* sepElem fsep <* infoElem "#}"

-- --------------------------------------------------------------------------

instance PrettyAst ModuleName where
  astPretty (ModuleName _ s) = resultPretty $ constrElem ModuleName <*> infoElem s

-- --------------------------------------------------------------------------

instance PrettyAst ExportSpecList where
  astPretty (ExportSpecList _ especs) =
    resultPretty $ constrElem ExportSpecList <*> parenList (noInfoList especs)

-- --------------------------------------------------------------------------

instance PrettyAst ExportSpec where
  astPretty (EVar _ name) = resultPretty $ constrElem EVar <*> (annNoInfoElem $ astPretty name)
  astPretty (EAbs _ name) = resultPretty $ constrElem EAbs <*> (annNoInfoElem $ astPretty name)
  astPretty (EThingAll _ name) =
    resultPretty $ constrElem EThingAll <*> (annNoInfoElem $ astPretty name) <* infoElem "(..)"
  astPretty (EThingWith _ name nameList) =
    resultPretty $ constrElem EThingWith
      <*> (annNoInfoElem $ astPretty name)
      <*> parenList (noInfoList nameList)
  astPretty (EModuleContents _ m) = resultPretty $ constrElem EModuleContents <*> (annNoInfoElem $ astPretty m)

-- --------------------------------------------------------------------------

instance PrettyAst ImportDecl where
  astPretty (ImportDecl _ mod qual src mbPkg mbName mbSpecs) =
    resultPretty.onsideNest $ pure impl
      -- mySep
      <*  infoElem "import"
      <*  sepElem fsep
      <*> pure src <* (infoElem $ if src then "{-# SOURCE #-}" else "")
      <*  sepElem fsep
      <*> pure qual <* (infoElem $ if qual then "qualified" else "")
      <*  sepElem fsep
      <*> traverseSep (sepElem fsep) infoElem mbPkg
      <*> (annNoInfoElem $ astPretty mod)
      <*  sepElem fsep
      <*> traverseSep (sepElem fsep) (annNoInfoElem.astPretty) mbName
      <*> traverseSep (sepElem fsep) (annNoInfoElem.astPretty) mbSpecs
    where impl s q p m n sp = ImportDecl annStub m q s p n sp

-- --------------------------------------------------------------------------

instance PrettyAst ImportSpecList where
  astPretty (ImportSpecList _ b ispecs) =
    resultPretty $ constrElem ImportSpecList
      <*> pure b <* (infoElem $ if b then "hiding" else "")
      <*  sepElem hsep
      <*> parenList (noInfoList ispecs)

-- --------------------------------------------------------------------------

instance PrettyAst ImportSpec where
  astPretty (IVar _ name)                = resultPretty $ constrElem IVar <*> (annNoInfoElem $ astPretty name)
  astPretty (IAbs _ name)                = resultPretty $ constrElem IAbs <*> (annNoInfoElem $ astPretty name)
  astPretty (IThingAll _ name)           =
    resultPretty $ constrElem IThingAll <*> (annNoInfoElem $ astPretty name) <* infoElem "(..)"
  astPretty (IThingWith _ name nameList) =
    resultPretty $ constrElem IThingWith <*> (annNoInfoElem $ astPretty name) <*> parenList (noInfoList nameList)

identDeriving :: Deriving a -> AstElem (Deriving SrcSpanInfo)
identDeriving d = do
  xs <- ppBody letIndent [ppDeriving d]
  return $ head xs

identDeriving' :: Deriving a -> AstElem (Deriving SrcSpanInfo)
identDeriving' d = ppBody letIndent [ppDeriving d] >>= return.head

-------------------------  Declarations ------------------------------

instance PrettyAst Decl where
  astPretty (TypeDecl _ head htype) =
    resultPretty.onsideNest $ constrElem TypeDecl
      <* blankline
      -- mySep
      <* infoElem "type"
      <* sepElem fsep
      <*> (annInfoElem $ astPretty head)
      <* sepElem fsep
      <* infoElem "="
      <* sepElem fsep
      <*> (annInfoElem $ astPretty htype)
  astPretty (TypeFamDecl _ head mkind) =
    resultPretty.onsideNest $ constrElem TypeFamDecl
      <* blankline
      -- mySep
      <* infoElem "type"
    <*  sepElem fsep
      <* infoElem "family"
      <* sepElem fsep
      <*> ppFsepDhead head
      <* sepElem fsep
      <*> ppOptKind mkind
  astPretty (DataDecl _ don mContext head constrList mDeriving) =
    resultPretty $ onsideHead (constrElem DataDecl)
      <*  sepElem fsep
      <*> ppConstrList constrList
      -- '$$$' is the same as myVcat
      <*  sepElem myVcat
      <*> traverse ppDeriving mDeriving
    where
      onsideHead f = onsideNest $ f
        <* blankline
        <*> (annInfoElem $ astPretty don)
        <*  sepElem fsep
        <*> ppContext mContext
        <*  sepElem fsep
        <*> ppFsepDhead head
  astPretty (GDataDecl _ don mContext hd mkind gadtDecl mDeriving) =
    resultPretty $ onsideHead (constrElem GDataDecl)
      <*  sepElem myVcat -- '$$$' is the same as myVcat
      <*> ppBody classIndent (noInfoList gadtDecl)
      <*  sepElem myVcat
      <*> traverse identDeriving mDeriving
      where
        identDeriving d = ppBody letIndent [ppDeriving d] >>= return.head
        onsideHead f = onsideNest $ f
          <* blankline
          -- mySep
          <*> (annInfoElem $ astPretty don)
          <*  sepElem fsep
          <*> ppContext mContext
          <*  sepElem fsep
          <*> ppFsepDhead hd
          <*  sepElem fsep
          <*> ppOptKind mkind
          <*  sepElem fsep
          <*  infoElem "where"
  astPretty (DataFamDecl _ mContext head mKind) =
    resultPretty.onsideNest $ constrElem DataFamDecl
      <* blankline
      -- mySep
      <*  infoElem "data"
      <*  sepElem fsep
      <*  infoElem "family"
      <*  sepElem fsep
      <*> ppContext mContext
      <*  sepElem fsep
      <*> ppFsepDhead head
      <*  sepElem fsep
      <*> ppOptKind mKind
  astPretty (TypeInsDecl _ tl tr) =
    resultPretty.onsideNest $ constrElem TypeInsDecl
      <* blankline
      -- mySep
      <*  infoElem "type"
      <*  sepElem fsep
      <*  infoElem "instance"
      <*  sepElem fsep
      <*> (annInfoElem $ astPretty tl)
      <*  sepElem fsep
      <*  infoElem "="
      <*  sepElem fsep
      <*> (annInfoElem $ astPretty tr)
  astPretty (DataInsDecl _ don t qConDecl mDeriving) =
    resultPretty $ onsideHead (constrElem DataInsDecl)
      <*  sepElem hsep
      <*> ppConstrList qConDecl
      -- '$$$' is the same as myVcat
      <*  sepElem myVcat
      <*> traverse ppDeriving mDeriving
      where
        onsideHead f = onsideNest $ f
          <* blankline
          -- mySep
          <*> (annInfoElem $ astPretty don)
          <*  sepElem fsep
          <*  infoElem "instance"
          <*  sepElem fsep
          <*> (annInfoElem $ astPretty t)
  astPretty (GDataInsDecl _ don t mKind gadtDecl mDeriving) =
    resultPretty $ onsideHead (constrElem GDataInsDecl)
      <*  sepElem myVcat -- '$$$' is the same as myVcat
      <*> ppBody classIndent (noInfoList gadtDecl)
      <*  sepElem myVcat
      <*> traverse ppDeriving mDeriving
    where
      onsideHead f = onsideNest $ f
        <* blankline
        -- mySep
        <*> (annInfoElem $ astPretty don)
        <*  sepElem fsep
        <*  infoElem "instance"
        <*  sepElem fsep
        <*> (annInfoElem $ astPretty t)
        <*  sepElem fsep
        <*> ppOptKind mKind
        <*  infoElem "where"
  astPretty (ClassDecl _ mContext head funDep mClassDecl) =
    resultPretty $ onsideHead (constrElem ClassDecl) <*> traverse cDecl mClassDecl
    where
      cDecl cd = ppBody classIndent (noInfoList cd)
      onsideHead f = onsideNest $ f
        <* blankline
        -- mySep
        <*  infoElem "class"
        <*  sepElem fsep
        <*> ppContext mContext
        <*  sepElem fsep
        <*> ppFsepDhead head
        <*  sepElem fsep
        <*> ppFunDeps funDep
        <*  if null $ fromMaybe [] mClassDecl then infoElem "" else (sepElem fsep *> infoElem "where" <* sepElem myVcat)
  astPretty (InstDecl _ mContext instHead mInstDecl) =
    resultPretty $ onsideHead (constrElem InstDecl) <*> traverse instDecl mInstDecl
    where
      instDecl is = ppBody classIndent (noInfoList is)
      onsideHead f = onsideNest $ f
        <* blankline
        -- mySep
        <*  infoElem "instance"
        <*  sepElem fsep
        <*> ppContext mContext
        <*  sepElem fsep
        <*> ppInstHeadInDecl instHead
        <*  if null $ fromMaybe [] mInstDecl then infoElem "" else (sepElem fsep *> infoElem "where" <* sepElem myVcat)
  astPretty (DerivDecl _ mContext instHead) =
    resultPretty.onsideNest $ constrElem DerivDecl
      <* blankline
      -- mySep
      <*  infoElem "deriving"
      <*  sepElem fsep
      <*  infoElem "instance"
      <* sepElem fsep
      <*> ppContext mContext
      <* sepElem fsep
      <*> ppInstHeadInDecl instHead
  astPretty (InfixDecl _ assoc mInt op) =
    resultPretty.onsideNest $ constrElem InfixDecl
      <* blankline
      -- mySep
      <*> (annInfoElem $ astPretty assoc)
      <*  sepElem fsep
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
      <*> (annInfoElem $ astPretty e)
  astPretty (TypeSig _ ns t) =
    resultPretty.onsideNest $ constrElem TypeSig
      <*  blankline
      -- mySep
      <*> intersperse (infoElem "," <* sepElem fsep) (noInfoList ns)
      <*  (sepElem $ case ns of [n] -> hsep; _ -> fsep)
      <*  infoElem "::"
      <*> (annInfoElem $ astPretty t)
  astPretty (FunBind _ ms) =
    resultPretty $ constrElem FunBind
      <*> intersperse sep (noInfoList ms)
    where
      sep = do
        PrettyMode mode _ <- ask
        if layout mode == PPOffsideRule then (infoElem "" <* sepElem myVcat) else infoElem ";"

  astPretty (PatBind _ pat mType rhs mBinds) =
    resultPretty $ constrElem PatBind
      -- myFsep
      <*> (annInfoElem $ astPretty pat)
      <*  sepElem myFsep
      <*> traverseSep  (sepElem myFsep) (\t -> infoElem "::" *> sepElem hsep *> (annInfoElem.astPretty) t) mType
      <*> (annInfoElem $ astPretty rhs)
      <*  sepElem myVcat
      <*> traverse ppWhere mBinds
  astPretty (ForImp _ callConv mSafety mStr n t) =
    resultPretty.onsideNest $ constrElem ForImp
      <*  blankline
      -- mySep
      <*  infoElem "foreign import"
      <*  sepElem fsep
      <*> (annInfoElem $ astPretty callConv)
      <*  sepElem fsep
      <*> traverseSep (sepElem fsep) (annInfoElem.astPretty) mSafety
      <*> traverseSep (sepElem fsep) infoElem mStr
      <*> (annInfoElem $ astPretty n)
      <*  sepElem fsep
      <*  infoElem "::"
      <*> (annInfoElem $ astPretty t)
  astPretty (ForExp _ callConv mStr n t) =
    resultPretty.onsideNest $ constrElem ForExp
      <*  blankline
      -- mySep
      <*  infoElem "foreign export"
      <*  sepElem fsep
      <*> (annInfoElem $ astPretty callConv)
      <*  sepElem fsep
      <*> traverseSep (sepElem fsep) infoElem mStr
      <*> (annInfoElem $ astPretty n)
      <*  sepElem fsep
      <*  infoElem "::"
      <*> (annInfoElem $ astPretty t)
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
    resultPretty.onsideNest $ constrElem InlineSig
      <*  blankline
      -- mySep
      <*> pure b <* (infoElem $ if b then "{-# INLINE" else "{-# NOINLINE")
      <*  sepElem fsep
      <*> traverseSep (sepElem fsep) (annInfoElem.astPretty) mActivation
      <*> (annInfoElem $ astPretty qName)
      <*  infoElem "#-}"
  astPretty (InlineConlikeSig _ mActivation qName) =
    resultPretty.onsideNest $ constrElem InlineConlikeSig
      <*  blankline
      -- mySep
      <* infoElem "{-# INLINE_CONLIKE"
      <*  sepElem fsep
      <*> traverseSep (sepElem fsep) (annInfoElem.astPretty) mActivation
      <*> (annInfoElem $ astPretty qName)
      <*  infoElem "#-}"
  astPretty (SpecSig _ qName ts) =
    resultPretty.onsideNest $ constrElem SpecSig
      <*  blankline
      -- mySep
      <*  infoElem "{-# SPECIALISE"
      <*  sepElem fsep
      <*> (annInfoElem $ astPretty qName)
      <*  sepElem fsep
      <*  infoElem "::"
      <*  sepElem fsep
      <*> intersperse (infoElem "," <* sepElem fsep) (noInfoList ts)
      <*  sepElem fsep
      <*  infoElem "#-}"
  astPretty (SpecInlineSig _ b mActivation qName ts) =
    resultPretty.onsideNest $ constrElem SpecInlineSig
      <*  blankline
      -- mySep
      <*  infoElem "{-# SPECIALISE"
      <*  sepElem fsep
      <*> pure b <* (infoElem $ if b then "INLINE" else "NOINLINE")
      <*  sepElem fsep
      <*> traverseSep (sepElem fsep) (annInfoElem.astPretty) mActivation
      <*> (annInfoElem $ astPretty qName)
      <*  sepElem fsep
      <*  infoElem "::"
      <*  sepElem fsep
      <*> intersperse (infoElem "," <* sepElem fsep) (noInfoList ts)
      <*  sepElem fsep
      <*  infoElem "#-}"
  astPretty (InstSig _ mContext ih ) =
    resultPretty.onsideNest $ constrElem InstSig
      <*  blankline
      -- mySep
      <*  infoElem "{-# SPECIALISE"
      <*  sepElem fsep
      <*  infoElem "instance"
      <*  sepElem fsep
      <*> traverseSep (sepElem fsep) (annInfoElem.astPretty) mContext
      <*> ppInstHeadInDecl ih
      <*  sepElem fsep
      <*  infoElem "#-}"
  astPretty (AnnPragma _ annotation) =
    resultPretty.onsideNest $ constrElem AnnPragma
      <*  blankline
      -- mySep
      <*  infoElem "{-# ANN"
      <*  sepElem fsep
      <*> (annInfoElem $ astPretty annotation)
      <*  sepElem fsep
      <*  infoElem "#-}"

ppConstrList :: PrettyAst ast => [ast a] -> AstElem [ast SrcSpanInfo]
ppConstrList [] = sequenceA []
ppConstrList cs = sepElem hsep
  *> infoElem "="
  *> intersperse (sepElem hsep *> infoElem "|" *> sepElem myVcat) (noInfoList cs)

ppFsepDhead :: DeclHead a -> AstElem (DeclHead SrcSpanInfo)
ppFsepDhead dh = constrElem DHead
  <*> (annInfoElem $ astPretty name)
  <*  sepElem fsep
  <*> intersperse (sepElem fsep) (noInfoList tvs)
  where
    (name, tvs) = sDeclHead dh

ppInstHeadInDecl :: InstHead a -> AstElem (InstHead SrcSpanInfo)
ppInstHeadInDecl ih = constrElem IHead
  <*> (annInfoElem $ astPretty qn)
  <*  sepElem fsep
  <*> intersperse (sepElem fsep) (map (annNoInfoElem.ppAType) ts)
  where
    (qn, ts) = sInstHead ih

-- --------------------------------------------------------------------------

ppWarnDepr :: PrettyAst ast => ([ast a], String) -> AstElem ([ast SrcSpanInfo], String)
ppWarnDepr ([], txt) = pure (,) <*> pure []  <*> infoElem txt
ppWarnDepr (ns, txt) = pure (,)
  <*> intersperse (infoElem "," <* sepElem fsep) (noInfoList ns)
  <*  sep ns
  <*> infoElem txt
  where
    sep [n] = sepElem hsep
    sep _   = sepElem fsep

-- --------------------------------------------------------------------------

instance PrettyAst DeclHead where
  astPretty (DHead _ n tvs) =
    -- mySep
    resultPretty.onsideNest $ constrElem DHead
      <*> (annInfoElem $ astPretty n)
      <*  sepElem fsep
      <*> intersperse (sepElem fsep) (noInfoList tvs)
  astPretty (DHInfix _ tva n tvb) =
    -- mySep
    resultPretty.onsideNest $ constrElem DHInfix
      <*> (annInfoElem $ astPretty tva)
      <* sepElem fsep
      <*> (annInfoElem $ astPretty n)
      <* sepElem fsep
      <*> (annInfoElem $ astPretty tvb)
  astPretty (DHParen _ dh)        =
    -- parens (pretty dh)
    resultPretty.parens $ constrElem DHParen <*> (annNoInfoElem $ astPretty dh)

-- --------------------------------------------------------------------------

instance PrettyAst InstHead where
  astPretty (IHead _ qn ts) =
    resultPretty.onsideNest $ constrElem IHead
      -- mySep
      <*> (annInfoElem $ astPretty qn)
      <*  sepElem fsep
      <*> intersperse (sepElem fsep) (infoList ts)
  astPretty (IHInfix _ ta qn tb) =
    resultPretty.onsideNest $ constrElem IHInfix
      -- mySep
      <*> (annInfoElem $ astPretty ta)
      <*  sepElem fsep
      <*> (annInfoElem $ astPretty qn)
      <*  sepElem fsep
      <*> (annInfoElem $ astPretty tb)
  astPretty (IHParen _ ih) =
    resultPretty.parens $ constrElem IHParen <*> (annInfoElem $ astPretty ih)

-- --------------------------------------------------------------------------

instance PrettyAst DataOrNew where
  astPretty (DataType _) = resultPretty $ constrElem DataType <* infoElem "data"
  astPretty (NewType  _) = resultPretty $ constrElem NewType <* infoElem "newtype"

-- --------------------------------------------------------------------------

instance PrettyAst Assoc where
  astPretty (AssocNone  _) = resultPretty $ constrElem AssocNone  <* infoElem "infix"
  astPretty (AssocLeft  _) = resultPretty $ constrElem AssocLeft  <* infoElem "infixl"
  astPretty (AssocRight _) = resultPretty $ constrElem AssocRight <* infoElem "infixr"

-- --------------------------------------------------------------------------

instance PrettyAst Match where
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
            fn  = \l' n' r' -> (n', l' : r' : map (annNoInfoElem.astPrettyPrec 2) pbs')
          in fn (op *> (annInfoElem $ astPretty l)) (annInfoElem $ ppName n) (cp *> (annInfoElem $ astPretty r))
        _ -> (annInfoElem $ astPretty n, map (annNoInfoElem.astPrettyPrec 2) pbs)
      res f rhs mWhere = resultPretty $ f
        <*  sepElem myFsep
        <*> (annInfoElem $ astPretty rhs)
        <*  sepElem myVcat -- same as $$$ in original pretty
        <*> traverse ppWhere mWhere

ppWhere (BDecls  _ []) = constrElem BDecls  <*> pure []
ppWhere (BDecls  _ l)  = nest 2 $ constrElem BDecls  <* infoElem "where" <* sepElem myVcat <*> ppBody whereIndent (noInfoList l)
ppWhere (IPBinds _ b)  = nest 2 $ constrElem IPBinds <* infoElem "where" <* sepElem myVcat <*> ppBody whereIndent (noInfoList b)

-- --------------------------------------------------------------------------

instance PrettyAst ClassDecl where
  astPretty (ClsDecl _ d) = resultPretty $ constrElem ClsDecl <*> (annInfoElem $ astPretty d)
  astPretty (ClsDataFam _ context dh optkind) =
    resultPretty.onsideNest $ constrElem ClsDataFam
      -- mySep
      <*  infoElem "data"
      <*  sepElem fsep
      <*> ppContext context
      <*  sepElem fsep
      <*> ppFsepDhead dh
      <*  sepElem fsep
      <*> ppOptKind optkind
  astPretty (ClsTyFam _ dh optkind)  =
    resultPretty.onsideNest $ constrElem ClsTyFam
      -- mySep
      <*  infoElem "type"
      <*  sepElem fsep
      <*> ppFsepDhead dh
      <*  sepElem fsep
      <*> ppOptKind optkind
  astPretty (ClsTyDef _ ntype htype) =
    resultPretty.onsideNest $ constrElem ClsTyDef
      -- mySep
      <*> (annInfoElem $ astPretty ntype)
      <*  sepElem fsep
      <*  infoElem "="
      <*  sepElem fsep
      <*> (annInfoElem $ astPretty htype)

-- --------------------------------------------------------------------------

instance PrettyAst InstDecl where
  astPretty (InsDecl _ decl) = resultPretty $ constrElem InsDecl <*> (annInfoElem $ astPretty decl)
  astPretty (InsType _ ntype htype) =
    resultPretty.onsideNest $ constrElem InsType
      -- mySep
      <*  infoElem "type"
      <*  sepElem fsep
      <*> (annInfoElem $ astPretty ntype)
      <*  sepElem fsep
      <*  infoElem "="
      <*  sepElem fsep
      <*> (annInfoElem $ astPretty htype)
  astPretty (InsData _ don ntype constrList derives) =
    resultPretty $ onsideHead (constrElem InsData)
      <*  sepElem hsep
      <*> constrList' (noInfoList constrList)
      <*  sepElem myVcat
      <*> traverse ppDeriving derives
      where
        onsideHead f = onsideNest $ f <*> (annInfoElem $ astPretty don) <*  sepElem fsep <*> (annInfoElem $ astPretty ntype)
        cSep1 = infoElem "=" <* sepElem hsep
        cSep2 = sepElem myVcat <* infoElem "|" <* sepElem hsep
        constrList' (e1:e2:es) = sequenceA $ (e1 <* cSep1) : e2 : (map (cSep2 *>) es)
        constrList' es = sequenceA es
  astPretty (InsGData _ don ntype optkind gadtList derives) =
    resultPretty.onsideNest $ onsideHead (constrElem InsGData)
      <*  sepElem myVcat
      <*> ppBody classIndent (noInfoList gadtList)
      <*  sepElem myVcat
      <*> traverse ppDeriving derives
    where
      onsideHead f = onsideNest $ f
        -- mySep
        <*> (annInfoElem $ astPretty don)
        <*  sepElem fsep
        <*> (annInfoElem $ astPretty ntype)
        <*  sepElem fsep
        <*> ppOptKind optkind
        <*  sepElem fsep
        <*  infoElem "where"

------------------------- FFI stuff -------------------------------------
instance PrettyAst Safety where
  astPretty (PlayRisky _)  = resultPretty $ constrElem PlayRisky <* infoElem "unsafe"
  astPretty (PlaySafe _ b) = resultPretty $ constrElem PlaySafe  <*> pure b <* (infoElem $ if b then "threadsafe" else "safe")

-- --------------------------------------------------------------------------

instance PrettyAst CallConv where
  astPretty (StdCall   _) = resultPretty $ constrElem StdCall   <* infoElem "stdcall"
  astPretty (CCall     _) = resultPretty $ constrElem CCall     <* infoElem "ccall"
  astPretty (CPlusPlus _) = resultPretty $ constrElem CPlusPlus <* infoElem "cplusplus"
  astPretty (DotNet    _) = resultPretty $ constrElem DotNet    <* infoElem "dotnet"
  astPretty (Jvm       _) = resultPretty $ constrElem Jvm       <* infoElem "jvm"
  astPretty (Js        _) = resultPretty $ constrElem Js        <* infoElem "js"

------------------------- Pragmas ---------------------------------------

instance PrettyAst Rule where
  astPretty (Rule _ tag activ rvs rhs lhs) =
    resultPretty.onsideNest $ constrElem Rule
    -- mySep
      <*> infoElem tag
      <*  sepElem fsep
      <*> traverse (annInfoElem.astPretty) activ
      <*  sepElem fsep
      <*> traverse ppRuleVars rvs
      <*  sepElem fsep
      <*> (annInfoElem $ astPretty rhs)
      <*  sepElem fsep
      <*  infoElem "="
      <*  sepElem fsep
      <*> (annInfoElem $ astPretty lhs)

ppRuleVars []  = pure []
ppRuleVars rvs = onsideNest $ infoElem "forall" *> sepElem fsep *> intersperse (sepElem fsep) (noInfoList rvs) <* infoElem "."

-- --------------------------------------------------------------------------

instance PrettyAst Activation where
  astPretty (ActiveFrom  _ i) = resultPretty $ constrElem ActiveFrom  <* infoElem "["  <*> pure i <* (infoElem $ show i) <* infoElem "]"
  astPretty (ActiveUntil _ i) = resultPretty $ constrElem ActiveUntil <* infoElem "[~" <*> pure i <* (infoElem $ show i) <* infoElem "]"

-- --------------------------------------------------------------------------

instance PrettyAst RuleVar where
    astPretty (RuleVar _ n) = resultPretty $ constrElem RuleVar <*> (annInfoElem $ astPretty n)
    astPretty (TypedRuleVar _ n t) =
      resultPretty.onsideNest.parens $ constrElem TypedRuleVar
        -- mySep
        <*  sepElem fsep
        <*> (annInfoElem $ astPretty n)
        <*  sepElem fsep
        <*  infoElem "::"
        <*  sepElem fsep
        <*> (annInfoElem $ astPretty t)
        <*  sepElem fsep

-- --------------------------------------------------------------------------

instance PrettyAst ModulePragma where
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
      <*>  (annNoInfoElem $ astPretty ann)
      <*   sepElem myFsep
      <*   infoElem "#-}"

-- --------------------------------------------------------------------------

instance PrettyAst Annotation where
  astPretty (Ann _ n e) =
    resultPretty $ constrElem Ann
      -- myFsep
      <*> (annNoInfoElem $ astPretty n)
      <*  sepElem myFsep
      <*> (annNoInfoElem $ astPretty e)
  astPretty (TypeAnn _ n e) =
    resultPretty $ constrElem TypeAnn
      -- myFsep
      <* infoElem "type"
      <* sepElem myFsep
      <*> (annNoInfoElem $ astPretty n)
      <* sepElem myFsep
      <*> (annNoInfoElem $ astPretty e)
  astPretty (ModuleAnn _ e) =
    resultPretty $ constrElem ModuleAnn
      -- myFsep
      <* infoElem "module"
      <* sepElem myFsep
      <*> (annNoInfoElem $ astPretty e)

------------------------- Data & Newtype Bodies -------------------------

instance PrettyAst QualConDecl where
  astPretty (QualConDecl _ mtvs mctxt con) =
    resultPretty $ constrElem QualConDecl
      <*> traverseSep (sepElem myFsep) (ppForall . noInfoList) mtvs
      <*  sepElem myFsep
      <*> ppContext mctxt
      <*  sepElem myFsep
      <*> (annInfoElem $ astPretty con)

-- --------------------------------------------------------------------------

instance PrettyAst GadtDecl where
  astPretty (GadtDecl _ name ty) =
    resultPretty $ constrElem GadtDecl
      <*> (annInfoElem $ astPretty name)
      <*  sepElem myFsep
      <*  infoElem "::"
      <*  sepElem myFsep
      <*> (annInfoElem $ astPretty ty)

-- --------------------------------------------------------------------------

instance PrettyAst ConDecl where
  astPretty (RecDecl _ name fieldList) =
    resultPretty $ constrElem RecDecl
      <*> (annInfoElem $ astPretty name)
      <*> braceList (noInfoList fieldList)
  astPretty (ConDecl _ name typeList) =
    resultPretty.onsideNest $ constrElem ConDecl
    -- mySep
      <*> annInfoElem (ppName name)
      <*  sepElem fsep
      <*> intersperse (sepElem fsep) (map (annNoInfoElem.astPrettyPrec prec_atype) typeList)
  astPretty (InfixConDecl _ l name r) =
    resultPretty $ constrElem InfixConDecl
    -- myFsep
      <*> annInfoElem (astPrettyPrec prec_btype l)
      <*  sepElem myFsep
      <*> ppNameInfix name
      <*  sepElem myFsep
      <*> annInfoElem (astPrettyPrec prec_btype r)

-- --------------------------------------------------------------------------

instance PrettyAst FieldDecl where
  astPretty (FieldDecl _ names ty) =
    resultPretty $ constrElem FieldDecl
      <*> intersperse (infoElem "," <* sepElem myFsepSimple) (noInfoList names)
      <*  sepElem myFsepSimple
      <*  infoElem "::"
      <*  sepElem myFsepSimple
      <*> (annInfoElem $ astPretty ty)

-- --------------------------------------------------------------------------

instance PrettyAst BangType where
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

instance PrettyAst Deriving where
  astPretty (Deriving _ []) = resultPretty $ constrElem Deriving <* infoElem "deriving" <* sepElem hsep <*> parenList []
  astPretty (Deriving _ [ih@(IHead _ d [])]) =
    resultPretty $ constrElem Deriving
       <* infoElem "deriving" <* sepElem hsep <*> sequenceA [(annNoInfoElem $ astPretty ih)]
  astPretty (Deriving _ ihs) =
    resultPretty $ constrElem Deriving
       <* infoElem "deriving" <* sepElem hsep <*> parenList (noInfoList ihs)

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
      [(qn, [])] -> [constrElem IHead <*> (annNoInfoElem $ astPretty qn) <*> pure []]
      _ -> map ppDer iheads
    ppDer (qn, ts) = onsideNest $ constrElem IHead
      -- mySep
      <*> (annNoInfoElem $ astPretty qn)
      <*  sepElem fsep
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

instance PrettyAst Type where
  astPrettyPrec p (TyForall _ mtvs ctxt htype) = resultPretty t
    -- parensIf (p > 0) $ myFsep [ppForall mtvs, ppContext ctxt, pretty htype]
    where
      t = parensIf (p > 0) $ constrElem TyForall
        -- myFsep
        <*> traverseSep (sepElem myFsep) (ppForall . noInfoList) mtvs
        <*> ppContext ctxt
        <* sepElem myFsep
        <*> (annNoInfoElem $ astPretty htype)
  astPrettyPrec p (TyFun _ a b) = resultPretty t
    -- myFsep [ppBType a, text "->", pretty b]
    where
      t = parensIf (p > 0) $ constrElem TyFun
        <*> (annNoInfoElem $ ppBType a)
        <*  sepElem myFsep
        <*  infoElem "->"
        <*  sepElem myFsep
        <*> (annNoInfoElem $ astPretty b)
  astPrettyPrec _ (TyTuple _ bxd l) =
    resultPretty $ constrElem TyTuple
      <*> pure bxd
      <*> l'
    where
      l' = case bxd of
        Boxed   -> parenList (noInfoList l)
        Unboxed -> hashParenList (noInfoList l)
  astPrettyPrec _ (TyList _ t)  = resultPretty $ constrElem TyList <*> t'
    where t' = enclose (infoElem "[") (infoElem "]") ((annNoInfoElem $ astPretty t))
  astPrettyPrec  p (TyApp _ a b) = resultPretty t
    -- parensIf (p > prec_btype) $ myFsep [pretty a, ppAType b]
    where
      t = parensIf (p > prec_btype) $
        constrElem TyApp
        <*  sepElem myFsep
        <*> (annNoInfoElem $ astPretty a)
        <*  sepElem myFsep
        <*> (annNoInfoElem $ ppAType a)
        <*   sepElem myFsep
  astPrettyPrec _ (TyVar _ t)  = resultPretty $ constrElem TyVar  <*> (annNoInfoElem $ astPretty t)
  astPrettyPrec _ (TyCon _ t)  = resultPretty $ constrElem TyCon <*> (annNoInfoElem $ astPretty t)
  astPrettyPrec _ (TyParen _ t)  = resultPretty $ constrElem TyParen <*> t'
    where t' = enclose (infoElem "(") (infoElem ")") ((annNoInfoElem $ astPretty t))
  astPrettyPrec _ (TyInfix _ a op b)  = resultPretty $ constrElem TyInfix
    -- myFsep [pretty a, ppQNameInfix op, pretty b]
    <*> (annNoInfoElem $ astPretty a)
    <* sepElem myFsep
    <*> ppQNameInfix op
    <* sepElem myFsep
    <*> (annNoInfoElem $ astPretty b)
  astPrettyPrec  _ (TyKind _ t k) = resultPretty t'
    -- parens (myFsep [pretty t, text "::", pretty k])
    where
      t' = parens $ constrElem TyKind
        -- myFsep
        <*  sepElem myFsep
        <*> (annNoInfoElem $ astPretty t)
        <*   sepElem myFsep
        <*   infoElem "::"
        <*> (annNoInfoElem $ astPretty k)
        <*   sepElem myFsep

-- --------------------------------------------------------------------------

instance PrettyAst TyVarBind where
  astPretty (KindedVar _ var kind) =
    resultPretty.parens $ constrElem KindedVar
      -- myFsep
      <*  sepElem myFsep
      <*> (annInfoElem $ astPretty var)
      <*  sepElem myFsep
      <*  infoElem "::"
      <*  sepElem myFsep
      <*> (annInfoElem $ astPretty kind)
      <*  sepElem myFsep
  astPretty (UnkindedVar _ var) = resultPretty $ constrElem UnkindedVar <*> (annInfoElem $ astPretty var)

ppForall :: [AstElem a] -> AstElem [a]
ppForall [] = pure []
ppForall vs = infoElem "forall" *> sepElem myFsep *> intersperse (sepElem myFsep) vs <* infoElem "."

---------------------------- Kinds ----------------------------

instance PrettyAst Kind where
  astPrettyPrec _ (KindStar _) = resultPretty $ constrElem KindStar <* infoElem "*"
  astPrettyPrec _ (KindBang _) = resultPretty $ constrElem KindBang <* infoElem "!"
  astPrettyPrec n (KindFn _ a b)  =
    resultPretty.parensIf (n > 0) $ constrElem KindFn
      -- myFsep
      <*> (annInfoElem $ astPrettyPrec 1 a)
      <*  sepElem myFsep
      <*  infoElem "->"
      <*  sepElem myFsep
      <*> (annInfoElem $ astPretty b)
  astPrettyPrec _ (KindParen _ k) =
    resultPretty.parens $ constrElem KindParen <*> (annInfoElem $ astPretty k)
  astPrettyPrec _ (KindVar _ n) = resultPretty $ constrElem KindVar <*> (annInfoElem $ astPretty n)

ppOptKind :: Maybe (Kind a) -> AstElem (Maybe (Kind SrcSpanInfo))
ppOptKind k = traverse (\ a -> infoElem "::" *> (annInfoElem $ astPretty a)) k

------------------- Functional Dependencies -------------------

instance PrettyAst FunDep where
  astPretty (FunDep _ from to) =
    resultPretty $ constrElem FunDep
      <*> intersperse (sepElem myFsep) (noInfoList from)
      <*  sepElem myFsep
      <*  infoElem "->"
      <*> intersperse (sepElem myFsep) (noInfoList to)

ppFunDeps :: PrettyAst ast => [ast a] -> AstElem [ast SrcSpanInfo]
ppFunDeps []  = sequenceA []
ppFunDeps fds = infoElem "|" *> sepElem myFsep *> intersperse (infoElem "," <* sepElem myFsep) (noInfoList fds)

------------------------- Expressions -------------------------

instance PrettyAst Rhs where
  astPretty (UnGuardedRhs _ e) =
    resultPretty $ constrElem UnGuardedRhs
      <*  infoElem "="
      <*> (annInfoElem $ astPretty e)
  astPretty (GuardedRhss _ guardList) =
    resultPretty $ constrElem GuardedRhss
      <*> intersperse (sepElem myVcat) (noInfoList guardList)

-- --------------------------------------------------------------------------

instance PrettyAst GuardedRhs where
  astPretty (GuardedRhs _ guards ppBody) =
    resultPretty $ constrElem GuardedRhs
    -- myFsep
      <*  infoElem "|"
      <*  sepElem myFsep
      <*> intersperse (infoElem "," <* sepElem myFsep) (noInfoList guards)
      <*  sepElem myFsep
      <*  infoElem "="
      <*  sepElem myFsep
      <*> (annInfoElem $ astPretty ppBody)

-- --------------------------------------------------------------------------

instance PrettyAst Literal where
  astPretty (Int _ i s)  = resultPretty $ constrElem Int  <*> pure i <* (noInfoElem $ show i) <*> pure s
  astPretty (Char _ c s) = resultPretty $ constrElem Char <*> pure c <* (noInfoElem $ show c) <*> pure s
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

instance PrettyAst Exp where
  astPrettyPrec _ (Lit _ l) = resultPretty $ constrElem Lit <*> (annNoInfoElem $ astPretty l)
  -- lambda stuff
  astPrettyPrec p (InfixApp _ a op b) = resultPretty . parensIf (p > 2) $
    constrElem InfixApp
      <*> annNoInfoElem (astPrettyPrec 2 a)
      <*  sepElem myFsep
      <*> (annNoInfoElem $ astPretty op)
      <*  sepElem myFsep
      <*> annNoInfoElem (astPrettyPrec 1 b)
  astPrettyPrec p (NegApp _ e) = resultPretty . parensIf (p > 0) $
    constrElem NegApp
      <*  infoElem "-"
      <*> annInfoElem (astPrettyPrec 4 e)
  astPrettyPrec p (App _ a b) = resultPretty . parensIf (p > 3) $
    constrElem App
      <*> annInfoElem (astPrettyPrec 3 a)
      <*  sepElem myFsep
      <*> annInfoElem (astPrettyPrec 4 b)
  astPrettyPrec p (Lambda _ patList body) = resultPretty . parensIf (p > 1) $
    constrElem Lambda
      <*  infoElem "\\"
      <*  sepElem myFsep
      <*> intersperse (sepElem myFsep) (map (annInfoElem . astPrettyPrec 2) patList)
      <*  sepElem myFsep
      <*  infoElem "->"
      <*> (annInfoElem $ astPretty body)
  -- keywords
  -- two cases for lets
  astPrettyPrec p (Let _ (BDecls _ declList) letBody)  =
    resultPretty.parensIf (p > 1) $ ppLetExp BDecls  declList letBody
  astPrettyPrec p (Let _ (IPBinds _ bindList) letBody) =
    resultPretty.parensIf (p > 1) $ ppLetExp IPBinds bindList letBody
  astPrettyPrec p (If _ cond thenexp elsexp) =
    resultPretty.parensIf (p > 1) $ constrElem If
      <*  infoElem "if"
      <*  sepElem myFsep
      <*> (annNoInfoElem $ astPretty cond)
      <*  sepElem myFsep
      <*  infoElem "then"
      <*  sepElem myFsep
      <*> (annNoInfoElem $ astPretty thenexp)
      <*  sepElem myFsep
      <*  infoElem "else"
      <*  sepElem myFsep
      <*> (annNoInfoElem $ astPretty elsexp)
  astPrettyPrec p (Case _ cond altList) =
    resultPretty.parensIf (p > 1) $ constrElem Case
      <*  infoElem "case"
      <*  sepElem myFsep
      <*> (annNoInfoElem $ astPretty cond)
      <*  sepElem myFsep
      <*  infoElem "of"
      <*  sepElem myVcat
      <*> ppBody caseIndent (noInfoList altList)
  astPrettyPrec p (Do _ stmtList) =
    resultPretty.parensIf (p > 1) $ constrElem Do
      <*  infoElem "do"
      <*  sepElem myVcat
      <*> ppBody doIndent (noInfoList stmtList)
  astPrettyPrec p (MDo _ stmtList) =
    resultPretty.parensIf (p > 1) $ constrElem MDo
      <*  infoElem "mdo"
      <*  sepElem myVcat
      <*> ppBody doIndent (noInfoList stmtList)
  -- Constructors & Vars
  astPrettyPrec _ (Var _ name) = resultPretty $ constrElem Var <*> (annNoInfoElem $ astPretty name)
  astPrettyPrec _ (IPVar _ ipname) = resultPretty $ constrElem IPVar <*> (annInfoElem $ astPretty ipname)
  astPrettyPrec _ (Con _ name) = resultPretty $ constrElem Con <*> (annInfoElem $ astPretty name)
  astPrettyPrec _ (Tuple _ expList) = resultPretty $ constrElem Tuple
    <*> parenList (noInfoList expList)
  astPrettyPrec _ (TupleSection _ mExpList) = resultPretty $ constrElem TupleSection
    <*> parenList (map (traverse $ annNoInfoElem.astPretty) mExpList)
  -- weird stuff
  astPrettyPrec _ (Paren _ e) = resultPretty.parens $ constrElem Paren <*> (annInfoElem $ astPretty e)
  astPrettyPrec _ (LeftSection _ e op) =
    resultPretty.parens $ constrElem LeftSection
      <*> (annInfoElem $ astPretty e)
      <*  sepElem hsep
      <*> (annInfoElem $ astPretty op)
  astPrettyPrec _ (RightSection _ op e) =
    resultPretty.parens $ constrElem RightSection
      <*> (annInfoElem $ astPretty op)
      <*  sepElem hsep
      <*> (annInfoElem $ astPretty e)
  astPrettyPrec _ (RecConstr _ c fieldList) = resultPretty $ constrElem RecConstr
    <*> (annInfoElem $ astPretty c) <*> braceList (noInfoList fieldList)
  astPrettyPrec _ (RecUpdate _ e fieldList) = resultPretty $ constrElem RecUpdate
    <*> (annInfoElem $ astPretty e) <*> braceList (noInfoList fieldList)
  -- Lists
  astPrettyPrec _ (List _ list) =  resultPretty $ constrElem List
     <*> brackets (intersperse (infoElem "," <* sepElem myFsepSimple) $ noInfoList list)
  astPrettyPrec _ (EnumFrom _ e) =
    resultPretty $ constrElem EnumFrom
      <*  infoElem "["
      <*> (annInfoElem $ astPretty e)
      <*  sepElem myFsepSimple
      <*  infoElem ".."
      <*  infoElem "]"
  astPrettyPrec _ (EnumFromTo _ from to) =
    resultPretty $ constrElem EnumFromTo
      <*  infoElem "["
      <*> (annInfoElem $ astPretty from)
      <*  sepElem myFsepSimple
      <*  infoElem ".."
      <*  sepElem myFsepSimple
      <*> (annInfoElem $ astPretty to)
      <*  infoElem "]"
  astPrettyPrec _ (EnumFromThen _ from thenE) =
    resultPretty $ constrElem EnumFromThen
      <*  infoElem "["
      <*> (annInfoElem $ astPretty from)
      <*  infoElem ","
      <*  sepElem myFsepSimple
      <*> (annInfoElem $ astPretty thenE)
      <*  sepElem myFsepSimple
      <*  infoElem ".."
      <*  infoElem "]"
  astPrettyPrec _ (EnumFromThenTo _ from thenE to) =
    resultPretty $ constrElem EnumFromThenTo
      <*  infoElem "["
      <*> (annInfoElem $ astPretty from)
      <*  infoElem ","
      <*  sepElem myFsepSimple
      <*> (annInfoElem $ astPretty thenE)
      <*  sepElem myFsepSimple
      <*  infoElem ".."
      <*  sepElem myFsepSimple
      <*> (annInfoElem $ astPretty to)
      <*  infoElem "]"
  astPrettyPrec _ (ListComp _ e qualList) =
    resultPretty $ constrElem ListComp
      <*  infoElem "["
      <*> (annInfoElem $ astPretty e)
      <*  sepElem myFsepSimple
      <*  infoElem "|"
      <*  sepElem myFsepSimple
      <*> intersperse (infoElem "," <* sepElem myFsepSimple) (noInfoList qualList)
      <*  infoElem "]"
  astPrettyPrec _ (ParComp _ e qualLists) =
    resultPretty $ constrElem ParComp
      <*  infoElem "["
      <*> (annInfoElem $ astPretty e)
      <*  sepElem myFsepSimple
      <*  infoElem "|"
      <*  sepElem myFsepSimple
      <*> sequenceA (map qList qualLists)
      where
        qsSep = infoElem "," <* sepElem myFsepSimple <* infoElem "|" <*  sepElem myFsepSimple
        qList qs = intersperse qsSep (noInfoList qs)
  astPrettyPrec p (ExpTypeSig _ e ty) =
    -- myFsep
    resultPretty . parensIf (p > 0) $ constrElem ExpTypeSig
      <*> (annInfoElem $ astPretty e)
      <*  sepElem myFsep
      <*  infoElem "::"
      <*> (annInfoElem $ astPretty ty)
  -- Template Haskell
  astPrettyPrec _ (BracketExp _ b) = resultPretty $ constrElem BracketExp <*> (annInfoElem $ astPretty b)
  astPrettyPrec _ (SpliceExp _ s) = resultPretty $ constrElem SpliceExp <*> (annInfoElem $ astPretty s)
  astPrettyPrec _ (TypQuote _ t)  =
    resultPretty $ constrElem TypQuote <* infoElem "\'\'" <*> (annInfoElem $ astPretty t)
  astPrettyPrec _ (VarQuote _ x)  =
    resultPretty $ constrElem VarQuote <* infoElem "\'" <*> (annInfoElem $ astPretty x)
  astPrettyPrec _ (QuasiQuote _ n qt) =
    resultPretty $ constrElem QuasiQuote
      <*  infoElem "["
      <*> infoElem n
      <*  infoElem "|"
      <*> infoElem qt
      <*  infoElem "|]"
  -- Hsx
  astPrettyPrec _ (XTag _ n attrs mattr cs) = unimplemented
  astPrettyPrec _ (XETag _ n attrs mattr) =
    resultPretty $ constrElem XETag
      -- myFsep
      <*  infoElem "<"
      <*  sepElem myFsep
      <*> (annInfoElem $ astPretty n)
      <*  sepElem myFsep
      <*> intersperse (sepElem myFsep) (noInfoList attrs)
      <*  sepElem myFsep
      <*> traverse (annInfoElem.astPretty) mattr
      <*  sepElem myFsep
      <*  infoElem "/>"
  astPrettyPrec _ (XPcdata _ s) = resultPretty $ constrElem XPcdata <*> infoElem s
  astPrettyPrec _ (XExpTag _ e) =
    resultPretty $ constrElem XExpTag
      -- myFsep
      <*  infoElem "<%"
      <*  sepElem myFsep
      <*> (annInfoElem $ astPretty e)
      <*  sepElem myFsep
      <*  infoElem "%>"
  astPrettyPrec _ (XChildTag _ cs) =
    resultPretty $ constrElem XChildTag
      -- myFsep
      <*  infoElem "<%>"
      <*  sepElem myFsep
      <*> intersperse (sepElem myFsep) (noInfoList cs)
      <*  sepElem myFsep
      <*  infoElem "</%>"
  -- Pragmas
  astPrettyPrec p (CorePragma _ s e) =
    resultPretty $ constrElem CorePragma
      -- myFsep
      <*  infoElem "{-# CORE"
      <*  sepElem myFsep
      <*> infoElem s
      <*  sepElem myFsep
      <*  infoElem "#-}"
      <*  sepElem myFsep
      <*> (annInfoElem $ astPretty e)
  astPrettyPrec _ (SCCPragma  _ s e) =
    resultPretty $ constrElem SCCPragma
      -- myFsep
      <*  infoElem "{-# SCC"
      <*  sepElem myFsep
      <*> infoElem s
      <*  sepElem myFsep
      <*  infoElem "#-}"
      <*  sepElem myFsep
      <*> (annInfoElem $ astPretty e)
  astPrettyPrec _ (GenPragma  _ s (a,b) (c,d) e) =
    resultPretty $ constrElem GenPragma
      -- myFsep
      <*  infoElem "{-# GENERATED"
      <*  sepElem myFsep
      <*> infoElem s
      <*  sepElem myFsep
      <*> tpl(a, b)
      <*  sepElem myFsep
      <*> tpl(c, d)
      <*  sepElem myFsep
      <*  infoElem "#-}"
      <*  sepElem myFsep
      <*> (annInfoElem $ astPretty e)
      where
        tpl (x, y) = pure (x, y)
          <* noInfoElem (show x)
          <* sepElem myFsep
          <* infoElem ":"
          <* sepElem myFsep
          <* noInfoElem (show y)
  -- Arrows
  astPrettyPrec p (Proc _ pat e) =
    resultPretty.parensIf (p > 1) $ constrElem Proc
      <*  infoElem "proc"
      <*  sepElem myFsep
      <*> (annInfoElem $ astPretty pat)
      <*  sepElem myFsep
      <*  infoElem "->"
      <*  sepElem myFsep
      <*> (annInfoElem $ astPretty e)
  astPrettyPrec p (LeftArrApp _ l r) =
    resultPretty.parensIf (p > 0) $ constrElem LeftArrApp
      <*> (annInfoElem $ astPretty l)
      <*  sepElem myFsep
      <*  infoElem "-<"
      <*  sepElem myFsep
      <*> (annInfoElem $ astPretty r)
  astPrettyPrec p (RightArrApp _ l r) =
    resultPretty.parensIf (p > 0) $ constrElem RightArrApp
      <*> (annInfoElem $ astPretty l)
      <*  sepElem myFsep
      <*  infoElem ">-"
      <*  sepElem myFsep
      <*> (annInfoElem $ astPretty r)
  astPrettyPrec p (LeftArrHighApp _ l r) =
    resultPretty.parensIf (p > 0) $ constrElem LeftArrHighApp
      <*> (annInfoElem $ astPretty l)
      <*  sepElem myFsep
      <*  infoElem "-<<"
      <*  sepElem myFsep
      <*> (annInfoElem $ astPretty r)
  astPrettyPrec p (RightArrHighApp _ l r) =
    resultPretty.parensIf (p > 0) $ constrElem RightArrHighApp
      <*> (annInfoElem $ astPretty l)
      <*  sepElem myFsep
      <*  infoElem ">>-"
      <*  sepElem myFsep
      <*> (annInfoElem $ astPretty r)

-- --------------------------------------------------------------------------

instance PrettyAst XAttr where
  astPretty (XAttr _ n v) =
    resultPretty $ constrElem XAttr
    <*> (annInfoElem $ astPretty n)
    <*  sepElem myFsep
    <*  infoElem "="
    <*  sepElem myFsep
    <*> (annInfoElem $ astPretty v)

-- --------------------------------------------------------------------------

instance PrettyAst XName where
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
  <*> (annInfoElem $ astPretty b)

ppWith f binds = nest 2 $ f <* infoElem "with" <* sepElem myVcat <*> ppBody withIndent (noInfoList binds)
withIndent = whereIndent

--------------------- Template Haskell -------------------------

instance PrettyAst Bracket where
  astPretty (ExpBracket _ e) =
    resultPretty $ constrElem ExpBracket <*> ppBracket "[|" ((annInfoElem $ astPretty e))
  astPretty (PatBracket _ p) =
    resultPretty $ constrElem PatBracket <*> ppBracket "[p|" ((annInfoElem $ astPretty p))
  astPretty (TypeBracket _ t) =
    resultPretty $ constrElem TypeBracket <*> ppBracket "[t|" ((annInfoElem $ astPretty t))
  astPretty (DeclBracket _ d) =
    resultPretty $ constrElem DeclBracket <*> ppBracket "[d|" d'
    where d' = intersperse (sepElem myFsep) (infoList d)

ppBracket :: String -> AstElem a -> AstElem a
ppBracket o x = infoElem o
  *> sepElem myFsep
  *> x
  <* sepElem myFsep
  <* infoElem "|]"

-- --------------------------------------------------------------------------

instance PrettyAst Splice where
  astPretty (IdSplice _ s) = resultPretty $ constrElem IdSplice <* infoElem "$" <*> infoElem s
  astPretty (ParenSplice _ e) =
    resultPretty $ constrElem ParenSplice
      <* infoElem "$(" <* sepElem myFsep <*> (annInfoElem $ astPretty e) <* sepElem myFsep <* infoElem ")"

------------------------- Patterns -----------------------------

instance PrettyAst Pat where
  astPrettyPrec _ (PVar _ name) = resultPretty $ constrElem PVar <*> (annInfoElem $ astPretty name)
  astPrettyPrec _ (PLit _ lit) = resultPretty $ constrElem PLit <*> (annInfoElem $ astPretty lit)
  astPrettyPrec p (PNeg _ pat) =
    resultPretty.parensIf (p > 0) $ constrElem PNeg
    -- myFsep
      <*  infoElem "-"
      <*  sepElem myFsep
      <*> (annInfoElem $ astPretty pat)
  astPrettyPrec p (PInfixApp _ a op b) =
    resultPretty.parensIf (p > 0) $ constrElem PInfixApp
    -- myFsep
      <*> annInfoElem (astPrettyPrec 1 a)
      <*  sepElem myFsep
      <*> ppQNameInfix op
      <*  sepElem myFsep
      <*> annInfoElem (astPrettyPrec 1 b)
  astPrettyPrec p (PApp _ n ps) =
    resultPretty.parensIf (p > 1 && not (null ps)) $ constrElem PApp
    -- myFsep
      <*> (annInfoElem $ astPretty n)
      <*  sepElem myFsep
      <*> intersperse (sepElem myFsep) (map (annNoInfoElem.astPrettyPrec 2) ps)
  astPrettyPrec _ (PTuple _ ps) = resultPretty $ constrElem PTuple <*> parenList (noInfoList ps)
  astPrettyPrec _ (PList _ ps) =
    resultPretty $ constrElem PList <*> braceList (noInfoList ps)
  astPrettyPrec _ (PParen _ pat) = resultPretty.parens $ constrElem PParen <*> (annInfoElem $ astPretty pat)
  astPrettyPrec _ (PRec _ c fields) = resultPretty.braces $ constrElem PRec
    <*> (annInfoElem $ astPretty c)
    <*> braceList (noInfoList fields)
  -- special case that would otherwise be buggy
  astPrettyPrec _ (PAsPat _ name (PIrrPat _ pat)) =
    resultPretty.braces $ constrElem PAsPat
    -- myFsep
      <*> (annInfoElem $ astPretty name)
      <*  infoElem "@"
      <*  sepElem myFsep
      <*  infoElem "~"
      <*> pat'
    where
      pat' = constrElem PIrrPat <*> annInfoElem (astPrettyPrec 2 pat)
  astPrettyPrec _ (PAsPat _ name pat) =
    resultPretty.braces $ constrElem PAsPat
    -- hcat
      <*> (annInfoElem $ astPretty name)
      <*  infoElem "@"
      <*> annInfoElem (astPrettyPrec 2 pat)
  astPrettyPrec _ (PWildCard _) = resultPretty.braces $ constrElem PWildCard <* infoElem "_"
  astPrettyPrec _ (PIrrPat _ pat) =
    resultPretty.braces $ constrElem PIrrPat
      <*  infoElem "~"
      <*> annInfoElem (astPrettyPrec 2 pat)
  astPrettyPrec p (PatTypeSig _ pat ty) =
    resultPretty.parensIf (p > 0) $ constrElem PatTypeSig
    -- myFsep
      <*> (annInfoElem $ astPretty pat)
      <*  sepElem myFsep
      <*  infoElem "::"
      <*  sepElem myFsep
      <*> (annInfoElem $ astPretty ty)
  astPrettyPrec p (PViewPat _ e pat) =
    resultPretty.parensIf (p > 0) $ constrElem PViewPat
    -- myFsep
      <*> (annInfoElem $ astPretty e)
      <*  sepElem myFsep
      <*  infoElem "->"
      <*  sepElem myFsep
      <*> (annInfoElem $ astPretty pat)
  astPrettyPrec p (PNPlusK _ n k) =
    resultPretty.parensIf (p > 0) $ constrElem PNPlusK
    -- myFsep
      <*> (annInfoElem $ astPretty n)
      <*  sepElem myFsep
      <*  infoElem "+"
      <*  sepElem myFsep
      <*> pure k <* infoElem (show k)
  -- HaRP
  astPrettyPrec _ (PRPat _ rs) = resultPretty $ constrElem PRPat <*> bracketList (noInfoList rs)
  astPrettyPrec _ (PXTag _ n attrs mattr cp) = unimplemented
  astPrettyPrec _ (PXETag _ n attrs mattr) =
    resultPretty $ constrElem PXETag
    -- myFsep
      <*  infoElem "<"
      <*> (annInfoElem $ astPretty n)
      <*  sepElem myFsep
      <*> intersperse (sepElem myFsep) (map (annInfoElem.astPretty) attrs)
      <*  sepElem myFsep
      <*> traverse (annInfoElem.astPretty) mattr
      <*  sepElem myFsep
      <*  infoElem "/>"
  astPrettyPrec _ (PXPcdata _ s) = resultPretty $ constrElem PXPcdata <*> infoElem s
  astPrettyPrec _ (PXPatTag _ p) =
    resultPretty $ constrElem PXPatTag
      -- myFsep
      <*  infoElem "<%"
      <*  sepElem myFsep
      <*> (annInfoElem $ astPretty p)
      <*  sepElem myFsep
      <*  infoElem "%>"
  astPrettyPrec _ (PXRPats _ ps) =
    resultPretty $ constrElem PXRPats
      -- myFsep
      <*  infoElem "<["
      <*  sepElem myFsep
      <*> intersperse (sepElem myFsep) (map (annInfoElem.astPretty) ps)
      <*  sepElem myFsep
      <*  infoElem "]>"
  -- Generics
  astPrettyPrec _ (PExplTypeArg _ qn t) =
    resultPretty $ constrElem PExplTypeArg
      <*> (annInfoElem $ astPretty qn)
      <*  sepElem myFsep
      <*  infoElem "{|"
      <*  sepElem myFsep
      <*> (annInfoElem $ astPretty t)
      <*  sepElem myFsep
      <*  infoElem "|}"
  astPrettyPrec _ (PBangPat _ pat) =
    resultPretty $ constrElem PBangPat
      <*  infoElem "!"
      <*> annInfoElem (astPrettyPrec 2 pat)

-- --------------------------------------------------------------------------

instance PrettyAst PXAttr where
  astPretty (PXAttr _ n p) =
    resultPretty $ constrElem PXAttr
      <*> (annInfoElem $ astPretty n)
      <*  sepElem myFsep
      <*  infoElem "="
      <*  sepElem myFsep
      <*> (annInfoElem $ astPretty p)

-- --------------------------------------------------------------------------

instance PrettyAst PatField where
  astPretty (PFieldPat _ n p) =
    resultPretty $ constrElem PFieldPat
      <*> (annInfoElem $ astPretty n)
      <*  sepElem myFsep
      <*  infoElem "="
      <*  sepElem myFsep
      <*> (annInfoElem $ astPretty p)
  astPretty (PFieldPun _ name) = resultPretty $ constrElem PFieldPun <*> (annInfoElem $ astPretty name)
  astPretty (PFieldWildcard _) = resultPretty $ constrElem PFieldWildcard <* infoElem ".."

--------------------- Regular Patterns -------------------------

instance PrettyAst RPat where
  astPretty (RPOp _ r op) = resultPretty $ constrElem RPOp <*> (annInfoElem $ astPretty r) <*> (annInfoElem $ astPretty op)
  astPretty (RPEither _ r1 r2) =
    resultPretty.parens $ constrElem RPEither
    -- myFsep
      <*> (annInfoElem $ astPretty r1)
      <*  sepElem myFsep
      <*  infoElem "|"
      <*  sepElem myFsep
      <*> (annInfoElem $ astPretty r2)
  astPretty (RPSeq _ rs) =
    resultPretty.parens $ constrElem RPSeq
    -- myFsep
      <*  infoElem "(/"
      <*  sepElem myFsep
      <*> intersperse (sepElem myFsep) (noInfoList rs)
      <*  sepElem myFsep
      <*  infoElem "/)"
  astPretty (RPGuard _ r gs) =
    resultPretty.parens $ constrElem RPGuard
    -- myFsep
      <*  infoElem "(|"
      <*  sepElem myFsep
      <*> (annInfoElem $ astPretty r)
      <*  sepElem myFsep
      <*> intersperse (sepElem myFsep) (noInfoList gs)
      <*  sepElem myFsep
      <*  infoElem "/)"
   -- special case that would otherwise be buggy
  astPretty (RPAs _ n (RPPat _ (PIrrPat _ p))) =
    let
      ip = constrElem PIrrPat <*> (annInfoElem $ astPretty p)
      rp = constrElem RPPat   <*> ip
    in
      resultPretty $ constrElem RPAs
      -- myFsep
        <*> (annInfoElem $ astPretty n)
        <*  infoElem "@:"
        <*  sepElem myFsep
        <*  infoElem "~"
        <*> rp
  astPretty (RPAs _ n r) =
    resultPretty $ constrElem RPAs
      -- hcat
        <*> (annInfoElem $ astPretty n)
        <*  infoElem "@"
        <*> (annInfoElem $ astPretty r)
  astPretty (RPPat _ p) = resultPretty $ constrElem RPPat <*> (annInfoElem $ astPretty p)
  astPretty (RPParen _ rp) = resultPretty.parens $ constrElem RPParen <*> (annInfoElem $ astPretty rp)

-- --------------------------------------------------------------------------

instance PrettyAst RPatOp where
  astPretty (RPStar  _) = resultPretty $ constrElem RPStar  <* infoElem "*"
  astPretty (RPStarG _) = resultPretty $ constrElem RPStarG <* infoElem "*!"
  astPretty (RPPlus  _) = resultPretty $ constrElem RPPlus  <* infoElem "+"
  astPretty (RPPlusG _) = resultPretty $ constrElem RPPlusG <* infoElem "+!"
  astPretty (RPOpt   _) = resultPretty $ constrElem RPOpt   <* infoElem "?"
  astPretty (RPOptG  _) = resultPretty $ constrElem RPOptG  <* infoElem "?!"

------------------------- Case bodies  -------------------------

instance PrettyAst Alt where
  astPretty (Alt _ e gAlts binds) =
    resultPretty $ constrElem Alt
      <*> (annInfoElem $ astPretty e)
      <*  sepElem hsep
      <*> (annInfoElem $ astPretty gAlts)
      <*  sepElem myVcat
      <*> traverse ppWhere binds

-- --------------------------------------------------------------------------

instance PrettyAst GuardedAlts where
  astPretty (UnGuardedAlt _ e) = resultPretty $ constrElem UnGuardedAlt
    <* infoElem "->" <* sepElem hsep <*> (annInfoElem $ astPretty e)
  astPretty (GuardedAlts _ altList) = resultPretty $ constrElem GuardedAlts
    <*> intersperse (sepElem myVcat) (noInfoList altList)

-- --------------------------------------------------------------------------

instance PrettyAst GuardedAlt where
  astPretty (GuardedAlt _ guards body) =
    resultPretty $ constrElem GuardedAlt
    -- myFsep
      <*  infoElem "|"
      <*  sepElem myFsep
      <*> intersperse (infoElem "," <* sepElem myFsep) (noInfoList guards)
      <*  sepElem myFsep
      <*  infoElem "->"
      <*  sepElem myFsep
      <*> (annInfoElem $ astPretty body)

------------------------- Statements in monads, guards & list comprehensions -----

instance PrettyAst Stmt where
  astPretty (Generator _ e from) = resultPretty $ constrElem Generator
    <*> (annInfoElem $ astPretty e)
    <*  sepElem hsep
    <*  infoElem "<-"
    <*  sepElem hsep
    <*> (annInfoElem $ astPretty from)
  astPretty (Qualifier _ e) = resultPretty $ constrElem Qualifier <*> (annPoints $ astPretty e)
  astPretty (LetStmt _ (BDecls _ declList)) =
    resultPretty $ constrElem LetStmt <*> ppLetStmt (constrElem BDecls) declList
  astPretty (LetStmt _ (IPBinds _ bindList)) =
    resultPretty $ constrElem LetStmt <*> ppLetStmt (constrElem IPBinds) bindList
  astPretty (RecStmt _ stmtList) =
    resultPretty $ constrElem RecStmt
      <*  infoElem "rec"
      <*  sepElem myVcat
      <*> ppBody letIndent (noInfoList stmtList)

ppLetStmt f ls = f <* infoElem "let" <* sepElem myVcat <*> ppBody letIndent (infoList ls)

-- --------------------------------------------------------------------------

instance PrettyAst QualStmt where
  -- myFsep
  astPretty (QualStmt _ s) = resultPretty $ constrElem QualStmt <*> (annInfoElem $ astPretty s)
  astPretty (ThenTrans _ f) =
    resultPretty $ constrElem ThenTrans <* infoElem "then" <* sepElem myFsep <*> (annInfoElem $ astPretty f)
  astPretty (ThenBy _ f e) =
    resultPretty $ constrElem ThenBy
      <* infoElem "then"
      <* sepElem myFsep
      <*> (annInfoElem $ astPretty f)
      <* sepElem myFsep
      <* infoElem "by"
      <* sepElem myFsep
      <*> (annInfoElem $ astPretty e)
  astPretty (GroupBy _ e) =
    resultPretty $ constrElem GroupBy
      <* infoElem "then"
      <* sepElem myFsep
      <* infoElem "group"
      <* sepElem myFsep
      <* infoElem "by"
      <* sepElem myFsep
      <*> (annInfoElem $ astPretty e)
  astPretty (GroupUsing   _ f) =
    resultPretty $ constrElem GroupUsing
      <* infoElem "then"
      <* sepElem myFsep
      <* infoElem "group"
      <* sepElem myFsep
      <* infoElem "using"
      <* sepElem myFsep
      <*> (annInfoElem $ astPretty f)
  astPretty (GroupByUsing _ e f) =
    resultPretty $ constrElem GroupByUsing
      <* infoElem "then"
      <* sepElem myFsep
      <* infoElem "group"
      <* sepElem myFsep
      <* infoElem "by"
      <* sepElem myFsep
      <*> (annInfoElem $ astPretty e)
      <* sepElem myFsep
      <* infoElem "using"
      <* sepElem myFsep
      <*> (annInfoElem $ astPretty f)

------------------------- Record updates

instance PrettyAst FieldUpdate where
  astPretty (FieldUpdate _ name e) =
    resultPretty $ constrElem FieldUpdate
      <*> (annInfoElem $ astPretty name)
      <* sepElem myFsep
      <* infoElem "="
      <* sepElem myFsep
      <*> (annInfoElem $ astPretty e)
  astPretty (FieldPun _ name) = resultPretty $ constrElem FieldPun <*> (annInfoElem $ astPretty name)
  astPretty (FieldWildcard _) = resultPretty $ constrElem FieldWildcard <* infoElem ".."

------------------------- Names -------------------------

instance PrettyAst QOp where
  astPretty (QVarOp _ n) = resultPretty $ constrElem QVarOp <*> ppQNameInfix n
  astPretty (QConOp _ n) = resultPretty $ constrElem QConOp <*> ppQNameInfix n

-- --------------------------------------------------------------------------

instance PrettyAst SpecialCon where
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

instance PrettyAst QName where
  astPretty qn
    | needParens = resultPretty $ enclose (infoElem "(" <* sepElem hsep) (infoElem ")") (annInfoElem $ rawQName qn)
    | otherwise =  rawQName qn
    where
      needParens = case qn of
        UnQual _    (Symbol _ _) -> True
        Qual   _  _ (Symbol _ _) -> True
        Special _ (Cons _)    -> True
        Special _ (FunCon _)  -> True
        _ -> False

-- --------------------------------------------------------------------------
-- QName utils
rawQName :: QName a -> DocM (QName SrcSpanInfo)
rawQName (Qual _ mn n)  =
  resultPretty $ constrElem Qual
    <*> (annNoInfoElem $ astPretty mn)
    <*  infoElem "."
    <*> annInfoElem (ppName n)
rawQName (UnQual _ n)   = resultPretty $ constrElem UnQual <*> annNoInfoElem (ppName n)
rawQName (Special _ sc) = resultPretty $ constrElem Special <*> (annNoInfoElem $ astPretty sc)

ppQNameInfix :: QName a -> AstElem (QName SrcSpanInfo)
ppQNameInfix name
  | isSymbolName (getName name) = annNoInfoElem $ rawQName name
  | otherwise = infoElem "`" *> annNoInfoElem (rawQName name) <* infoElem "`"

-- --------------------------------------------------------------------------

instance PrettyAst Op where
  astPretty (VarOp _ n) = resultPretty $ constrElem VarOp <*> ppNameInfix n
  astPretty (ConOp _ n) = resultPretty $ constrElem ConOp <*> ppNameInfix n

-- --------------------------------------------------------------------------
ppNameInfix :: Name a -> AstElem (Name SrcSpanInfo)
ppNameInfix name
  | isSymbolName name = annInfoElem $ ppName name
  | otherwise = infoElem "`" *> annInfoElem (ppName name) <* infoElem "`"

instance PrettyAst Name where
  astPretty n@(Ident _ _) = ppName n
  astPretty (Symbol _ s) =
    resultPretty.parens $ constrElem Symbol
      <*  sepElem hsep
      <*> noInfoElem s

-- --------------------------------------------------------------------------

isSymbolName :: Name l -> Bool
isSymbolName (Symbol _ _) = True
isSymbolName _ = False

getName :: QName l -> Name l
getName (UnQual _ s) = s
getName (Qual _ _ s) = s
getName (Special _ (Cons _)) = Symbol annStub ":"
getName (Special _ (FunCon _)) = Symbol annStub  "->"
getName (Special _ s) = Ident annStub (specialName s)

specialName :: SpecialCon l -> String
specialName (UnitCon _) = "()"
specialName (ListCon _) = "[]"
specialName (FunCon  _) = "->"
specialName (TupleCon _ b n) = "(" ++ hash ++ replicate (n-1) ',' ++ hash ++ ")"
    where hash = if b == Unboxed then "#" else ""
specialName (Cons _) = ":"
specialName (UnboxedSingleCon _) = "(# #)"

ppName :: Name a -> DocM (Name SrcSpanInfo)
ppName (Symbol _ s) = resultPretty $ constrElem Symbol <*> noInfoElem s
ppName (Ident  _ s) = resultPretty $ constrElem Ident <*> noInfoElem s

-- --------------------------------------------------------------------------

instance PrettyAst IPName where
  astPretty (IPDup _ s) = resultPretty $ constrElem IPDup
    <* infoElem "?" <*> infoElem s
  astPretty (IPLin _ s) = resultPretty $ constrElem IPLin
    <* infoElem "%" <*> infoElem s

-- --------------------------------------------------------------------------

instance PrettyAst IPBind where
  astPretty (IPBind _ ipname exp) = resultPretty $ constrElem IPBind
    <*> (annInfoElem $ astPretty ipname)
    <*  sepElem myFsep
    <*  infoElem "="
    <*  sepElem myFsep
    <*> (annInfoElem $ astPretty exp)

-- --------------------------------------------------------------------------

instance PrettyAst  CName where
  astPretty (VarName _ name) = resultPretty $ constrElem VarName <*> (annNoInfoElem $ astPretty name)
  astPretty (ConName _ name) = resultPretty $ constrElem ConName <*> (annNoInfoElem $ astPretty name)

-- --------------------------------------------------------------------------

instance PrettyAst Context where
  astPretty (CxEmpty _) = resultPretty.onsideNest $ constrElem CxEmpty <* infoElem "()" <* sepElem fsep <* infoElem "=>"
  astPretty (CxSingle _ asst) =
    resultPretty.onsideNest $ constrElem CxSingle
      <*> (annNoInfoElem $ astPretty asst) <* sepElem fsep <* infoElem "=>"
  astPretty (CxTuple _ assts) =
    resultPretty $ constrElem CxTuple
      <*> parenList (noInfoList assts) -- myFsep and parenList -> myFsep and myFsepSimple ???
      <* sepElem myFsep
      <* infoElem "=>"
  astPretty (CxParen _ asst) = resultPretty $ constrElem CxParen <*> enclose (infoElem "(") (infoElem ")") ((annNoInfoElem $ astPretty asst))

ppContext :: Maybe (Context a) -> AstElem (Maybe (Context SrcSpanInfo))
ppContext context = traverse impl context
  where
    impl c = onsideNest $ (parens $ (annInfoElem $ astPretty c)) <*  sepElem fsep <*  infoElem "=>"

-- --------------------------------------------------------------------------
-- hacked for multi-parameter type classes
instance PrettyAst Asst where
  astPretty (ClassA _ a ts)   = -- myFsep $ ppQName a : map ppAType ts
    resultPretty $ constrElem ClassA
      <*> (annNoInfoElem $ astPretty a)
      <*  sepElem myFsep
      <*> intersperse (sepElem myFsep) (map (annNoInfoElem.ppAType) ts)
  astPretty (InfixA _ a op b) =  -- myFsep $ [pretty a, ppQNameInfix op, pretty b]
    resultPretty $ constrElem InfixA
      <*> (annNoInfoElem $ astPretty a)
      <*   sepElem myFsep
      <*> ppQNameInfix op
      <*   sepElem myFsep
      <*> (annNoInfoElem $ astPretty b)
  astPretty (IParam _ i t)    = -- myFsep $ [pretty i, text "::", pretty t]
    resultPretty $ constrElem IParam
      <*> (annInfoElem $ astPretty i)
      <*  sepElem myFsep
      <*  infoElem "::"
      <*> (annInfoElem $ astPretty t)
  astPretty (EqualP _ t1 t2)  = -- myFsep $ [pretty t1, text "~", pretty t2]
    resultPretty $ constrElem EqualP
      <*> (annInfoElem $ astPretty t1)
      <*   sepElem myFsep
      <*   infoElem "~"
      <*> (annInfoElem $ astPretty t2)

------------------------- pp utils -------------------------

format :: String -> DocM SrcSpan
format s = do
  SrcLoc f l c <- getPos
  let newColumn = c + length s
  putPos $ SrcLoc f l newColumn
  return $ SrcSpan f l c l newColumn

getPos :: MonadState DocState m => m SrcLoc
getPos = gets pos

putPos :: MonadState DocState m => SrcLoc -> m SrcLoc
putPos l = do
  DocState _ n <- get
  put $! DocState l n
  return l

line :: MonadState DocState m => m ()
line = do
  DocState (SrcLoc f l c) n <- get
  putPos $! SrcLoc f (l + 1) (if n > 0 then n else 1)
  return ()

space :: MonadState DocState m => Int -> m ()
space x = do
  SrcLoc f l c <- getPos
  putPos $! SrcLoc f l $! c + x
  return ()

-- --------------------------------------------------------------------------
-- AstElem definition

type AstElem = WriterT [SrcSpan] DocM


-- --------------------------------------------------------------------------
-- AstElem utils

infoElem :: String -> AstElem String
infoElem s = annInfoElem $ format s >> return s

noInfoElem :: String -> AstElem String
noInfoElem s = annNoInfoElem $  format s >> return s

sepElem :: DocM() -> AstElem ()
sepElem s = annNoInfoElem s

annNoInfoElem :: DocM a -> AstElem a
annNoInfoElem a = lift a

annInfoElem :: DocM a -> AstElem a
annInfoElem a = do
  sp <- getPos
  a' <- lift a
  ep <- getPos
  tell $ if sp == ep then [] else [mkSrcSpan sp ep]
  return a'

annPoints  :: (Annotated ast) => DocM (ast SrcSpanInfo) -> AstElem (ast SrcSpanInfo)
annPoints a = do
  a' <- lift a
  tell.srcInfoPoints $ ann a'
  return a'

annStub = undefined

unimplemented = undefined

constrElem :: (a -> b) -> AstElem b
constrElem f = lift.return $ f annStub

infoList xs = map (annInfoElem.astPretty) xs
noInfoList xs = map (annNoInfoElem.astPretty) xs

intersperse :: Applicative f => f a1 -> [f a] -> f [a]
intersperse _ [] = pure []
intersperse sep (e:es) = sequenceA $ e : (map (sep *>) es)

nest :: Int -> AstElem a -> AstElem a
nest n a = (sepElem $ impl n) *> a <* (sepElem $ impl (-n))
  where
    impl x = do
      DocState l n <- get
      put $! DocState l $ 1 + n + x
      return ()

onsideNest :: AstElem b -> AstElem b
onsideNest x = do
  PrettyMode m _  <- ask
  nest (onsideIndent m) x

traverseSep sep f m = traverse (\e -> f e <* sep) m

resultPretty :: Annotated ast => AstElem (ast SrcSpanInfo) -> DocM (ast SrcSpanInfo)
resultPretty a = do
  sp <- getPos
  (a', ps) <- runWriterT a
  ep <- getPos
  let span = SrcSpanInfo (mkSrcSpan sp ep) ps
  return $ amap (const span) a'

-- --------------------------------------------------------------------------
-- separators

vcat :: DocM ()
vcat = do
  DocState (SrcLoc f l c) n <- get
  let s = if n < c then line else space $ n - c
  _ <- s
  return ()

hsep :: DocM ()
hsep = space 1

hcat :: DocM ()
hcat = pure ()

-- fsep prototype
fsep :: DocM ()
fsep  = do
  PrettyMode mode style <- ask
  c <- getPos
  if layout mode == PPOffsideRule || layout mode == PPSemiColon
  then
    if srcColumn c >= lineLength style then line else (pure ())
  else
    hsep

{-
a $$$ b = layoutChoice (a vcat) (a <+>) b

mySep = layoutChoice mySep' hsep
  where
    -- ensure paragraph fills with indentation.
    mySep' [x]    = x
    mySep' (x:xs) = x <+> fsep xs
    mySep' []     = error "Internal error: mySep"
-}

myVcat = layoutChoice vcat hsep

myFsepSimple = layoutChoice fsep hsep

-- same, except that continuation lines are indented,
-- which is necessary to avoid triggering the offside rule.
-- myFsep prototype
myFsep  = myFsepSimple

-- --------------------------------------------------------------------------
-- paren related functions

parenListSep :: AstElem String
parenListSep = infoElem "," <* sepElem myFsepSimple

parenList :: [AstElem a] -> AstElem [a]
parenList xs =  parens $ intersperse parenListSep xs

hashParenList :: [AstElem a] -> AstElem [a]
hashParenList xs = infoElem "(#" *> intersperse parenListSep xs <* infoElem "#)"

braceList :: [AstElem a] -> AstElem [a]
braceList xs = braces $ intersperse parenListSep xs

bracketList :: [AstElem a] -> AstElem [a]
bracketList xs = brackets $ intersperse (sepElem myFsepSimple) xs

enclose ob cb x = ob *> x <* cb

parens :: AstElem a -> AstElem a
parens d = infoElem "(" *> d <* infoElem ")"

braces :: AstElem a -> AstElem a
braces d = infoElem "{" *> d <* infoElem "}"

brackets :: AstElem a -> AstElem a
brackets d = infoElem "[" *> d <* infoElem "]"

parensIf :: Bool -> AstElem a -> AstElem a
parensIf p d = if p then parens d else d

-- --------------------------------------------------------------------------
-- Wrap in braces and semicolons, with an extra space at the start in
-- case the first doc begins with "-", which would be scanned as {-

flatBlock  :: [AstElem a] -> AstElem [a]
flatBlock xs = braces $ sepElem hsep *> intersperse (infoElem ";") xs

-- Same, but put each thing on a separate line
prettyBlock :: [AstElem a] -> AstElem [a]
prettyBlock xs = braces $ sepElem hsep *> intersperse (infoElem ";" <* sepElem vcat) xs

-- --------------------------------------------------------------------------
-- general utils

layoutChoice a b  = do
  PrettyMode mode _ <- ask
  if layout mode == PPOffsideRule || layout mode == PPSemiColon
  then a
  else b


blankline :: AstElem ()
blankline = do
  PrettyMode mode _ <- ask
  if spacing mode && layout mode /= PPNoLayout
      then
        sepElem hsep <* sepElem vcat
      else
        sepElem (pure ())

ppBody :: (PR.PPHsMode -> Int) -> [AstElem a] -> AstElem [a]
ppBody f dl =  do
  (PrettyMode mode _) <- ask
  let i = f mode
  case layout mode of
    PPOffsideRule -> indent i
    PPSemiColon   -> indentExplicit i
    _ -> flatBlock dl
  where
    indent i = nest i $ intersperse (sepElem vcat) dl
    indentExplicit i = nest i $ prettyBlock dl

topLevel :: (Annotated ast, PrettyAst ast) => [ast a] -> AstElem [ast SrcSpanInfo]
topLevel dl = do
  PrettyMode mode _ <- ask
  let dl' = noInfoList dl
  case layout mode of
    PPOffsideRule -> sepElem vcat *> intersperse (sepElem myVcat) dl'
    PPSemiColon -> sepElem vcat *> prettyBlock dl'
    PPInLine -> sepElem vcat *> prettyBlock dl'
    PPNoLayout -> sepElem hsep *> flatBlock dl'


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