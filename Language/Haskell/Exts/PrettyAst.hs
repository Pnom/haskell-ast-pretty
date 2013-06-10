{-# LANGUAGE FlexibleContexts #-}
module Language.Haskell.Exts.PrettyAst
  ( PrettyAst(..)
  , renderWithMode
  , renderWithDefMode
  , renderWithTrace
  , PrettyMode(..)
  , defPrettyMode
  , PR.Style(..)
  , PR.style
  , PR.PPHsMode(..)
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
  nestSize :: !Int,
  prettifyingTrace :: ![String]
  } deriving Show

defDocState fl = DocState (SrcLoc fl  1  1) 0 []

data PrettyMode = PrettyMode PR.PPHsMode PR.Style
defPrettyMode = PrettyMode PR.defaultMode PR.style

type DocM = ReaderT PrettyMode (State DocState)

-- | render the document with a given file name and mode.
renderWithMode :: (Annotated ast, PrettyAst ast) => String -> PrettyMode -> ast SrcSpanInfo -> ast SrcSpanInfo
renderWithMode fl mode doc = evalState (runReaderT (astPretty doc) mode) (defDocState fl)

-- | render the document with a given file name and 'defaultMode'.
renderWithDefMode :: (Annotated ast, PrettyAst ast) => String -> ast SrcSpanInfo -> ast SrcSpanInfo
renderWithDefMode fl doc = renderWithMode fl defPrettyMode doc

-- | render the document with a given file name and mode.
renderWithTrace :: (Annotated ast, PrettyAst ast) => String -> PrettyMode -> ast SrcSpanInfo -> (ast SrcSpanInfo, [String])
renderWithTrace fl mode doc = (res, prettifyingTrace s)
  where (res, s) = runState (runReaderT (astPretty doc) mode) (defDocState fl)

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
  astPretty (Module _ mbHead os imp decls)
    | null imp && null decls = resultPretty emptyBody
    | isNothing mbHead       = resultPretty vcatBody
    | otherwise = resultPretty $ do
      PrettyMode mode _ <- ask
      case layout mode of
        PPOffsideRule -> vcatBody
        PPSemiColon   -> semiColonBody
        PPInLine      -> semiColonBody
        PPNoLayout    -> noLayoutBody
    where
      body = pure (\os h i d ->  Module annStub h os i d)
        <*  implicitElem "{"
        <*> intersperse semiColon (annListElem annNoInfoElem os)
        <*  implicitElem "}"
        <*  sepElemIf (not $ null os) layoutCat
        <*> traverse (annNoInfoElem . astPretty) mbHead
        <*  sepElemIf (isJust mbHead) layoutCat

      emptyBody = body <* implicitElem "{" <*> pure [] <* implicitElem ";" <*> pure [] <* implicitElem "}"

      vcatBody = body
        <*  implicitElem "{"
        <*> intersperse semiColon (annListElem annNoInfoElem imp)
        <*  implicitElem ";"
        <*  sepElemIf (not $ null imp) layoutCat
        <*> intersperse semiColon (map declFn decls)
        <*  sepElemIf (not $ null decls) layoutCat
        <*  implicitElem "}"

      semiColonBody = body
        <*  nestMode onsideIndent (infoElem "{" <* sepElem vcat)
        <*> nestMode onsideIndent (intersperse semiColon $ annListElem annNoInfoElem imp)
        <*  (if null imp then pure "" else semiColon)
        <*> nestMode onsideIndent (intersperse semiColon $ map declFn decls)
        <*  infoElem "}"

      noLayoutBody = body
        <*  infoElem "{"
        <*  sepElem layoutCat
        <*> intersperse semiColon (annListElem annNoInfoElem imp)
        <*  (if not $ null imp then semiColon else pure "")
        <*> intersperse semiColon (map declFn decls)
        <*  infoElem "}"

      semiColon = do
        PrettyMode mode _ <- ask
        let sep = if isNothing mbHead then implicitElem else infoElem
        case layout mode of
          PPOffsideRule -> implicitElem ";" <* sepElem vcat
          PPNoLayout    -> sep ";" <* sepElem hsep
          PPSemiColon   -> sep ";" <* sepElem vcat
          PPInLine      -> sep ";" <* sepElem vcat

      layoutCat = do
        PrettyMode mode _ <- ask
        case layout mode of
          PPOffsideRule -> vcat
          PPNoLayout    -> hsep
          PPSemiColon   -> vcat
          PPInLine      -> vcat

      declFn d@(FunBind _ _) = prettyFunBind $ astPretty d
      declFn d = annNoInfoElem $ astPretty d

      prettyFunBind fb = do
        (FunBind span ms) <- lift fb
        if null ms
          then return $ FunBind span []
          else do
            let
              startFb@(SrcSpan fl ln cl _ _) = srcInfoSpan . ann $ head ms
              endFb = srcInfoSpan .ann $ last ms
              -- read all points from Math
              matchPs = concatMap (srcInfoPoints.ann) ms
              fbPs = srcInfoPoints span
              span' = SrcSpanInfo (mergeSrcSpan startFb endFb) matchPs

            tell $ AstElemInfo (Just $ SrcLoc fl ln cl) fbPs
            return $ FunBind span' ms



  astPretty (XmlPage _ _mn os n attrs mattr cs) = unimplemented
  astPretty (XmlHybrid _ mbHead os imp decls n attrs mattr cs) = unimplemented

--------------------------  Module Header ------------------------------

instance PrettyAst ModuleHead where
  -- mySep
  astPretty (ModuleHead _ m mbWarn mbExportList) =
    resultPretty.(nestMode onsideIndent) $ constrElem ModuleHead
      -- MySep
      <*  infoElem "module"
      <*  sepElem fsep
      <*> (annNoInfoElem $ astPretty m)
      <*> traverse (\x -> sepElem fsep *> (annNoInfoElem $ astPretty x)) mbWarn
      <*> traverse (\x -> sepElem fsep *> (annNoInfoElem $ astPretty x)) mbExportList
      <*  sepElem fsep
      <*  infoElem "where"

-- --------------------------------------------------------------------------

instance PrettyAst WarningText where
    astPretty w = case w of
      (DeprText _ s) -> impl DeprText "{-# DEPRECATED" s
      (WarnText _ s) -> impl WarnText "{-# WARNING"    s
      where
        -- mySep
      impl f c s = resultPretty.(nestMode onsideIndent) $ constrElem f <* infoElem c <* sepElem fsep <*> infoElem s <* sepElem fsep <* infoElem "#}"

-- --------------------------------------------------------------------------

instance PrettyAst ModuleName where
  astPretty (ModuleName _ s) = resultPretty $ constrElem ModuleName <*> noInfoElem s

-- --------------------------------------------------------------------------

instance PrettyAst ExportSpecList where
  astPretty (ExportSpecList _ especs) =
    resultPretty $ constrElem ExportSpecList <*> parenList (annListElem annNoInfoElem especs)

-- --------------------------------------------------------------------------

instance PrettyAst ExportSpec where
  astPretty (EVar _ name) = resultPretty $ constrElem EVar <*> (annNoInfoElem $ astPretty name)
  astPretty (EAbs _ name) = resultPretty $ constrElem EAbs <*> (annNoInfoElem $ astPretty name)
  astPretty (EThingAll _ name) =
    resultPretty $ constrElem EThingAll <*> (annNoInfoElem $ astPretty name) <* infoElem "(" <* infoElem ".." <* infoElem ")"
  astPretty (EThingWith _ name nameList) =
    resultPretty $ constrElem EThingWith
      <*> (annNoInfoElem $ astPretty name)
      <*> parenList (annListElem annNoInfoElem nameList)
  astPretty (EModuleContents _ m) = resultPretty $ constrElem EModuleContents <*> (annNoInfoElem $ astPretty m)

-- --------------------------------------------------------------------------

instance PrettyAst ImportDecl where
  astPretty (ImportDecl _ mod qual src mbPkg mbName mbSpecs) =
    resultPretty.(nestMode onsideIndent) $ pure impl
      -- mySep
      <*  infoElem "import"
      <*  sepElem fsep
      <*> (if src then pure src <* infoElem "{-# SOURCE #-}" <* sepElem fsep else pure src)
      <*> (if qual then pure qual <* infoElem "qualified" <* sepElem fsep else pure qual)
      <*> traverse (\x -> infoElem x <* sepElem fsep) mbPkg
      <*> (annNoInfoElem $ astPretty mod)
      <*> traverse (\ x -> sepElem fsep *> infoElem "as" *> sepElem hsep *> (annNoInfoElem $ astPretty x)) mbName
      <*> traverse (\ x -> sepElem fsep *> (annNoInfoElem $ astPretty x)) mbSpecs
    where impl s q p m n sp = ImportDecl annStub m q s p n sp

-- --------------------------------------------------------------------------

instance PrettyAst ImportSpecList where
  astPretty (ImportSpecList _ b ispecs) =
    resultPretty $ constrElem ImportSpecList
      <*> (if b then pure b <* infoElem "hiding" else pure b)
      <*  sepElemIf (not $ null ispecs) hsep
      <*  infoElem "("
      <*> intersperse parenListSep (annListElem annNoInfoElem ispecs)
      <*  infoElem ")"

-- --------------------------------------------------------------------------

instance PrettyAst ImportSpec where
  astPretty (IVar _ name)                = resultPretty $ constrElem IVar <*> annNoInfoElem (astPretty name)
  astPretty (IAbs _ name)                = resultPretty $ constrElem IAbs <*> pointsInfoElem (astPretty name)
  astPretty (IThingAll _ name)           =
    resultPretty $ constrElem IThingAll <*> (annNoInfoElem $ astPretty name) <* infoElem "(..)"
  astPretty (IThingWith _ name nameList) =
   resultPretty $ constrElem IThingWith <*> (annNoInfoElem $ astPretty name) <*> parenList (annListElem annNoInfoElem nameList)

-------------------------  Declarations ------------------------------

instance PrettyAst Decl where
  astPretty (TypeDecl _ head htype) =
    blankline.resultPretty.(nestMode onsideIndent) $ constrElem TypeDecl
      -- mySep
      <* infoElem "type"
      <* sepElem fsep
      <*> (annNoInfoElem $ astPretty head)
      <* sepElem fsep
      <* infoElem "="
      <* sepElem fsep
      <*> (annNoInfoElem $ astPretty htype)
  astPretty (TypeFamDecl _ head mkind) =
    blankline.resultPretty.(nestMode onsideIndent) $ constrElem TypeFamDecl
      <*  infoElem "type"
      <*  sepElem fsep
      <*  infoElem "family"
      <*  sepElem fsep
      <*> ppFsepDhead head
      <*  sepElemIf (isJust mkind) fsep
      <*> ppOptKind mkind
  astPretty (DataDecl _ don mContext head constrList mDeriving) =
    blankline.resultPretty.(nestMode onsideIndent) $ constrElem DataDecl
      <*> (annNoInfoElem $ astPretty don)
      <*  sepElem fsep
      <*> traverse (\ c -> (annNoInfoElem $ astPretty c) <* sepElem fsep) mContext
      <*> ppFsepDhead head
      <*  sepElem hsep
      <*> ppConstrList constrList
      <*> traverse (\x -> (nestMode onsideIndent) $ sepElem myVcat *> ppDeriving x) mDeriving
  astPretty (GDataDecl _ don mContext hd mkind gadtDecl mDeriving) =
    blankline.resultPretty $ constrElem GDataDecl
      -- mySep
      <*> (annNoInfoElem $ astPretty don)
      <*  sepElem fsep
      <*> traverse (\ c -> (annNoInfoElem $ astPretty c) <* sepElem fsep) mContext
      <*> ppFsepDhead hd
      <*  sepElemIf (isJust mkind) fsep
      <*> ppOptKind mkind
      <*  sepElem fsep
      <*  infoElem "where"
      <*> ppBody classIndent (annListElem annNoInfoElem gadtDecl)
      <*> traverse identDeriving mDeriving
      where
        identDeriving d = (nestMode letIndent) $ sepElem myVcat *> ppDeriving d
  astPretty (DataFamDecl _ mContext head mKind) =
    blankline.resultPretty.(nestMode onsideIndent) $ constrElem DataFamDecl
      -- mySep
      <*  infoElem "data"
      <*  sepElem fsep
      <*  infoElem "family"
      <*  sepElem fsep
      <*> traverse (\ c -> (annNoInfoElem $ astPretty c) <* sepElem fsep) mContext
      <*> ppFsepDhead head
      <*  sepElemIf (isJust mKind) fsep
      <*> ppOptKind mKind
  astPretty (TypeInsDecl _ tl tr) =
    blankline.resultPretty.(nestMode onsideIndent) $ constrElem TypeInsDecl
      -- mySep
      <*  infoElem "type"
      <*  sepElem fsep
      <*  infoElem "instance"
      <*  sepElem fsep
      <*> (annNoInfoElem $ astPretty tl)
      <*  sepElem fsep
      <*  infoElem "="
      <*  sepElem fsep
      <*> (annNoInfoElem $ astPretty tr)
  astPretty (DataInsDecl _ don t qConDecl mDeriving) =
    blankline.resultPretty.(nestMode onsideIndent) $ constrElem DataInsDecl
      -- mySep
      <*> (annNoInfoElem $ astPretty don)
      <*  sepElem fsep
      <*  infoElem "instance"
      <*  sepElem fsep
      <*> (annNoInfoElem $ astPretty t)
      <*  sepElem hsep
      <*> ppConstrList qConDecl
      <*> traverse (\x -> (nestMode onsideIndent) $ sepElem myVcat *> ppDeriving x) mDeriving
  astPretty (GDataInsDecl _ don t mKind gadtDecl mDeriving) =
    blankline.resultPretty.(nestMode onsideIndent) $ constrElem GDataInsDecl
      <*> (annNoInfoElem $ astPretty don)
      <*  sepElem fsep
      <*  infoElem "instance"
      <*  sepElem fsep
      <*> (annNoInfoElem $ astPretty t)
      <*  sepElemIf (isJust mKind) fsep
      <*> ppOptKind mKind
      <*  infoElem "where"
      <*> ppBody classIndent (annListElem annNoInfoElem gadtDecl)
      <*> traverse (\x -> (nestMode onsideIndent) $sepElem myVcat *> ppDeriving x) mDeriving
  astPretty (ClassDecl _ mContext head funDep mClassDecl) =
    blankline.resultPretty.(nestMode onsideIndent) $ constrElem ClassDecl
      <*  infoElem "class"
      <*  sepElem fsep
      <*> traverse (\ c -> (annNoInfoElem $ astPretty c) <* sepElem fsep) mContext
      <*> ppFsepDhead head
      <*  sepElem fsep
      <*> ppFunDeps funDep
      <*  (if null $ fromMaybe [] mClassDecl then infoElem "" else sepElem fsep *> infoElem "where")
      <*> traverse cDecl mClassDecl
    where
      cDecl cd = ppBody classIndent (annListElem annNoInfoElem cd)
  astPretty (InstDecl _ mContext instHead mInstDecl) =
    blankline.resultPretty $ constrElem InstDecl
      <*  infoElem "instance"
      <*  sepElem fsep
      <*> traverse (\ c -> (annNoInfoElem $ astPretty c) <* sepElem fsep) mContext
      <*> ppInstHeadInDecl instHead
      <*> traverse instDecl mInstDecl
    where
      instDecl is = sepElem fsep *> infoElem "where" *> ppBody classIndent (annListElem annNoInfoElem is)
  astPretty (DerivDecl _ mContext instHead) =
    blankline.resultPretty.(nestMode onsideIndent) $ constrElem DerivDecl
      -- mySep
      <*  infoElem "deriving"
      <*  sepElem fsep
      <*  infoElem "instance"
      <* sepElem fsep
      <*> traverse (\ c -> (annNoInfoElem $ astPretty c) <* sepElem fsep) mContext
      <*> ppInstHeadInDecl instHead
  astPretty (InfixDecl _ assoc mInt op) =
    blankline.resultPretty.(nestMode onsideIndent) $ constrElem InfixDecl
      -- mySep
      <*> (annNoInfoElem $ astPretty assoc)
      <*  sepElem fsep
      <*> traverse (\x -> infoElem (show x) *> pure x <* sepElem fsep) mInt
      <*> intersperse (sepElem fsep) (annListElem annNoInfoElem op)
  astPretty (DefaultDecl _ t) =
    blankline.resultPretty $ constrElem DefaultDecl
      <*  infoElem "default"
      <*  sepElem hsep
      <*> parenList (annListElem annNoInfoElem t)
  astPretty (SpliceDecl _ e) =
    blankline.resultPretty $ constrElem SpliceDecl
      <*> annNoInfoElem (astPretty e)
  astPretty (TypeSig _ ns t) =
    blankline.resultPretty.(nestMode onsideIndent) $ constrElem TypeSig
      -- mySep
      <*> intersperse (infoElem "," <* sepElem fsep) (annListElem annNoInfoElem ns)
      <*  (sepElem $ case ns of [n] -> hsep; _ -> fsep)
      <*  infoElem "::"
      <*  sepElem fsep
      <*> (annNoInfoElem $ astPretty t)
  astPretty (FunBind _ ms) =
    resultPretty $ constrElem FunBind
      <*> intersperse (sep  <* sepElem myVcat) (annListElem annNoInfoElem ms)
      <*  (if not $ null ms then sep else pure "")
    where
      sep = do
        PrettyMode mode _ <- ask
        if layout mode == PPOffsideRule then (pure "") else (infoElem ";")
  astPretty (PatBind _ pat mType rhs mBinds) =
    resultPretty $
      (
        (nestMode onsideIndent) $ constrElem PatBind
        -- myFsep
        <*> (annNoInfoElem $ astPretty pat)
        <*> traverse (\x -> (sepElem myFsep) *> infoElem "::" *> sepElem hsep *> (annNoInfoElem.astPretty) x) mType
        <*  sepElem myFsep
        <*> (annNoInfoElem $ astPretty rhs)
      )
      <*> ppWhere mBinds
  astPretty (ForImp _ callConv mSafety mStr n t) =
    blankline.resultPretty.(nestMode onsideIndent) $ constrElem ForImp
      -- mySep
      <*  infoElem "foreign import"
      <*  sepElem fsep
      <*> (annNoInfoElem $ astPretty callConv)
      <*> traverse (\ x -> sepElem fsep *> (annNoInfoElem $ astPretty x)) mSafety
      <*> traverse (\ x -> sepElem fsep *> infoElem x) mStr
      <*  sepElem fsep
      <*> (annNoInfoElem $ astPretty n)
      <*  sepElem fsep
      <*  infoElem "::"
      <*> (annNoInfoElem $ astPretty t)
  astPretty (ForExp _ callConv mStr n t) =
    blankline.resultPretty.(nestMode onsideIndent) $ constrElem ForExp
      -- mySep
      <*  infoElem "foreign export"
      <*  sepElem fsep
      <*> (annNoInfoElem $ astPretty callConv)
      <*> traverse (\ x -> sepElem fsep *> infoElem x) mStr
      <*  sepElem fsep
      <*> (annNoInfoElem $ astPretty n)
      <*  sepElem fsep
      <*  infoElem "::"
      <*> (annNoInfoElem $ astPretty t)
  astPretty (RulePragmaDecl _ rs) =
    blankline.resultPretty $ constrElem RulePragmaDecl
      -- myVcat
      <*  infoElem "{-# RULES"
      <*  sepElem myVcat
      <*> intersperse (sepElem myVcat) (annListElem annNoInfoElem rs)
      <*  sepElem myVcat
      <*  sepElem hsep
      <*  infoElem "#-}"
  astPretty (DeprPragmaDecl _ ds) =
    blankline.resultPretty $ constrElem DeprPragmaDecl
      -- myVcat
      <*  infoElem "{-# DEPRECATED"
      <*  sepElem myVcat
      <*> intersperse (sepElem myVcat) (map ppWarnDepr ds)
      <*  sepElem myVcat
      <*  sepElem hsep
      <*  infoElem "#-}"
  astPretty (WarnPragmaDecl _ ws) =
    blankline.resultPretty $ constrElem WarnPragmaDecl
      -- myVcat
      <*  infoElem "{-# WARNING"
      <*  sepElem myVcat
      <*> intersperse (sepElem myVcat) (map ppWarnDepr ws)
      <*  sepElem myVcat
      <*  sepElem hsep
      <*  infoElem "#-}"
  astPretty (InlineSig _ b mActivation qName) =
    blankline.resultPretty.(nestMode onsideIndent) $ constrElem InlineSig
      -- mySep
      <*> pure b <* (infoElem $ if b then "{-# INLINE" else "{-# NOINLINE")
      <*> traverse (\ x -> sepElem fsep *> (annNoInfoElem $ astPretty x)) mActivation
      <*  sepElem fsep
      <*> (annNoInfoElem $ astPretty qName)
      <*  sepElem hsep
      <*  infoElem "#-}"
  astPretty (InlineConlikeSig _ mActivation qName) =
    blankline.resultPretty.(nestMode onsideIndent) $ constrElem InlineConlikeSig
      -- mySep
      <* infoElem "{-# INLINE_CONLIKE"
      <*> traverse (\ x -> sepElem fsep *> (annNoInfoElem $ astPretty x)) mActivation
      <*  sepElem fsep
      <*> (annNoInfoElem $ astPretty qName)
      <*  infoElem "#-}"
  astPretty (SpecSig _ qName ts) =
    blankline.resultPretty.(nestMode onsideIndent) $ constrElem SpecSig
      -- mySep
      <*  infoElem "{-# SPECIALISE"
      <*  sepElem fsep
      <*> (annNoInfoElem $ astPretty qName)
      <*  sepElem fsep
      <*  infoElem "::"
      <*  sepElem fsep
      <*> intersperse (infoElem "," <* sepElem fsep) (annListElem annNoInfoElem ts)
      <*  sepElem fsep
      <*  infoElem "#-}"
  astPretty (SpecInlineSig _ b mActivation qName ts) =
    blankline.resultPretty.(nestMode onsideIndent) $ constrElem SpecInlineSig
      -- mySep
      <*  infoElem "{-# SPECIALISE"
      <*  sepElem fsep
      <*> pure b <* (infoElem $ if b then "INLINE" else "NOINLINE")
      <*> traverse (\ x -> sepElem fsep *> (annNoInfoElem $ astPretty x)) mActivation
      <*  sepElem fsep
      <*> (annNoInfoElem $ astPretty qName)
      <*  sepElem fsep
      <*  infoElem "::"
      <*  sepElem fsep
      <*> intersperse (infoElem "," <* sepElem fsep) (annListElem annNoInfoElem ts)
      <*  sepElem fsep
      <*  infoElem "#-}"
  astPretty (InstSig _ mContext ih ) =
    blankline.resultPretty.(nestMode onsideIndent) $ constrElem InstSig
      -- mySep
      <*  infoElem "{-# SPECIALISE"
      <*  sepElem fsep
      <*  infoElem "instance"
      <*> traverse (\ x -> sepElem fsep *> (annNoInfoElem $ astPretty x)) mContext
      <*  sepElem fsep
      <*> ppInstHeadInDecl ih
      <*  sepElem fsep
      <*  infoElem "#-}"
  astPretty (AnnPragma _ annotation) =
    blankline.resultPretty.(nestMode onsideIndent) $ constrElem AnnPragma
      -- mySep
      <*  infoElem "{-# ANN"
      <*  sepElem fsep
      <*> (annNoInfoElem $ astPretty annotation)
      <*  sepElem fsep
      <*  infoElem "#-}"

ppConstrList :: (Annotated ast, PrettyAst ast) => [ast a] -> AstElem [ast SrcSpanInfo]
ppConstrList [] = sequenceA []
ppConstrList cs = infoElem "=" *> sepElem hsep
  *> intersperse (sepElem hsep *> infoElem "|" *> sepElem myVcat) (annListElem annNoInfoElem cs)

ppFsepDhead :: DeclHead a -> AstElem (DeclHead SrcSpanInfo)
ppFsepDhead dh = annNoInfoElem.resultPretty $ constrElem DHead
  <*> pointsInfoElem (astPretty name)
  <*  sepElemIf (not $ null tvs) fsep
  <*> intersperse (sepElem fsep) (annListElem annNoInfoElem tvs)
  where
    (name, tvs) = sDeclHead dh

ppInstHeadInDecl :: InstHead a -> AstElem (InstHead SrcSpanInfo)
ppInstHeadInDecl ih = annNoInfoElem.resultPretty $ constrElem IHead
  <*> annNoInfoElem (astPretty qn)
  <*  sepElemIf (not $ null ts) fsep
  <*> intersperse (sepElem fsep) (map (annNoInfoElem.ppAType) ts)
  where
    (qn, ts) = sInstHead ih

-- --------------------------------------------------------------------------

ppWarnDepr ::  (Annotated ast, PrettyAst ast) => ([ast a], String) -> AstElem ([ast SrcSpanInfo], String)
ppWarnDepr ([], txt) = pure (,) <*> pure []  <*> infoElem txt
ppWarnDepr (ns, txt) = pure (,)
  <*> intersperse (infoElem "," <* sepElem fsep) (annListElem annNoInfoElem ns)
  <*  sep ns
  <*> infoElem txt
  where
    sep [n] = sepElem hsep
    sep _   = sepElem fsep

-- --------------------------------------------------------------------------

instance PrettyAst DeclHead where
  astPretty (DHead _ n tvs) =
    -- mySep
    resultPretty.(nestMode onsideIndent) $ constrElem DHead
      <*> pointsInfoElem (astPretty n)
      <*  sepElem fsep
      <*> intersperse (sepElem fsep) (annListElem annNoInfoElem tvs)
  astPretty (DHInfix _ tva n tvb) =
    -- mySep
    resultPretty.(nestMode onsideIndent) $ constrElem DHInfix
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
    resultPretty.(nestMode onsideIndent) $ constrElem IHead
      -- mySep
      <*> annNoInfoElem (astPretty qn)
      <*  sepElemIf (not $ null ts) fsep
      <*> intersperse (sepElem fsep) (annListElem annNoInfoElem ts)
  astPretty (IHInfix _ ta qn tb) =
    resultPretty.(nestMode onsideIndent) $ constrElem IHInfix
      -- mySep
      <*> annNoInfoElem (astPretty ta)
      <*  sepElem fsep
      <*> annNoInfoElem (astPretty qn)
      <*  sepElem fsep
      <*> annInfoElem (astPretty tb)
  astPretty (IHParen _ ih) =
    resultPretty.parens $ constrElem IHParen <*> (annNoInfoElem $ astPretty ih)

-- --------------------------------------------------------------------------

instance PrettyAst DataOrNew where
  astPretty (DataType _) = resultPretty $ constrElem DataType <* noInfoElem "data"
  astPretty (NewType  _) = resultPretty $ constrElem NewType  <* noInfoElem "newtype"

-- --------------------------------------------------------------------------

instance PrettyAst Assoc where
  astPretty (AssocNone  _) = resultPretty $ constrElem AssocNone  <* noInfoElem "infix"
  astPretty (AssocLeft  _) = resultPretty $ constrElem AssocLeft  <* noInfoElem "infixl"
  astPretty (AssocRight _) = resultPretty $ constrElem AssocRight <* noInfoElem "infixr"

-- --------------------------------------------------------------------------

instance PrettyAst Match where
  astPretty m =
    case m of
      (InfixMatch _ pa n pbs rhs mWhere) ->
        resultPretty.(nestMode onsideIndent) $ constrElem InfixMatch
          <*> annNoInfoElem (astPretty pa)
          <*  sepElem myFsep
          <*> annNoInfoElem (ppNameInfix n)
          <*  sepElem myFsep
          <*> ppPbs pbs
          <*> annNoInfoElem (astPretty rhs)
          <*> ppWhere mWhere
      (Match _ n pbs rhs mWhere) ->
        resultPretty $ constrElem Match
          <*> ppName n
          <*  sepElem myFsep
          <*> ppPbs pbs
          <*> annNoInfoElem (astPretty rhs)
          <*> ppWhere mWhere
    where
      ppName n@(Symbol _ _) = pointsInfoElem (ppNameInfix n)
      ppName n = annNoInfoElem (astPretty n)
      ppPbs ps = intersperse (sepElem myFsep) (map (annNoInfoElem.astPrettyPrec 2) ps)
        <*  sepElemIf (not $ null ps) myFsep

ppWhere :: Maybe (Binds a) -> AstElem (Maybe (Binds SrcSpanInfo))
ppWhere mWhere = traverse (\x -> (nestMode onsideIndent) $ sepElem myVcat *> infoElem "where" *> impl x) mWhere
  where
    impl (BDecls  _ []) = annNoInfoElem.resultPretty $ constrElem BDecls  <*  implicitElem "{" <*> pure [] <* implicitElem "}"
    impl (BDecls  _ l)  = annNoInfoElem.resultPretty $ constrElem BDecls  <*> ppBody whereIndent (annListElem annNoInfoElem l)
    impl (IPBinds _ b)  = annNoInfoElem.resultPretty $ constrElem IPBinds <*> ppBody whereIndent (annListElem annNoInfoElem b)

-- --------------------------------------------------------------------------

instance PrettyAst ClassDecl where
  astPretty (ClsDecl _ d) = resultPretty $ constrElem ClsDecl <*> (annInfoElem $ astPretty d)
  astPretty (ClsDataFam _ context dh mkind) =
    resultPretty.(nestMode onsideIndent) $ constrElem ClsDataFam
      -- mySep
      <*  infoElem "data"
      <*  sepElem fsep
      <*> traverse (\ c -> (annNoInfoElem $ astPretty c) <* sepElem fsep) context
      <*> ppFsepDhead dh
      <*  sepElemIf (isJust mkind) fsep
      <*> ppOptKind mkind
  astPretty (ClsTyFam _ dh mkind)  =
    resultPretty.(nestMode onsideIndent) $ constrElem ClsTyFam
      -- mySep
      <*  infoElem "type"
      <*  sepElem fsep
      <*> ppFsepDhead dh
      <*  sepElemIf (isJust mkind) fsep
      <*> ppOptKind mkind
  astPretty (ClsTyDef _ ntype htype) =
    resultPretty.(nestMode onsideIndent) $ constrElem ClsTyDef
      -- mySep
      <*> (annInfoElem $ astPretty ntype)
      <*  sepElem fsep
      <*  infoElem "="
      <*  sepElem fsep
      <*> (annInfoElem $ astPretty htype)

-- --------------------------------------------------------------------------

instance PrettyAst InstDecl where
  astPretty (InsDecl _ decl) = resultPretty $ constrElem InsDecl
    <*> pointsInfoElem (astPretty decl)
  astPretty (InsType _ ntype htype) =
    resultPretty.(nestMode onsideIndent) $ constrElem InsType
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
      <*> constrList' (annListElem annNoInfoElem constrList)
      <*> traverse (\x -> sepElem myVcat *> ppDeriving x) derives
      where
        onsideHead f = (nestMode onsideIndent) $ f <*> (annInfoElem $ astPretty don) <*  sepElem fsep <*> (annInfoElem $ astPretty ntype)
        cSep1 = infoElem "=" <* sepElem hsep
        cSep2 = sepElem myVcat <* infoElem "|" <* sepElem hsep
        constrList' (e1:e2:es) = sequenceA $ (e1 <* cSep1) : e2 : (map (cSep2 *>) es)
        constrList' es = sequenceA es
  astPretty (InsGData _ don ntype mkind gadtList derives) =
    resultPretty.(nestMode onsideIndent) $ onsideHead (constrElem InsGData)
      <*> ppBody classIndent (annListElem annNoInfoElem gadtList)
      <*> traverse (\x -> sepElem myVcat *> ppDeriving x) derives
    where
      onsideHead f = (nestMode onsideIndent) $ f
        -- mySep
        <*> (annInfoElem $ astPretty don)
        <*  sepElem fsep
        <*> (annInfoElem $ astPretty ntype)
        <*  sepElemIf (isJust mkind) fsep
        <*> ppOptKind mkind
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
    resultPretty.(nestMode onsideIndent) $ constrElem Rule
    -- mySep
      <*> infoElem tag
      <*> traverse (\x -> sepElem fsep *> (annInfoElem $ astPretty x)) activ
      <*> traverse (\x -> sepElem fsep *> ppRuleVars x) rvs
      <*  sepElem fsep
      <*> (annInfoElem $ astPretty rhs)
      <*  sepElem fsep
      <*  infoElem "="
      <*  sepElem fsep
      <*> (annInfoElem $ astPretty lhs)

ppRuleVars []  = pure []
ppRuleVars rvs = (nestMode onsideIndent) $ infoElem "forall" *> sepElem fsep *> intersperse (sepElem fsep) (annListElem annNoInfoElem rvs) <* infoElem "."

-- --------------------------------------------------------------------------

instance PrettyAst Activation where
  astPretty (ActiveFrom  _ i) = resultPretty $ constrElem ActiveFrom  <* infoElem "["  <*> pure i <* (infoElem $ show i) <* infoElem "]"
  astPretty (ActiveUntil _ i) = resultPretty $ constrElem ActiveUntil <* infoElem "[~" <*> pure i <* (infoElem $ show i) <* infoElem "]"

-- --------------------------------------------------------------------------

instance PrettyAst RuleVar where
    astPretty (RuleVar _ n) = resultPretty $ constrElem RuleVar <*> (annInfoElem $ astPretty n)
    astPretty (TypedRuleVar _ n t) =
      resultPretty.(nestMode onsideIndent).parens $ constrElem TypedRuleVar
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
    resultPretty.(nestMode onsideIndent) $ constrElem LanguagePragma
    -- myFsep
      <* infoElem "{-# LANGUAGE"
      <* sepElem myFsep
      <*> intersperse (infoElem "," <* sepElem myFsep) (annListElem annNoInfoElem ns)
      <* sepElem myFsep
      <* infoElem "#-}"
  astPretty (OptionsPragma _ mbTool s) = do
    -- myFsep
    let
      opt = "{-# OPTIONS" ++ case mbTool of
        Nothing -> " "
        Just (UnknownTool u) -> "_" ++ show u
        Just tool -> "_" ++ show tool
      in
      resultPretty.(nestMode onsideIndent) $ constrElem OptionsPragma
        <*> pure mbTool
        <*  infoElem opt
        <*> encloseIf (not $ null s) (sepElem myFsep ) (sepElem myFsep) (noInfoElem s)
        <*  infoElem "#-}"
  astPretty (AnnModulePragma _ ann) =
    resultPretty.(nestMode onsideIndent) $ constrElem AnnModulePragma
      -- myFsep
      <*   infoElem "{-# ANN"
      <*   sepElem myFsep
      <*>  (annNoInfoElem $ astPretty ann)
      <*   sepElem myFsep
      <*   infoElem "#-}"

-- --------------------------------------------------------------------------

instance PrettyAst Annotation where
  astPretty (Ann _ n e) =
    resultPretty.(nestMode onsideIndent) $ constrElem Ann
      -- myFsep
      <*> (annNoInfoElem $ astPretty n)
      <*  sepElem myFsep
      <*> (annNoInfoElem $ astPretty e)
  astPretty (TypeAnn _ n e) =
    resultPretty.(nestMode onsideIndent) $ constrElem TypeAnn
      -- myFsep
      <* infoElem "type"
      <* sepElem myFsep
      <*> (annNoInfoElem $ astPretty n)
      <* sepElem myFsep
      <*> (annNoInfoElem $ astPretty e)
  astPretty (ModuleAnn _ e) =
    resultPretty.(nestMode onsideIndent) $ constrElem ModuleAnn
      -- myFsep
      <* infoElem "module"
      <* sepElem myFsep
      <*> (annNoInfoElem $ astPretty e)

------------------------- Data & Newtype Bodies -------------------------

instance PrettyAst QualConDecl where
  astPretty (QualConDecl _ mtvs mContext con) =
    -- myFsep
    resultPretty.(nestMode onsideIndent) $ constrElem QualConDecl
      <*> traverse (\ x -> (ppForall $ annListElem annNoInfoElem x) <* sepElem myFsep) mtvs
      <*> traverse (\ c -> (annNoInfoElem $ astPretty c) <* sepElem fsep) mContext
      <*> pointsInfoElem (astPretty con)

-- --------------------------------------------------------------------------

instance PrettyAst GadtDecl where
  astPretty (GadtDecl _ name ty) =
    resultPretty $ constrElem GadtDecl
      <*> annNoInfoElem (astPretty name)
      <*  sepElem myFsep
      <*  infoElem "::"
      <*  sepElem myFsep
      <*> annNoInfoElem (astPretty ty)

-- --------------------------------------------------------------------------

instance PrettyAst ConDecl where
  astPretty (RecDecl _ name fieldList) =
    resultPretty $ constrElem RecDecl
      <*> annNoInfoElem (astPretty name)
      <*  sepElem hsep
      <*  infoElem "{"
      <*  sepElemIf (not $ null fieldList) hsep
      <*> intersperse parenListSep (annListElem annNoInfoElem fieldList)
      <*  sepElemIf (not $ null fieldList) hsep
      <*  infoElem "}"
  astPretty (ConDecl _ name typeList) =
    resultPretty.(nestMode onsideIndent) $ constrElem ConDecl
    -- mySep
      <*> annNoInfoElem (astPretty name)
      <*  sepElemIf (not $ null typeList) fsep
      <*> intersperse (sepElem fsep) (map (annNoInfoElem.astPrettyPrec prec_atype) typeList)
  astPretty (InfixConDecl _ l name r) =
    resultPretty.(nestMode onsideIndent) $ constrElem InfixConDecl
    -- myFsep
      <*> annInfoElem (astPrettyPrec prec_btype l)
      <*  sepElem myFsep
      <*> annNoInfoElem (ppNameInfix name)
      <*  sepElem myFsep
      <*> annInfoElem (astPrettyPrec prec_btype r)

-- --------------------------------------------------------------------------

instance PrettyAst FieldDecl where
  astPretty (FieldDecl _ names ty) =
    resultPretty $ constrElem FieldDecl
      <*> intersperse (noInfoElem "," <* sepElem myFsepSimple) (annListElem annNoInfoElem names)
      <*  sepElem myFsepSimple
      <*  infoElem "::"
      <*  sepElem myFsepSimple
      <*> annNoInfoElem (astPretty ty)

-- --------------------------------------------------------------------------

instance PrettyAst BangType where
  astPrettyPrec _ (BangedTy _ ty) =
    resultPretty $ constrElem BangedTy
      <*  infoElem "!"
      <*> (annInfoElem $ ppAType ty)
  astPrettyPrec p (UnBangedTy _ ty) =
    resultPretty $ constrElem UnBangedTy
      <*> pointsInfoElem (ppType p ty)
  astPrettyPrec p (UnpackedTy _ ty) =
    resultPretty $ constrElem UnpackedTy
      <*  infoElem "{-# UNPACK #-}"
      <*  sepElem hsep
      <*  infoElem "!"
      <*> (annInfoElem $ ppType p ty)

-- --------------------------------------------------------------------------

instance PrettyAst Deriving where
  astPretty (Deriving _ ihs) = case ihs of
    [] -> resultPretty $ dheads
      <*  infoElem "("
      <*> pure []
      <*  infoElem ")"
    [ih@(IHead _ d [])] -> resultPretty $ dheads <*> ilist
    _ -> resultPretty $ dheads
      <*  infoElem "("
      <*> ilist
      <*  infoElem ")"
    where
      dheads = constrElem Deriving <* infoElem "deriving" <* sepElem hsep
      ilist = intersperse parenListSep (annListElem annNoInfoElem ihs)

ppDeriving :: Deriving a -> AstElem (Deriving SrcSpanInfo)
ppDeriving d = annNoInfoElem (astPretty d)

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
  astPrettyPrec p (TyForall _ mtvs ctxt htype) =
    resultPretty.(nestMode onsideIndent) $
      parensIf (p > 0) $ constrElem TyForall
        -- myFsep
        <*> traverse (\ x -> (ppForall $ annListElem annNoInfoElem x) <* sepElem myFsep) mtvs
        <*> traverse (\ c -> (annNoInfoElem $ astPretty c) <* sepElem myFsep) ctxt
        <*> (annNoInfoElem $ astPretty htype)
  astPrettyPrec p (TyFun _ a b) = resultPretty $ (nestMode onsideIndent) t
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
        Boxed   -> parenList (annListElem annNoInfoElem l)
        Unboxed -> hashParenList (annListElem annNoInfoElem l)
  astPrettyPrec _ (TyList _ t)  = resultPretty $ constrElem TyList <*> t'
    where t' = enclose (infoElem "[") (infoElem "]") ((annNoInfoElem $ astPretty t))
  astPrettyPrec  p (TyApp _ a b) =
    resultPretty.(nestMode onsideIndent) $
      encloseIf (p > prec_btype) (noInfoElem "(") (noInfoElem ")")$
        constrElem TyApp
        <*> (annNoInfoElem $ astPretty a)
        <*  sepElem myFsep
        <*> (annNoInfoElem $ ppAType b)
  astPrettyPrec _ (TyVar _ t)  = resultPretty $ constrElem TyVar  <*> (annNoInfoElem $ astPretty t)
  astPrettyPrec _ (TyCon _ t)  = resultPretty $ constrElem TyCon <*> pointsInfoElem (astPretty t)
  astPrettyPrec _ (TyParen _ t)  = resultPretty $ constrElem TyParen <*> t'
    where t' = enclose (infoElem "(") (infoElem ")") ((annNoInfoElem $ astPretty t))
  astPrettyPrec _ (TyInfix _ a op b)  = resultPretty.(nestMode onsideIndent) $ constrElem TyInfix
    <*> (annNoInfoElem $ astPretty a)
    <* sepElem myFsep
    <*> annNoInfoElem (ppQNameInfix op)
    <* sepElem myFsep
    <*> (annNoInfoElem $ astPretty b)
  astPrettyPrec  _ (TyKind _ t k) = resultPretty $ (nestMode onsideIndent) t'
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
    resultPretty.(nestMode onsideIndent) $ constrElem KindedVar
      -- myFsep
      <*  infoElem "("
      <*> annNoInfoElem (astPretty var)
      <*  sepElem myFsep
      <*  infoElem "::"
      <*  sepElem myFsep
      <*> annNoInfoElem (astPretty kind)
      <*  infoElem ")"
  astPretty (UnkindedVar _ var) = resultPretty $ constrElem UnkindedVar <*> annNoInfoElem (astPretty var)

ppForall :: [AstElem a] -> AstElem [a]
ppForall [] = pure []
ppForall vs = (nestMode onsideIndent) $ infoElem "forall"
  *> sepElem myFsep
  *> intersperse (sepElem myFsep) vs
  <* sepElem myFsep
  <* infoElem "."

---------------------------- Kinds ----------------------------

instance PrettyAst Kind where
  astPrettyPrec _ (KindStar _) = resultPretty $ constrElem KindStar <* noInfoElem "*"
  astPrettyPrec _ (KindBang _) = resultPretty $ constrElem KindBang <* noInfoElem "!"
  astPrettyPrec n (KindFn _ a b)  =
    resultPretty.(nestMode onsideIndent).parensIf (n > 0) $ constrElem KindFn
      -- myFsep
      <*> annNoInfoElem (astPrettyPrec 1 a)
      <*  sepElem myFsep
      <*  infoElem "->"
      <*  sepElem myFsep
      <*> annNoInfoElem (astPretty b)
  astPrettyPrec _ (KindParen _ k) =
    resultPretty.parens $ constrElem KindParen <*> annNoInfoElem (astPretty k)
  astPrettyPrec _ (KindVar _ n) = resultPretty $ constrElem KindVar <*> annNoInfoElem (astPretty n)

ppOptKind :: Maybe (Kind a) -> AstElem (Maybe (Kind SrcSpanInfo))
ppOptKind k = traverse (\ x -> infoElem "::" *> sepElem fsep *> annNoInfoElem (astPretty x)) k

------------------- Functional Dependencies -------------------

instance PrettyAst FunDep where
  astPretty (FunDep _ from to) =
    resultPretty $ constrElem FunDep
      <*> intersperse (sepElem myFsep) (annListElem annNoInfoElem from)
      <*  sepElem myFsep
      <*  infoElem "->"
      <*> intersperse (sepElem myFsep) (annListElem annNoInfoElem to)

ppFunDeps :: (Annotated ast, PrettyAst ast) => [ast a] -> AstElem [ast SrcSpanInfo]
ppFunDeps []  = sequenceA []
ppFunDeps fds = (nestMode onsideIndent) $ infoElem "|" *> sepElem myFsep *> intersperse (infoElem "," <* sepElem myFsep) (annListElem annNoInfoElem fds)

------------------------- Expressions -------------------------

instance PrettyAst Rhs where
  astPretty (UnGuardedRhs _ e) =
    resultPretty $ constrElem UnGuardedRhs
      <*  infoElem "="
      <*  sepElem hsep
      <*> (annNoInfoElem $ astPretty e)
  astPretty (GuardedRhss _ guardList) =
    resultPretty $ constrElem GuardedRhss
      <*> intersperse (sepElem myVcat) (annListElem annNoInfoElem guardList)

-- --------------------------------------------------------------------------

instance PrettyAst GuardedRhs where
  astPretty (GuardedRhs _ guards ppBody) =
    resultPretty.(nestMode onsideIndent) $ constrElem GuardedRhs
    -- myFsep
      <*  infoElem "|"
      <*  sepElem myFsep
      <*> intersperse (infoElem "," <* sepElem myFsep) (annListElem annNoInfoElem guards)
      <*  sepElem myFsep
      <*  infoElem "="
      <*  sepElem myFsep
      <*> (annInfoElem $ astPretty ppBody)

-- --------------------------------------------------------------------------

instance PrettyAst Literal where
  astPretty (Int _ i s)  = resultPretty $ constrElem Int  <*> pure i <* (noInfoElem $ show i) <*> pure s
  astPretty (Char _ c s) = resultPretty $ constrElem Char <*> pure c <* (noInfoElem $ show c) <*> pure s
  astPretty (String _ s s') = resultPretty $ constrElem String <*> pure s <* (noInfoElem $ show s) <*> pure s'
  astPretty (Frac _ r s)    = resultPretty $ constrElem Frac <*> pure r <* (noInfoElem.show $ fromRational r) <*> pure s
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
  astPrettyPrec p (InfixApp _ a op b) =
    -- myFsep
    resultPretty.(nestMode onsideIndent).parensIf (p > 2) $
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
  astPrettyPrec p (App _ a b) =
    -- myFsep
    resultPretty.(nestMode onsideIndent).parensIf (p > 3) $
      constrElem App
        <*> annNoInfoElem (astPrettyPrec 3 a)
        <*  sepElem myFsep
        <*> annNoInfoElem (astPrettyPrec 4 b)
  astPrettyPrec p (Lambda _ patList body) =
    -- myFsep
    resultPretty.(nestMode onsideIndent).parensIf (p > 1) $
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
    -- myFsep
    resultPretty.(nestMode onsideIndent).parensIf (p > 1) $
      constrElem If
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
    -- myFsep
    resultPretty.parensIf (p > 1) $
        (
          (nestMode onsideIndent) $ constrElem Case
          <*  infoElem "case"
          <*  sepElem myFsep
          <*> (annNoInfoElem $ astPretty cond)
          <*  sepElem myFsep
          <*  infoElem "of"
        )
        <*> ppBody caseIndent (annListElem annNoInfoElem altList)
  astPrettyPrec p (Do _ stmtList) =
    resultPretty.parensIf (p > 1) $ constrElem Do
      <*  infoElem "do"
      <*> ppBody doIndent (annListElem annNoInfoElem stmtList)
  astPrettyPrec p (MDo _ stmtList) =
    resultPretty.parensIf (p > 1) $ constrElem MDo
      <*  infoElem "mdo"
      <*> ppBody doIndent (annListElem annNoInfoElem stmtList)
  -- Constructors & Vars
  astPrettyPrec _ (Var _ name) = resultPretty $ constrElem Var <*> (pointsInfoElem $ astPretty name)
  astPrettyPrec _ (IPVar _ ipname) = resultPretty $ constrElem IPVar <*> (annInfoElem $ astPretty ipname)
  astPrettyPrec _ (Con _ name) = resultPretty $ constrElem Con <*> (pointsInfoElem $ astPretty name)
  astPrettyPrec _ (Tuple _ expList) = resultPretty $ constrElem Tuple
    <*> parenList (annListElem annNoInfoElem expList)
  astPrettyPrec _ (TupleSection _ mExpList) =
    resultPretty $ constrElem TupleSection
      <*  infoElem "("
      <*> intersperse (infoElem "," <* sepElem myFsepSimple) (map (traverse $ annNoInfoElem.astPretty) mExpList)
      <*  infoElem ")"
  -- weird stuff
  astPrettyPrec _ (Paren _ e) = resultPretty $ constrElem Paren
    <*  infoElem "("
    <*> annNoInfoElem (astPretty e)
    <*  infoElem ")"
  astPrettyPrec _ (LeftSection _ e op) =
    resultPretty $ constrElem LeftSection
      <*  infoElem "("
      <*> annNoInfoElem (astPretty e)
      <*  sepElem hsep
      <*> annNoInfoElem (astPretty op)
      <*  infoElem ")"
  astPrettyPrec _ (RightSection _ op e) =
    resultPretty $ constrElem RightSection
      <*> (annInfoElem $ astPretty op)
      <*  sepElem hsep
      <*> (annInfoElem $ astPretty e)
  astPrettyPrec _ (RecConstr _ c fieldList) = resultPretty $ constrElem RecConstr
    <*> annNoInfoElem (astPretty c)
    <*  infoElem "{"
    <*  sepElemIf (not $ null fieldList) hsep
    <*> intersperse parenListSep (annListElem annNoInfoElem fieldList)
    <*  sepElemIf (not $ null fieldList) hsep
    <*  infoElem "}"
  astPrettyPrec _ (RecUpdate _ e fieldList) = resultPretty $ constrElem RecUpdate
    <*> (annInfoElem $ astPretty e) <*> braceList (annListElem annNoInfoElem fieldList)
  -- Lists
  astPrettyPrec _ (List _ list) =  resultPretty $ constrElem List
     <*  infoElem "["
     <*> intersperse (infoElem "," <* sepElem myFsepSimple) (annListElem annNoInfoElem list)
     <*  infoElem "]"
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
      <*> annNoInfoElem (astPretty from)
      <*  sepElem myFsepSimple
      <*  infoElem ".."
      <*  sepElem myFsepSimple
      <*> annNoInfoElem (astPretty to)
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
      <*> annNoInfoElem (astPretty e)
      <*  sepElem myFsepSimple
      <*  infoElem "|"
      <*  sepElem myFsepSimple
      <*> intersperse (infoElem "," <* sepElem myFsepSimple) (annListElem annNoInfoElem qualList)
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
        qList qs = intersperse qsSep (annListElem annNoInfoElem qs)
  astPrettyPrec p (ExpTypeSig _ e ty) =
    -- myFsep
    resultPretty.(nestMode onsideIndent).parensIf (p > 0) $
      constrElem ExpTypeSig
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
      <*  noInfoElem "["
      <*> noInfoElem n
      <*  noInfoElem "|"
      <*> noInfoElem qt
      <*  noInfoElem "|]"
  -- Hsx
  astPrettyPrec _ (XTag _ n attrs mattr cs) = unimplemented
  astPrettyPrec _ (XETag _ n attrs mattr) =
    resultPretty.(nestMode onsideIndent) $ constrElem XETag
      -- myFsep
      <*  infoElem "<"
      <*  sepElem myFsep
      <*> (annInfoElem $ astPretty n)
      <*  sepElem myFsep
      <*> intersperse (sepElem myFsep) (annListElem annNoInfoElem attrs)
      <*> traverse (\x -> sepElem myFsep *> (annInfoElem $ astPretty x)) mattr
      <*  sepElem myFsep
      <*  infoElem "/>"
  astPrettyPrec _ (XPcdata _ s) = resultPretty $ constrElem XPcdata <*> infoElem s
  astPrettyPrec _ (XExpTag _ e) =
    resultPretty.(nestMode onsideIndent) $ constrElem XExpTag
      -- myFsep
      <*  infoElem "<%"
      <*  sepElem myFsep
      <*> (annInfoElem $ astPretty e)
      <*  sepElem myFsep
      <*  infoElem "%>"
  astPrettyPrec _ (XChildTag _ cs) =
    resultPretty.(nestMode onsideIndent) $ constrElem XChildTag
      -- myFsep
      <*  infoElem "<%>"
      <*  sepElem myFsep
      <*> intersperse (sepElem myFsep) (annListElem annNoInfoElem cs)
      <*  sepElem myFsep
      <*  infoElem "</%>"
  -- Pragmas
  astPrettyPrec p (CorePragma _ s e) =
    resultPretty.(nestMode onsideIndent) $ constrElem CorePragma
      -- myFsep
      <*  noInfoElem "{-# CORE"
      <*  sepElem myFsep
      <*  noInfoElem "\""
      <*> infoElem s
      <*  noInfoElem "\""
      <*  sepElem myFsep
      <*  infoElem "#-}"
      <*  sepElem myFsep
      <*> (annInfoElem $ astPretty e)
  astPrettyPrec _ (SCCPragma  _ s e) =
    resultPretty.(nestMode onsideIndent) $ constrElem SCCPragma
      -- myFsep
      <*  noInfoElem "{-# SCC"
      <*  sepElem myFsep
      <*  noInfoElem "\""
      <*> infoElem s
      <*  noInfoElem "\""
      <*  sepElem myFsep
      <*  infoElem "#-}"
      <*  sepElem myFsep
      <*> (annNoInfoElem $ astPretty e)
  astPrettyPrec _ (GenPragma  _ s (a,b) (c,d) e) =
    resultPretty.(nestMode onsideIndent) $ constrElem GenPragma
      -- myFsep
      <*  noInfoElem "{-# GENERATED"
      <*  sepElem myFsep
      <*  noInfoElem "\""
      <*> infoElem s
      <*  noInfoElem "\""
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
    resultPretty.(nestMode onsideIndent).parensIf (p > 1) $
      constrElem Proc
        -- myFsep
        <*  infoElem "proc"
        <*  sepElem myFsep
        <*> (annInfoElem $ astPretty pat)
        <*  sepElem myFsep
        <*  infoElem "->"
        <*  sepElem myFsep
        <*> (annInfoElem $ astPretty e)
  astPrettyPrec p (LeftArrApp _ l r) =
    -- myFsep
    resultPretty.(nestMode onsideIndent).parensIf (p > 0) $
      constrElem LeftArrApp
        <*> (annInfoElem $ astPretty l)
        <*  sepElem myFsep
        <*  infoElem "-<"
        <*  sepElem myFsep
        <*> (annInfoElem $ astPretty r)
  astPrettyPrec p (RightArrApp _ l r) =
    -- myFsep
    resultPretty.(nestMode onsideIndent).parensIf (p > 0) $
      constrElem RightArrApp
        <*> (annInfoElem $ astPretty l)
        <*  sepElem myFsep
        <*  infoElem ">-"
        <*  sepElem myFsep
        <*> (annInfoElem $ astPretty r)
  astPrettyPrec p (LeftArrHighApp _ l r) =
    -- myFsep
    resultPretty.(nestMode onsideIndent).parensIf (p > 0) $
      constrElem LeftArrHighApp
        <*> (annInfoElem $ astPretty l)
        <*  sepElem myFsep
        <*  infoElem "-<<"
        <*  sepElem myFsep
        <*> (annInfoElem $ astPretty r)
  astPrettyPrec p (RightArrHighApp _ l r) =
    -- myFsep
    resultPretty.(nestMode onsideIndent).parensIf (p > 0) $
      constrElem RightArrHighApp
        <*> (annInfoElem $ astPretty l)
        <*  sepElem myFsep
        <*  infoElem ">>-"
        <*  sepElem myFsep
        <*> (annInfoElem $ astPretty r)

-- --------------------------------------------------------------------------

instance PrettyAst XAttr where
  astPretty (XAttr _ n v) =
    -- myFsep
    resultPretty.(nestMode onsideIndent) $ constrElem XAttr
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

ppLetExp f l b = (nestMode onsideIndent) $ constrElem Let
  -- myFsep
  <*  infoElem "let"
  <*  sepElem hsep
  <*> (constrElem f <*> ppBody letIndent (annListElem annNoInfoElem l))
  <*  sepElem myFsep
  <*  infoElem "in"
  <*> (annInfoElem $ astPretty b)


--------------------- Template Haskell -------------------------

instance PrettyAst Bracket where
  astPretty (ExpBracket _ e) =
    resultPretty $ constrElem ExpBracket <*> ppBracket "[|" ((annInfoElem $ astPretty e))
  astPretty (PatBracket _ p) =
    resultPretty $ constrElem PatBracket <*> ppBracket "[p|" ((annInfoElem $ astPretty p))
  astPretty (TypeBracket _ t) =
    resultPretty $ constrElem TypeBracket <*> ppBracket "[t|" ((annInfoElem $ astPretty t))
  astPretty (DeclBracket _ d) =
    -- myFsep
    resultPretty.(nestMode onsideIndent) $ constrElem DeclBracket <*> ppBracket "[d|" d'
      where d' = intersperse (sepElem myFsep) (annListElem annInfoElem d)

ppBracket :: String -> AstElem a -> AstElem a
ppBracket o x = (nestMode onsideIndent) $ infoElem o
  -- myFsep
  *> sepElem myFsep
  *> x
  <* sepElem myFsep
  <* infoElem "|]"

-- --------------------------------------------------------------------------

instance PrettyAst Splice where
  astPretty (IdSplice _ s) = resultPretty $ constrElem IdSplice <* infoElem "$" <*> infoElem s
  astPretty (ParenSplice _ e) =
    -- myFsep
    resultPretty.(nestMode onsideIndent) $ constrElem ParenSplice
      <* infoElem "$(" <* sepElem myFsep <*> (annInfoElem $ astPretty e) <* sepElem myFsep <* infoElem ")"

------------------------- Patterns -----------------------------

instance PrettyAst Pat where
  astPrettyPrec _ (PVar _ name) = resultPretty $ constrElem PVar <*> (annNoInfoElem $ astPretty name)
  astPrettyPrec _ (PLit _ lit) = resultPretty $ constrElem PLit <*> (annInfoElem $ astPretty lit)
  astPrettyPrec p (PNeg _ pat) =
    -- myFsep
    resultPretty.(nestMode onsideIndent).parensIf (p > 0) $
      constrElem PNeg
        <*  infoElem "-"
        <*  sepElem myFsep
        <*> (annInfoElem $ astPretty pat)
  astPrettyPrec p (PInfixApp _ a op b) =
    -- myFsep
    resultPretty.(nestMode onsideIndent).parensIf (p > 0) $
      constrElem PInfixApp
        <*> annNoInfoElem (astPrettyPrec 1 a)
        <*  sepElem myFsep
        <*> annNoInfoElem (ppQNameInfix op)
        <*  sepElem myFsep
        <*> annNoInfoElem (astPrettyPrec 1 b)
  astPrettyPrec p (PApp _ n ps) =
    -- myFsep
    resultPretty.(nestMode onsideIndent).parensIf (p > 1 && not (null ps)) $
      constrElem PApp
        <*> annNoInfoElem (astPretty n)
        <*  sepElem myFsep
        <*> intersperse (sepElem myFsep) (map (annNoInfoElem.astPrettyPrec 2) ps)
  astPrettyPrec _ (PTuple _ ps) = resultPretty $ constrElem PTuple <*> parenList (annListElem annNoInfoElem ps)
  astPrettyPrec _ (PList _ ps) =
    resultPretty $ constrElem PList
      <*  infoElem "["
      <*> intersperse parenListSep (annListElem annNoInfoElem ps)
      <*  infoElem "]"
  astPrettyPrec _ (PParen _ pat) =
    resultPretty $ constrElem PParen
      <*  infoElem "("
      <*> annNoInfoElem (astPretty pat)
      <*  infoElem ")"
  astPrettyPrec _ (PRec _ c fields) = resultPretty $ constrElem PRec
    <*> annNoInfoElem (astPretty c)
    <*> braceList (annListElem annNoInfoElem fields)
  -- special case that would otherwise be buggy
  astPrettyPrec _ (PAsPat _ name (PIrrPat _ pat)) =
    -- myFsep
    resultPretty.(nestMode onsideIndent) $
      constrElem PAsPat
        <*> annNoInfoElem (astPretty name)
        <*  infoElem "@"
        <*  sepElem myFsep
        <*  infoElem "~"
        <*> annNoInfoElem pat'
    where
      pat' = resultPretty $ constrElem PIrrPat <*> annNoInfoElem (astPrettyPrec 2 pat)
  astPrettyPrec _ (PAsPat _ name pat) =
    resultPretty $ constrElem PAsPat
    -- hcat
      <*> annNoInfoElem (astPretty name)
      <*  infoElem "@"
      <*> annNoInfoElem (astPrettyPrec 2 pat)
  astPrettyPrec _ (PWildCard _) = resultPretty$ constrElem PWildCard <* noInfoElem "_"
  astPrettyPrec _ (PIrrPat _ pat) =
    resultPretty $ constrElem PIrrPat
      <*  infoElem "~"
      <*> annInfoElem (astPrettyPrec 2 pat)
  astPrettyPrec p (PatTypeSig _ pat ty) =
    -- myFsep
    resultPretty.(nestMode onsideIndent).parensIf (p > 0) $
      constrElem PatTypeSig
        <*> (annInfoElem $ astPretty pat)
        <*  sepElem myFsep
        <*  infoElem "::"
        <*  sepElem myFsep
        <*> (annInfoElem $ astPretty ty)
  astPrettyPrec p (PViewPat _ e pat) =
    -- myFsep
    resultPretty.(nestMode onsideIndent).parensIf (p > 0) $
      constrElem PViewPat
        <*> (annInfoElem $ astPretty e)
        <*  sepElem myFsep
        <*  infoElem "->"
        <*  sepElem myFsep
        <*> (annInfoElem $ astPretty pat)
  astPrettyPrec p (PNPlusK _ n k) =
    -- myFsep
    resultPretty.(nestMode onsideIndent).parensIf (p > 0) $
      constrElem PNPlusK
        <*> annNoInfoElem (astPretty n)
        <*  sepElem myFsep
        <*  infoElem "+"
        <*  sepElem myFsep
        <*> pure k <* infoElem (show k)
  -- HaRP
  astPrettyPrec _ (PRPat _ rs) = resultPretty $ constrElem PRPat <*> bracketList (annListElem annNoInfoElem rs)
  astPrettyPrec _ (PXTag _ n attrs mattr cp) = unimplemented
  astPrettyPrec _ (PXETag _ n attrs mattr) =
    resultPretty.(nestMode onsideIndent) $ constrElem PXETag
    -- myFsep
      <*  infoElem "<"
      <*> (annInfoElem $ astPretty n)
      <*  sepElem myFsep
      <*> intersperse (sepElem myFsep) (map (annInfoElem.astPretty) attrs)
      <*> traverse (\x -> sepElem myFsep *> (annInfoElem $ astPretty x)) mattr
      <*  sepElem myFsep
      <*  infoElem "/>"
  astPrettyPrec _ (PXPcdata _ s) = resultPretty $ constrElem PXPcdata <*> infoElem s
  astPrettyPrec _ (PXPatTag _ p) =
    resultPretty.(nestMode onsideIndent) $ constrElem PXPatTag
      -- myFsep
      <*  infoElem "<%"
      <*  sepElem myFsep
      <*> (annInfoElem $ astPretty p)
      <*  sepElem myFsep
      <*  infoElem "%>"
  astPrettyPrec _ (PXRPats _ ps) =
    resultPretty.(nestMode onsideIndent) $ constrElem PXRPats
      -- myFsep
      <*  infoElem "<["
      <*  sepElem myFsep
      <*> intersperse (sepElem myFsep) (map (annInfoElem.astPretty) ps)
      <*  sepElem myFsep
      <*  infoElem "]>"
  -- Generics
  astPrettyPrec _ (PExplTypeArg _ qn t) =
    -- myFsep
    resultPretty.(nestMode onsideIndent) $ constrElem PExplTypeArg
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
    -- myFsep
    resultPretty.(nestMode onsideIndent) $ constrElem PXAttr
      <*> (annInfoElem $ astPretty n)
      <*  sepElem myFsep
      <*  infoElem "="
      <*  sepElem myFsep
      <*> (annInfoElem $ astPretty p)

-- --------------------------------------------------------------------------

instance PrettyAst PatField where
  astPretty (PFieldPat _ n p) =
    -- myFsep
    resultPretty.(nestMode onsideIndent) $ constrElem PFieldPat
      <*> (annInfoElem $ astPretty n)
      <*  sepElem myFsep
      <*  infoElem "="
      <*  sepElem myFsep
      <*> (annInfoElem $ astPretty p)
  astPretty (PFieldPun _ name) = resultPretty $ constrElem PFieldPun <*> annNoInfoElem (astPretty name)
  astPretty (PFieldWildcard _) = resultPretty $ constrElem PFieldWildcard <* noInfoElem ".."

--------------------- Regular Patterns -------------------------

instance PrettyAst RPat where
  astPretty (RPOp _ r op) = resultPretty $ constrElem RPOp <*> (annInfoElem $ astPretty r) <*> (annInfoElem $ astPretty op)
  astPretty (RPEither _ r1 r2) =
    resultPretty.(nestMode onsideIndent).parens $ constrElem RPEither
    -- myFsep
      <*> (annInfoElem $ astPretty r1)
      <*  sepElem myFsep
      <*  infoElem "|"
      <*  sepElem myFsep
      <*> (annInfoElem $ astPretty r2)
  astPretty (RPSeq _ rs) =
    resultPretty.(nestMode onsideIndent).parens $ constrElem RPSeq
    -- myFsep
      <*  infoElem "(/"
      <*  sepElem myFsep
      <*> intersperse (sepElem myFsep) (annListElem annNoInfoElem rs)
      <*  sepElem myFsep
      <*  infoElem "/)"
  astPretty (RPGuard _ r gs) =
    resultPretty.(nestMode onsideIndent).parens $ constrElem RPGuard
    -- myFsep
      <*  infoElem "(|"
      <*  sepElem myFsep
      <*> (annInfoElem $ astPretty r)
      <*  sepElem myFsep
      <*> intersperse (sepElem myFsep) (annListElem annNoInfoElem gs)
      <*  sepElem myFsep
      <*  infoElem "/)"
   -- special case that would otherwise be buggy
  astPretty (RPAs _ n (RPPat _ (PIrrPat _ p))) =
    let
      ip = annInfoElem.resultPretty $ constrElem PIrrPat <*> (annInfoElem $ astPretty p)
      rp = annInfoElem.resultPretty $ constrElem RPPat   <*> ip
    in
      resultPretty.(nestMode onsideIndent) $ constrElem RPAs
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
  astPretty (Alt _ e gAlts mBinds) =
    resultPretty $ constrElem Alt
      <*> (annNoInfoElem $ astPretty e)
      <*  sepElem hsep
      <*> (annNoInfoElem $ astPretty gAlts)
      <*> ppWhere mBinds

-- --------------------------------------------------------------------------

instance PrettyAst GuardedAlts where
  astPretty (UnGuardedAlt _ e) = resultPretty $ constrElem UnGuardedAlt
    <* infoElem "->" <* sepElem hsep <*> (annNoInfoElem $ astPretty e)
  astPretty (GuardedAlts _ altList) = resultPretty $ constrElem GuardedAlts
    <*> intersperse (sepElem myVcat) (annListElem annNoInfoElem altList)

-- --------------------------------------------------------------------------

instance PrettyAst GuardedAlt where
  astPretty (GuardedAlt _ guards body) =
    resultPretty.(nestMode onsideIndent) $ constrElem GuardedAlt
    -- myFsep
      <*  infoElem "|"
      <*  sepElem myFsep
      <*> intersperse (infoElem "," <* sepElem myFsep) (annListElem annNoInfoElem guards)
      <*  sepElem myFsep
      <*  infoElem "->"
      <*  sepElem myFsep
      <*> (annInfoElem $ astPretty body)

------------------------- Statements in monads, guards & list comprehensions -----

instance PrettyAst Stmt where
  astPretty (Generator _ e from) = resultPretty $ constrElem Generator
    <*> annNoInfoElem (astPretty e)
    <*  sepElem hsep
    <*  infoElem "<-"
    <*  sepElem hsep
    <*> annNoInfoElem (astPretty from)
  -- ListComp1.hs - Qualifier has empty info points
  astPretty (Qualifier _ e) = resultPretty $ constrElem Qualifier <*> annNoInfoElem (astPretty e)
  astPretty (LetStmt _ (BDecls _ declList)) =
    resultPretty $ constrElem LetStmt <*> ppLetStmt (constrElem BDecls) declList
  astPretty (LetStmt _ (IPBinds _ bindList)) =
    resultPretty $ constrElem LetStmt <*> ppLetStmt (constrElem IPBinds) bindList
  astPretty (RecStmt _ stmtList) =
    resultPretty $ constrElem RecStmt
      <*  infoElem "rec"
      <*> ppBody letIndent (annListElem annNoInfoElem stmtList)

ppLetStmt f ls = f <* infoElem "let" <*> ppBody letIndent (annListElem annInfoElem ls)

-- --------------------------------------------------------------------------

instance PrettyAst QualStmt where
  astPretty (QualStmt _ s) = resultPretty $ constrElem QualStmt <*> annNoInfoElem (astPretty s)
  astPretty (ThenTrans _ f) =
    -- myFsep
    resultPretty.(nestMode onsideIndent) $ constrElem ThenTrans <* infoElem "then" <* sepElem myFsep <*> annNoInfoElem (astPretty f)
  astPretty (ThenBy _ f e) =
    -- myFsep
    resultPretty.(nestMode onsideIndent) $ constrElem ThenBy
      <* infoElem "then"
      <* sepElem myFsep
      <*> annNoInfoElem (astPretty f)
      <* sepElem myFsep
      <* infoElem "by"
      <* sepElem myFsep
      <*> annNoInfoElem (astPretty e)
  astPretty (GroupBy _ e) =
    -- myFsep
    resultPretty.(nestMode onsideIndent) $ constrElem GroupBy
      <* infoElem "then"
      <* sepElem myFsep
      <* infoElem "group"
      <* sepElem myFsep
      <* infoElem "by"
      <* sepElem myFsep
      <*> annNoInfoElem (astPretty e)
  astPretty (GroupUsing   _ f) =
    -- myFsep
    resultPretty.(nestMode onsideIndent) $ constrElem GroupUsing
      <* infoElem "then"
      <* sepElem myFsep
      <* infoElem "group"
      <* sepElem myFsep
      <* infoElem "using"
      <* sepElem myFsep
      <*> annNoInfoElem (astPretty f)
  astPretty (GroupByUsing _ e f) =
    -- myFsep
    resultPretty.(nestMode onsideIndent) $ constrElem GroupByUsing
      <* infoElem "then"
      <* sepElem myFsep
      <* infoElem "group"
      <* sepElem myFsep
      <* infoElem "by"
      <* sepElem myFsep
      <*> annNoInfoElem (astPretty e)
      <* sepElem myFsep
      <* infoElem "using"
      <* sepElem myFsep
      <*> annNoInfoElem (astPretty f)

------------------------- Record updates

instance PrettyAst FieldUpdate where
  astPretty (FieldUpdate _ name e) =
    -- myFsep
    resultPretty.(nestMode onsideIndent) $ constrElem FieldUpdate
      <*> annNoInfoElem (astPretty name)
      <* sepElem myFsep
      <* infoElem "="
      <* sepElem myFsep
      <*> annNoInfoElem (astPretty e)
  astPretty (FieldPun _ name) = resultPretty $ constrElem FieldPun <*> (annInfoElem $ astPretty name)
  astPretty (FieldWildcard _) = resultPretty $ constrElem FieldWildcard <* infoElem ".."

------------------------- Names -------------------------

instance PrettyAst QOp where
  astPretty (QVarOp _ n) = resultPretty $ constrElem QVarOp <*> pointsInfoElem (ppQNameInfix n)
  astPretty (QConOp _ n) = resultPretty $ constrElem QConOp <*> pointsInfoElem (ppQNameInfix n)

-- --------------------------------------------------------------------------

instance PrettyAst SpecialCon where
  astPretty (UnitCon _) = resultPretty $ constrElem UnitCon <* infoElem "(" <* infoElem ")"
  astPretty (ListCon _) = resultPretty $ constrElem ListCon <* infoElem "[" <* infoElem "]"
  astPretty (FunCon _) = resultPretty $ constrElem FunCon  <* infoElem "->"
  astPretty (TupleCon _ b n) =
    let
      hash = if b == Unboxed then "#" else ""
      point = "(" ++ hash ++ replicate (n-1) ',' ++ hash ++ ")" in
    resultPretty $ constrElem TupleCon
      <* infoElem point
      <*> pure b
      <*> pure n
  astPretty (Cons _) = resultPretty $ constrElem Cons <* noInfoElem ":"
  astPretty (UnboxedSingleCon _) = resultPretty $ constrElem UnboxedSingleCon <* infoElem "(# #)"

-- --------------------------------------------------------------------------

instance PrettyAst QName where
  astPretty (Qual _ (ModuleName _ mn) (Symbol _ s))  = do
        op  <- format "("
        ss  <- getPos
        mn' <- format mn
        p   <- format "."
        s'  <- format s
        es  <- getPos
        cp  <- format ")"
        let si = SrcSpanInfo (mergeSrcSpan op cp) [op, (mkSrcSpan ss es), cp]
        return $ Qual si (ModuleName si mn) (Symbol si s)
  astPretty (Qual _ mn n)  = resultPretty $ constrElem Qual
    <*> annNoInfoElem (astPretty mn)
    <*  infoElem "."
    <*> pointsInfoElem (astPretty n)

  astPretty (UnQual _ n)   = resultPretty $ constrElem UnQual  <*> pointsInfoElem (astPretty n)
  astPretty (Special _ sc) = resultPretty $ constrElem Special <*> pointsInfoElem (astPretty sc)

-- --------------------------------------------------------------------------
-- QName utils

ppQNameInfix :: QName a -> DocM (QName SrcSpanInfo)
ppQNameInfix (Qual _ mn n)  =
  resultPretty $ constrElem Qual
    <*> annNoInfoElem (astPretty mn)
    <*  infoElem "."
    <*> pointsInfoElem (astPretty n)
ppQNameInfix (UnQual _ n)   = resultPretty $ constrElem UnQual <*> pointsInfoElem (ppNameInfix n)
ppQNameInfix (Special _ sc) = resultPretty $ constrElem Special <*> (pointsInfoElem $ astPretty sc)

-- --------------------------------------------------------------------------

instance PrettyAst Op where
  astPretty (VarOp _ n) = resultPretty $ constrElem VarOp <*> pointsInfoElem (ppNameInfix n)
  astPretty (ConOp _ n) = resultPretty $ constrElem ConOp <*> pointsInfoElem (ppNameInfix n)

-- --------------------------------------------------------------------------

ppNameInfix :: Name a -> DocM (Name SrcSpanInfo)
ppNameInfix (Symbol _ s) = resultPretty $ constrElem Symbol <*> noInfoElem s
ppNameInfix (Ident  _ s) = resultPretty $ constrElem Ident
  <*  infoElem "'"
  <*> infoElem s
  <*  infoElem "'"

instance PrettyAst Name where
  astPretty (Ident  _ s) = resultPretty $ constrElem Ident  <*> noInfoElem s
  astPretty (Symbol _ s) = resultPretty $ constrElem Symbol
    <*  infoElem "("
    <*> infoElem s
    <*  infoElem ")"

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

-- --------------------------------------------------------------------------

instance PrettyAst IPName where
  astPretty (IPDup _ s) = resultPretty $ constrElem IPDup
    <* infoElem "?" <*> infoElem s
  astPretty (IPLin _ s) = resultPretty $ constrElem IPLin
    <* infoElem "%" <*> infoElem s

-- --------------------------------------------------------------------------

instance PrettyAst IPBind where
  astPretty (IPBind _ ipname exp) =
    -- myFsep
    resultPretty.(nestMode onsideIndent) $ constrElem IPBind
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
  astPretty (CxEmpty _) =
    resultPretty.(nestMode onsideIndent) $ constrElem CxEmpty
      <* infoElem "("
      <* infoElem ")"
      <* sepElem fsep
      <* infoElem "=>"
  astPretty (CxSingle _ asst) =
    resultPretty.(nestMode onsideIndent) $ constrElem CxSingle
      <*> annNoInfoElem (astPretty asst)
      <*  sepElem fsep
      <*  infoElem "=>"
  astPretty (CxTuple _ assts) =
    resultPretty.(nestMode onsideIndent) $ constrElem CxTuple
      <*> parenList (annListElem annNoInfoElem assts) -- myFsep and parenList -> myFsep and myFsepSimple ???
      <* sepElem myFsep
      <* infoElem "=>"
  astPretty (CxParen _ ctxt) = resultPretty $ constrElem CxParen
    <*  infoElem "("
    <*> annNoInfoElem (parenContext ctxt)
    <*  infoElem ")"
    <* sepElem myFsep
    <* infoElem "=>"
    where
      parenContext (CxEmpty _) =
        resultPretty.(nestMode onsideIndent) $ constrElem CxEmpty
      parenContext (CxSingle _ asst) =
        resultPretty.(nestMode onsideIndent) $ constrElem CxSingle
          <*> annNoInfoElem (astPretty asst)
      parenContext (CxTuple _ assts) =
        resultPretty.(nestMode onsideIndent) $ constrElem CxTuple
          <*> intersperse parenListSep (annListElem annNoInfoElem assts)

-- --------------------------------------------------------------------------
-- hacked for multi-parameter type classes
instance PrettyAst Asst where
  astPretty (ClassA _ a ts)   =
    -- myFsep
    resultPretty.(nestMode onsideIndent) $ constrElem ClassA
      <*> (annNoInfoElem $ astPretty a)
      <*  sepElem myFsep
      <*> intersperse (sepElem myFsep) (map (annNoInfoElem.ppAType) ts)
  astPretty (InfixA _ a op b) =
    -- myFsep
    resultPretty.(nestMode onsideIndent) $ constrElem InfixA
      <*> (annNoInfoElem $ astPretty a)
      <*   sepElem myFsep
      <*> annNoInfoElem (ppQNameInfix op)
      <*   sepElem myFsep
      <*> (annNoInfoElem $ astPretty b)
  astPretty (IParam _ i t) =
    -- myFsep
    resultPretty.(nestMode onsideIndent) $ constrElem IParam
      <*> (annInfoElem $ astPretty i)
      <*  sepElem myFsep
      <*  infoElem "::"
      <*> (annInfoElem $ astPretty t)
  astPretty (EqualP _ t1 t2) =
    -- myFsep
    resultPretty.(nestMode onsideIndent) $ constrElem EqualP
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
  DocState _ n t <- get
  put $! DocState l n t
  return l

line :: MonadState DocState m => m ()
line = do
  DocState (SrcLoc f l c) n _ <- get
  putPos $! SrcLoc f (l + 1) (n + 1)
  return ()

traceAst :: MonadState DocState m => String -> m ()
traceAst s = do
  DocState l n t <- get
  let msg = "l = " ++ show (srcLine l) ++ " c = " ++ show (srcColumn l) ++ " n = " ++ show n ++ " : " ++ s
  put $! DocState l n (t ++ [msg])
  return ()

space :: MonadState DocState m => Int -> m ()
space x = do
  SrcLoc f l c <- getPos
  putPos $! SrcLoc f l $! c + x
  return ()

-- --------------------------------------------------------------------------
-- AstElem definition

data AstElemInfo = AstElemInfo {
  startPos     :: Maybe SrcLoc,
  infoPoints   :: [SrcSpan]
} deriving Show

instance Monoid AstElemInfo where
   mempty  = AstElemInfo Nothing []
   mappend (AstElemInfo x xs) (AstElemInfo y ys) = AstElemInfo (startPosCalc x y) ps
    where
      startPosCalc Nothing p = p
      startPosCalc p _ = p
      ps = xs ++ ys


type AstElem = WriterT AstElemInfo DocM


-- --------------------------------------------------------------------------
-- AstElem utils

spanFromString :: String -> DocM SrcSpan
spanFromString s = do
  sp <- getPos
  _  <- format s
  ep <- getPos
  return $ mkSrcSpan sp ep

stringElem ::(SrcSpanInfo -> [SrcSpan]) -> String ->  AstElem String
stringElem f s = do
  start <- getPos
  span  <- lift $ spanFromString s
  tell $ AstElemInfo (Just start) (f $ SrcSpanInfo span [span])
  return s

noPoints, mainPoint, allPoints :: SrcSpanInfo -> [SrcSpan]
noPoints  _ = []
mainPoint (SrcSpanInfo s _)  = [s]
allPoints (SrcSpanInfo _ ps) = ps

annElem :: (Annotated ast) => (SrcSpanInfo -> [SrcSpan]) -> (SrcSpanInfo -> SrcSpanInfo) -> DocM (ast SrcSpanInfo) -> AstElem (ast SrcSpanInfo)
annElem pointFn spanFn el = do
  e <- lift el
  let
    span = ann e
    (SrcSpan fl ln cl _ _) = srcInfoSpan span

  tell $ AstElemInfo (Just $ SrcLoc fl ln cl) (pointFn span)
  return $ amap spanFn e

infoElem :: String -> AstElem String
infoElem s = stringElem mainPoint s

noInfoElem :: String -> AstElem String
noInfoElem s = stringElem noPoints s

implicitElem :: String -> AstElem String
implicitElem s = stringElem mainPoint "" >> return s

sepElem :: DocM() -> AstElem ()
sepElem s = lift s

sepElemIf :: Bool -> DocM() -> AstElem()
sepElemIf p s = sepElem $ if p then s else pure ()

annNoInfoElem, annInfoElem, pointsInfoElem :: (Annotated ast) => DocM (ast SrcSpanInfo) -> AstElem (ast SrcSpanInfo)

annNoInfoElem a = annElem noPoints id a

annInfoElem a = annElem mainPoint id a

pointsInfoElem a = annElem allPoints id a

takeAllPointsInfoElem a = annElem allPoints (noInfoSpan . srcInfoSpan) a

annStub = undefined

unimplemented = undefined

constrElem :: (a -> b) -> AstElem b
constrElem f = lift.return $ f annStub

annListElem :: PrettyAst ast => (DocM (ast SrcSpanInfo) -> AstElem (ast SrcSpanInfo)) -> [ast a] -> [AstElem (ast SrcSpanInfo)]
annListElem f xs = map (f.astPretty) xs

intersperse :: Applicative f => f a1 -> [f a] -> f [a]
intersperse _ [] = pure []
intersperse sep (e:es) = sequenceA $ e : (map (sep *>) es)

nest :: Int -> AstElem a -> AstElem a
nest n a = (sepElem $ impl n) *> a <* (sepElem $ impl (-n))
  where
    impl x = do
      DocState l n t <- get
      put $! DocState l (n + x) t
      return ()

nestMode f a = do
  PrettyMode m _  <- ask
  nest (f m) a

resultPretty :: Annotated ast => AstElem (ast SrcSpanInfo) -> DocM (ast SrcSpanInfo)
resultPretty a = do
  (a', AstElemInfo (Just start) ps) <- runWriterT a
  end <- getPos
  let span = SrcSpanInfo (mkSrcSpan start end) ps
  return $ amap (const span) a'

-- --------------------------------------------------------------------------
-- separators

vcat :: DocM ()
vcat = do
  DocState (SrcLoc f l c) n _ <- get
  let s = if n < c then line else space $ n - c + 1
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
    if srcColumn c >= lineLength style then line else hsep
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

encloseIf p ob cb x = if p then ob *> x <* cb else x

parens :: AstElem a -> AstElem a
parens d = noInfoElem "(" *> d <* noInfoElem ")"

braces :: AstElem a -> AstElem a
braces d = noInfoElem "{" *> d <* noInfoElem "}"

brackets :: AstElem a -> AstElem a
brackets d = noInfoElem "[" *> d <* noInfoElem "]"

parensIf :: Bool -> AstElem a -> AstElem a
parensIf p d = if p then parens d else d

-- --------------------------------------------------------------------------
-- general utils

layoutChoice a b  = do
  PrettyMode mode _ <- ask
  if layout mode == PPOffsideRule || layout mode == PPSemiColon
  then a
  else b

layoutSep :: String -> AstElem String
layoutSep s =  do
  (PrettyMode mode _) <- ask
  case layout mode of
    PPOffsideRule -> implicitElem s
    PPSemiColon   -> infoElem s
    _ -> implicitElem s

blankline :: Annotated ast => DocM (ast SrcSpanInfo) -> DocM (ast SrcSpanInfo)
blankline x = newLine >> x
  where
    newLine = do
      PrettyMode mode _ <- ask
      if spacing mode && layout mode /= PPNoLayout
        then line
        else return ()

ppBody :: (PR.PPHsMode -> Int) -> [AstElem a] -> AstElem [a]
ppBody _ [] = implicitElem "{" *> pure [] <* implicitElem "}"
ppBody f dl =  do
  (PrettyMode mode _) <- ask
  nest (f mode) $
    case layout mode of
      PPOffsideRule ->
           sepElem vcat
        *> implicitElem "{ - just begin of body"
        *> intersperse (implicitElem ";" <* sepElem vcat) dl
        <* implicitElem "}"
      PPSemiColon   ->
           sepElem vcat
        *> infoElem "{"
        *> sepElem hsep
        *> nestMode onsideIndent (intersperse (infoElem ";" <* sepElem vcat) dl)
        <* infoElem "}"
      _ -> sepElem hsep
        *> infoElem "{"
        *> sepElem hsep
        *> intersperse (infoElem ";" <* sepElem hsep) dl
        <* infoElem "}"

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
