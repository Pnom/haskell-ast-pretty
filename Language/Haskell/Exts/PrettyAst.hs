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

defDocState = DocState (SrcLoc "unknown.hs"  1  1) 0

data PrettyMode = PrettyMode PR.PPHsMode PR.Style
defPrettyMode = PrettyMode PR.defaultMode PR.style

type DocM = ReaderT PrettyMode (State DocState)

-- | render the document with a given mode.
renderWithMode :: PrettyMode -> DocM a -> a
renderWithMode mode doc = evalState (runReaderT doc mode) defDocState

-- | render the document with 'defaultMode'.
renderWithDefMode :: DocM a -> a
renderWithDefMode a = renderWithMode defPrettyMode a

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
      <*> traverseSep (sepElem myVcat) prettyNoInfoElem mbHead
      <*> prettyLs imp
      <*  sepElem myVcat
      <*> prettyLs decls
      where
        impl os h i d = Module undefined h os i d
        vcatList dl = intersperse (sepElem myVcat) $ noInfoList dl
        prettyLs dl = (if isJust mbHead then topLevel else vcatList) dl
  astPretty (XmlPage _ _mn os n attrs mattr cs) = undefined
  astPretty (XmlHybrid _ mbHead os imp decls n attrs mattr cs) = undefined

--------------------------  Module Header ------------------------------

instance PrettyAst ModuleHead where
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

instance PrettyAst WarningText where
    astPretty w = case w of
      (DeprText _ s) -> impl DeprText "{-# DEPRECATED" s
      (WarnText _ s) -> impl WarnText "{-# WARNING"    s
      where
        -- mySep
      impl f c s = resultPretty $ constrElem f <* infoElem c <* sepElem hsep <*> infoElem s <* sepElem fsep <* infoElem "#}"

-- --------------------------------------------------------------------------

instance PrettyAst ModuleName where
  astPretty (ModuleName _ s) = resultPretty $ constrElem ModuleName <*> infoElem s

-- --------------------------------------------------------------------------

instance PrettyAst ExportSpecList where
  astPretty (ExportSpecList _ especs) =
    resultPretty $ constrElem ExportSpecList <*> parenList (noInfoList especs)

-- --------------------------------------------------------------------------

instance PrettyAst ExportSpec where
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

instance PrettyAst ImportDecl where
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

-- --------------------------------------------------------------------------

instance PrettyAst ImportSpecList where
  astPretty (ImportSpecList _ b ispecs) =
    resultPretty $ constrElem ImportSpecList
      <*> pure b <* (infoElem $ if b then "hiding" else "")
      <*  sepElem hsep
      <*> parenList (noInfoList ispecs)

-- --------------------------------------------------------------------------

instance PrettyAst ImportSpec where
  astPretty (IVar _ name)                = resultPretty $ constrElem IVar <*> prettyNoInfoElem name
  astPretty (IAbs _ name)                = resultPretty $ constrElem IAbs <*> prettyNoInfoElem name
  astPretty (IThingAll _ name)           =
    resultPretty $ constrElem IThingAll <*> prettyNoInfoElem name <* infoElem "(..)"
  astPretty (IThingWith _ name nameList) =
    resultPretty $ constrElem IThingWith <*> prettyNoInfoElem name <*> parenList (noInfoList nameList)

identDeriving :: Deriving a -> AstElem (Deriving SrcSpanInfo)
identDeriving d = do
  xs <- ppBody letIndent [ppDeriving d]
  return $ head xs

identDeriving' :: Deriving a -> AstElem (Deriving SrcSpanInfo)
identDeriving' d = ppBody letIndent [ppDeriving d] >>= return.head

-------------------------  Declarations ------------------------------

instance PrettyAst Decl where
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
      <*> ppFsepDhead head
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
    <*> ppFsepDhead head
    <*  sepElem hsep
    <*> ppConstrList constrList
    -- '$$$' is the same as myVcat
    <*  sepElem myVcat
    <*> traverse ppDeriving mDeriving
  astPretty (GDataDecl _ don mContext hd mkind gadtDecl mDeriving) =
    resultPretty $ constrElem GDataDecl
      <* blankline
      -- mySep
      <*> prettyInfoElem don
      <*  sepElem hsep
      <*> ppContext mContext
      <*  sepElem fsep
      <*> ppFsepDhead hd
      <*  sepElem fsep
      <*> ppOptKind mkind
      <*  sepElem fsep
      <*  infoElem "where"
      -- '$$$' is the same as myVcat
      <*  sepElem myVcat
      <*> ppBody classIndent (noInfoList gadtDecl)
      <*  sepElem myVcat
      <*> traverse identDeriving mDeriving
      where
        identDeriving d = ppBody letIndent [ppDeriving d] >>= return.head
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
      <*> ppFsepDhead head
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
      -- '$$$' is the same as myVcat
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
      -- '$$$' is the same as myVcat
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
      <*> ppFsepDhead head
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
      sep = do
        PrettyMode mode _ <- ask
        if layout mode == PPOffsideRule then (infoElem "" <* sepElem myVcat) else infoElem ";"

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

ppConstrList :: PrettyAst ast => [ast a] -> AstElem [ast SrcSpanInfo]
ppConstrList [] = sequenceA []
ppConstrList cs = sepElem hsep
  *> infoElem "="
  *> intersperse (sepElem hsep *> infoElem "|" *> sepElem myVcat) (noInfoList cs)

ppFsepDhead :: DeclHead a -> AstElem (DeclHead SrcSpanInfo)
ppFsepDhead dh = constrElem DHead
  <*> prettyInfoElem name
  <*  sepElem fsep
  <*> intersperse (sepElem fsep) (noInfoList tvs)
  where
    (name, tvs) = sDeclHead dh

ppInstHeadInDecl :: InstHead a -> AstElem (InstHead SrcSpanInfo)
ppInstHeadInDecl ih = constrElem IHead
  <*> prettyInfoElem qn
  <*  sepElem fsep
  <*> intersperse (sepElem fsep) (map (annNoInfoElem.ppAType) ts)
  where
    (qn, ts) = sInstHead ih

-- --------------------------------------------------------------------------

ppWarnDepr :: PrettyAst ast => ([ast a], String) -> AstElem ([ast SrcSpanInfo], String)
ppWarnDepr ([], txt) = pure (,) <*> pure []  <*> infoElem txt
ppWarnDepr (ns, txt) = pure (,)
  <*> intersperse1 (sepElem hsep) (infoElem "," <* sepElem fsep) (noInfoList ns)
  <*  sep ns
  <*> infoElem txt
  where
    sep [n] = sepElem hsep
    sep _   = sepElem fsep

-- --------------------------------------------------------------------------

instance PrettyAst DeclHead where
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
    resultPretty.parens $ constrElem DHParen <*> prettyNoInfoElem dh

-- --------------------------------------------------------------------------

instance PrettyAst InstHead where
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
    resultPretty.parens $ constrElem IHParen <*> prettyInfoElem ih

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
            l'   = op *> prettyInfoElem l
            r'   = cp *> prettyInfoElem r
            n'   = annInfoElem $ ppName n
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

instance PrettyAst ClassDecl where
  astPretty (ClsDecl _ d) = resultPretty $ constrElem ClsDecl <*> prettyInfoElem d
  astPretty (ClsDataFam _ context dh optkind) =
    resultPretty $ constrElem ClsDataFam
      -- mySep
      <*  infoElem "data"
      <*  sepElem hsep
      <*> ppContext context
      <*  sepElem fsep
      <*> ppFsepDhead dh
      <*  sepElem fsep
      <*> ppOptKind optkind
  astPretty (ClsTyFam _ dh optkind)  =
    resultPretty $ constrElem ClsTyFam
      -- mySep
      <*  infoElem "type"
      <*  sepElem hsep
      <*> ppFsepDhead dh
      <*  sepElem fsep
      <*> ppOptKind optkind
  astPretty (ClsTyDef _ ntype htype) =
    resultPretty $ constrElem ClsTyDef
      -- mySep
      <*> prettyInfoElem ntype
      <*  sepElem hsep
      <*  infoElem "="
      <*  sepElem fsep
      <*> prettyInfoElem htype

-- --------------------------------------------------------------------------

instance PrettyAst InstDecl where
  astPretty (InsDecl _ decl) = resultPretty $ constrElem InsDecl <*> prettyInfoElem decl
  astPretty (InsType _ ntype htype) = resultPretty $ constrElem InsType
    -- mySep
    <*  infoElem "type"
    <*  sepElem hsep
    <*> prettyInfoElem ntype
    <*  sepElem fsep
    <*  infoElem "="
    <*  sepElem fsep
    <*> prettyInfoElem htype
  astPretty (InsData _ don ntype constrList derives) =
    resultPretty $ constrElem InsData
      -- mySep
      <*> prettyInfoElem don
      <*  sepElem hsep
      <*> prettyInfoElem ntype
      <*  sepElem fsep
      <*> intersperse1 constrSep1 constrSep2 (noInfoList constrList)
      <*  sepElem myVcat
      <*> traverse ppDeriving derives
      where
        constrSep1 = infoElem "=" <* sepElem hsep
        constrSep2 = sepElem myVcat <* infoElem "|" <* sepElem hsep
  astPretty (InsGData _ don ntype optkind gadtList derives) =
    resultPretty $ constrElem InsGData
      -- mySep
      <*> prettyInfoElem don
      <*  sepElem hsep
      <*> prettyInfoElem ntype
      <*  sepElem fsep
      <*> ppOptKind optkind
      <*  sepElem fsep
      <*  infoElem "where"
      <*  sepElem myVcat
      <*> ppBody classIndent (noInfoList gadtList)
      <*  sepElem myVcat
      <*> traverse ppDeriving derives

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
    resultPretty $ constrElem Rule
    -- mySep
      <*> infoElem tag
      <*  sepElem hsep
      <*> traverse prettyInfoElem activ
      <*  sepElem fsep
      <*> traverse ppRuleVars rvs
      <*  sepElem fsep
      <*> prettyInfoElem rhs
      <*  sepElem fsep
      <*  infoElem "="
      <*  sepElem fsep
      <*> prettyInfoElem lhs

ppRuleVars []  = pure []
ppRuleVars rvs = infoElem "forall" *> sepElem hsep *> intersperse (sepElem fsep) (noInfoList rvs) <* infoElem "."

-- --------------------------------------------------------------------------

instance PrettyAst Activation where
  astPretty (ActiveFrom  _ i) = resultPretty $ constrElem ActiveFrom  <* infoElem "["  <*> pure i <* (infoElem $ show i) <* infoElem "]"
  astPretty (ActiveUntil _ i) = resultPretty $ constrElem ActiveUntil <* infoElem "[~" <*> pure i <* (infoElem $ show i) <* infoElem "]"

-- --------------------------------------------------------------------------

instance PrettyAst RuleVar where
    astPretty (RuleVar _ n) = resultPretty $ constrElem RuleVar <*> prettyInfoElem n
    astPretty (TypedRuleVar _ n t) =
      resultPretty.parens $ constrElem TypedRuleVar
        <*  sepElem hsep
        <*> prettyInfoElem n
        <*  sepElem fsep
        <*  infoElem "::"
        <*  sepElem fsep
        <*> prettyInfoElem t
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
      <*>  prettyNoInfoElem ann
      <*   sepElem myFsep
      <*   infoElem "#-}"

-- --------------------------------------------------------------------------

instance PrettyAst Annotation where
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

------------------------- Data & Newtype Bodies -------------------------

instance PrettyAst QualConDecl where
  astPretty (QualConDecl _ mtvs mctxt con) =
    resultPretty $ constrElem QualConDecl
      <*> traverseSep (sepElem myFsep) (ppForall . noInfoList) mtvs
      <*  sepElem myFsep
      <*> ppContext mctxt
      <*  sepElem myFsep
      <*> prettyInfoElem con

-- --------------------------------------------------------------------------

instance PrettyAst GadtDecl where
  astPretty (GadtDecl _ name ty) =
    resultPretty $ constrElem GadtDecl
      <*> prettyInfoElem name
      <*  sepElem myFsep
      <*  infoElem "::"
      <*  sepElem myFsep
      <*> prettyInfoElem ty

-- --------------------------------------------------------------------------

instance PrettyAst ConDecl where
  astPretty (RecDecl _ name fieldList) =
    resultPretty $ constrElem RecDecl
      <*> prettyInfoElem name
      <*> braceList (noInfoList fieldList)
  astPretty (ConDecl _ name typeList) =
    resultPretty $ constrElem ConDecl
    -- mySep
      <*> annInfoElem (ppName name)
      <*  sepElem hsep
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
      <*> prettyInfoElem ty

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
       <* infoElem "deriving" <* sepElem hsep <*> sequenceA [prettyNoInfoElem ih]
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

instance PrettyAst Type where
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
      t' = parens $ constrElem TyKind
        -- myFsep
        <*  sepElem myFsep
        <*> prettyNoInfoElem t
        <*   sepElem myFsep
        <*   infoElem "::"
        <*> prettyNoInfoElem k
        <*   sepElem myFsep

-- --------------------------------------------------------------------------

instance PrettyAst TyVarBind where
  astPretty (KindedVar _ var kind) =
    resultPretty.parens $ constrElem KindedVar
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
      <*> prettyInfoElem b
  astPrettyPrec _ (KindParen _ k) =
    resultPretty.parens $ constrElem KindParen <*> prettyInfoElem k
  astPrettyPrec _ (KindVar _ n) = resultPretty $ constrElem KindVar <*> prettyInfoElem n

ppOptKind :: Maybe (Kind a) -> AstElem (Maybe (Kind SrcSpanInfo))
ppOptKind k = traverse (\ a -> infoElem "::" *> prettyInfoElem a) k

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
      <*> prettyInfoElem e
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
      <*> prettyInfoElem ppBody

-- --------------------------------------------------------------------------

instance PrettyAst Literal where
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

instance PrettyAst Exp where
  astPrettyPrec _ (Lit _ l) = resultPretty $ constrElem Lit <*> prettyInfoElem l
  -- lambda stuff
  astPrettyPrec p (InfixApp _ a op b) = resultPretty . parensIf (p > 2) $
    constrElem InfixApp
      <*> annInfoElem (astPrettyPrec 2 a)
      <*  sepElem myFsep
      <*> prettyInfoElem op
      <*  sepElem myFsep
      <*> annInfoElem (astPrettyPrec 1 b)
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
      <*> prettyInfoElem body
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
      <*> prettyInfoElem cond
      <*  sepElem myFsep
      <*  infoElem "then"
      <*  sepElem myFsep
      <*> prettyInfoElem thenexp
      <*  infoElem "else"
      <*  sepElem myFsep
      <*> prettyInfoElem elsexp
  astPrettyPrec p (Case _ cond altList) =
    resultPretty.parensIf (p > 1) $ constrElem Case
      <*  infoElem "case"
      <*  sepElem myFsep
      <*> prettyInfoElem cond
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
  astPrettyPrec _ (Var _ name) = resultPretty $ constrElem Var <*> prettyInfoElem name
  astPrettyPrec _ (IPVar _ ipname) = resultPretty $ constrElem IPVar <*> prettyInfoElem ipname
  astPrettyPrec _ (Con _ name) = resultPretty $ constrElem Con <*> prettyInfoElem name
  astPrettyPrec _ (Tuple _ expList) = resultPretty $ constrElem Tuple
    <*> parenList (noInfoList expList)
  astPrettyPrec _ (TupleSection _ mExpList) = resultPretty $ constrElem TupleSection
    <*> parenList (map (traverse prettyNoInfoElem) mExpList)
  -- weird stuff
  astPrettyPrec _ (Paren _ e) = resultPretty.parens $ constrElem Paren <*> prettyInfoElem e
  astPrettyPrec _ (LeftSection _ e op) =
    resultPretty.parens $ constrElem LeftSection
      <*> prettyInfoElem e
      <*  sepElem hsep
      <*> prettyInfoElem op
  astPrettyPrec _ (RightSection _ op e) =
    resultPretty.parens $ constrElem RightSection
      <*> prettyInfoElem op
      <*  sepElem hsep
      <*> prettyInfoElem e
  astPrettyPrec _ (RecConstr _ c fieldList) = resultPretty $ constrElem RecConstr
    <*> prettyInfoElem c <*> braceList (noInfoList fieldList)
  astPrettyPrec _ (RecUpdate _ e fieldList) = resultPretty $ constrElem RecUpdate
    <*> prettyInfoElem e <*> braceList (noInfoList fieldList)
  -- Lists
  astPrettyPrec _ (List _ list) =  resultPretty $ constrElem List
     <*> brackets (intersperse (infoElem "," <* sepElem myFsepSimple) $ noInfoList list)
  astPrettyPrec _ (EnumFrom _ e) =
    resultPretty $ constrElem EnumFrom
      <*  infoElem "["
      <*> prettyInfoElem e
      <*  sepElem myFsepSimple
      <*  infoElem ".."
      <*  infoElem "]"
  astPrettyPrec _ (EnumFromTo _ from to) =
    resultPretty $ constrElem EnumFromTo
      <*  infoElem "["
      <*> prettyInfoElem from
      <*  sepElem myFsepSimple
      <*  infoElem ".."
      <*  sepElem myFsepSimple
      <*> prettyInfoElem to
      <*  infoElem "]"
  astPrettyPrec _ (EnumFromThen _ from thenE) =
    resultPretty $ constrElem EnumFromThen
      <*  infoElem "["
      <*> prettyInfoElem from
      <*  infoElem ","
      <*  sepElem myFsepSimple
      <*> prettyInfoElem thenE
      <*  sepElem myFsepSimple
      <*  infoElem ".."
      <*  infoElem "]"
  astPrettyPrec _ (EnumFromThenTo _ from thenE to) =
    resultPretty $ constrElem EnumFromThenTo
      <*  infoElem "["
      <*> prettyInfoElem from
      <*  infoElem ","
      <*  sepElem myFsepSimple
      <*> prettyInfoElem thenE
      <*  sepElem myFsepSimple
      <*  infoElem ".."
      <*  sepElem myFsepSimple
      <*> prettyInfoElem to
      <*  infoElem "]"
  astPrettyPrec _ (ListComp _ e qualList) =
    resultPretty $ constrElem ListComp
      <*  infoElem "["
      <*> prettyInfoElem e
      <*  sepElem myFsepSimple
      <*  infoElem "|"
      <*  sepElem myFsepSimple
      <*> intersperse (infoElem "," <* sepElem myFsepSimple) (noInfoList qualList)
      <*  infoElem "]"
  astPrettyPrec _ (ParComp _ e qualLists) =
    resultPretty $ constrElem ParComp
      <*  infoElem "["
      <*> prettyInfoElem e
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
      <*> prettyInfoElem e
      <*  sepElem myFsep
      <*  infoElem "::"
      <*> prettyInfoElem ty
  -- Template Haskell
  astPrettyPrec _ (BracketExp _ b) = resultPretty $ constrElem BracketExp <*> prettyInfoElem b
  astPrettyPrec _ (SpliceExp _ s) = resultPretty $ constrElem SpliceExp <*> prettyInfoElem s
  astPrettyPrec _ (TypQuote _ t)  =
    resultPretty $ constrElem TypQuote <* infoElem "\'\'" <*> prettyInfoElem t
  astPrettyPrec _ (VarQuote _ x)  =
    resultPretty $ constrElem VarQuote <* infoElem "\'" <*> prettyInfoElem x
  astPrettyPrec _ (QuasiQuote _ n qt) =
    resultPretty $ constrElem QuasiQuote
      <*  infoElem "["
      <*> infoElem n
      <*  infoElem "|"
      <*> infoElem qt
      <*  infoElem "|]"
  -- Hsx
  astPrettyPrec _ (XTag _ n attrs mattr cs) = undefined
  astPrettyPrec _ (XETag _ n attrs mattr) =
    resultPretty $ constrElem XETag
      -- myFsep
      <*  infoElem "<"
      <*  sepElem myFsep
      <*> prettyInfoElem n
      <*  sepElem myFsep
      <*> intersperse (sepElem myFsep) (noInfoList attrs)
      <*  sepElem myFsep
      <*> traverse prettyInfoElem mattr
      <*  sepElem myFsep
      <*  infoElem "/>"
  astPrettyPrec _ (XPcdata _ s) = resultPretty $ constrElem XPcdata <*> infoElem s
  astPrettyPrec _ (XExpTag _ e) =
    resultPretty $ constrElem XExpTag
      -- myFsep
      <*  infoElem "<%"
      <*  sepElem myFsep
      <*> prettyInfoElem e
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
      <*> prettyInfoElem e
  astPrettyPrec _ (SCCPragma  _ s e) =
    resultPretty $ constrElem SCCPragma
      -- myFsep
      <*  infoElem "{-# SCC"
      <*  sepElem myFsep
      <*> infoElem s
      <*  sepElem myFsep
      <*  infoElem "#-}"
      <*  sepElem myFsep
      <*> prettyInfoElem e
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
      <*> prettyInfoElem e
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
      <*> prettyInfoElem pat
      <*  sepElem myFsep
      <*  infoElem "->"
      <*  sepElem myFsep
      <*> prettyInfoElem e
  astPrettyPrec p (LeftArrApp _ l r) =
    resultPretty.parensIf (p > 0) $ constrElem LeftArrApp
      <*> prettyInfoElem l
      <*  sepElem myFsep
      <*  infoElem "-<"
      <*  sepElem myFsep
      <*> prettyInfoElem r
  astPrettyPrec p (RightArrApp _ l r) =
    resultPretty.parensIf (p > 0) $ constrElem RightArrApp
      <*> prettyInfoElem l
      <*  sepElem myFsep
      <*  infoElem ">-"
      <*  sepElem myFsep
      <*> prettyInfoElem r
  astPrettyPrec p (LeftArrHighApp _ l r) =
    resultPretty.parensIf (p > 0) $ constrElem LeftArrHighApp
      <*> prettyInfoElem l
      <*  sepElem myFsep
      <*  infoElem "-<<"
      <*  sepElem myFsep
      <*> prettyInfoElem r
  astPrettyPrec p (RightArrHighApp _ l r) =
    resultPretty.parensIf (p > 0) $ constrElem RightArrHighApp
      <*> prettyInfoElem l
      <*  sepElem myFsep
      <*  infoElem ">>-"
      <*  sepElem myFsep
      <*> prettyInfoElem r

-- --------------------------------------------------------------------------

instance PrettyAst XAttr where
  astPretty (XAttr _ n v) =
    resultPretty $ constrElem XAttr
    <*> prettyInfoElem n
    <*  sepElem myFsep
    <*  infoElem "="
    <*  sepElem myFsep
    <*> prettyInfoElem v

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
  <*> prettyInfoElem b

ppWith f binds = f
  <*  (sepElem $ nest 2)
  <*  infoElem "with"
  <*  sepElem myVcat
  <*> ppBody withIndent (noInfoList binds)
withIndent = whereIndent

--------------------- Template Haskell -------------------------

instance PrettyAst Bracket where
  astPretty (ExpBracket _ e) =
    resultPretty $ constrElem ExpBracket <*> ppBracket "[|" (prettyInfoElem e)
  astPretty (PatBracket _ p) =
    resultPretty $ constrElem PatBracket <*> ppBracket "[p|" (prettyInfoElem p)
  astPretty (TypeBracket _ t) =
    resultPretty $ constrElem TypeBracket <*> ppBracket "[t|" (prettyInfoElem t)
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
      <* infoElem "$(" <* sepElem myFsep <*> prettyInfoElem e <* sepElem myFsep <* infoElem ")"

------------------------- Patterns -----------------------------

instance PrettyAst Pat where
  astPrettyPrec _ (PVar _ name) = resultPretty $ constrElem PVar <*> prettyInfoElem name
  astPrettyPrec _ (PLit _ lit) = resultPretty $ constrElem PLit <*> prettyInfoElem lit
  astPrettyPrec p (PNeg _ pat) =
    resultPretty.parensIf (p > 0) $ constrElem PNeg
    -- myFsep
      <*  infoElem "-"
      <*  sepElem myFsep
      <*> prettyInfoElem pat
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
      <*> prettyInfoElem n
      <*  sepElem myFsep
      <*> intersperse (sepElem myFsep) (map (annNoInfoElem.astPrettyPrec 2) ps)
  astPrettyPrec _ (PTuple _ ps) = resultPretty $ constrElem PTuple <*> parenList (noInfoList ps)
  astPrettyPrec _ (PList _ ps) =
    resultPretty $ constrElem PList <*> braceList (noInfoList ps)
  astPrettyPrec _ (PParen _ pat) = resultPretty.parens $ constrElem PParen <*> prettyInfoElem pat
  astPrettyPrec _ (PRec _ c fields) = resultPretty.braces $ constrElem PRec
    <*> prettyInfoElem c
    <*> braceList (noInfoList fields)
  -- special case that would otherwise be buggy
  astPrettyPrec _ (PAsPat _ name (PIrrPat _ pat)) =
    resultPretty.braces $ constrElem PAsPat
    -- myFsep
      <*> prettyInfoElem name
      <*  infoElem "@"
      <*  sepElem myFsep
      <*  infoElem "~"
      <*> pat'
    where
      pat' = constrElem PIrrPat <*> annInfoElem (astPrettyPrec 2 pat)
  astPrettyPrec _ (PAsPat _ name pat) =
    resultPretty.braces $ constrElem PAsPat
    -- hcat
      <*> prettyInfoElem name
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
      <*> prettyInfoElem pat
      <*  sepElem myFsep
      <*  infoElem "::"
      <*  sepElem myFsep
      <*> prettyInfoElem ty
  astPrettyPrec p (PViewPat _ e pat) =
    resultPretty.parensIf (p > 0) $ constrElem PViewPat
    -- myFsep
      <*> prettyInfoElem e
      <*  sepElem myFsep
      <*  infoElem "->"
      <*  sepElem myFsep
      <*> prettyInfoElem pat
  astPrettyPrec p (PNPlusK _ n k) =
    resultPretty.parensIf (p > 0) $ constrElem PNPlusK
    -- myFsep
      <*> prettyInfoElem n
      <*  sepElem myFsep
      <*  infoElem "+"
      <*  sepElem myFsep
      <*> pure k <* infoElem (show k)
  -- HaRP
  astPrettyPrec _ (PRPat _ rs) = resultPretty $ constrElem PRPat <*> bracketList (noInfoList rs)
  astPrettyPrec _ (PXTag _ n attrs mattr cp) = undefined
  astPrettyPrec _ (PXETag _ n attrs mattr) =
    resultPretty $ constrElem PXETag
    -- myFsep
      <*  infoElem "<"
      <*> prettyInfoElem n
      <*  sepElem myFsep
      <*> intersperse (sepElem myFsep) (map prettyInfoElem attrs)
      <*  sepElem myFsep
      <*> traverse prettyInfoElem mattr
      <*  sepElem myFsep
      <*  infoElem "/>"
  astPrettyPrec _ (PXPcdata _ s) = resultPretty $ constrElem PXPcdata <*> infoElem s
  astPrettyPrec _ (PXPatTag _ p) =
    resultPretty $ constrElem PXPatTag
      -- myFsep
      <*  infoElem "<%"
      <*  sepElem myFsep
      <*> prettyInfoElem p
      <*  sepElem myFsep
      <*  infoElem "%>"
  astPrettyPrec _ (PXRPats _ ps) =
    resultPretty $ constrElem PXRPats
      -- myFsep
      <*  infoElem "<["
      <*  sepElem myFsep
      <*> intersperse (sepElem myFsep) (map prettyInfoElem ps)
      <*  sepElem myFsep
      <*  infoElem "]>"
  -- Generics
  astPrettyPrec _ (PExplTypeArg _ qn t) =
    resultPretty $ constrElem PExplTypeArg
      <*> prettyInfoElem qn
      <*  sepElem myFsep
      <*  infoElem "{|"
      <*  sepElem myFsep
      <*> prettyInfoElem t
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
      <*> prettyInfoElem n
      <*  sepElem myFsep
      <*  infoElem "="
      <*  sepElem myFsep
      <*> prettyInfoElem p

-- --------------------------------------------------------------------------

instance PrettyAst PatField where
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

instance PrettyAst RPat where
  astPretty (RPOp _ r op) = resultPretty $ constrElem RPOp <*> prettyInfoElem r <*> prettyInfoElem op
  astPretty (RPEither _ r1 r2) =
    resultPretty.parens $ constrElem RPEither
    -- myFsep
      <*> prettyInfoElem r1
      <*  sepElem myFsep
      <*  infoElem "|"
      <*  sepElem myFsep
      <*> prettyInfoElem r2
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
      <*> prettyInfoElem r
      <*  sepElem myFsep
      <*> intersperse (sepElem myFsep) (noInfoList gs)
      <*  sepElem myFsep
      <*  infoElem "/)"
   -- special case that would otherwise be buggy
  astPretty (RPAs _ n (RPPat _ (PIrrPat _ p))) =
    let
      ip = constrElem PIrrPat <*> prettyInfoElem p
      rp = constrElem RPPat   <*> ip
    in
      resultPretty $ constrElem RPAs
      -- myFsep
        <*> prettyInfoElem n
        <*  infoElem "@:"
        <*  sepElem myFsep
        <*  infoElem "~"
        <*> rp
  astPretty (RPAs _ n r) =
    resultPretty $ constrElem RPAs
      -- hcat
        <*> prettyInfoElem n
        <*  infoElem "@"
        <*> prettyInfoElem r
  astPretty (RPPat _ p) = resultPretty $ constrElem RPPat <*> prettyInfoElem p
  astPretty (RPParen _ rp) = resultPretty.parens $ constrElem RPParen <*> prettyInfoElem rp

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
      <*> prettyInfoElem e
      <*  sepElem hsep
      <*> prettyInfoElem gAlts
      <*  sepElem myVcat
      <*> traverse ppWhere binds

-- --------------------------------------------------------------------------

instance PrettyAst GuardedAlts where
  astPretty (UnGuardedAlt _ e) = resultPretty $ constrElem UnGuardedAlt
    <* infoElem "->" <* sepElem hsep <*> prettyInfoElem e
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
      <*> prettyInfoElem body

------------------------- Statements in monads, guards & list comprehensions -----

instance PrettyAst Stmt where
  astPretty (Generator _ e from) = resultPretty $ constrElem Generator
    <*> prettyInfoElem e
    <*  sepElem hsep
    <*  infoElem "<-"
    <*  sepElem hsep
    <*> prettyInfoElem from
  astPretty (Qualifier _ e) = resultPretty $ constrElem Qualifier <*> prettyInfoElem e
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

instance PrettyAst FieldUpdate where
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
    <*> prettyNoInfoElem mn
    <*  infoElem "."
    <*> annInfoElem (ppName n)
rawQName (UnQual _ n)   = resultPretty $ constrElem UnQual <*> annInfoElem (ppName n)
rawQName (Special _ sc) = resultPretty $ constrElem Special <*> prettyNoInfoElem sc

ppQNameInfix :: QName a -> AstElem (QName SrcSpanInfo)
ppQNameInfix name
  | isSymbolName (getName name) = annInfoElem $ rawQName name
  | otherwise = infoElem "`" *> annInfoElem (rawQName name) <* infoElem "`"

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
    <*> prettyInfoElem ipname
    <*  sepElem myFsep
    <*  infoElem "="
    <*  sepElem myFsep
    <*> prettyInfoElem exp

-- --------------------------------------------------------------------------

instance PrettyAst  CName where
  astPretty (VarName _ name) = resultPretty $ constrElem VarName <*> prettyNoInfoElem name
  astPretty (ConName _ name) = resultPretty $ constrElem ConName <*> prettyNoInfoElem name

-- --------------------------------------------------------------------------

instance PrettyAst Context where
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
    impl c = (parens $ prettyInfoElem c)
      <*  sepElem hsep
      <*  infoElem "=>"

-- --------------------------------------------------------------------------
-- hacked for multi-parameter type classes
instance PrettyAst Asst where
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

------------------------- pp utils -------------------------

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

prettyNoInfoElem :: (PrettyAst ast) => ast a -> AstElem (ast SrcSpanInfo)
prettyNoInfoElem a = annNoInfoElem $ astPretty a

prettyInfoElem :: (Annotated ast, PrettyAst ast) => ast a -> AstElem (ast SrcSpanInfo)
prettyInfoElem a = annInfoElem $ astPretty a

annNoInfoElem :: DocM a -> AstElem a
annNoInfoElem a = lift a

annInfoElem :: DocM a -> AstElem a
annInfoElem a = do
  sp <- lift getPos
  a' <- lift a
  ep <- lift getPos
  tell $ if sp == ep then [] else [mkSrcSpan sp ep]
  return a'

constrElem :: (a -> b) -> AstElem b
constrElem f = lift.return $ f undefined

infoList xs = map prettyInfoElem xs
noInfoList xs = map prettyNoInfoElem xs

intersperse :: Applicative f => f a1 -> [f a] -> f [a]
intersperse _ [] = pure []
intersperse sep (e:es) = sequenceA $ e : (map (sep *>) es)

intersperse1 :: Applicative f => f a1 -> f a2 -> [f a] -> f [a]
intersperse1 _ _ [] = pure []
intersperse1 sFrst sep [e] = sequenceA [e]
intersperse1 sFrst sep (e1: e2 : es) = sequenceA $ (e1 <* sFrst) : e2 : (map (sep *>) es)

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
  PrettyMode _ style  <- ask
  c <- getPos
  case mode style of
    PageMode ->
      if srcColumn c >= lineLength style then line else (pure ())
    _ -> undefined

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
flatBlock xs = infoElem "{" *> sepElem hsep *> intersperse (infoElem ";" <* sepElem hsep) xs <* infoElem "}"

-- Same, but put each thing on a separate line
prettyBlock :: [AstElem a] -> AstElem [a]
prettyBlock xs = infoElem "{" *> sepElem hsep *> intersperse (infoElem ";" <* sepElem vcat) xs <* infoElem "}"

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
    indent i = intersperse ((sepElem $ nest i) *> sepElem vcat) dl
    indentExplicit i = (sepElem $ nest i) *> prettyBlock dl

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
