{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module AstSerial where

import Test.SmallCheck
import Test.SmallCheck.Series

import GHC.Generics
import GHC.Real
import Language.Haskell.Exts.Annotated

deriving instance Generic SrcSpan
instance Monad m => Serial m SrcSpan

deriving instance Generic SrcSpanInfo
instance Monad m => Serial m SrcSpanInfo

deriving instance Generic SrcLoc
instance Monad m => Serial m SrcLoc

deriving instance Generic Boxed
instance Monad m => Serial m Boxed

deriving instance Generic (Ratio l)
instance (Monad m, Serial m l) => Serial m (Ratio l)

deriving instance Generic Tool
instance Monad m => Serial m Tool

deriving instance Generic (GuardedAlt l)
instance (Monad m, Serial m l) => Serial m (GuardedAlt l)

deriving instance Generic (GuardedAlts l)
instance (Monad m, Serial m l) => Serial m (GuardedAlts l)

deriving instance Generic (Alt l)
instance (Monad m, Serial m l) => Serial m (Alt l)

deriving instance Generic (FieldUpdate l)
instance (Monad m, Serial m l) => Serial m (FieldUpdate l)

deriving instance Generic (QualStmt l)
instance (Monad m, Serial m l) => Serial m (QualStmt l)

deriving instance Generic (Stmt l)
instance (Monad m, Serial m l) => Serial m (Stmt l)

deriving instance Generic (PatField l)
instance (Monad m, Serial m l) => Serial m (PatField l)

deriving instance Generic (RPat l)
instance (Monad m, Serial m l) => Serial m (RPat l)

deriving instance Generic (RPatOp l)
instance (Monad m, Serial m l) => Serial m (RPatOp l)

deriving instance Generic (PXAttr l)
instance (Monad m, Serial m l) => Serial m (PXAttr l)

deriving instance Generic (Pat l)
instance (Monad m, Serial m l) => Serial m (Pat l)

deriving instance Generic (WarningText l)
instance (Monad m, Serial m l) => Serial m (WarningText l)

deriving instance Generic (RuleVar l)
instance (Monad m, Serial m l) => Serial m (RuleVar l)

deriving instance Generic (Rule l)
instance (Monad m, Serial m l) => Serial m (Rule l)

deriving instance Generic (Activation l)
instance (Monad m, Serial m l) => Serial m (Activation l)

deriving instance Generic (ModulePragma l)
instance (Monad m, Serial m l) => Serial m (ModulePragma l)

deriving instance Generic (CallConv l)
instance (Monad m, Serial m l) => Serial m (CallConv l)

deriving instance Generic (Safety l)
instance (Monad m, Serial m l) => Serial m (Safety l)

deriving instance Generic (Splice l)
instance (Monad m, Serial m l) => Serial m (Splice l)

deriving instance Generic (Bracket l)
instance (Monad m, Serial m l) => Serial m (Bracket l)

deriving instance Generic (XAttr l)
instance (Monad m, Serial m l) => Serial m (XAttr l)

deriving instance Generic (XName l)
instance (Monad m, Serial m l) => Serial m (XName l)

deriving instance Generic (Exp l)
instance (Monad m, Serial m l) => Serial m (Exp l)

deriving instance Generic (Literal l)
instance (Monad m, Serial m l) => Serial m (Literal l)

deriving instance Generic (Asst l)
instance (Monad m, Serial m l) => Serial m (Asst l)

deriving instance Generic (Context l)
instance (Monad m, Serial m l) => Serial m (Context l)

deriving instance Generic (FunDep l)
instance (Monad m, Serial m l) => Serial m (FunDep l)

deriving instance Generic (Kind l)
instance (Monad m, Serial m l) => Serial m (Kind l)

deriving instance Generic (TyVarBind l)
instance (Monad m, Serial m l) => Serial m (TyVarBind l)

deriving instance Generic (Type l)
instance (Monad m, Serial m l) => Serial m (Type l)

deriving instance Generic (GuardedRhs l)
instance (Monad m, Serial m l) => Serial m (GuardedRhs l)

deriving instance Generic (Rhs l)
instance (Monad m, Serial m l) => Serial m (Rhs l)

deriving instance Generic (BangType l)
instance (Monad m, Serial m l) => Serial m (BangType l)

deriving instance Generic (InstDecl l)
instance (Monad m, Serial m l) => Serial m (InstDecl l)

deriving instance Generic (ClassDecl l)
instance (Monad m, Serial m l) => Serial m (ClassDecl l)

deriving instance Generic (GadtDecl l)
instance (Monad m, Serial m l) => Serial m (GadtDecl l)

deriving instance Generic (FieldDecl l)
instance (Monad m, Serial m l) => Serial m (FieldDecl l)

deriving instance Generic (ConDecl l)
instance (Monad m, Serial m l) => Serial m (ConDecl l)

deriving instance Generic (QualConDecl l)
instance (Monad m, Serial m l) => Serial m (QualConDecl l)

deriving instance Generic (Match l)
instance (Monad m, Serial m l) => Serial m (Match l)

deriving instance Generic (IPBind l)
instance (Monad m, Serial m l) => Serial m (IPBind l)

deriving instance Generic (Binds l)
instance (Monad m, Serial m l) => Serial m (Binds l)

deriving instance Generic (Deriving l)
instance (Monad m, Serial m l) => Serial m (Deriving l)

deriving instance Generic (InstHead l)
instance (Monad m, Serial m l) => Serial m (InstHead l)

deriving instance Generic (DeclHead l)
instance (Monad m, Serial m l) => Serial m (DeclHead l)

deriving instance Generic (DataOrNew l)
instance (Monad m, Serial m l) => Serial m (DataOrNew l)

deriving instance Generic (Annotation l)
instance (Monad m, Serial m l) => Serial m (Annotation l)

deriving instance Generic (Decl l)
instance (Monad m, Serial m l) => Serial m (Decl l)

deriving instance Generic (Assoc l)
instance (Monad m, Serial m l) => Serial m (Assoc l)

deriving instance Generic (ImportSpec l)
instance (Monad m, Serial m l) => Serial m (ImportSpec l)

deriving instance Generic (ImportSpecList l)
instance (Monad m, Serial m l) => Serial m (ImportSpecList l)

deriving instance Generic (ImportDecl l)
instance (Monad m, Serial m l) => Serial m (ImportDecl l)

deriving instance Generic (ExportSpec l)
instance (Monad m, Serial m l) => Serial m (ExportSpec l)

deriving instance Generic (ExportSpecList l)
instance (Monad m, Serial m l) => Serial m (ExportSpecList l)

deriving instance Generic (ModuleHead l)
instance (Monad m, Serial m l) => Serial m (ModuleHead l)

deriving instance Generic (Module l)
instance (Monad m, Serial m l) => Serial m (Module l)

deriving instance Generic (CName l)
instance (Monad m, Serial m l) => Serial m (CName l)

deriving instance Generic (Op l)
instance (Monad m, Serial m l) => Serial m (Op l)

deriving instance Generic (QOp l)
instance (Monad m, Serial m l) => Serial m (QOp l)

deriving instance Generic (IPName l)
instance (Monad m, Serial m l) => Serial m (IPName l)

deriving instance Generic (Name l)
instance (Monad m, Serial m l) => Serial m (Name l)

deriving instance Generic (QName l)
instance (Monad m, Serial m l) => Serial m (QName l)

deriving instance Generic (SpecialCon l)
instance (Monad m, Serial m l) => Serial m (SpecialCon l)

deriving instance Generic (ModuleName l)
instance (Monad m, Serial m l) => Serial m (ModuleName l)

