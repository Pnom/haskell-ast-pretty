module AstSerial() where

import Test.SmallCheck
import Test.SmallCheck.Series
import Language.Haskell.Exts.Annotated
import Language.Haskell.Exts.Pretty
import AstPretty

import Data.List

-- before 'DeriveGeneric' will work

-- --------------------------------------------------------------------------

instance Serial DocState where
  series = cons2 DocState
  coseries = undefined

-- --------------------------------------------------------------------------

instance Serial SrcLoc where
  series = cons3 SrcLoc
  coseries = undefined

-- --------------------------------------------------------------------------

instance Serial SrcSpan where
  series = cons2 impl
    where impl f (sl, sc, el, ec) = SrcSpan f sl sc el ec
  coseries = undefined

-- --------------------------------------------------------------------------

instance Serial SrcSpanInfo where
  series = cons2 SrcSpanInfo
  coseries = undefined

-- --------------------------------------------------------------------------

instance (Serial a) => Serial (ModuleName a) where
  series = cons2 ModuleName
  coseries = undefined

-- --------------------------------------------------------------------------

instance (Serial a) => Serial (CName a) where
  series = cons2 VarName \/ cons2 ConName
  coseries = undefined

-- --------------------------------------------------------------------------

instance (Serial a) => Serial (SpecialCon a) where
  series = cons1 UnitCon \/ cons1 ListCon \/ cons1 ListCon \/ cons1 FunCon \/ cons3 TupleCon \/ cons1 Cons  \/ cons1 UnboxedSingleCon
  coseries = undefined

-- --------------------------------------------------------------------------

instance Serial Boxed where
  series = cons0 Boxed \/ cons0 Unboxed
  coseries = undefined

-- --------------------------------------------------------------------------

instance (Serial a) => Serial (ExportSpec a) where
  series = cons2 EVar \/ cons2 EAbs \/ cons2 EThingAll \/ cons3 EThingWith \/ cons2 EModuleContents
  coseries = undefined

-- --------------------------------------------------------------------------

instance (Serial a) => Serial (QName a) where
  series =  cons3 Qual \/ cons2 UnQual \/ cons2 Special
  coseries = undefined

-- --------------------------------------------------------------------------

instance (Serial a) => Serial (Name a) where
  series =  cons2 Ident \/ cons2 Symbol
  coseries = undefined
