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

import qualified Text.PrettyPrint as P

import Debug.Trace

data DocState = DocState {
  pos :: !SrcLoc,
  nestSize :: !Int
  } deriving Show

data PrettyMode = PrettyMode PR.PPHsMode

defPrettyMode = PrettyMode PR.defaultMode

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

class AstPretty ast where
  astPretty :: ast a -> DocM (ast SrcSpanInfo)

-- --------------------------------------------------------------------------

instance AstPretty ModuleName where
  astPretty (ModuleName l s) = do
    span <- format s
    return $ ModuleName (noInfoSpan span) s

------------------------- Pragmas ---------------------------------------

instance AstPretty ModulePragma where
  astPretty (LanguagePragma _ []) = do
    b <- getPos
    return $ LanguagePragma (noInfoSpan $ mkSrcSpan b b) []

  astPretty (LanguagePragma _ ns) = do
    (span, ls) <- genericParenList (format "{-# LANGUAGE") (format "#-}") (infoPrettyList (myFsep $ format ",") ns)
    return $ LanguagePragma span ls

  astPretty (OptionsPragma _ mbTool s) = do
    let
      t = case mbTool of
        Nothing -> ""
        Just (UnknownTool u) -> show u
        Just tool -> show tool

    sp <- format $ "{-# OPTIONS_" ++ t
    _  <- format s
    cp <- format "#-}"
    let span = SrcSpanInfo (mergeSrcSpan sp cp) [sp, cp]
    return $ OptionsPragma span mbTool s

  astPretty (AnnModulePragma _ ann) = do
    sp <- format "{-# ANN"
    ann'  <- astPretty ann
    cp <- format "#-}"
    let span = SrcSpanInfo (mergeSrcSpan sp cp) [sp, cp]
    return $ AnnModulePragma span ann'

-- --------------------------------------------------------------------------

instance AstPretty Annotation where
  astPretty (Ann _ n e) = do
    sp <- getPos
    n' <- astPretty n
    e' <- astPretty e
    ep <- getPos
    let span = noInfoSpan $ mkSrcSpan sp ep
    return $ Ann span n' e'

  astPretty (TypeAnn _ n e) = do
    sp <- getPos
    t <- format "type"
    n' <- astPretty n
    e' <- astPretty e
    ep <- getPos
    let span = SrcSpanInfo (mkSrcSpan sp ep) [t]
    return $ TypeAnn span n' e'

  astPretty (ModuleAnn _ e) = do
    sp <- getPos
    t <- format "module"
    e' <- astPretty e
    ep <- getPos
    let span = SrcSpanInfo (mkSrcSpan sp ep) [t]
    return $ ModuleAnn span e'

------------------------- Expressions -------------------------

instance AstPretty Exp where
  astPretty = undefined

-- --------------------------------------------------------------------------

instance AstPretty  CName where

  astPretty (VarName _ name) = do
    n <- astPretty name
    return $ VarName (noInfoSpan.srcInfoSpan $ ann n) n

  astPretty (ConName _ name) = do
    n <- astPretty name
    return $ ConName (noInfoSpan.srcInfoSpan $ ann n) n

-- --------------------------------------------------------------------------

instance AstPretty SpecialCon where

  astPretty (UnitCon _) = do
    span <- format "()"
    return $ UnitCon (noInfoSpan  span)

  astPretty (ListCon _) = do
    span <- format "[]"
    return $ ListCon (noInfoSpan  span)

  astPretty (FunCon _) = do
    span <- format "->"
    return $ FunCon (noInfoSpan  span)

  astPretty (TupleCon _ b n) = do
    let hash = if b == Unboxed then "#" else ""
    span <- format $ "(" ++ hash ++ replicate (n-1) ',' ++ hash ++ ")"
    return $ TupleCon (noInfoSpan  span) b n

  astPretty (Cons _) = do
    span <- format ":"
    return $ Cons (noInfoSpan  span)

  astPretty (UnboxedSingleCon _) = do
    span <- format "(# #)"
    return $ UnboxedSingleCon (noInfoSpan  span)

-- --------------------------------------------------------------------------

instance AstPretty ExportSpec where

  astPretty (EVar _ name) = do
    qn <- astPretty name
    return $ EVar (noInfoSpan.srcInfoSpan $ ann qn) qn

  astPretty (EAbs _ name) = do
    qn <- astPretty name
    return $ EAbs (noInfoSpan.srcInfoSpan $ ann qn) qn

  astPretty (EThingAll _ name) = do
    qn <- astPretty name
    undefined -- what about "(..)"?
    return $ EThingAll (noInfoSpan.srcInfoSpan $ ann qn) qn

  astPretty (EThingWith l name nameList) = do
    n <- astPretty name
    (p, ns) <- parenList nameList
    let sp = ( ann n <++> p) <** srcInfoPoints p
    return $ EThingWith sp n ns

  astPretty (EModuleContents _ m) = do
          qn <- astPretty m
          return $ EModuleContents (noInfoSpan.srcInfoSpan $ ann qn) qn

-- --------------------------------------------------------------------------

instance AstPretty QName where
  astPretty qn
    | needParens = do
      openParen <- format "("
      _ <- space 1
      qn' <- rawQName qn
      closeParen <- format ")"
      let span = (openParen <^^> closeParen) <** [openParen, srcInfoSpan $ ann qn', closeParen]
      return $ amap (const span) qn'

    | otherwise = rawQName qn
    where
      needParens = case qn of
        UnQual _    (Symbol _ _) -> True
        Qual   _  _ (Symbol _ _) -> True
        Special _ (Cons _)    -> True
        Special _ (FunCon _)  -> True
        _ -> False

-- --------------------------------------------------------------------------
-- QName utils

rawQName :: QName t -> DocM (QName SrcSpanInfo)

rawQName (Qual _ mn n)  = do
  m' <- astPretty mn
  _  <- format "."
  n'  <- rawName n
  let span  = ann m' <++> ann n'
  return $ Qual span m' n'

rawQName (UnQual _ n) = do
  n' <- rawName n
  return $ UnQual (ann n') n'

rawQName (Special _ sc) = do
  val <- astPretty sc
  return $ Special (noInfoSpan.srcInfoSpan $ ann val) val

-- --------------------------------------------------------------------------

instance AstPretty Name where
  astPretty n@(Ident _ _) = rawName n

  astPretty (Symbol _ str) = do
    openParen <- format "("
    _ <- space 1
    name <- format str
    closeParen <- format ")"
    let span = (openParen <^^> closeParen) <** [openParen, name, closeParen]
    return $ Symbol span str

-- --------------------------------------------------------------------------

isSymbol :: Name l -> Bool
isSymbol (Symbol _ _) = True
isSymbol _ = False

-- --------------------------------------------------------------------------

rawName :: Name t -> DocM (Name SrcSpanInfo)

rawName (Symbol _ s) = do
  span <- format s
  return $ Symbol (noInfoSpan  span) s

rawName (Ident _ s) = do
  span <- format s
  return $ Ident (noInfoSpan  span) s

-- --------------------------------------------------------------------------

noInfoPrettyList :: AstPretty ast =>
  DocM s -> [ast a] -> DocM (SrcSpanInfo, [ast SrcSpanInfo])

noInfoPrettyList _ [] = do
  b <- getPos
  return (noInfoSpan $ mkSrcSpan b b, [])

noInfoPrettyList sep (e:es) = do
  e' <- astPretty e
  begp <- getPos
  xs <- foldM (\ acc i -> do
    p <- sep
    x <- astPretty i
    return (x:acc))
    [e']
    es
  endp <- getPos
  return (noInfoSpan $ mkSrcSpan begp endp, reverse xs)

-- --------------------------------------------------------------------------

infoPrettyList :: AstPretty ast =>
  DocM SrcSpan -> [ast a] -> DocM (SrcSpanInfo, [ast SrcSpanInfo])

infoPrettyList _ [] = do
  b <- getPos
  return (noInfoSpan $ mkSrcSpan b b, [])

infoPrettyList sep (e:es) = do
  e' <- astPretty e
  begp <- getPos
  (ps, xs) <- foldM (\ (ps, xs) i -> do
    p <- sep
    x <- astPretty i
    return (p:ps, x:xs))
    ([], [e'])
    es
  endp <- getPos
  let span = SrcSpanInfo (mkSrcSpan begp endp) (reverse ps )
  return (span, reverse xs)

-- --------------------------------------------------------------------------

genericParenList :: AstPretty ast =>
  DocM SrcSpan -> DocM SrcSpan -> DocM (SrcSpanInfo, [ast SrcSpanInfo]) -> DocM (SrcSpanInfo, [ast SrcSpanInfo])

genericParenList openParen closeParen ls = do
  op <- openParen
  (s, xs) <- ls
  cp <- closeParen
  let ps = (op : srcInfoPoints s) ++ [cp]
  return (SrcSpanInfo (mergeSrcSpan op cp) ps, xs)

-- --------------------------------------------------------------------------

vcat :: DocM ()
vcat = do
  DocState (SrcLoc f l c) n <- get
  let s = if n < c then line else space $ n - c
  _ <- s
  return ()

-- --------------------------------------------------------------------------

hsep :: DocM a -> DocM a
hsep sep = do
  s <- sep
  _ <- space 1
  return s

-- --------------------------------------------------------------------------

fsep :: DocM a -> DocM a
fsep sep = do
  -- should be like fsep from Text-PrettyPrint-HughesPJ
  s <- sep
  _ <- line
  return s

------------------------- pp utils -------------------------

parenList :: AstPretty ast => [ast a] -> DocM (SrcSpanInfo, [ast SrcSpanInfo])
parenList xs = genericParenList (format "(") (format ")") (infoPrettyList (layoutChoice fsep hsep $ format ",") xs)

-- --------------------------------------------------------------------------

braceList :: AstPretty ast => [ast a] -> DocM (SrcSpanInfo, [ast SrcSpanInfo])
braceList xs = genericParenList (format "{") (format "}") (infoPrettyList (layoutChoice fsep hsep $ format ",") xs)

-- --------------------------------------------------------------------------

myFsepSimple = layoutChoice fsep hsep

-- --------------------------------------------------------------------------

myFsep = layoutChoice fsep' hsep
  where
    fsep' dl = do
      PrettyMode mode <- ask
      let n = onsideIndent mode
      nest n >> fsep ( nest (-n) >> dl)

-- --------------------------------------------------------------------------

layoutChoice :: (DocM a -> DocM a) -> (DocM a -> DocM a) -> DocM a -> DocM a
layoutChoice a b f = do
  PrettyMode mode <- ask
  if layout mode == PPOffsideRule || layout mode == PPSemiColon
  then a f
  else b f
