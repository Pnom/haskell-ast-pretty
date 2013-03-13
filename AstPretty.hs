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

emptySpan :: DocM SrcSpan
emptySpan = do
  sp <- getPos
  return $ mkSrcSpan sp sp

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

raw :: String -> AstElement String
raw s = Ast $ do
  s' <- format s
  return (s, [s'])

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
  astPretty (Module pos mbHead os imp decls) = do
    -- myVcat
    sp <- getPos
    _ <- markLine
    (_, os') <- noInfoPrettyList myVcat os
    h'  <- maybePP mbHead
    (_, imp') <- fn mbHead imp
    (_, decls') <- fn mbHead decls
    ep <- getPos
    let span = SrcSpanInfo (mkSrcSpan sp ep) []
    return $ Module span h' os' imp' decls'
    where
      fn :: AstPretty ast => Maybe b -> [ast a] -> DocM (SrcSpanInfo, [ast SrcSpanInfo])
      fn b = if isJust b then topLevel else noInfoPrettyList empty
  astPretty (XmlPage pos _mn os n attrs mattr cs) = undefined
  astPretty (XmlHybrid pos mbHead os imp decls n attrs mattr cs) = undefined

--------------------------  Module Header ------------------------------

instance AstPretty ModuleHead where
  astPretty (ModuleHead _ m mbWarn mbExportList) = do
    -- mySep
    sp <- format "module"
    _  <- space 1
    m' <- astPretty m
    _  <- fsep
    w' <- maybePP mbWarn
    _  <- fsep
    el <- maybePP mbExportList
    _  <- fsep
    cp <- format "where"
    let span = SrcSpanInfo (mergeSrcSpan sp cp) [sp, cp]
    return $ ModuleHead span m' w' el
{-
 -- mySep
  astPretty (ModuleHead _ m mbWarn mbExportList) =
    resultPretty $ startPretty ModuleHead
      `point` format "module"
      `separate` space 1
      `ast` astPretty m
      `separate` fsep
      `ast` maybePP mbWarn
      `ast` maybePP mbExportList
      `separate` fsep
      `point` format "where"
-}

-- --------------------------------------------------------------------------

instance AstPretty WarningText where
  astPretty w = case w of
    (DeprText _ s) -> impl DeprText "{-# DEPRECATED" s
    (WarnText _ s) -> impl WarnText "{-# WARNING"    s
    where
      -- mySep
      impl f c s = resultPretty $ startPretty f <> infoPoint c <> sepPoint hsep <\/> raw s <> sepPoint fsep <> infoPoint "#}"


-- --------------------------------------------------------------------------

instance AstPretty ModuleName where
  astPretty (ModuleName _ s) = do
    span <- format s
    return $ ModuleName (noInfoSpan span) s

-- --------------------------------------------------------------------------

instance AstPretty ExportSpecList where
  astPretty (ExportSpecList _ especs)  = do
    (span, es) <- parenList especs
    return $ ExportSpecList span es

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

instance AstPretty ImportDecl where
  astPretty (ImportDecl _ mod qual src pkg asMod specs)= do
    -- mySep
    _ <- markLine
    sp <- format "import"
    _  <- space 1
    src'  <- if src  then format "{-# SOURCE #-}" else emptySpan
    qual' <- if qual then format "qualified"      else emptySpan
    m' <- astPretty mod
    undefined

-------------------------  Declarations ------------------------------

instance AstPretty Decl where astPretty = undefined

------------------------- Pragmas ---------------------------------------

instance AstPretty ModulePragma where
  astPretty (LanguagePragma _ []) = do
    -- myFsep
    sp <- getPos
    return $ LanguagePragma (noInfoSpan $ mkSrcSpan sp sp) []

  astPretty (LanguagePragma _ ns) = do
    -- myFsep
    (span, ls) <- genericParenList (format "{-# LANGUAGE") (format "#-}") $ infoPrettyList (punctuate (format ",") myFsep) ns
    return $ LanguagePragma span ls

  astPretty (OptionsPragma _ mbTool s) = do
    -- myFsep
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
    -- myFsep
    sp <- format "{-# ANN"
    ann'  <- astPretty ann
    cp <- format "#-}"
    let span = SrcSpanInfo (mergeSrcSpan sp cp) [sp, cp]
    return $ AnnModulePragma span ann'

-- --------------------------------------------------------------------------

instance AstPretty Annotation where
  astPretty (Ann _ n e) = do
    -- myFsep
    sp <- getPos
    n' <- astPretty n
    e' <- astPretty e
    ep <- getPos
    let span = noInfoSpan $ mkSrcSpan sp ep
    return $ Ann span n' e'

  astPretty (TypeAnn _ n e) = do
    -- myFsep
    sp <- getPos
    t <- format "type"
    n' <- astPretty n
    e' <- astPretty e
    ep <- getPos
    let span = SrcSpanInfo (mkSrcSpan sp ep) [t]
    return $ TypeAnn span n' e'

  astPretty (ModuleAnn _ e) = do
    -- myFsep
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
  sp <- getPos
  return (noInfoSpan $ mkSrcSpan sp sp, [])

noInfoPrettyList sep (e:es) = do
  sp <- getPos
  e' <- astPretty e
  xs <- foldM (\ acc i -> do
    p <- sep
    x <- astPretty i
    return (x:acc))
    [e']
    es
  ep <- getPos
  return (noInfoSpan $ mkSrcSpan sp ep, reverse xs)

-- --------------------------------------------------------------------------

infoPrettyList :: AstPretty ast =>
  DocM SrcSpan -> [ast a] -> DocM (SrcSpanInfo, [ast SrcSpanInfo])

infoPrettyList _ [] = do
  b <- getPos
  return (noInfoSpan $ mkSrcSpan b b, [])

infoPrettyList sep (e:es) = do
  sp <- getPos
  e' <- astPretty e
  (ps, xs) <- foldM (\ (ps, xs) i -> do
    p <- sep
    x <- astPretty i
    return (p:ps, x:xs))
    ([], [e'])
    es
  ep <- getPos
  let span = SrcSpanInfo (mkSrcSpan sp ep) (reverse ps )
  return (span, reverse xs)

-- --------------------------------------------------------------------------

punctuate :: DocM SrcSpan -> DocM () -> DocM SrcSpan
punctuate p sep = do
  p' <- p
  _  <- sep
  return p'

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

maybePP :: AstPretty ast => Maybe (ast a) -> DocM (Maybe (ast SrcSpanInfo))
maybePP Nothing = return Nothing
maybePP (Just a) = do
  a' <- astPretty a
  return $ Just a'

-- --------------------------------------------------------------------------

parenList :: AstPretty ast => [ast a] -> DocM (SrcSpanInfo, [ast SrcSpanInfo])
parenList xs = let sep = punctuate (format ",") (layoutChoice fsep hsep) in
  genericParenList (format "(") (format ")") $ infoPrettyList sep xs

-- --------------------------------------------------------------------------

braceList :: AstPretty ast => [ast a] -> DocM (SrcSpanInfo, [ast SrcSpanInfo])
braceList xs = let sep = punctuate (format ",") (layoutChoice fsep hsep) in
  genericParenList (format "{") (format "}") $ infoPrettyList sep xs

-- --------------------------------------------------------------------------

-- Wrap in braces and semicolons, with an extra space at the start in
-- case the first doc begins with "-", which would be scanned as {-
flatBlock xs =
  let
    sep = punctuate (format ";") hsep
    ob = punctuate (format "{") (space 1)
    cb = format "}" in
  genericParenList ob cb $ infoPrettyList sep xs

-- Same, but put each thing on a separate line
prettyBlock xs =
  let
    sep = punctuate (format ";") vcat
    ob = punctuate (format "{") (space 1)
    cb = format "}" in
  genericParenList ob cb $ infoPrettyList sep xs

-- --------------------------------------------------------------------------

topLevel :: AstPretty ast => [ast a] -> DocM (SrcSpanInfo, [ast SrcSpanInfo])
topLevel dl = do
  PrettyMode mode _ <- ask
  case layout mode of
    PPOffsideRule -> do
      _ <- vcat
      noInfoPrettyList vcat dl
    PPSemiColon -> do
      _ <- vcat
      prettyBlock dl
    PPInLine -> do
      _ <- vcat
      prettyBlock dl
    PPNoLayout -> do
      _ <- space 1
      flatBlock dl

-- --------------------------------------------------------------------------

mySep :: AstPretty ast =>
  DocM SrcSpan -> [ast a] -> DocM (SrcSpanInfo, [ast SrcSpanInfo])
-- mySep prototype

mySep _ [] = error "Internal error: mySep"

mySep _ [x] = infoPrettyList undefined [x]

mySep p (x:xs) = do
  sp <- getPos
  x' <- astPretty x
  p' <- p
  _ <- space 1
  (ps, xs') <- infoPrettyList (punctuate p fsep) xs
  ep <- getPos

  let span = SrcSpanInfo (mkSrcSpan sp ep) (p' : srcInfoPoints ps)
  return (span, x' : xs')

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


