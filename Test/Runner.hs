import Language.Haskell.Exts.PrettyAst
import Language.Haskell.Exts.Annotated
import System.FilePath
import Data.List

setLayoutToDefPRMode l = let m = defaultMode in
  PPHsMode
    (classIndent m)
    (doIndent m)
    (caseIndent   m)
    (letIndent    m)
    (whereIndent  m)
    (onsideIndent m)
    (spacing      m)
    l
    (linePragmas m)

setLayoutToDefMode l = PrettyMode (setLayoutToDefPRMode l) style

setSpanFilename :: String -> SrcSpanInfo -> SrcSpanInfo
setSpanFilename f (SrcSpanInfo s ps) = SrcSpanInfo (changeSpan s) (map changeSpan ps)
  where
    changeSpan (SrcSpan _ sl sc el ec) = SrcSpan f sl sc el ec

data SrcSpanInfo' = SrcSpanInfo' SrcSpanInfo
instance Show SrcSpanInfo' where
  show (SrcSpanInfo' (SrcSpanInfo s ps)) =
    "(SrcSpanInfo (" ++ simpleSpan s ++ ") [" ++ intercalate ", " (map simpleSpan ps) ++ "])"
    where simpleSpan (SrcSpan f sl sc el ec) = "SrcSpan \"" ++ f ++ "\" " ++ show sl ++ " " ++ show sc ++ " " ++ show el ++ " " ++ show ec

reportPrettifying :: PPLayout -> FilePath -> IO ()
reportPrettifying l filePath = do
  putStrLn ""
  putStrLn $ "File: " ++ filePath ++ "; Layout: " ++
    case l of { PPOffsideRule -> "PPOffsideRule"; PPSemiColon -> "PPSemiColon"; PPInLine -> "PPInLine"; PPNoLayout -> "PPNoLayout" }

  ParseOk parsingRes <- parseFile filePath
  let fileName = takeFileName filePath

  let (prettyRes, trace) = renderWithTrace fileName (setLayoutToDefMode l) parsingRes

  putStrLn "parsing result"
  putStrLn . show $ fmap (\x -> SrcSpanInfo' $ setSpanFilename fileName x) parsingRes
  putStrLn ""

  putStrLn "result of ast prettifying"
  putStrLn . show $ fmap SrcSpanInfo' prettyRes
  putStrLn ""

  putStrLn "ast prettifying trace:"
  putStrLn $ show trace

  putStrLn "----------------------------------------"
  putStrLn "prettyPrintWithMode parsingRes:"
  putStrLn $ prettyPrintWithMode (setLayoutToDefPRMode l) parsingRes
  putStrLn "----------------------------------------"
  putStrLn "exactPrint parsingRes:"
  putStrLn $ exactPrint parsingRes []
  putStrLn "----------------------------------------"
  putStrLn "exactPrint prettyRes:"
  putStrLn $ exactPrint prettyRes []
  putStrLn "----------------------------------------"
  putStrLn ""

reportPrettifyingAllLayouts :: FilePath -> IO ()
reportPrettifyingAllLayouts f = do
  reportPrettifying PPOffsideRule f
  reportPrettifying PPSemiColon   f
  reportPrettifying PPInLine      f

data TestElem = TestElem {
  input    :: Module SrcSpanInfo,
  example :: (PPLayout -> Module SrcSpanInfo)
}

testElemWithLayout l e = (file, pretty == example e l)
  where
    file   = srcSpanFilename . srcInfoSpan . ann $ input e
    pretty = renderWithMode file (setLayoutToDefMode l) $ input e

undefinedModule = Module (SrcSpanInfo (SrcSpan "undefinedModule.hs" 1 1 1 1) []) Nothing [] [] []

withKeyword = TestElem
  (Module (SrcSpanInfo (SrcSpan "WithKeyword.hs" 1 1 2 1)  [SrcSpan "WithKeyword.hs" 1 1 1 1, SrcSpan "WithKeyword.hs" 1 1 1 1, SrcSpan "WithKeyword.hs" 1 1 1 1, SrcSpan "WithKeyword.hs" 2 1 2 1, SrcSpan "WithKeyword.hs" 2 1 2 1]) Nothing [] [] [PatBind (SrcSpanInfo (SrcSpan "WithKeyword.hs" 1 1 1 9) []) (PVar (SrcSpanInfo (SrcSpan "WithKeyword.hs" 1 1 1 5) []) (Ident (SrcSpanInfo (SrcSpan "WithKeyword.hs" 1 1 1 5) []) "with")) Nothing (UnGuardedRhs (SrcSpanInfo (SrcSpan "WithKeyword.hs" 1 6 1 9)  [SrcSpan "WithKeyword.hs" 1 6 1 7]) (Lit (SrcSpanInfo (SrcSpan "WithKeyword.hs" 1 8 1 9) []) (Int (SrcSpanInfo (SrcSpan "WithKeyword.hs" 1 8 1 9) []) 1 "1"))) Nothing])
  (\l -> case l of
    PPOffsideRule -> Module (SrcSpanInfo (SrcSpan "WithKeyword.hs" 1 1 2 1)  [SrcSpan "WithKeyword.hs" 1 1 1 1, SrcSpan "WithKeyword.hs" 1 1 1 1, SrcSpan "WithKeyword.hs" 1 1 1 1, SrcSpan "WithKeyword.hs" 2 1 2 1]) Nothing [] [] [PatBind (SrcSpanInfo (SrcSpan "WithKeyword.hs" 1 1 1 9) []) (PVar (SrcSpanInfo (SrcSpan "WithKeyword.hs" 1 1 1 5) []) (Ident (SrcSpanInfo (SrcSpan "WithKeyword.hs" 1 1 1 5) []) "with")) Nothing (UnGuardedRhs (SrcSpanInfo (SrcSpan "WithKeyword.hs" 1 6 1 9)  [SrcSpan "WithKeyword.hs" 1 6 1 7]) (Lit (SrcSpanInfo (SrcSpan "WithKeyword.hs" 1 8 1 9) []) (Int (SrcSpanInfo (SrcSpan "WithKeyword.hs" 1 8 1 9) []) 1 "1"))) Nothing]
    _ ->  undefinedModule)