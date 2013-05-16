import Language.Haskell.Exts.PrettyAst
import Language.Haskell.Exts.Annotated
import System.FilePath
import Data.Traversable
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

simpleSpanInfo :: SrcSpanInfo -> ((Int, Int, Int, Int), [(Int, Int, Int, Int)])
simpleSpanInfo s = (simpleSpan $ srcInfoSpan s, map simpleSpan (srcInfoPoints s))
  where
    simpleSpan s = (srcSpanStartLine s, srcSpanStartColumn s, srcSpanEndLine s, srcSpanEndColumn s)

reportPrettifying :: PPLayout -> FilePath -> IO ()
reportPrettifying l f = do
  putStrLn ""
  putStrLn $ "File: " ++ f ++ "; Layout: " ++
    case l of { PPOffsideRule -> "PPOffsideRule"; PPSemiColon -> "PPSemiColon"; PPInLine -> "PPInLine"; PPNoLayout -> "PPNoLayout" }

  ParseOk parsingRes <- parseFile f

  let (prettyRes, trace) = renderWithTrace f (setLayoutToDefMode l) parsingRes

  putStrLn "raw parsing result"
  putStrLn $ show parsingRes
  putStrLn ""
  putStrLn "raw  result of ast prettifying"
  putStrLn $ show prettyRes
  putStrLn ""

  putStrLn "short parsing result"
  putStrLn . show $ fmap simpleSpanInfo parsingRes
  putStrLn ""

  putStrLn "short result of ast prettifying"
  putStrLn . show $ fmap simpleSpanInfo prettyRes
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