import Language.Haskell.Exts.PrettyAst
import Language.Haskell.Exts.Annotated
import System.FilePath
import Data.Traversable
import Data.Either

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
  testDoc PPOffsideRule f
  testDoc PPSemiColon   f
  testDoc PPInLine      f

data TestElem = TestElem {
  input    :: Module SrcSpanInfo,
  example :: (PPLayout -> Module SrcSpanInfo)
}

testElemWithLayout e l = (file, pretty == example e l)
  where
    file   = srcSpanFilename . srcInfoSpan . ann $ input e
    pretty = renderWithMode file (setLayoutToDefMode l) $ input e

