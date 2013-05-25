import Language.Haskell.Exts.PrettyAst
import Language.Haskell.Exts.Annotated
import System.FilePath
import System.Directory
import System.Environment (getArgs)
import Data.List (intercalate)
import Control.Monad (mapM)
import Test.TestCases
import Data.Traversable (traverse)

-- todo
-- update cabal file
-- let user chose layout for tests

main :: IO ()
main = runTests =<< getArgs

-- Run the selected tests - or all of them if the supplied list is empty
runTests :: [FilePath] -> IO ()
runTests testsToRun = do
    files <- if null testsToRun then getDirectoryContents examplesDir else return testsToRun
    putStrLn "Testing PrettyAst:"
    let
      test l = testLayout l $ map (examplesDir </>) files
    _ <- sequence $ map test [PPOffsideRule, PPSemiColon, PPInLine, PPNoLayout]
    putStrLn "\nAll parsing tests completed!\n"

-- run tests with specific layout
testLayout :: PPLayout -> [FilePath] -> IO ()
testLayout layout ts = mapM (\f -> testModule layout f) ts >> return ()

-- run test with specific layout on the one file
testModule :: PPLayout -> FilePath -> IO ()
testModule layout filePath = do
  ParseOk parsingRes <- parseFile filePath
  let
    fileName   = takeFileName filePath
    prettyTest = renderWithMode fileName (setLayoutToDefMode layout) parsingRes
    error msg = do
      _ <- reportPrettifying layout filePath
      fail $ fileName ++ " " ++ show layout ++ " : " ++ msg
  case prettyTestReference layout fileName of
    Nothing  -> error "Undefined test case"
    Just ref ->
      if ref == prettyTest
        then return ()
        else error "failed test"

-- setup layout to PPHsMode
setLayoutToDefPRMode :: PPLayout -> PPHsMode
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

setLayoutToDefMode :: PPLayout -> PrettyMode
setLayoutToDefMode l = PrettyMode (setLayoutToDefPRMode l) style


-- change filename in SrcSpanInfo
setSpanFilename :: String -> SrcSpanInfo -> SrcSpanInfo
setSpanFilename f (SrcSpanInfo s ps) = SrcSpanInfo (changeSpan s) (map changeSpan ps)
  where
    changeSpan (SrcSpan _ sl sc el ec) = SrcSpan f sl sc el ec

-- use this data to simplify output for show SrcSpanInfo
data SrcSpanInfo' = SrcSpanInfo' SrcSpanInfo
instance Show SrcSpanInfo' where
  show (SrcSpanInfo' (SrcSpanInfo s ps)) =
    "(SrcSpanInfo (" ++ simpleSpan s ++ ") [" ++ intercalate ", " (map simpleSpan ps) ++ "])"
    where simpleSpan (SrcSpan f sl sc el ec) = "SrcSpan \"" ++ f ++ "\" " ++ show sl ++ " " ++ show sc ++ " " ++ show el ++ " " ++ show ec

simplifySpanInfo :: SrcSpanInfo -> ((Int, Int, Int, Int), [(Int, Int, Int, Int)])
simplifySpanInfo s = (simpleSpan $ srcInfoSpan s, map simpleSpan (srcInfoPoints s))
  where
    simpleSpan s = (srcSpanStartLine s, srcSpanStartColumn s, srcSpanEndLine s, srcSpanEndColumn s)

-- make the test and print detailed result
reportPrettifying :: PPLayout -> FilePath -> IO ()
reportPrettifying l filePath = do
  putStrLn ""
  putStrLn $ "File: " ++ filePath ++ "; Layout: " ++ show l

  ParseOk parsingRes <- parseFile filePath
  let
    fileName = takeFileName filePath
    (prettyRes, trace) = renderWithTrace fileName (setLayoutToDefMode l) parsingRes
    standartPrettyStr  = prettyPrintWithMode (setLayoutToDefPRMode l) parsingRes
    ParseOk standartPretty = parseFileContents standartPrettyStr

  putStrLn "raw result of ast prettifying"
  putStrLn . show $ fmap SrcSpanInfo' prettyRes
  putStrLn ""

  putStrLn "parsing result"
  putStrLn . show $ fmap (\x -> simplifySpanInfo $ setSpanFilename fileName x) parsingRes
  putStrLn ""

  putStrLn "result of standart prettifying"
  putStrLn . show $ fmap (\x -> simplifySpanInfo $ setSpanFilename fileName x) standartPretty
  putStrLn ""

  putStrLn "result of ast prettifying"
  putStrLn . show $ fmap simplifySpanInfo prettyRes
  putStrLn ""

  putStrLn "ast prettifying trace:"
  putStrLn $ show trace

  putStrLn "----------------------------------------"
  putStrLn "exactPrint parsingRes:"
  putStrLn $ exactPrint parsingRes []
  putStrLn "----------------------------------------"
  putStrLn "prettyPrintWithMode parsingRes:"
  putStrLn standartPrettyStr
  putStrLn "----------------------------------------"
  putStrLn "exactPrint prettyRes:"
  putStrLn $ exactPrint prettyRes []
  putStrLn "----------------------------------------"
  putStrLn ""

reportPrettyFiles :: PPLayout -> [FilePath] -> IO [()]
reportPrettyFiles l fs = traverse (\d -> reportPrettifying l $ examplesDir </> d) fs

instance Show PPLayout where
  show PPOffsideRule = "PPOffsideRule"
  show PPSemiColon   = "PPSemiColon"
  show PPInLine      = "PPInLine"
  show PPNoLayout    = "PPNoLayout"

examplesDir :: FilePath
examplesDir = "Test" </> "examples"

testFiles :: [FilePath]
testFiles = ["WithKeyword.hs"
  ,"Ex1.hs"
  ,"SimpleDeriving.hs"
  ,"EmptyContext.hs"
  ,"ListComp1.hs"
  ]