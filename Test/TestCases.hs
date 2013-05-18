module Test.TestCases (prettyTestReference) where

import Language.Haskell.Exts.Annotated
import Data.List (find)

prettyTestReference :: PPLayout -> String -> Maybe (Module SrcSpanInfo)
prettyTestReference layout fileName = find (\x -> fileName == takeFileName x) $ testCases layout
  where
    takeFileName x = srcSpanFilename . srcInfoSpan $ ann x
    testCases :: PPLayout -> [Module SrcSpanInfo]
    testCases PPOffsideRule = offsideRule
    testCases PPSemiColon   = semiColonRule
    testCases PPInLine      = inlineRule
    testCases _             = []
    

offsideRule   = [
   Module (SrcSpanInfo (SrcSpan "WithKeyword.hs" 1 1 2 1)  [SrcSpan "WithKeyword.hs" 1 1 1 1, SrcSpan "WithKeyword.hs" 1 1 1 1, SrcSpan "WithKeyword.hs" 1 1 1 1, SrcSpan "WithKeyword.hs" 2 1 2 1]) Nothing [] [] [PatBind (SrcSpanInfo (SrcSpan "WithKeyword.hs" 1 1 1 9) []) (PVar (SrcSpanInfo (SrcSpan "WithKeyword.hs" 1 1 1 5) []) (Ident (SrcSpanInfo (SrcSpan "WithKeyword.hs" 1 1 1 5) []) "with")) Nothing (UnGuardedRhs (SrcSpanInfo (SrcSpan "WithKeyword.hs" 1 6 1 9)  [SrcSpan "WithKeyword.hs" 1 6 1 7]) (Lit (SrcSpanInfo (SrcSpan "WithKeyword.hs" 1 8 1 9) []) (Int (SrcSpanInfo (SrcSpan "WithKeyword.hs" 1 8 1 9) []) 1 "1"))) Nothing]
  ]
semiColonRule = []
inlineRule    = []


