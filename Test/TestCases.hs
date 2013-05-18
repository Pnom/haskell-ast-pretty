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
   ,Module (SrcSpanInfo (SrcSpan "Ex1.hs" 1 1 3 1) [SrcSpan "Ex1.hs" 1 1 1 1, SrcSpan "Ex1.hs" 1 1 1 1, SrcSpan "Ex1.hs" 1 1 1 1, SrcSpan "Ex1.hs" 3 1 3 1]) Nothing [] [] [PatBind (SrcSpanInfo (SrcSpan "Ex1.hs" 1 1 2 15) [SrcSpan "Ex1.hs" 2 3 2 8]) (PVar (SrcSpanInfo (SrcSpan "Ex1.hs" 1 1 1 4) []) (Ident (SrcSpanInfo (SrcSpan "Ex1.hs" 1 1 1 4) []) "yes")) Nothing (UnGuardedRhs (SrcSpanInfo (SrcSpan "Ex1.hs" 1 5 1 8) [SrcSpan "Ex1.hs" 1 5 1 6]) (Lit (SrcSpanInfo (SrcSpan "Ex1.hs" 1 7 1 8) []) (Int (SrcSpanInfo (SrcSpan "Ex1.hs" 1 7 1 8) []) 1 "1"))) (Just (BDecls (SrcSpanInfo (SrcSpan "Ex1.hs" 2 9 2 15) [SrcSpan "Ex1.hs" 2 9 2 9]) [PatBind (SrcSpanInfo (SrcSpan "Ex1.hs" 2 9 2 14) []) (PVar (SrcSpanInfo (SrcSpan "Ex1.hs" 2 9 2 10) []) (Ident (SrcSpanInfo (SrcSpan "Ex1.hs" 2 9 2 10) []) "x")) Nothing (UnGuardedRhs (SrcSpanInfo (SrcSpan "Ex1.hs" 2 11 2 14) [SrcSpan "Ex1.hs" 2 11 2 12]) (Lit (SrcSpanInfo (SrcSpan "Ex1.hs" 2 13 2 14) []) (Int (SrcSpanInfo (SrcSpan "Ex1.hs" 2 13 2 14) []) 1 "1"))) Nothing]))]
  ]
semiColonRule = []
inlineRule    = []


