{-# LANGUAGE DeriveGeneric #-}
import AstSerial
import AstPretty
import AstVerify
import Control.Monad.State

import Test.SmallCheck
import Test.SmallCheck.Series
import Test.SmallCheck.Property
import GHC.Generics
import Language.Haskell.Exts.Annotated
import Language.Haskell.Exts.Pretty

import Data.List


isDocStateCorrect (DocState p n) = not (null $ srcFilename p) && srcLine p > 0 && srcColumn p > 0 && n >= 0

prettyResult :: DocState -> DocM (ast SrcSpanInfo) -> ast SrcSpanInfo
prettyResult start m = fst $ runState m start

-- --------------------------------------------------------------------------

propAstCorrect :: (AstVerify ast, AstPretty ast) => DocState -> ast a -> Property
propAstCorrect start ast =  isDocStateCorrect start ==>
        isAstCorrect $ prettyResult start (astPretty ast)

-- --------------------------------------------------------------------------

propLineCorrect :: (AstVerify ast, AstPretty ast) => DocState -> ast a -> Property
propLineCorrect start@(DocState pos n) ast = isDocStateCorrect start && n == 0 ==>
        let 
        ast' = prettyResult start $ do 
                _ <- line
                astPretty ast
        in
                isAstCorrect ast' &&
                (1 + srcLine pos, 1) == (srcSpanStart.srcInfoSpan $ ann ast')

-- smallCheck 5 propLineModule
propLineModule :: DocState -> ModuleName l -> Property
propLineModule st name = propLineCorrect st name

-- --------------------------------------------------------------------------

propNestCorrect :: (Show (ast SrcSpanInfo), AstVerify ast, AstPretty ast) => DocState -> ast a -> Int -> Property
propNestCorrect start@(DocState pos n) ast nst = isDocStateCorrect start && nst >= 0 ==>
        let 
        ast' = prettyResult start $ do 
                _ <- nest nst
                _ <- line
                astPretty ast
                
        in
                isAstCorrect ast' &&
                (1 + srcLine pos, if n + nst == 0 then 1 else n + nst) == 
                        (srcSpanStart.srcInfoSpan $ ann ast')

propNestModule :: DocState -> ModuleName l -> Int -> Property
propNestModule st name n = propNestCorrect st name n

-- --------------------------------------------------------------------------

propSpaceCorrect :: (AstVerify ast, AstPretty ast) => DocState -> ast a -> Int -> Property
propSpaceCorrect start@(DocState pos _) ast s = isDocStateCorrect start && s >= 0 ==>
        let 
        ast' = prettyResult start $ do 
                _ <- space s
                astPretty ast
        in
                isAstCorrect ast' &&
                (srcLine pos, s + srcColumn pos) == (srcSpanStart.srcInfoSpan $ ann ast')

propSpaceModule :: DocState -> ModuleName l -> Int -> Property
propSpaceModule st name sp = propSpaceCorrect st name sp

-- --------------------------------------------------------------------------

propAstCorrectAll :: (Show (ast SrcSpanInfo), AstVerify ast, AstPretty ast) => DocState -> [ast a] -> Property
propAstCorrectAll start@(DocState p n) asts = let pretty i = fst $ (runState $ astPretty i) start in
        srcLine p > 0 && srcColumn p > 0 && n >= 0 ==>
        forAllElem (map pretty asts) isAstCorrect

-- --------------------------------------------------------------------------

propSrcSpanCorrect s@(SrcSpan fl sl sc el ec) = sl > 0 && sc > 0 && el > 0 && ec > 0
                    ==> isSrcSpanCorrect s
       
-- --------------------------------------------------------------------------

propModuleName st name = propAstCorrect st $ ModuleName undefined name 

-- --------------------------------------------------------------------------

propCName st n = (isAstCorrect n) ==> propAstCorrectAll st [VarName undefined n, ConName undefined n]



