{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

import AstPretty
import AstVerify
import AstSerial
import Control.Monad.State
import Control.Monad.Reader

import Debug.Trace
import Test.SmallCheck
import Test.SmallCheck.Series

import GHC.Generics

import Language.Haskell.Exts.Annotated
import Language.Haskell.Exts.Pretty

deriving instance Generic DocState
instance Monad m => Serial m DocState

-- --------------------------------------------------------------------------

isDocStateCorrect (DocState p n) = not (null $ srcFilename p) && srcLine p > 0 && srcColumn p > 0 && n >= 0

-- --------------------------------------------------------------------------

propAstCorrect :: (Monad m, AstVerify ast, AstPretty ast) => DocState -> ast a -> Property m
propAstCorrect start ast =  isDocStateCorrect start ==>
  isAstCorrect $ renderWithDefMode start (astPretty ast)

-- --------------------------------------------------------------------------

propLineCorrect :: (Monad m, AstVerify ast, AstPretty ast) => DocState -> ast a -> Property m
propLineCorrect start@(DocState pos n) ast = isDocStateCorrect start && n == 0 ==>
  let
    ast' = renderWithDefMode start $ do
      _ <- line
      astPretty ast
  in
    isAstCorrect ast' &&
    (1 + srcLine pos, 1) == (srcSpanStart.srcInfoSpan $ ann ast')

propLineModule :: Monad m => DocState -> String -> Property m
propLineModule st name = propLineCorrect st (ModuleName undefined name)

-- --------------------------------------------------------------------------

propNestCorrect :: (Monad m, AstVerify ast, AstPretty ast) => DocState -> ast a -> Int -> Property m
propNestCorrect start@(DocState pos n) ast nst = isDocStateCorrect start && nst >= 0 ==>
  let
  ast' = renderWithDefMode start $ do
    _ <- nest nst
    _ <- line
    astPretty ast

  in
    isAstCorrect ast' &&
    (1 + srcLine pos, if n + nst == 0 then 1 else n + nst) ==
      (srcSpanStart.srcInfoSpan $ ann ast')

propNestModule :: Monad m => DocState -> String -> Int -> Property m
propNestModule st name n = propNestCorrect st (ModuleName undefined name) n

-- --------------------------------------------------------------------------

propSpaceCorrect :: (Monad m, AstVerify ast, AstPretty ast) => DocState -> ast a -> Int -> Property m
propSpaceCorrect start@(DocState pos _) ast s = isDocStateCorrect start && s >= 0 ==>
  let
  ast' = renderWithDefMode start $ do
    _ <- space s
    astPretty ast
  in
    isAstCorrect ast' &&
    (srcLine pos, s + srcColumn pos) == (srcSpanStart.srcInfoSpan $ ann ast')

propSpaceModule :: Monad m => DocState -> String -> Int -> Property m
propSpaceModule st name sp = propSpaceCorrect st (ModuleName undefined name) sp

-- --------------------------------------------------------------------------

propAstCorrectAll :: (Monad m, Show (ast SrcSpanInfo), AstVerify ast, AstPretty ast) => DocState -> [ast a] -> Property m
propAstCorrectAll start@(DocState p n) asts = let pretty i = renderWithDefMode start $ astPretty i in
  srcLine p > 0 && srcColumn p > 0 && n >= 0 ==>
  all isAstCorrect (map pretty asts)

-- --------------------------------------------------------------------------

propSrcSpanCorrect s@(SrcSpan fl sl sc el ec) = sl > 0 && sc > 0 && el > 0 && ec > 0
  ==> isSrcSpanCorrect s

-- --------------------------------------------------------------------------

propModuleName st name = propAstCorrect st $ ModuleName undefined name

-- --------------------------------------------------------------------------

propCName st n = (isAstCorrect n) ==> propAstCorrectAll st [VarName undefined n, ConName undefined n]

-- --------------------------------------------------------------------------

zeroSt = DocState (SrcLoc "unknown.hs"  1  1) 0
