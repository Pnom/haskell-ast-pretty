module AstVerify ( AstVerify(isAstCorrect), isSrcSpanCorrect, isSrcSpanInfoCorrect) where

import Language.Haskell.Exts.Annotated
import Language.Haskell.Exts.Pretty


class Annotated ast => AstVerify ast where
        isAstCorrect :: ast SrcSpanInfo  -> Bool

-- --------------------------------------------------------------------------

isSrcSpanCorrect  (SrcSpan fl sl sc el ec) = let moreThenZero = sl >= 0 && sc >= 0 && el >= 0 && ec >= 0 in
        ((sl == el && sc <= ec) || sl < el) && moreThenZero

-- --------------------------------------------------------------------------        

isSpanMatchString (SrcSpan _ sl sc el ec) str = (length str) == ec - sc && sl == el

-- --------------------------------------------------------------------------

isSrcSpanInfoCorrect  (SrcSpanInfo s []) = isSrcSpanCorrect s
isSrcSpanInfoCorrect  (SrcSpanInfo s (p:ps)) = isSrcSpanCorrect s && checkSpan p && loop ps
        where
        loop []       = True
        loop [_]      = True
        loop (x:y:zs) = checkSpan y && x < y && loop (y:zs)
        
        checkSpan x = isSrcSpanCorrect x && mergeSrcSpan s x == s && srcSpanFilename x == srcSpanFilename s

-- --------------------------------------------------------------------------

data ParenthesisTag = WithParen | WithoutParen
        deriving (Show, Eq)
        
-- --------------------------------------------------------------------------
-- ModuleName instance

instance AstVerify ModuleName where
        isAstCorrect (ModuleName (SrcSpanInfo span []) str) = isSrcSpanCorrect span && isSpanMatchString span str
        isAstCorrect (ModuleName _ _) = False        

-- --------------------------------------------------------------------------
--  CName instance 

instance AstVerify CName where

        isAstCorrect cname = let 
                span = srcInfoSpan $ ann cname
                ps = srcInfoPoints $ ann cname
                name = case cname of
                        (VarName _ n) -> n
                        (ConName _ n) -> n
                in
                        (isSrcSpanCorrect.srcInfoSpan $ ann cname) &&
                        null ps &&
                        span == (srcInfoSpan $ ann name)

-- --------------------------------------------------------------------------
-- SpecialCon instance

instance AstVerify SpecialCon where
        isAstCorrect s = let 
                span = srcInfoSpan $ ann s
                ps = srcInfoPoints $ ann s
                in 
                        isSrcSpanCorrect span &&
                        null ps &&
                        isSpanMatchString span (specialConContent s)

-- --------------------------------------------------------------------------

specialConContent (UnitCon _) = "()"
specialConContent (ListCon _) = "[]"
specialConContent (FunCon  _) = "->"
specialConContent (Cons    _) = ":"
specialConContent (UnboxedSingleCon _) = "(# #)"
specialConContent (TupleCon _ b n) = let hash = if b == Unboxed then "#" else "" in 
        "(" ++ hash ++ replicate (n-1) ',' ++ hash ++ ")"

-- --------------------------------------------------------------------------
-- ExportSpec instance 

instance AstVerify ExportSpec where
        
        isAstCorrect = undefined

-- --------------------------------------------------------------------------
-- QName instance

instance AstVerify QName where
        isAstCorrect qn@(Qual l m n) = let 
                        span = srcInfoSpan l
                        nameSpan = srcInfoSpan $ ann n
                        moduleSpan = srcInfoSpan $ ann m
                in
                        case (qNameParenthesisTag qn, srcInfoPoints l) of
                                (WithParen, [openParen, qNameSpan, closeParen]) -> 
                                        isSrcSpanInfoCorrect l &&
                                        isAstCorrect m && 
                                        (isNameCorrect WithoutParen n) &&
                                        srcSpanEnd moduleSpan < srcSpanStart nameSpan &&  -- for '.' between module name and name 
                                        (mergeSrcSpan moduleSpan nameSpan) == qNameSpan && 
                                        (mergeSrcSpan openParen closeParen) == span
                                _ -> 
                                        (null $  srcInfoPoints l) &&
                                        isSrcSpanInfoCorrect l &&
                                        isAstCorrect m && 
                                        isAstCorrect n &&
                                        srcSpanEnd moduleSpan < srcSpanStart nameSpan &&
                                        (mergeSrcSpan moduleSpan nameSpan) == span
        
        isAstCorrect qn@(UnQual l n) = let 
                        span = srcInfoSpan l
                        nameSpan = srcInfoSpan $ ann n
                in
                        case (qNameParenthesisTag qn,  srcInfoPoints l) of
                                (WithParen, [openParen, qNameSpan, closeParen]) ->                            
                                        isSrcSpanInfoCorrect l &&
                                        (isNameCorrect WithoutParen n) &&
                                        nameSpan == qNameSpan && 
                                        (mergeSrcSpan openParen closeParen) == span
                                _ ->
                                        (null $ srcInfoPoints l) &&
                                        isSrcSpanInfoCorrect l && 
                                        isAstCorrect n &&
                                        nameSpan == span
                        
        isAstCorrect qn@(Special l n) = let 
                        span = srcInfoSpan l
                        specSpan = srcInfoSpan $ ann n
                in
                        case (qNameParenthesisTag qn,  srcInfoPoints l) of
                                (WithParen, [openParen, qNameSpan, closeParen]) ->                            
                                        isSrcSpanInfoCorrect l &&
                                        isAstCorrect n &&
                                        specSpan == qNameSpan && 
                                        (mergeSrcSpan openParen closeParen) == span
                                _ ->
                                        (null $ srcInfoPoints l) &&
                                        isSrcSpanInfoCorrect l && 
                                        isAstCorrect n &&
                                        specSpan == span

-- --------------------------------------------------------------------------

qNameParenthesisTag :: (SrcInfo l) => QName l -> ParenthesisTag
qNameParenthesisTag (UnQual _    (Symbol _ _)) = WithParen
qNameParenthesisTag (Qual   _  _ (Symbol _ _)) = WithParen
qNameParenthesisTag (Special _ (Cons _))       = WithParen
qNameParenthesisTag (Special _ (FunCon _))     = WithParen
qNameParenthesisTag _                          = WithoutParen

-- --------------------------------------------------------------------------

instance AstVerify Name where
        isAstCorrect name@(Symbol _ _) = isNameCorrect WithParen name
        isAstCorrect name = isNameCorrect WithoutParen name

-- --------------------------------------------------------------------------

nameContent :: Name l -> String
nameContent (Ident  _ s) = s
nameContent (Symbol _ s) = s

-- --------------------------------------------------------------------------

isNameCorrect :: ParenthesisTag -> Name SrcSpanInfo -> Bool
isNameCorrect WithoutParen name = let 
                span = srcInfoSpan $ ann name
                ps = srcInfoPoints $ ann name
        in
                isSrcSpanCorrect span && 
                null ps && 
                isSpanMatchString span (nameContent name)
        
isNameCorrect WithParen (Symbol spI@(SrcSpanInfo span [openParen, nameSpan, closeParen]) str) = 
        isSrcSpanInfoCorrect spI && 
        isSpanMatchString nameSpan str &&
        srcSpanStart span == srcSpanStart openParen && 
        srcSpanEnd span == srcSpanStart closeParen &&
        openParen < nameSpan -- for whitespace between '(' and symbol
        
isNameCorrect _ _ = False

        

        
