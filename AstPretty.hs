import Language.Haskell.Exts.Annotated
import Control.Monad.State

import Debug.Trace

data DocState = DocState {      
        pos :: !SrcLoc,
        nestSize :: !Int
        } deriving Show

type DocM a = State DocState a

-- --------------------------------------------------------------------------        

format :: String -> DocM SrcSpan
format s = do
        SrcLoc f l c <- getPos
        let newColumn = c + (length s)
        putPos $ (SrcLoc f l newColumn)
        return $ SrcSpan f l c l newColumn

-- --------------------------------------------------------------------------        

getPos :: DocM SrcLoc
getPos = gets pos

putPos :: SrcLoc -> DocM SrcLoc
putPos l = do
        DocState _ n <- get
        put $! DocState l n        
        return l
        
-- --------------------------------------------------------------------------        

line :: DocM SrcLoc
line = do
        DocState (SrcLoc f l c) n <- get
        putPos $ SrcLoc f (l + 1) n

-- --------------------------------------------------------------------------        

right :: Int -> DocM SrcLoc
right x = do
        SrcLoc f l c <- getPos
        putPos $ SrcLoc f l (c + x)
        
-- --------------------------------------------------------------------------        

space :: Int -> DocM SrcSpan
space x = do
        SrcLoc f l c <- right x
        return $ SrcSpan f l c l (c + 1)  
        
-- --------------------------------------------------------------------------        

nest :: Int -> DocM ()
nest x = do
        DocState l n <- get
        put $ DocState l (n + x)
        return ()

-- --------------------------------------------------------------------------        

class AstPretty ast where
        astPretty :: ast a -> DocM (ast SrcSpanInfo)  

-- --------------------------------------------------------------------------

instance AstPretty ModuleName where
        astPretty (ModuleName l s) = do
                span <- format s
                return $ ModuleName (noInfoSpan span) s        


-- --------------------------------------------------------------------------
-- QName instance 

instance AstPretty SpecialCon where
        astPretty sc = undefined

-- --------------------------------------------------------------------------
-- QName instance 

instance AstPretty QName where
        astPretty qn
                | isSymbol (getName qn) = do
                        openParen <- format "("
                        p <- space 1
                        qn' <- rawQName qn
                        closeParen <- format ")"
                        let span = SrcSpanInfo (mergeSrcSpan openParen closeParen) (openParen : p : ((srcInfoPoints $ ann qn') ++ [closeParen]))
                        return $ amap (const span) qn'
                | otherwise = rawQName qn

-- --------------------------------------------------------------------------
-- QName utils

rawQName :: QName t -> DocM (QName SrcSpanInfo)

rawQName (Qual _ mn n)  = do
        m' <- astPretty mn
        pnt <- format "."
        n'  <- rawName n
        let span  = (ann m') <++> (ann n')
        return $ Qual (span <** [pnt]) m' n'

rawQName (UnQual _ n) = do 
        n' <- rawName n
        return $ UnQual (ann n') n'

rawQName (Special _ sc) = do
        val <- astPretty sc
        return $ Special (noInfoSpan.srcInfoSpan $ ann val) val

-- --------------------------------------------------------------------------

getName :: QName l -> Name l
getName (UnQual _ s) = s
getName (Qual _ _ s) = s
getName (Special l (Cons _)) = Symbol l ":"
getName (Special l (FunCon _)) = Symbol l "->"
getName (Special l s) = Ident l (specialName s)

-- --------------------------------------------------------------------------

specialName :: SpecialCon l -> String
specialName (UnitCon _) = "()"
specialName (ListCon _) = "[]"
specialName (FunCon  _) = "->"
specialName (TupleCon _ b n) = "(" ++ hash ++ replicate (n-1) ',' ++ hash ++ ")"
    where hash = case b of
                   Unboxed -> "#"
                   _       -> ""
specialName (Cons _) = ":"

-- --------------------------------------------------------------------------
-- Name instance 

instance AstPretty Name where
        astPretty n@(Ident _ _) = rawName n
        
        astPretty n@(Symbol _ str) = do
                        -- maybe generalization is possible, like taketToParen "(" ")" $ ....
                        openParen <- format "("
                        p <- space 1
                        _ <- format str
                        closeParen <- format ")"
                        return $ Symbol (SrcSpanInfo (mergeSrcSpan openParen closeParen) [openParen, p, closeParen]) str

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

{-
SrcSpan \{srcSpanFilename = (\"<\w+>.hs\"), srcSpanStartLine = (\d+), srcSpanStartColumn = (\d+), srcSpanEndLine = (\d+), srcSpanEndColumn = (\d+)\}

\(SrcSpan \1 \2 \3 \4 \5\)
-}        
        
