import Language.Haskell.Exts.Annotated
import Control.Monad.State
import AstUtils

data DocState = DocState {      
        loc  :: SrcLoc,
        nestSize :: Int
        } deriving Show

--type DocM a = State SrcLoc a
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
getPos = do
        DocState l n <- get
        return l

putPos :: SrcLoc -> DocM SrcLoc
putPos l = do
        DocState _ n <- get
        put $ DocState l n        
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
                return $ ModuleName (SrcSpanInfo span []) s        
        
-- --------------------------------------------------------------------------

instance AstPretty QName where

        astPretty qn =  undefined

-- --------------------------------------------------------------------------

epQName :: QName t -> DocM (QName SrcSpanInfo)

epQName (Qual _ mn n)  = do
        m' <- astPretty mn
        pnt <- format "."
        n'  <- epName n
        let SrcSpanInfo span _ = combine (ann m') (ann n')
        return $ Qual (SrcSpanInfo span [pnt]) m' n'

epQName (UnQual _ n) = do 
        n' <- epName n
        return $ UnQual (ann n') n'

epQName (Special _ sc) = undefined

-- --------------------------------------------------------------------------

instance AstPretty Name where
        astPretty n@(Ident _ _) = epName n

        astPretty n@(Symbol _ str) = do
                        -- maybe generalization is possible, like taketToParen "(" ")" $ ....
                        SrcLoc f ls cs <- getPos
                        openParen <- format "("
                        p <- space 1
                        _ <- format str
                        closeParen <- format ")"
                        SrcLoc _ le ce <- getPos
                        let span = SrcSpan f ls cs le ce
                        return $ Symbol (SrcSpanInfo span [openParen, p, closeParen]) str

-- --------------------------------------------------------------------------

epName :: Name t -> DocM (Name SrcSpanInfo)
epName (Symbol _ s) = do
        span <- format s
        return $ Symbol (SrcSpanInfo span []) s
        
epName (Ident _ s) = do
        span <- format s
        return $ Ident (SrcSpanInfo span []) s

-- --------------------------------------------------------------------------        
-- tests

srcSpan :: Int -> Int -> SrcSpan
srcSpan y x = SrcSpan {srcSpanFilename = "<unknown>.hs", srcSpanStartLine = 1, srcSpanStartColumn = 1, srcSpanEndLine = 1 + y, srcSpanEndColumn = 1 + x}

ident s = Ident (SrcSpanInfo (srcSpan 0 $ length s) []) s

symbol s = Symbol (SrcSpanInfo (srcSpan 0 $ length s) []) s

moduleName s = ModuleName (SrcSpanInfo (srcSpan 0 $ length s) []) s

zeroSt = DocState (SrcLoc "unknown.hs"  1  1) 0

showRes x = do
        let (r, s) = (runState x) zeroSt
        putStrLn ""
        putStr "SrcLoc: "
        putStrLn $ show s 
        putStrLn ""
        putStrLn $ show r
        
-- -------------------------
-- Name tests


t =  showRes.astPretty $ symbol "#~"

ex = exactPrint (fst $ runState (astPretty $ symbol "#~") zeroSt) []

{-
SrcSpan \{srcSpanFilename = (\"<\w+>.hs\"), srcSpanStartLine = (\d+), srcSpanStartColumn = (\d+), srcSpanEndLine = (\d+), srcSpanEndColumn = (\d+)\}

\(SrcSpan \1 \2 \3 \4 \5\)
-}        
        
