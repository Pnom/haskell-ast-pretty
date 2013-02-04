module AstUtils where 

import Language.Haskell.Exts.Annotated

-- ------------------------------------------------------

class AstSrc e where
        move  :: SrcLoc -> e -> e
        begin :: e -> SrcLoc
        end   :: e -> SrcLoc
        combine   :: e -> e -> e

-----------------------------------------------------------------------------

instance AstSrc SrcSpan where
        move (SrcLoc f ln cl) (SrcSpan _ sl sc el ec) = 
                SrcSpan f ln cl (ln + el - sl) (cl + ec - sc)
       
        begin  (SrcSpan f sl sc el ec) = SrcLoc f sl sc
        
        end    (SrcSpan f sl sc el ec) = SrcLoc f el ec

        combine  (SrcSpan f1 sl1 sc1 el1 ec1) (SrcSpan f2 sl2 sc2 el2 ec2) =
                SrcSpan f1 sl1 sc1 el2 ec2

-----------------------------------------------------------------------------

instance AstSrc SrcSpanInfo where

        move  to (SrcSpanInfo l ps) = SrcSpanInfo (move to l) (map (move to) ps)
        
        begin (SrcSpanInfo l ps) = begin l
        
        end   (SrcSpanInfo l ps) = end l

        combine (SrcSpanInfo l1 ps1) (SrcSpanInfo l2 ps2) = let span = combine l1 l2 in
                SrcSpanInfo span (ps1 ++ ps2)

