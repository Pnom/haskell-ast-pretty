{-# LANGUAGE DoAndIfThenElse #-}
module IfThenElseLayout where

askCardsForExchange :: Hand -> IO [Card]
askCardsForExchange h = do
                         putStrLn "Wich card do you want to exchange? (Max. 3)"
                         response <- getLine
                         if length (readCards response) > 3 || not (all (flip elem h) h) then
                                 askCardsForExchange h
                         else
                                 return (readCards response)

