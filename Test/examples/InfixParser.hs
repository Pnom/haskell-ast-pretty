module InfixParser where

type Parse a b = [a] -> [(b, [a])]

(<|>) :: Parse a b -> Parse a b -> Parse a b
(p1 <|> p2) i = p1 i ++ p2 i
