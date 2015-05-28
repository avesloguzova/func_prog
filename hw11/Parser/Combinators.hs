module Combinators
    ( module Parser
    , many, many1
    , symbol, anySymbol, string, oneOf
    , digit, natural, integer
    , spaces
    , try
    , endBy, endBy1
    , sepBy, sepBy1
    , foldr1P, foldl1P
    , between, brackets, parens, braces, angles
    ) where

import Parser
import Data.Char

symbol :: Eq lex => lex -> Parser lex ()
symbol l =  pure (\_ -> ()) <*> satisfy (== l)

anySymbol :: Parser lex lex
anySymbol = satisfy (const True)

digit :: Parser Char Int
digit = pure (\x -> read [x]) <*> satisfy isDigit

string :: Eq lex => [lex] -> Parser lex ()
string [] = pure ()
string  (x:xs) = pure (\_ _ -> ()) <*> satisfy (==x) <*> string xs


oneOf :: Eq lex => [lex] -> Parser lex lex
oneOf [] = empty
oneOf (x:xs) = satisfy (==x) <|> oneOf xs

many :: Parser lex a -> Parser lex [a]
many p = many1 p <|> pure []

many1 :: Parser lex a -> Parser lex [a]
many1 p = pure (\h t -> h : t) <*> p <*> many p

natural :: Parser Char Integer
natural = pure (\ns -> read ns) <*> many1 (satisfy isDigit)

integer :: Parser Char Integer
integer = pure(\_ n -> (-n) ) <*> satisfy (=='-') <*> natural <|> natural

spaces :: Parser Char ()
spaces = pure (const ()) <*> many (satisfy isSpace)

try :: Parser lex a -> Parser lex (Maybe a)
try p = pure (\a -> Just a) <*> p <|> pure Nothing

endBy :: Parser lex a -> Parser lex b -> Parser lex [a]
endBy lexeme sep = endBy1 lexeme sep <|> pure []

endBy1 :: Parser lex a -> Parser lex b -> Parser lex [a]
endBy1 lexeme sep = many1 (pure (\a  _ -> a) <*> lexeme <*> sep)

sepBy :: Parser lex a -> Parser lex b -> Parser lex [a]
sepBy lexeme sep = sepBy1 lexeme sep <|> pure []

sepBy1 :: Parser lex a -> Parser lex b -> Parser lex [a]
sepBy1 lexeme sep = pure (\a  _  a' as -> a:a':as) <*> lexeme <*> sep <*> lexeme <*> many (pure (\_  a -> a) <*> sep <*> lexeme)

between :: Parser lex a -> Parser lex b -> Parser lex c -> Parser lex c
between a b c = pure (\_ c _ -> c) <*> a <*> c <*> b

brackets :: Parser Char a -> Parser Char a
brackets = between (symbol '[') (symbol ']')

parens :: Parser Char a -> Parser Char a
parens = between (symbol '(') (symbol ')')

braces :: Parser Char a -> Parser Char a
braces = between (symbol '{') (symbol '}')

angles :: Parser Char a -> Parser Char a
angles = between (symbol '<') (symbol '>')

foldr1P :: (a -> b -> a -> a) -> Parser lex a -> Parser lex b -> Parser lex a
foldr1P f p1 p2 = pure f' <*> p1 <*> (many (pure (\a b -> (a,b)) <*> p2 <*> p1)) where
    f' acc [] = acc
    f' acc ((b,a):c) = f acc b (f' a c)

foldl1P :: (a -> b -> a -> a) -> Parser lex a -> Parser lex b -> Parser lex a
foldl1P f p1 p2 = pure f' <*> p1 <*> (many (pure (\a b -> (a,b)) <*> p2 <*> p1)) where
    f' acc [] = acc
    f' acc ((b,a):c) = f' (f acc b a) c
