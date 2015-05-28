-- Список экспорта менять нельзя!
module Parser
    ( Parser
    , pure, (<$>), (<$), (<*>), (<*), (*>)
    , empty, (<|>)
    , satisfy, eof
    , evalParser
    , parserTestOK
    , parserTestFail
    ) where

import Control.Applicative
import Test.HUnit
import Data.Foldable(toList)

type Error = Either String -- Можно заменить на Maybe, если есть желание.
newtype Parser lex a = Parser { runParser :: [lex] -> Error (a, [lex]) }

-- (0.5 балла)
evalParser :: Parser lex a -> [lex] -> Error a
evalParser pars ls = fmap fst $ (runParser pars) ls 

-- (0.5 балла)
satisfy :: (lex -> Bool) -> Parser lex lex
satisfy fn = Parser f' where
    f' [] = Left "List is empty"
    f' (x:xs) | fn x  = Right (x, xs)
              | otherwise = Left "Lexeme doesn't satisfy predicate"

-- (0.5 балла)
eof :: Parser lex ()
eof = Parser f' where 
    f' [] = Right ((),[])
    f' _  = Left "List isn't empty"

instance Functor (Parser lex) where
    fmap f (Parser fn) = Parser (\x -> fmap (\(a,lst) -> (f a, lst)) (fn x))

instance Applicative (Parser lex) where
    -- (0.5 балла)
    pure x = Parser (\lst -> Right (x, lst))
    -- (1.5 балл)
    (<*>) (Parser pf) (Parser pa) = Parser $ \lst -> 
        case pf lst of
            Left str -> Left str
            Right (fab,lst') -> 
                let a = pa lst'
                    x = (pure fab) <*> (fmap fst a)
                    y = (fmap snd a) in
                pure (\a b -> (a,b)) <*> x <*> y 



instance Alternative (Parser lex) where
    -- (0.5 балла)
    empty = Parser $ \_ -> Left "Empty"
    -- (0.5 балла)
    (<|>) (Parser p1) (Parser p2) = Parser $ \lst -> 
        case p1 lst of 
            Left _ -> p2 lst
            x -> x



parserTestOK :: (Eq a, Show a, Eq lex, Show lex) => Parser lex a -> [lex] -> (a, [lex]) -> Test
parserTestOK (Parser p) s r = p s ~?= pure r

parserTestFail :: (Eq a, Show a) => Parser lex a -> [lex] -> Test
parserTestFail (Parser p) s = TestCase $ assertBool "Parser should fail" $ null $ toList (p s)
