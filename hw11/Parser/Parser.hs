{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- 1 балл
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
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Test.HUnit((~?=),Test(TestCase),assertBool)

-- Реализуйте Parser, используя трансформеры монад.
newtype Parser lex a = Parser (StateT [lex] (Either String) a) -- напишите сами
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

runParser :: Parser lex a -> [lex] -> Either String (a, [lex])
runParser (Parser p) l = runStateT p l

evalParser :: Parser lex a -> [lex] -> Either String a
evalParser p l = fmap fst $ runParser p l

satisfy :: (lex -> Bool) -> Parser lex lex
satisfy predicate = Parser $ do
	lst <- get
	case lst of
		[] -> lift $ Left "Empty list"
		(x:xs) -> if predicate x
			then do
				put xs
				lift $ Right x
			else
				lift $ Left "False"

eof :: Parser lex ()
eof = Parser $ do
    lst <- get
    case lst of
      [] -> lift $ Right ()
      _ -> lift $ Left "Not EOF"

parserTestOK :: (Eq a, Show a, Eq lex, Show lex) => Parser lex a -> [lex] -> (a, [lex]) -> Test
parserTestOK p s r = runParser p s ~?= Right r

parserTestFail :: (Eq a, Show a) => Parser lex a -> [lex] -> Test
parserTestFail p s = TestCase $ assertBool "Parser should fail" $ either (const True) (const False) (runParser p s)
