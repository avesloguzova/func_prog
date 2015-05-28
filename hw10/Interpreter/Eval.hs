-- (2 балла)
module Eval
    ( Eval, runEval
    , Error, Store
    , update, getVar
    ) where

import qualified Data.Map as M
import Data.List
import Control.Monad
import Control.Applicative

import Expr

type Error = String
type Store = M.Map String Value

newtype Eval a = Eval { runEval :: Store -> (Maybe a, [Error], Store) }

instance Functor Eval where
    fmap f (Eval re) = Eval $ \s -> let (m,err,st) = re s in (fmap f m, err, st)

instance Applicative Eval where
    pure x = Eval (\s -> (Just x, [], s))
    Eval m <*> Eval k = Eval $ \s ->
      let (m1, err1, st1) = m s
          (k1, err2, st2) = k st1 in
      (m1 <*> k1, err1 ++ err2, st2)

instance Monad Eval where
    return = pure
    Eval m >>= k = Eval $ \s ->
      let (m1, err1, st1) = m s  in case m1 of
          Just m1' ->
            let (Eval fn) = k m1'
                (m2, err2, st2) = fn st1 in
            (m2, err1 ++ err2, st2)
          Nothing -> (Nothing, err1, st1)

instance Alternative Eval where
    empty = Eval (\s -> (Nothing, ["empty"], M.empty))
    Eval l <|> Eval r = Eval $ \s ->
        let (l1,err1,st1) = l s in case l1 of
            Just l1'-> (l1,err1,st1)
            Nothing -> r s

-- MonadPlus - аналог Alternative для монад
-- mzero - вычисление, которое ничего не делает, сразу завершается неуспехом
-- mplus m1 m2 пытается выполнить m1, если тот завершился неуспехом, выполняет m2
-- Примеры использования этого класса есть в Utils.hs
instance MonadPlus Eval where
    mzero = empty
    mplus = (<|>)

update :: String -> Value -> Eval ()
update k v = Eval $ \s -> (Just (), [], M.insert k v s)

getVar :: String -> Eval Value
getVar k = Eval $ \s ->
    case M.lookup k s of
      Just v -> (Just v, [], s)
      Nothing -> (Nothing, ["Variable not found"], s)
