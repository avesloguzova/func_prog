-- (3 балла)
module Eval
    ( EvalT, runEvalT
    , Eval, runEval
    , Store
    , update, getVar
    ) where

import qualified Data.Map as M
import Data.List
import Control.Monad
import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Trans

import Expr

type Store = M.Map String Value

-- newtype Eval a = Eval { runEval :: Store -> (Maybe a, [String], Store) }

-- Реализуйте трансформер монад EvalT.
newtype EvalT m a = EvalT (Store -> m (Maybe a, [String], Store))
type Eval = EvalT Identity

runEvalT :: EvalT m a -> Store -> m (Maybe a, [String], Store)
runEvalT (EvalT fn) st = fn st

runEval :: Eval a -> Store -> (Maybe a, [String], Store)
runEval ev st = runIdentity (runEvalT ev st)

instance Functor m => Functor (EvalT m) where
    fmap f (EvalT ev) = EvalT $ \s -> fmap f' $ ev s where
        f' (m, err, st) = (fmap f m, err, st)

instance (Functor m, Monad m) => Applicative (EvalT m) where
    pure x = EvalT $ \s -> return (Just x, [], s)
    EvalT m <*> EvalT k = EvalT $ \s -> do
        (m1, err1, st1) <- m s
        (m2, err2, st2) <- k st1
        return (m1 <*> m2, err1 ++ err2, st2)

instance Monad m => Monad (EvalT m) where
    return x = EvalT $ \s -> return (Just x, [], s)
    EvalT m >>= k = EvalT $ \s -> do
        (m1, err1, st1) <- m s
        case m1 of
          Just m1' -> do
            let (EvalT fn) = k m1'
            (m2, err2, st2) <- fn st1
            return (m2, err1 ++ err2, st2)
          Nothing ->
            return (Nothing, err1, st1)

instance (Functor m, MonadPlus m) => Alternative (EvalT m) where
    empty = EvalT $ \s -> return (Nothing, ["empty"], s)
    EvalT l <|> EvalT r = EvalT $ \s -> do
        (l1, err1, st1) <- l s
        case l1 of
          Just l1' -> return (l1, err1, st1)
          Nothing -> r s

instance MonadPlus m => MonadPlus (EvalT m) where
    mzero = EvalT $ \s -> return (Nothing, ["mzero"], s)
    mplus (EvalT l) (EvalT r) = EvalT $ \s -> do
        (l1, err1, st1) <- l s
        case l1 of
          Just l1' -> return (l1, err1, st1)
          Nothing -> r s

instance MonadTrans EvalT where
    lift m = EvalT $ \s -> do
        m' <- m
        return (Just m', [], s)

update :: Monad m => String -> Value -> EvalT m ()
update k v = EvalT $ \s -> return (Just (), [], M.insert k v s)

getVar :: Monad m => String -> EvalT m Value
getVar k = EvalT $ \s -> do
    case M.lookup k s of
      Just v -> return (Just v, [], s)
      Nothing -> return (Nothing, ["Variable not found"], s)
