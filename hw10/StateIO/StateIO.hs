-- (1.5 балла)
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module StateIO
    ( StateIO
    , runStateIO, execStateIO, evalStateIO
    ) where

import Control.Monad.State
import Data.IORef

newtype StateIO s a = StateIO { getStateIO :: IORef s -> IO a }

instance Monad (StateIO s) where
    return x = StateIO $ \_ -> return x
    (>>=) (StateIO fn) b = StateIO $ \ref -> do
        a <- fn ref
        getStateIO (b a) ref

instance MonadState s (StateIO s) where
    get = StateIO $ \ref -> readIORef ref
    put val = StateIO $ \ref -> writeIORef ref val

runStateIO :: StateIO s a -> s -> IO (a,s)
runStateIO (StateIO fn) st = do
    ref <- newIORef st
    val <- fn ref
    st' <- readIORef ref
    return (val, st')

execStateIO :: StateIO s a -> s -> IO s
execStateIO st s = do
    (_, s') <- runStateIO st s
    return s'

evalStateIO :: StateIO s a -> s -> IO a
evalStateIO st s = do
    (a, _) <- runStateIO st s
    return a
