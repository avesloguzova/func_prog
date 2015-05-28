-- (0.5 балла)
module Counter
    ( Counter
    , tick
    , runCounter
    ) where

-- Монада Counter считает количество тиков, т.е. вызовов функции tick
data Counter a = Counter Int a

-- Возвращает результат вычислений и количество тиков
runCounter :: Counter a -> (a, Int)
runCounter (Counter n a) = (a, n)

tick :: Counter () 
tick = Counter 1 () 

instance Monad Counter where
    -- return :: a -> m a
    return a = Counter 0 a
    -- (>>=) :: m a -> (a -> m b) -> m b 
    (>>=) (Counter n a) fn = case fn a of
        Counter m b -> Counter (n + m) b
