-- (3.5 балла)

import System.Random
import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent
import System.Environment

{-
Решите задачу о философах http://en.wikipedia.org/wiki/Dining_philosophers_problem
Количество философов передается в параметрах командной строки.
Жизненый цикл философа:
a) Философ сообщает (вывод сообщения на экран) о том, что он готов обедать.
b) Ждет пока не освободятся обе вилки.
c) Берет вилки, сообщает об этом, начинает обедать, что занимает рандомное время (от 1 до 3 секунд).
d) Кладет вилки, сообщает об этом, начинает думать, что занимает рандомное время (от 1 до 3 секунд).
e) Возвращается к шагу (a).

Для реализации используйте библиотеку STM.
Вам также понадобятся функции forkIO, threadDelay и randomRIO.
-}

randomDelay :: IO ()
randomDelay = do
    v <- randomRIO (1,3)
    threadDelay (10^6 * v)

loop :: Int -> TVar Bool -> TVar Bool -> IO ()
loop n f1 f2 = forever $ do
    atomically $ do
        takeFork f1
        takeFork f2
    putStrLn $ show n ++ " take forks"
    randomDelay
    atomically $ do
        releaseFork f2
        releaseFork f1
    putStrLn $ show n ++ " release forks"
    randomDelay
  where
    takeFork f = do
        v <- readTVar f
        if v then retry else writeTVar f True
    releaseFork f = writeTVar f False

main :: IO ()
main = do
    args <- getArgs
    let count = read $ head args
    forks <- replicateM count (newTVarIO False)
    let doFork i = forkIO $ loop i (forks !! pred i) (forks !! (i `mod` count))
    mapM_ (doFork . succ) [0..count-1]
    forever randomDelay

