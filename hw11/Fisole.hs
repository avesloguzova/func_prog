-- (3 балла)

module Fisole
    ( Fisole, runFisole
    , abortF, putCharF, getCharF
    ) where

import System.IO
import System.IO.Error

-- Монада для работы с файлом.
data Fisole a = Fisole (Handle -> IO (Either String a))

-- Второй аргумент - имя файла с которым будут работать функции putCharF и getCharF.
-- Если произошли какие-то ошибки ввода-вывода (т.е. исключения бросались), нужно их поймать и вернуть Left с каким-нибудь сообщением об ошибке.
runFisole :: Fisole a -> String -> IO (Either String a)
runFisole (Fisole fs) path =
    catchIOError (withFile path ReadWriteMode fs) (\e -> return $ Left $ ioeGetErrorString e)


instance Functor Fisole where
    fmap fn (Fisole fs) = Fisole $ \h -> do
        r <- fs h
        return $ fmap fn r

instance Monad Fisole where
    return x = Fisole $ \_ -> return $ Right x
    (>>=) (Fisole fs) fn = Fisole $ \h -> do
        r <- fs h
        case r of
          Right x -> let (Fisole fs') = fn x in fs' h
          Left s -> let (Fisole fs') = abortF s in fs' h

-- abortF s завершает вычисление, s - сообщение об ошибке.
abortF :: String -> Fisole a
abortF e = Fisole $ \_ -> return $ Left e

-- putCharF записывает в файл символ.
putCharF :: Char -> Fisole ()
putCharF c = Fisole $ \h -> hPutChar h c >>= \r -> return $ Right r

-- getCharF считывает символ из файла.
getCharF :: Fisole Char
getCharF = Fisole $ \h -> hGetChar h >>= \c -> return $ Right c
