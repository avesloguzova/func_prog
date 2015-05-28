-- (2 балла)
{-# LANGUAGE TupleSections #-}

import Control.Monad
import qualified Data.Map as M
import Test.HUnit

import Expr
import Eval

getInt :: Eval Value -> Eval Integer
getInt m = do
    val <- m
    case val of
        I int -> return int
        _ -> mzero

getBool :: Eval Value -> Eval Bool
getBool m =  do
    val <- m
    case val of
        B bool -> return bool
        _ -> mzero

if' :: Eval Value -> Eval () -> Maybe (Eval ()) -> Eval ()
if' c t e = do
    b <- getBool c
    case b of True -> t
              otherwise -> maybe (return ()) id e

mapInt :: (Integer -> Value) -> Eval Value -> Eval Value
mapInt f e = getInt e >>= \i -> return $ f i

mapBool :: (Bool -> Value) -> Eval Value -> Eval Value
mapBool f e = getBool e >>= \b -> return $ f b

mapInt2 :: (Integer -> Integer -> Value) -> Eval Value -> Eval Value -> Eval Value
mapInt2 f e1 e2 = do
    i1 <- getInt e1
    i2 <- getInt e2
    return $ f i1 i2

mapBool2 :: (Bool -> Bool -> Value) -> Eval Value -> Eval Value -> Eval Value
mapBool2 f e1 e2 = do
    b1 <- getBool e1
    b2 <- getBool e2
    return $ f b1 b2

evalExpr :: Expr -> Eval Value
evalExpr e = case e of
    Const val -> return val
    Var str -> getVar str
    UnOp unop e1 -> case unop of
        Not -> mapBool (B . not) (evalExpr e1)
        Neg -> mapInt (I . negate) (evalExpr e1)
    BinOp binop e1 e2 -> do
        case binop of
            Plus -> mapInt2 (\x y -> I (x + y)) (evalExpr e1) (evalExpr e2)
            Mul -> mapInt2 (\x y -> I (x * y)) (evalExpr e1) (evalExpr e2)
            Minus -> mapInt2 (\x y -> I (x - y)) (evalExpr e1) (evalExpr e2)
            And -> mapBool2 (\x y -> B (x && y)) (evalExpr e1) (evalExpr e2)
            Or -> mapBool2 (\x y -> B (x || y)) (evalExpr e1) (evalExpr e2)
            Less -> mapInt2 (\x y -> B (x < y)) (evalExpr e1) (evalExpr e2)
            Greater -> mapInt2 (\x y -> B (x > y)) (evalExpr e1) (evalExpr e2)
            Equals -> mapInt2 (\x y -> B (x == y)) (evalExpr e1) (evalExpr e2)

evalStatement :: Statement -> Eval ()
evalStatement (Compound body) =
    mapM_ evalStatement body
evalStatement this@(While cond body) = do
    val <- getBool $ evalExpr cond
    if val
      then evalStatement body >> evalStatement this
      else return ()
evalStatement (Assign varName expr) =
    evalExpr expr >>= update varName
evalStatement (If cond thenBody elseBody) = do
    val <- getBool $ evalExpr cond
    if val
      then evalStatement thenBody
      else maybe (return ()) evalStatement elseBody



------------------------------------------------------------------------------------------------
-- tests
------------------------------------------------------------------------------------------------

test1 = not_ (Var "x") .| Var "y" .< Const (I 3) .& Var "z" .= Var "y" .&
    Const (I 5) .< Var "y" .+ Const (I 7) .* Var "z" .+ Var "y" .* Const (I 3)

test2 = neg (Const $ I 5) .+ neg (Const $ I 3) .* Const (I 2) .- Const (I 7)

test3 = Compound
    [ "r" $= Const (I 1)
    , While (Var "n" .> Const (I 0)) $ Compound
        [ "r" $= Var "r" .* Var "n"
        , "n" $= Var "n" .- Const (I 1)
        ]
    ]

main = fmap (\_ -> ()) $ runTestTT $ test
    [ TestCase $ assertBool "Expected an error" $ errorsCount (runEval (evalExpr test1) $ M.fromList [("x",I 3),("y",I 5),("f",I 5)]) > 0
    , let m = M.fromList [("x",B True),("y",I 5),("z",I 5)] in runEval (evalExpr test1) m ~?= (Just (B False), [], m)
    , let m = M.fromList [("x",B True),("y",I 2),("z",I 2)] in runEval (evalExpr test1) m ~?= (Just (B True ), [], m)
    , runEval (evalExpr test2) M.empty ~?= (Just (I (-18)), [], M.empty)
    , runEval (evalStatement test3) (M.fromList [("n",I 5)]) ~?= (Just (), [], M.fromList [("n",I 0),("r",I 120)])
    ]
  where
    errorsCount (_,es,_) = length es
