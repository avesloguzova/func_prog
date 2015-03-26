import qualified Data.Map as M
import Prelude hiding (lookup)
import Test.HUnit

------------------------------------------------------------------------------
-- 1. Реализуйте функции для работы с комплекснми числами.

data Complex = Complex { real :: Double, im :: Double } deriving (Show, Eq)

fromDouble :: Double -> Complex
fromDouble r = Complex r 0

-- Мнимая единица
i :: Complex
i = Complex 0 1

infixl 6 +., -.
(+.) :: Complex -> Complex -> Complex
(+.) (Complex r1 i1) (Complex r2 i2) = Complex (r1 + r2) (i1 + i2)

(-.) :: Complex -> Complex -> Complex
(-.) (Complex r1 i1) (Complex r2 i2) = Complex (r1 - r2) (i1 - i2)

infixl 7 *., /.
(*.) :: Complex -> Complex -> Complex
(*.) (Complex r1 i1) (Complex r2 i2) = Complex (r1 * r2 - i1 * i2) (r1 * i2 + r2 * i1)

(/.) :: Complex -> Complex -> Complex
(/.)(Complex r1 i1) (Complex r2 i2) = Complex ((r1*r2 + i1*i2) / sqrsum) ((-r1 * i2 + i1*r2) / sqrsum)
 where sqrsum = r2*r2 + i2*i2

conj :: Complex -> Complex
conj (Complex r i) = Complex r (-i)

-- tests

testsComplex =
    [ i *. i ~?= fromDouble (-1)
    , fromDouble 3 +. i ~?= Complex 3 1
    , fromDouble 3 *. i ~?= Complex 0 3
    , (fromDouble 3 +. fromDouble 4 *. i) *. (fromDouble 4 +. fromDouble 3 *. i) ~?= fromDouble 25 *. i
    , conj (fromDouble 3 +. fromDouble 4 *. i) ~?= fromDouble 3 -. fromDouble 4 *. i
    , fromDouble 2 /. (fromDouble 1 +. i) ~?= fromDouble 1 -. i
    ]

------------------------------------------------------------------------------
-- 2

data Tree a = Node { value :: a, children :: [Tree a] }

-- (a) Возвращает высоту дерева
height :: Tree a -> Int
height (Node _ []) = 1
height (Node _ xs) = 1 + maximum (map height xs)

-- (b) Возвращает среднее арифметическое значений во всех узлах дерева
-- Необходимо вычислить эту функцию, выполнив один проход по дереву
avg :: Tree Int -> Int
avg node = floor (fromIntegral ressum / fromIntegral count)
 where (ressum, count) = avg' node
       avg' (Node val xs) =  foldl fn (val, 1) $ map avg' xs 
       fn (s, c) (a, b) = (s + a, c + b)

-- (c) Возвращает ширину дерева
-- Ширина дерева определяется следующим образом:
-- Количество вершин на определенном уровне называется шириной уровня.
-- Ширина дерева - это максимальная ширина уровня по всем уровням.
width :: Tree a -> Int
width t = width' [t] 0 where
  width' [] w = w
  width' ts w = 
    let level = concatMap children ts
        cw = length level
    in if cw > w then width' level cw else width' level w

-- tests

(tree1, tree2, tree3) =
    ( b [b [l [b []],
            l [b [],
               l [b [l [],
                     l [],
                     b []],
                  l []]]],
         b [],
         b [],
         l []]
    , b [b [b [],
            b [b [],
               b []]],
         b [b [],
            l [b [],
               b []]],
         l [b []]]
    , b [tree1, tree2]
    )
  where l = Node 500; b = Node 300

(testsHeight, testsAvg, testsWidth) = (
    [ height tree1 ~?= 6
    , height tree2 ~?= 4
    , height tree3 ~?= 7
    ],
    [ avg tree1 ~?= 393
    , avg tree2 ~?= 330
    , avg tree3 ~?= 362
    ],
    [ width tree1 ~?= 4
    , width tree2 ~?= 5
    , width tree3 ~?= 7
    ])

------------------------------------------------------------------------------
-- 3

data Value = I Int | B Bool deriving (Eq, Show)
data BinOp = Plus | Mul | Minus | Less | Greater | Equals
data UnOp = Neg | Not
data Expr = BinOp BinOp Expr Expr | UnOp UnOp Expr | Const Value | If Expr Expr Expr | Var String
data Statement = Assign String Expr | While Expr Statement | Compound [Statement]

infixr 0 @=
(@=) = Assign
(.+) = BinOp Plus
(.-) = BinOp Minus
(.*) = BinOp Mul
(.<) = BinOp Less
int = Const . I
bool = Const . B
neg = UnOp Neg

type Error = String

-- evalExpr m e интерпретирует выражение e, в m передается значение переменных.
-- evalExpr возвращает либо успешно вычисленный результат, либо список ошибок.
-- Ошибки бывают двух видов: необъявленная переменная и несоответствие типов.
-- Возвращается список ошибок, т.к. выражение может содержать больше одной ошибки.
evalExpr :: M.Map String Value -> Expr -> Either [Error] Value
evalExpr _ (Const val) = Right val
evalExpr vars (Var str) = case M.lookup str vars of 
  Just var -> Right var
  Nothing -> Left ["Variable not found"]
evalExpr vars (If cond exprTrue exprFalse) = case evalExpr vars cond of
  Right (B True) -> evalExpr vars exprTrue
  Right (B False) -> evalExpr vars exprFalse
  Right (I _) -> Left ["Type inconsistency"]
  left -> left
evalExpr vars (UnOp op expr) = case (evalExpr vars expr,op) of
  (Right (B True), Not) -> Right (B False)
  (Right (B False),Not) -> Right (B True)
  (Right (B _),Neg) -> Left ["Type inconsistency"]
  (Right (I n),Neg) -> Right (I (-n))
  (Right (I _),Not) -> Left ["Type inconsistency"]
  (left, _) -> left
evalExpr vars (BinOp op expr1 expr2) = case (evalExpr vars expr1, evalExpr vars expr2, op) of 
  (Right (B b1), Right (B b2),Equals) -> Right (B (b1 == b2))
  (Right (B _), Right (B _),_) -> Left ["Type inconsistency"]
  (Right (I i1), Right(I i2),Plus) -> Right (I (i1 + i2))
  (Right (I i1), Right(I i2),Mul) -> Right (I (i1 * i2))
  (Right (I i1), Right(I i2),Minus) -> Right (I (i1 - i2))
  (Right (I i1), Right(I i2),Less) -> Right (B (i1 < i2))
  (Right (I i1), Right(I i2),Greater) -> Right (B (i1 > i2))
  (Right (I i1), Right(I i2),Equals) -> Right (B (i1 == i2))
  (Right (B _), Right (I _), _ ) -> Left ["Type inconsistency"]
  (Right (I _), Right (B _), _ ) -> Left ["Type inconsistency"]
  (left, Right _, _ ) -> left
  (Right _,left, _ ) -> left
  (Left er1, Left er2, _) -> Left (er1++er2)


-- evalStatement принимает текущее значение переменных и statement и возвращает новое значение переменных после его выполнения.
evalStatement :: M.Map String Value -> Statement -> Either [Error] (M.Map String Value)
evalStatement vars (Assign name expr) = case evalExpr vars expr of 
  Right val -> Right (M.insert name val vars)
  Left errors -> Left errors
evalStatement vars (While expr statement) = case (evalExpr vars expr,evalStatement vars statement) of 
  (Right (B True),Right newVars) ->  evalStatement newVars (While expr statement)
  (Right (B True),left) -> left
  (Right (B False), _) -> Right vars
  (Right (I _), _) -> Left ["Type inconsistency"]
  (Left errors, _) -> Left errors
evalStatement vars (Compound []) = Right vars
evalStatement vars (Compound (x:xs)) = case evalStatement vars x of 
  Right newVars -> evalStatement newVars (Compound xs)



-- tests

max' x y = If (x .< y) y x
expr1 = Var "x" .+ int 3
expr2 = If (Var "x") (Var "y" .- int 3) (int 2)
stat1 = Compound
    [ "x" @= int 3 .+ int 4
    , "y" @= Var "x" .* int 6
    , "z" @= neg $ max' (Var "x") (Var "y")
    ]
stat2 = Compound
    [ "r" @= int 1
    , "i" @= int 0
    , While (Var "i" .< Var "n") $ Compound
        [ "i" @= Var "i" .+ int 1
        , "r" @= Var "r" .* Var "i"
        ]
    ]

testsExpr = [ errorsCount (evalExpr M.empty expr1) ~?= 1
            , evalExpr (M.fromList [("x", B True), ("y", I 5)]) expr2 ~?= Right (I 2)
            , evalExpr (M.fromList [("x", B False), ("y", B False)]) expr2 ~?= Right (I 2)
            , errorsCount (evalExpr (M.fromList [("x", B True), ("y", B False)]) expr2) ~?= 1
            , fmap (M.lookup "z") (evalStatement M.empty stat1) ~?= Right (Just $ I $ -42)
            , fmap (M.lookup "r") (evalStatement (M.fromList [("n", I 6)]) stat2) ~?= Right (Just $ I 720)
            ]
  where errorsCount = either length (const 0)

------------------------------------------------------------------------------
-- 4. Реализовать двоичное дерево поиска без балансировки.

data Map k v = Leaf | Branch k v (Map k v) (Map k v)

lookup :: Ord k => k -> Map k v -> Maybe v
lookup key Leaf = Nothing
lookup k (Branch key val left right) = if k == key then Just val
                                       else if k < key then lookup k left
                                            else lookup k right

insert :: Ord k => k -> v -> Map k v -> (Map k v, Maybe v)
insert key val Leaf = (Branch key val Leaf Leaf, Just val)
insert newkey newval (Branch key val left right) = if newkey==key then ((Branch key val left right), Nothing)
                                                   else if newkey < key then ((Branch key val (fst(insert newkey newval left)) right), Just newval)
                                                        else ((Branch key val left (fst(insert newkey newval right))), Just newval)

delete :: Ord k => k -> Map k v -> Maybe (Map k v)
delete = undefined

fromList :: Ord k => [(k, v)] -> Map k v
fromList ts = fromList' Leaf ts
 where
  fromList' m [] = m
  fromList' m ((key,val):kvs) = fromList' (fst (insert key val m)) kvs 

toList :: Map k v -> [(k, v)]
toList Leaf = []
toList (Branch key val left right) = (toList left) ++ [(key,val)] ++ (toList right)

-- tests

sort :: Ord a => [a] -> [a]
sort = map fst . toList . fromList . map (\x -> (x, ()))

------------------------------------------------------------------------------
-- main

main = fmap (\_ -> ()) $ runTestTT $ test
      $  label "complex" testsComplex
      ++ label "height" testsHeight
      ++ label "avg" testsAvg
      ++ label "width" testsWidth
      ++ label "Expr" testsExpr
      ++ label "Map" -- можете сами написать тесты на каждую функцию :)
            [ sort [10,24,13,56,35,13,6,23] ~?= [6,10,13,23,24,35,56] ]
  where
    label :: String -> [Test] -> [Test]
    label l = map (\(i,t) -> TestLabel (l ++ " [" ++ show i ++ "]") t) . zip [1..]
