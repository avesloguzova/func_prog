import Data.Char
findDigit:: String -> Maybe Int
findDigit [] = Nothing
findDigit (x:xs) = if isDigit x then Just $ digitToInt x
	               else findDigit xs
findDigitAndIndex:: String -> Maybe (Int,Int)
findDigitAndIndex [] = Nothing
findDigitAndIndex xs = findDigitAndIndex' xs 0
 where findDigitAndIndex' [] _ = Nothing
       findDigitAndIndex' (x:xs) n = if isDigit x then Just $ (digitToInt x, n)
	               else findDigitAndIndex' xs (n+1)

findDigitAndIndexOrLen:: String -> Either(Int,Int) Int
findDigitAndIndexOrLen xs = findDigitAndIndexOrLen' xs 0
 where findDigitAndIndexOrLen' [] n = Right n
       findDigitAndIndexOrLen' (x:xs) n = if isDigit x then Left $ (digitToInt x, n)
	               else findDigitAndIndexOrLen' xs (n+1)

data Result = DigitAndIndex Int Int
             |LetterAndIndex Char Int
             |Len Int

findDigitAndIndexOrLetterAndIndexOrLen:: String -> Result
findDigitAndIndexOrLetterAndIndexOrLen xs = helper xs 0
 where helper [] n = Len n
       helper (x:xs) n = if isDigit x then DigitAndIndex (digitToInt x) n
       	             else if isLetter x then LetterAndIndex x n
       	             	  else helper xs (n+1)

printResult:: Result -> String
printResult (Len n) = (intToDigit n ):[]
printResult (LetterAndIndex c n) = c:(intToDigit n ):[]
printResult (DigitAndIndex d n) = (intToDigit d ):(intToDigit n ):[]

data Tree a = Leaf a
             |Branch a (Tree a) (Tree a)

height:: Tree a -> Int
height (Leaf _) = 1
height (Branch _ t1 t2) = 1 + (max (height t1) (height t2))

mid t = sum / count 
 where (sum,count) = helper t
       helper (Leaf d) = (d,1.0)
       helper (Branch d t1 t2) = let (s1,c1) = helper t1
                                     (s2,c2) = helper t2
                                  in (s1 + s2 + d, c1 + c2 + 1.0) 
data Value = B Bool
            |I Int

data Expr = Mul Expr Expr 
           |Plus Expr Expr
           |Cons Value
           |If Expr Expr Expr
           
eval::Expr -> Value
eval (Mul e1 e2) = Value (n1 * n2) where Value n1 = eval e1
                                       Value n2 = eval e2
eval (Plus e1 e2) =  Value (n1 + n2) where Value n1 = eval e1
                                         Value n2 = eval e2
eval (Cons v) = v
eval (If True e _) = eval e
eval (If False _ e) = eval e 






