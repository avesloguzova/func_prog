-- (0.5 балла)
import Counter

-- Эти две функции отличаются от обычных тем, что, вызывая tick, они считают сколько шагов заняло их выполнение.
filter' :: (a -> Bool) -> [a] -> Counter [a]
filter' _ [] = tick >> return [] 
filter' f (x:xs) 
    | f x = tick >> filter' f xs  >>= (\ys -> return (x:ys))
    | otherwise = tick >> filter' f xs

append :: [a] -> [a] -> Counter [a]
append xs [] = tick >> return xs
append xs (y:ys) = tick >> append (xs ++ [y]) ys

-- Реализуйте qsort через filter' и append
qsort :: Ord a => [a] -> Counter [a]
qsort [] = tick >> return []
qsort (x:xs) = do
    gt <- filter' (> x) xs >>= qsort
    lt <- filter' (< x) xs >>= qsort
    append lt (x:gt)



-- Первый вызов должен занимать большее количество тиков ~ в 2 раза
main = do
    print $ runCounter $ qsort [1..15]
    print $ runCounter $ qsort [8,4,12,2,6,10,14,1,3,5,7,9,11,13,15]
