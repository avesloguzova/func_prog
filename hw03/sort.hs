sort :: [Integer] -> [Integer]
sort [] = []
sort (x:xs) = append' (go xs)
  where
    go [] = (x,[])
    go (y:zs') =
        let (x,zs) = go zs'
        in if x >= y then (x, append y zs) else (y, append x zs)

append' (x,y) = append x y
append x xs = xs ++ [x]
