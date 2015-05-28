module Complex where

-- (2 балл)

data Complex = Complex { real :: Double, im :: Double } deriving Eq

instance Num Complex where
    (+) (Complex r1 im1) (Complex r2 im2) = Complex (r1 + r2) (im1 + im2)
    (*) (Complex r1 i1) (Complex r2 i2) = Complex (r1 * r2 - i1 * i2) (r1 * i2 + r2 * i1)
    fromInteger n = Complex (fromInteger n) 0
    negate (Complex r im) = Complex (-r) (-im)
    abs (Complex r im) = Complex (sqrt(r*r+im*im)) 0
    signum _ = error "Complex: signum isn't defined"

instance Fractional Complex where
    (/) (Complex r1 i1) (Complex r2 i2) = Complex ((r1*r2 + i1*i2) / sqrsum) ((-r1 * i2 + i1*r2) / sqrsum) 
        where sqrsum = r2*r2 + i2*i2
    fromRational r = Complex (fromRational r) 0

-- show и read должны работать как описано в тестах в Main.hs
instance Show Complex where
    --show = undefined
    show (Complex r im) 
        | r == 0 = show im ++ " * i"
        | im > 0 = show r ++ " + " ++ show im ++ " * i"
        | im < 0 = show r ++ " - " ++ show (abs im) ++ " * i"
        | otherwise = show r

instance Read Complex where
    --readsPrec :: Int -> String -> [(a,String)]
    readsPrec i str = 
        let (r,str') = parseDouble str 
            parseDouble s = case (readsPrec i s :: [(Double, String)]) of 
                [] -> error "Parsing error"
                (x:xs) -> x 
        in case splitAt 3 str' of
            (" + ", str'') -> 
                let (im, " * i") = parseDouble str'' in [(Complex r im, "")]
            (" - ", str'') -> 
                let (im," * i") = parseDouble str'' in [(Complex r (-im),"")]
            (" * ", ('i':str'')) -> [(Complex 0 r,str'')]
            (_, str'') -> [(Complex r 0, str'')]



i :: Complex
i = Complex 0 1
