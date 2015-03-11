-- 1. fib n вовзращает n-ое число Фибоначчи.
--    Функция должна работать за линейное вермя и определена для всех целых n.
--    Для отрицательных n значение определяется по формуле fib n = fib (n + 2) - fib (n + 1).
--    (1 балл)
fib :: Integer -> Integer
fib n = fst(fib'(n))
		where 
			fib' n | n == 0 = (0,1)
				   | n > 0  = let(fibnm1, fibn) = (fib'(n-1))
								in (fibn,fibnm1 + fibn)
				   | otherwise  = let(fibnp,fibn) = (fib'(n+1))
				   					in(fibn,fibnp - fibn)


-- 2a. Написать функцию, возвращающую количество цифр числа.
--     Для целочисленного деления можете использовать функции div и mod.
--    (0.5 балла)
numberOfDigits :: Integer -> Integer
numberOfDigits n  = if (n `div` 10) == 0 
						then 1
				    	else (numberOfDigits (n `div` 10)) + 1

-- 2b. Написать функцию, возвращающую сумму цифр числа.
--    (0.5 балла)
sumOfDigits :: Integer -> Integer
sumOfDigits n | (n `div` 10) == 0 = (n `mod` 10)
			  | otherwise = ((sumOfDigits (n `div` 10)) + (n `mod` 10))



-- 3. gcd' возвращает НОД.
--    (1 балл)
gcd' :: Integer -> Integer -> Integer
gcd' a b | a < b = gcd b a
		 | otherwise = gcd a b 
		 where gcd a b | (b == 0) = a
		 			   | otherwise = gcd b (a `mod` b)


-- 4. minp p возвращает минимальное по модулю число x такое, что p x == True. Если такого x не существует,
-- minp не завершается.
--    (1 балл)
minp :: (Integer -> Bool) -> Integer
minp p = minp' p 0 
			where minp' p n  = if (p n)||(p  (-n)) 
								then n
								else minp' p (n+1)

-- 5. integral f a b возвращает значение определенного интеграла функции f на отрезке [a,b].
--    Для реализации можете использовать метод трапеций.
--    (2 балла)
-- так как вы не указали какие у нас узлы интегрирований или хотя бы сколько их,
-- я отрезок не разбивала.
integral :: (Double -> Double) -> Double -> Double -> Double
integral f a b = ((f a)+(f b))/2*(b-a)

-- 6. Реализуйте оператор примитивной рекурсии rec, используя функцию (-), укажите тип rec.
--    (1 балл)
rec :: a -> (Integer -> a -> a) -> Integer -> a
rec z s 0 = z
rec z s n = s n (rec z s (n-1))

-- 7. Реализуйте факторил при помощи rec.
--    (1 балл)
facRec :: Integer -> Integer
facRec n = rec 1 (*) n

-- 8. Реализуйте факториал при помощи fix.
--    (1 балл)

facFix :: Integer -> Integer
facFix n = fix (\fac' n -> if n == 0 then 1 else n * fac' (n-1)) n
  where fix f = f (fix f)
