import Data.List
isTruncatable n | n < 10  = False
				| length (intersect "245680" (show n)) /= 0 = False
				| otherwise  = all isPrime (map toInt truncs) where
						truncs = truncates (show n) 
							++  rtruncates (show n)

primesList = sieveTo 1000000
isPrime x = if (not (isPrimeHelper x))
			then False else x `elem` (takeWhile (<=x) primesList) 

isPrimeHelper n | n `elem` [2,3,5,7,11,13,17,19,21,23,29]  = True 
				| n `mod` 2 == 0  = False
				| n `mod` 3 == 0  = False
			    | n `mod` 5 == 0  = False
			    | n `mod` 7 == 0  = False
			    | n `mod` 11 == 0  = False
			    | n `mod` 13 == 0  = False
			    | n `mod` 17 == 0  = False
			    | n `mod` 19 == 0  = False
			    | n `mod` 21 == 0  = False
			    | n `mod` 23 == 0  = False
			    | n `mod` 29 == 0  = False
			    |otherwise = True

toInt :: [Char] -> Int
toInt x = read x

truncates [] = []
truncates (x:xs) = [x:xs] ++ truncates xs

rtruncates [] = []
rtruncates xs = [xs] ++ rtruncates (init xs)



primesTo m = 2 : erastos [3,5..m] where
			erastos [] = []
			erastos (p:xs) = p : erastos (xs `minus` [p, p+2*p..m])

sieveTo m = 2 : sieve [3,5..m] where
			sieve [] = []
			sieve (p:xs) = p : sieve (xs `minus` [p*p, p*p + p..m])

minus (x:xs) (y:ys) = case (compare x y) of 
		   LT -> x : minus  xs  (y:ys)
		   EQ ->     minus  xs     ys 
		   GT ->     minus (x:xs)  ys
minus  xs     _     = xs
--union (x:xs) (y:ys) = case (compare x y) of 
		   --LT -> x : union  xs  (y:ys)
		   --EQ -> x : union  xs     ys 
		   --GT -> y : union (x:xs)  ys
--union  xs     []    = xs
--union  []     ys    = ys

