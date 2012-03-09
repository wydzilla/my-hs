

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
union (x:xs) (y:ys) = case (compare x y) of 
		   LT -> x : union  xs  (y:ys)
		   EQ -> x : union  xs     ys 
		   GT -> y : union (x:xs)  ys
union  xs     []    = xs
union  []     ys    = ys

result = length $ sieveTo 1000000
