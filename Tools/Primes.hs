module Primes where

import Data.List (groupBy)

factorization::Integer -> [Integer]
factorization i = primeFactors i primes

primes' = [
	2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 
	61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 
	131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193,
	 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 
	271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 
	353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 
	433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 
	509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 
	601, 607, 613, 617, 619, 631, 641, 643, 647, 653, 659, 661, 673, 
	677, 683, 691, 701, 709, 719, 727, 733, 739, 743, 751, 757, 761, 
	769, 773, 787, 797, 809, 811, 821, 823, 827, 829, 839, 853, 857, 
	859, 863, 877, 881, 883, 887, 907, 911, 919, 929, 937, 941, 947, 
	953, 967, 971, 977, 983, 991, 997]

isPrime 1 = False
isPrime i = all (\x -> x== i || i`mod` x /= 0) $ primes' ++ [last primes' + 1 .. floor ((sqrt $fromIntegral i)) + 1]

primes = primes' ++ (filter isPrime [998..10000])

primeFactors 1 _ = []
primeFactors _ [] = []
primeFactors i (x:xs) | isPrimeFactor i x     = x:(primeFactors (i `div` x) (x:xs))
		      | otherwise             = primeFactors i xs

isPrimeFactor i j | j > i 		= False
		  | i `mod` j /= 0 	= False
		  |otherwise  		= True


coprimes i = groupBy (==) $ factorization i
---------------------------------------- ----------------------------------------
---------------------------------------- ----------------------------------------
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

isPrimeHelper n  | n `mod` 2 == 0  = False
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

--isPrime::Int -> Bool
isPrime' x = all (\n->x`mod`n/=0) [2..floor$ sqrt $ fromInteger x]
