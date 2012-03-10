import Data.Map
import Data.List

factorization::Integer -> [Integer]
factorization i = primeFactors i primes

primes = [
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

primeFactors 1 _ = []
primeFactors _ [] = []
primeFactors i (x:xs) | isPrimeFactor i x     = x:(primeFactors (i `div` x) (x:xs))
		      | otherwise             = primeFactors i xs

isPrimeFactor i j | j > i 		= False
		  | i `mod` j /= 0 	= False
		  |otherwise  		= True


--asssume that list is sorted
countOccurences xs = fromList [ (head x,length x)| x <- (groupBy (==) xs) ]
countDivs xd = product $ Data.List.map (+1) (elems xd)

triangles = tri 1

tri 1 = 1
tri n = tri (n -1) + n 

triangle n = (n * (n+1)) `div` 2


divs = countDivs . countOccurences . factorization

result = first $ List.filter (\(x,_)-> x>500)$ List.map (\x -> ((divs . triangle) x, triangle x)) [10000..100000]
