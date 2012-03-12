import Tools.Utils
import Tools.Primes (isPrime')
import Data.List (intersect)

rotate1 [] = []


--rotate1 x:xs = xs ++ [x]



rotate xs n = take (length xs) $ drop n $ cycle xs

rotations xs = map  (rotate xs) [1..length xs - 1]


test ds | intersect [2,4,5,6,8,0] ds /= []    = False
	| otherwise = all isPrime' $ map fromDigits $ rotations ds

isCircularPrime = test . (map toInteger) . digits

primes = filter isPrime' [1..1000000]

result = filter isCircularPrime primes
