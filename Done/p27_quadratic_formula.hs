import Tools.Primes

preFormula (a, b) n = n ^ 2 + a * n + b 

test n formula = isPrime'(formula n) 


countPrimes' formula n | test n formula  = countPrimes' formula (n+1)
	    	       | otherwise       = n

countPrimes formula = countPrimes' formula 1


testRanges = filter (\p -> (testParams p) && (testParams2 p)) $[(a,b)| a<-[-999,-997..999], b<-(takeWhile (<1000) primes)]

testParams ::  (Integer,Integer) -> Bool
testParams (a,b) | a^2<4*b       = True
		 | max ((-(fromIntegral a::Double) - delta)/2) ((-(fromIntegral a::Double) + delta)/2) < 0 = True
		 |otherwise = False
			where delta = sqrt  ( fromIntegral(a^2 - 4 * b)::Double) 

testParams2 (a,b) | a>=0 = True
		  | (b+1)>(-a) = True
		  | otherwise = False

formulas = map preFormula testRanges


result' = map countPrimes formulas
result = filter (\p -> ((countPrimes (preFormula p))) == 71 )  testRanges

