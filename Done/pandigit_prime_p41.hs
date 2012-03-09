import Primes
import Data.List

isprime_helper x = not $ elem (last x) "24568"  

result` = filter isPrimeHelper $ 
		map (\x -> read x::Integer)$filter isprime_helper $ permutations "1234567" 

result = maximum $ filter isPrime result`
	
