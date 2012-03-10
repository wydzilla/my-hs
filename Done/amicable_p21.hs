
import Data.List
import Primes


dhelper [] = 0
dhelper xs = sum [(head xs) ^ i | i <- [0..(length xs)]]

--wikipedia mowi ze funkcja d jest multiplikatywna
d i = product (Data.List.map dhelper $ coprimes i)

proper_d  i = (d i) - i

isAmicable i =  (i /= a) && (i == proper_d a) where a = proper_d i

result = sum $ filter isAmicable [2..10000]
