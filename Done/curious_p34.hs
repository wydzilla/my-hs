import Char
-- 99 999 999 jest juz wieksze niz 8*9!

nums = [10..99999999]

factorial n = [1,1,2,6,24,120,720,5040,40320,362880 ]!!n
toDigs n = map digitToInt $ show n

val n = sum $ map factorial $ toDigs n

test n = val n == n

result = sum $ filter test nums 
