import Data.Char

ir = concat $ map show [1..]

d n = digitToInt $ ir !! (n - 1)

result = product $ map d [10^n | n <- [0..6]]
