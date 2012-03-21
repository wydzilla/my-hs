import Data.List (permutations)
import Tools.Utils

test1 xs = (last xs) `mod` 2 == 0
test2 xs = (sum xs) `mod` 3 == 0
test3 xs = (last xs) == 5 || (last xs) == 0
test4  = divisibleBy 7
test5  = divisibleBy 11
test6 = divisibleBy 13
test7 = divisibleBy 17

divisibleBy n xs = (fromDigits xs) `mod` n == 0

subList startIndex xs = take 3 $ drop startIndex xs
test  xs = (test1  $ subList 1 xs) && 
	(test2  $ subList 2 xs) && 
	(test3  $ subList 3 xs) && 
	(test4  $ subList 4 xs) && 
	(test5  $ subList 5 xs) && (test6  $ subList 6 xs) && (test7  $ subList 7 xs)

result' = filter test $ permutations [0..9]

result = sum $ map fromDigits result'
