import Tools.Utils (isInt)
import Tools.Diagonal (diagonal)

pentagonalTest n = isInt $ (1 + sqrt (24 * n + 1)) / 6 

pentagonal n  = fromInteger $ floor $ (3 * n ^ 2 - n ) / 2

pentagonals = map pentagonal [1..]


pentagonalDensity = pentagonalDensity' 1 pentagonals
pentagonalDensity' _ [] = []
pentagonalDensity' n (x:xs) = (n/x) : pentagonalDensity' (n+1) xs


pairs = diagonal [[(a,b) | a <-pentagonals]| b<-pentagonals]


test (a,b) = pentagonalTest(a+b) && pentagonalTest (a-b)


result = head $ filter test pairs 
