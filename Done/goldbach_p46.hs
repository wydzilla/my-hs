
import Tools.Primes
import Data.List (nub, sort)

twiceSquares = ( map (\x -> 2* (x^2)) [0..1000])

goldbach = filter odd [(x+y)| x <- primes, y<-twiceSquares]

prod = nub $ sort goldbach


takeUntil (x:[]) = (0,0)
takeUntil (x:y:xs) | (x-y) == -2   = takeUntil (y:xs)
                   | otherwise    = (x,y)

result = takeUntil prod
