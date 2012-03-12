import Tools.Divisors

import Data.List (nub, sort)


isAbundant n = proper_d n > n

maxn = 28123

abundant = filter isAbundant [12..maxn]

smallAbundable' = filter (<=maxn) [x+y | x <- abundant, y<- abundant]

smallAbundable = nub $ sort smallAbundable'

sumAll = sum [1..maxn]

result = sumAll - (sum smallAbundable)
