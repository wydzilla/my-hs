


import Primes

isTrunk i = isLTrunk i && isRTrunk i

isLTrunk i  | not $isPrime i    = False
	    | i < 10           = True
	    | otherwise        = isLTrunk ( read (tail $show i)::Integer)

isRTrunk i | not $ isPrime i    = False
	   | i < 10             = True
	   | otherwise          = isRTrunk (read (init $show i)::Integer)
