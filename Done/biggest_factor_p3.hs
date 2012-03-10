factor ::Integer -> [Integer]
factor n = factors n 2

factors :: Integer -> Integer -> [Integer]
factors 1 _ = []
factors n m | n < m         = []
	    | n `mod` m == 0     = m : factors (n `div` m) m
	    | otherwise          = factors n (m + 1)

bigNum = 600851475143

