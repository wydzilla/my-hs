

tri n = n * (n+1) /2 
pen n = n * (3*n +1)/2
hex n = n * (2*n -1)

--isPen::RealFrac a => a->Bool
isPen n =  fromInteger (floor x) == x where x = ( 1 + sqrt (1+24 * n)) / 6


isHex n = fromInteger (floor x) == x where x = (1  + sqrt (1+ 8*n)) / 4
