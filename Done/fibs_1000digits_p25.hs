import Data.List
import Data.Bits
 
fib :: Int -> Integer
fib n = snd . foldl' fib' (1, 0) . dropWhile not $
            [testBit n k | k <- let s = bitSize n in [s-1,s-2..0]]
    where
        fib' (f, g) p
            | p         = (f*(f+2*g), ss)
            | otherwise = (ss, g*(2*f-g))
            where ss = f*f+g*g

--phi = (1 + sqrt 5) / 2
--fib n = floor(phi ^ n / (sqrt 5) + 0.5)

nfib 1474 = toInteger $ fib 1474
nfib 1475 = toInteger $ fib 1475
nfib n = nfib (n-1) + nfib(n-2)

countDigits n = length $ show n

