module Tools.Utils where

import Data.Char (digitToInt)


isInt x = (fromIntegral $ floor x) == x 


digits x = map digitToInt $ show x

fromDigits [] = 0
fromDigits (d:ds) = 10^(length ds) * d + fromDigits ds
