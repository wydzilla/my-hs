module Utils where

import Data.Char (digitToInt)


isInt x = (fromIntegral $ floor x) == x 


digits x = map digitToInt $ show x
