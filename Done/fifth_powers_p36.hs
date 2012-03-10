import Data.Char


upto = 354294


toDigits :: Int-> [Int]
toDigits n = map digitToInt $ show n


test x = (sum $ map (\n ->n^5) $ toDigits x ) == x
