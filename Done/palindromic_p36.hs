

import Numeric
import Data.Char


isPalindromic n = (show n) == (reverse $ show n)

showBin n = showIntAtBase 2 intToDigit n ""

isBinPalindromic n = (showBin n) == (reverse $ showBin n)
