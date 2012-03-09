import Text.Regex
import Data.Char

main = do 
			x <- readFile "names.txt"
			let y = map value $ names x
			let z = [a * b| a<- y, b <- [1..length y] ]
			print $ sum z
		

names s = splitRegex (mkRegex ",") s

valuec c = (ord c) - (ord 'A') + 1
value s = sum (map valuec s) 

