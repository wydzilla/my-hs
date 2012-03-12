import Text.Regex
import Data.Char
import Data.List (sort)

main = do 
			x <- readFile "names.txt"
			let y = map value $ sort $ map (\x -> tail $ init x) $ names x
			--let z = [a * b| a<- y, b <- [1..length y] ]
			let z = tot 1 y
			print z
		

names s = splitRegex (mkRegex ",") s

valuec c = (ord c) - (ord 'A') + 1
value s = sum (map valuec s) 

tot _ [] = 0
tot n (x:xs) = (n*x) + tot (n+1) xs
