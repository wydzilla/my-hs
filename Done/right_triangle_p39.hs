

isRight :: Integral a => (a, a) -> Bool
isRight (a, b) = (fromIntegral $ floor x) == x where x = sqrt $ fromIntegral (a^2 + b^2) 

pairs :: Integral a => [(a,a)]
pairs = [(x,y)| x<- [1..1000], y<-[1..1000]]

--r1 :: Integral a => [(a,a)]
--r1 = filter ($ filter isRight pairs

cc (x,y) = floor $ sqrt $ fromIntegral (x^2+y^2)

tris [] = []
tris ((x,y):xs) | x + y + c < 1000  = [x,y,c]:tris xs
		| otherwise         = tris xs 
		where c = cc (x,y)


groupBy (==)$ sort $ map sum $ tris $ filter isRight pairs 
map length $groupBy (==)$ sort $ map sum $ tris $ filter isRight pairs
