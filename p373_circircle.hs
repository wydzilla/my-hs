import Tools.Utils (isInt)
import Tools.Primes (coprimes, factorization)
import Data.List (sort, groupBy, genericLength, delete)
import Data.Maybe (mapMaybe)

radius a b c = (a * b * c) / (4 * area a b c)

area a b c = sqrt (p * (p-a)*(p-b)*(p-c)) where p = (a+b+c)/2

lcmList = foldl lcm 1

generateHeronians a b c = [(a * i, b * i, c * i)| i<-[1..]] 

isRadiusInt (a,b,c) = isInt $ radius a b c

sides (_,_,a,b,c) = (a,b,c)

triCurry f = \(a,b,c) -> f a b c
triUncurry f = \a -> (\b-> (\c->(f (a,b,c))))

--properUpToR :: (Integral a, RealFloat b) => a-> (a,a,a) -> [b]
properUpToR r (a, b, c) = map toInteger $ filter isInt 
			$ takeWhile (<= r) $
				map (triCurry radius) $ generateHeronians a b c  

--sItem :: Integral a => a -> (a,a,a,a,a) -> a 
sItem r i = sum $ properUpToR r $ sides i

--s :: Integer -> Integer
s r = sum $ map (sItem r) fundamentalHeronian

-- area, perimeter, a, b, c
fundamentalHeronian :: Integral a => [(a,a,a,a,a)]
fundamentalHeronian = [
		(6, 12, 5, 4, 3),
		(12, 16, 6, 5, 5),
		(12, 18, 8, 5, 5),
		(24, 32, 15, 13, 4),
		(30, 30, 13, 12, 5),
		(36, 36, 17, 10, 9),
		(36, 54, 26, 25, 3),
		(42, 42, 20, 15, 7),
		(60, 36, 13, 13, 10),
		(60, 40, 17, 15, 8),
		(60, 50, 24, 13, 13),
		(60, 60, 29, 25, 6),
		(66, 44, 20, 13, 11),
		(72, 64, 30, 29, 5),
		(84, 42, 15, 14, 13),
		(84, 48, 21, 17, 10),
		(84, 56, 25, 24, 7),
		(84, 72, 35, 29, 8),
		(90, 54, 25, 17, 12),
		(90, 108, 53, 51, 4),
		(114, 76, 37, 20, 19),
		(120, 50, 17, 17, 16),
		(120, 64, 30, 17, 17),
		(120, 80, 39, 25, 16),
		(126, 54, 21, 20, 13),
		(126, 84, 41, 28, 15),
		(126, 108, 52, 51, 5),
		(132, 66, 30, 25, 11),
		(156, 78, 37, 26, 15),
		(156, 104, 51, 40, 13),
		(168, 64, 25, 25, 14),
		(168, 84, 39, 35, 10),
		(168, 98, 48, 25, 25),
		(180, 80, 37, 30, 13),
		(180, 90, 41, 40, 9),
		(198, 132, 65, 55, 12),
		(204, 68, 26, 25, 17),
		(210, 70, 29, 21, 20),
		(210, 70, 28, 25, 17),
		(210, 84, 39, 28, 17),
		(210, 84, 37, 35, 12),
		(210, 140, 68, 65, 7),
		(210, 300, 149, 148, 3),
		(216, 162, 80, 73, 9),
		(234, 108, 52, 41, 15),
		(240, 90, 40, 37, 13),
		(252, 84, 35, 34, 15),
		(252, 98, 45, 40, 13),
		(252, 144, 70, 65, 9),
		(264, 96, 44, 37, 15),
		(264, 132, 65, 34, 33),
		(270, 108, 52, 29, 27),
		(288, 162, 80, 65, 17),
		(300, 150, 74, 51, 25),
		(300, 250, 123, 122, 5),
		(306, 108, 51, 37, 20),
		(330, 100, 44, 39, 17),
		(330, 110, 52, 33, 25),
		(330, 132, 61, 60, 11),
		(330, 220, 109, 100, 11),
		(336, 98, 41, 40, 17),
		(336, 112, 53, 35, 24),
		(336, 128, 61, 52, 15),
		(336, 392, 195, 193, 4),
		(360, 90, 36, 29, 25),
		(360, 100, 41, 41, 18),
		(360, 162, 80, 41, 41),
		(390, 156, 75, 68, 13),
		(396, 176, 87, 55, 34),
		(396, 198, 97, 90, 11),
		(396, 242, 120, 109, 13) ]


generateTriangles n  = [ (a,b,c) | a <-[1..n], b <- [fromIntegral $ceiling ((fromIntegral(a+1))* 0.5) .. a], c<-[a+1-b .. b]]

--generateTriangles n = [(a,b,c) | a <- [1..n] , b<- [1..n], c<-[1..n], (a + b) > c && (a +c) > b && (b+c)>a]
radius3 (a,b,c) = radius (fromIntegral a::Double) (fromIntegral b::Double) (fromIntegral c::Double)

isInt2 r = abs ((r::Double) - fromInteger(floor (r::Double))) < 0.00000001
maybeRadius2 :: Integral a => (a,a,a) -> Maybe a
maybeRadius2 (a, b, c) | isInt2 r = Just (floor r)
		       | otherwise = Nothing
			where r = radius (fromIntegral a::Double) (fromIntegral b::Double) (fromIntegral c::Double)

funcS2 :: Integral t => t -> t
funcS2 n = sum $ filter (<= n) $mapMaybe maybeRadius2 $ generateTriangles (2*n)

generateHeronians2 n = filter (triCurry heronianTest) $ generateTriangles n

testFactors a b c = [(a+b+c), (a+b-c), (a-b+c), (-a+b+c)]

heronianTest a b c = (foldl sfpProd 1 $ testFactors a b c ) == 1

sfpProd a b = x * y `div` (gcd x y) ^ 2 
			where x = sfp a
			      y = sfp b	

sfp n = product $ map removeSquareFactors $ coprimes n  

removeSquareFactors xs | (even $ length xs)  = 1
		       | otherwise  = head xs

sfps = map sfp [1..10000000]

isSqrtable n = (sfps !! n) == 1

getSquareFactors xs | (even $ length xs) =  (head xs)^(toInteger $floor (((genericLength xs)) * 0.5))
		    | otherwise = 1
intSqrt = product . intSqrtFactors
intSqrtFactors xs = map getSquareFactors $ groupBy (==) $ sort $ concat $ map factorization xs

ffact a b c = concat $ map factorization [a,b,c] 

--ys shorter
removeCommon xs [] rest = (xs, rest)
removeCommon xs (y:ys) rest | y `elem` xs = removeCommon (delete y xs) ys rest
                     	  | otherwise   = removeCommon xs ys (y:rest)

maybeRadius (a, b, c) | denominatorLi  == [] = Just $ product enumeratorLi
		  | otherwise = Nothing
		where (enumeratorLi, denominatorLi) = removeCommon (ffact a b c) ([2,2] ++ (factorization $ area2 a b c)) []

heronFactors a b c = [p, p - a, p - b, p - c] where p = (a+b+c) `div` 2


area2 a b c = intSqrt $ heronFactors a b c

funcS n = sum $ filter (<=n) $ mapMaybe (maybeRadius) $ generateHeronians2 n



