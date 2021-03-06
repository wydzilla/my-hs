module Tools.Diagonal where

genFromPair (e1, e2) = diagonal [[[ x*e1 , y*e2 ] | x <- [0..]] | y <- [0..]]

diagonal :: [[a]] -> [a]
diagonal = concat . stripe
	where
    stripe [] = []
    stripe ([]:xss) = stripe xss
    stripe ((x:xs):xss) = [x] : zipCons xs (stripe xss)

    zipCons [] ys = ys
    zipCons xs [] = map (:[]) xs
    zipCons (x:xs) (y:ys) = (x:y) : zipCons xs ys


