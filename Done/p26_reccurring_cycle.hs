
calcFraction d = calcFraction' 10 d []


calcFraction' e d results | elem xy results = dropWhile (\ab -> xy /= ab) results
	        	  | otherwise      = calcFraction' (y * 10) d (results ++ [xy])
			where (x,y) = e `divMod` d
			      xy = (x,y)

maxLen = maximum map (length.calcFraction) [1..1000]
result = filter (\x -> (length.calcFraction) x == 982) [1..1000] 
