

n 0 _ = 1

n row col = sum $ map (n (row - 1)) [0..col]

-- n 1 = [1..21]

m :: Num a => a -> [a] -> [[a]] -> [[a]]
m 21 _ acc = acc
m row l acc = m (row + 1) newRow ([newRow] ++ acc) 
		where newRow = map (\x -> sum $ take x l)  [1..21]

