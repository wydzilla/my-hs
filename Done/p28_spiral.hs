startsWith l = (l-2)^2 + 1

levelSum 1 = 1
levelSum l = 4 *x + 10 * k - 4
		where 
			x = startsWith l
			k = l - 1




spiral n = sum $ map levelSum $ filter odd [1..n]

result = spiral 1001
