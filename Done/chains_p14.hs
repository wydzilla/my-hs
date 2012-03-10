

chain 1 = [1]
chain n | even n  = n:(chain (n `div` 2))
	| otherwise = n:(chain (3 * n + 1))

chains = map chain [1..1000000]
lim (l,_) = l > 375
result = filter lim $ zip (map length chains) (map head chains)
