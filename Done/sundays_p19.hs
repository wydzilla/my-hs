isLeap x = (x `mod` 4 == 0 && x `mod` 100 /= 0) || x `mod` 400 == 0

lenOfMonth y 1 = 31
lenOfMonth y 2 = if isLeap y then 29 else 28 
lenOfMonth y 3 = 31
lenOfMonth y 4 = 30
lenOfMonth y 5 = 31
lenOfMonth y 6 = 30
lenOfMonth y 7 = 31
lenOfMonth y 8 = 31
lenOfMonth y 9 = 30
lenOfMonth y 10 = 31
lenOfMonth y 11 = 30
lenOfMonth y 12 = 31

lenOfYear y = sum $ map (lenOfMonth y) [1..12]

dayOfYear y m d = sum ( map (lenOfMonth y )[1..(m-1)] ) + d
dayOfWeek 1900 1 1 = 0
dayOfWeek y m d = ((sum $ map lenOfYear [1900..(y-1)]) + (dayOfYear  y m d) - 1) `mod` 7

result = length $ filter (==6) [dayOfWeek y m 1 | y <-[1901..2000], m<-[1..12]]
