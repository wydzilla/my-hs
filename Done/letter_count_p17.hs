toWord 0 = ""
toWord 1 = "one"
toWord 2 = "two"
toWord 3 = "three"
toWord 4 = "four"
toWord 5 = "five"
toWord 6 = "six"
toWord 7 = "seven"
toWord 8 = "eight"
toWord 9 = "nine"
toWord 10 = "ten"
toWord 11 = "eleven"
toWord 12 = "twelve"
toWord 13 = "thirteen"
toWord 14 = "fourteen"
toWord 15 = "fifteen"
toWord 16 = "sixteen"
toWord 17 = "seventeen"
toWord 18 = "eighteen"
toWord 19 = "nineteen"
toWord 20 = "twenty"
toWord 30 = "thirty"
toWord 40 = "forty"
toWord 50 = "fifty"
toWord 60 = "sixty"
toWord 70 = "seventy"
toWord 80 = "eighty"
toWord 90 = "ninety"
toWord 100 = "hundred"
toWord 1000 = "onethousand"

units x = x `mod` 10
tens x = (x `mod` 100) `div` 10
hunds x = (x `mod` 1000) `div` 100

hundswords x = if h >= 2 then toWord h ++ toWord 100 
			   else 
			   	if h==1 then "onehundred" 
			   	else ""
			   where h = hunds x

thswords x = if x==1000 then toWord 1000 else ""
tenswords x = if d>=2 then toWord (d*10) else "" where d = tens x

teenswords x = if 10<=x && x<=19 then  toWord x else ""
unitwords x = if (units x)<=9 && tens x /= 1 then toWord $ units x else "" 

asWords x = [thswords x, hundswords x, tenswords x, teenswords (x `mod` 100), 
			unitwords (x)] 

lettercount x = let s = sum $ map length $ asWords x in 
				if x > 100 && x `mod` 100 > 0 then s +3
				else s

