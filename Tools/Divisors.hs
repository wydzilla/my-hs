module Tools.Divisors where

import Tools.Primes

dhelper [] = 0
dhelper xs = sum [(head xs) ^ i | i <- [0..(length xs)]]

--wikipedia mowi ze funkcja d jest multiplikatywna
--suma podzielnikow
d i = product (map dhelper $ coprimes i)

--suma poprawnych podzielnikow
proper_d  i = (d i) - i

