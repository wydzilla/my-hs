check (x, y) = x**2 + y **2 == (1000 - x - y) ** 2

result = filter check [(x,y) | x<-[1..1000], y<-[1..1000]]
