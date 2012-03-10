
import Data.Set



step e s = insert e s


helper  = fromList [a^b | a <- [2..100] , b<-[2..100]]

result = length $ toList helper 
