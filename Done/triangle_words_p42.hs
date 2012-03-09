tri n = 1/2 * n * (n-1)
tris = map tri [1..2000]

import Data.Char (ord)
value c = (ord (toLower c)) - (ord 'a')

wordValue = map value

test x =  hasAny tris [x]

import System.IO
import Data.Char(toUpper)

main :: IO ()
main = do 
       inh <- openFile "words.txt" ReadMode
       mainloop inh 
       hClose inh

mainloop :: Handle -> Handle -> IO ()
mainloop inh = 
    do
    list <- []
    ineof <- hIsEOF inh
       if ineof
           then return ()
           else do inpStr <- hGetLine inh
                    
                   mainloop inh 

