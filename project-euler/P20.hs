module P16 where

import Data.Char (digitToInt)

factorial :: Integer -> Integer
factorial 1 = 1
factorial n = n * factorial (n - 1)

ans :: Integer -> Int
ans n = sum (map digitToInt (show $ factorial n))

-------------------------------------main-------------------------------------
main :: IO ()
main = do
  putStrLn "Answer:"
  print (ans 100)

-------------------------------------note-------------------------------------

-- TODO: Need to optimize