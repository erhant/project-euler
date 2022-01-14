module P16 where

import Data.Char (digitToInt)

ans :: Integer -> Int
ans n = sum (map digitToInt (show $ 2 ^ n))

-------------------------------------main-------------------------------------
main :: IO ()
main = do
  putStrLn "Answer:"
  print (ans 1000)

-------------------------------------note-------------------------------------

-- TODO: Need to optimize