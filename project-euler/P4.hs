module P4 where

-- | Largest Palindrome Product, the parameter is the number of digits
lpp :: Int -> Int
lpp d = 
  
-------------------------------------main-------------------------------------
main :: IO ()
main = do
  putStrLn "Answer:"
  print (lpf 600851475143)