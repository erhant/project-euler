module P2 where

fibonacciSum :: Int -> Int -> Int -> Int
fibonacciSum lim prev cur
  | cur > lim = 0 -- boundary condition returns 0
  | even cur = cur + fibonacciSum lim cur (prev + cur) -- add if current value is even
  | otherwise = fibonacciSum lim cur (prev + cur) -- recurse without adding otherwise

fibonacci :: Int -> Int
fibonacci lim
  | lim < 2 = 0
  | otherwise = fibonacciSum lim 1 2

-------------------------------------main-------------------------------------
main :: IO ()
main = do
  putStrLn "Answer:"
  print (fibonacci 4000000)
