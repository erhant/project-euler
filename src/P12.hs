module P12 where

import Data.List (find)
import Data.Map (fromListWith, toList)
import Data.Maybe (fromMaybe)

-- | taken from: https://stackoverflow.com/questions/10398698/haskell-counting-how-many-times-each-distinct-element-in-a-list-occurs/22398506
frequency :: [Int] -> [(Int, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

-- | Returns a list of prime factors of the given number. The list is sorted by implementation.
getPrimeFactors :: Int -> [Int]
getPrimeFactors n =
  _getPrimeFactors
    (2 : [3, 5 .. (floor (sqrt (fromIntegral n)))]) -- list of primes
    n
  where
    _getPrimeFactors :: [Int] -> Int -> [Int]
    _getPrimeFactors primes 1 = []
    _getPrimeFactors primes n = case find (\a -> (n `rem` a) == 0) primes of
      Just factor ->
        factor :
        _getPrimeFactors
          primes
          (n `div` factor)
      Nothing -> [n]

numFactorsOfNthTriangleNumber :: Int -> Int
numFactorsOfNthTriangleNumber n = foldr (\x y -> y * (snd x + 1)) 1 (frequency (getPrimeFactors (div (n * (n + 1)) 2)))

ans :: Int -> Int
ans lim =
  fromMaybe
    2 -- default value (number itself and 1, though this is not expected to happen)
    (find (\x -> numFactorsOfNthTriangleNumber x > lim) [1 ..])

-------------------------------------main-------------------------------------
main :: IO ()
main = do
  putStrLn "Answer:"
  let n = ans 500
  print (n * (n + 1) `div` 2)

-------------------------------------note-------------------------------------

{-
We can modify the factor tree code from P03.hs, and calculate the factors

-}