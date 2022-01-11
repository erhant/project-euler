module P3 where

import Data.List (find)
import Data.Maybe (fromJust, isNothing)

-- | Consumes twos in a number, so the end result is always an odd number
consumeTwos :: Int -> Int
consumeTwos n
  | even n = consumeTwos (div n 2)
  | otherwise = n

-- find (\a -> (n `rem` a) == 0) primes

-- | The factor tree here is not really a tree, but a line of nodes with one leaf each.
-- | As such, there is no need to recurse for each child. Each left child is a prime.
-- | Right child is the number that we keep factoring.

-- | Factor Tree implementation (the list of primes is store-passed)
factorTree :: [Int] -> Int -> Int -> Int
factorTree primes rightChild leftChild = case find (\a -> (rightChild `rem` a) == 0) primes of
  Just lc ->
    if lc == rightChild
      then lc -- number itself is a prime
      else
        factorTree
          primes -- list of primes (TODO: can be updated on each recursion to reduce search space)
          (rightChild `div` lc) -- new right child
          lc -- new left child
  Nothing -> rightChild

-- Largest Prime Factor using the factor tree method
lpf :: Int -> Int
lpf n
  | n < 2 = -1 -- edge case
  | otherwise = do
    let m = consumeTwos n -- first we get rid of twos
    if m == 1
      then 2 -- number was a power of 2
      else
        factorTree
          (filter odd [3 .. (floor (sqrt (fromIntegral m)))]) -- list of primes
          m -- right child: parent / smallest factor
          2 -- left child: smallest factor (we can give 2 even if the input was odd, because lpf n = lpf 2*n for odds)

-------------------------------------main-------------------------------------
main :: IO ()
main = do
  putStrLn "Answer:"
  print (lpf 600851475143)