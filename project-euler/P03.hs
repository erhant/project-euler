module P3 where

import Data.List (find)
import Data.Maybe (fromJust, isNothing)

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
  Nothing -> rightChild -- no factor, rightChild is a prime now

-- Largest Prime Factor using the factor tree method
lpf :: Int -> Int
lpf n =
  factorTree
    (2 : [3, 5 .. (floor (sqrt (fromIntegral n)))]) -- list of primes
    n -- right child: parent / smallest factor
    1 -- left child: smallest factor (1 for dummy)

-------------------------------------main-------------------------------------
main :: IO ()
main = do
  putStrLn "Answer:"
  print (lpf 600851475143)