module P9 where

import Data.List (find)

-- Finds a Pythagorean triplet a*a + b*b = c*c where a+b+c=n
findTriplet :: Int -> [Int]
findTriplet n =
  head
    [ [a, b, c]
      | b <- [1 .. n],
        a <- [1 .. b],
        let c = n - a - b,
        a ^ 2 + b ^ 2 == c ^ 2
    ]

-------------------------------------main-------------------------------------
main :: IO ()
main = do
  putStrLn "Answer:"
  let triplet = findTriplet 1000
  print triplet

  putStrLn "Product of Answer:"
  print (product triplet)

-------------------------------------note-------------------------------------

{-
A Pythagorean triplet is a set of three natural numbers, a < b < c,
for which a^2 + b^2 = c^2

Suppose that a + b + c = n exists (does for n = 1000)

Then:
a + b = n - c
a^2 + b^2 + 2ab = n^2 + c^2 - 2nc

Since a^2 + b^2 = c^2, then:
2(ab + nc) = n^2
-}