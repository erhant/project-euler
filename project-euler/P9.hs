module P9 where

findTriplet :: Int -> (Int, Int, Int)
findTriplet lim = (3, 4, 5)

-------------------------------------main-------------------------------------
main :: IO ()
main = do
  putStrLn "Answer:"
  print (findTriplet 1000)

-------------------------------------note-------------------------------------

{-
A Pythagorean triplet is a set of three natural numbers, a < b < c,
for which a^2 + b^2 = c^2

Constraint is a + b + c = n

Then:
a + b = n - c
a^2 + b^2 + 2ab = n^2 + c^2 - 2nc

Since a^2 + b^2 = c^2, then:
2(ab + nc) = n^2

We can treat ab as a single number d, and have:
2(d + nc) = n^2

If d is factorized thats good, if not, we can set a = 1 and b = d.
-}