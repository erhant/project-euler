module P6 where

-- | Finds (square of sums) - (sum of squares)
sqdiffNaive :: Int -> Int
sqdiffNaive n = 2 * _sqdiffNaive [1 .. n]
  where
    _sqdiffNaive :: [Int] -> Int
    _sqdiffNaive [] = 0
    _sqdiffNaive [n] = 0
    _sqdiffNaive (n : ns) = sum (map (* n) ns) + _sqdiffNaive ns

-- | Finds (square of sums) - (sum of squares) in a better way
sqdiffBetter :: Int -> Int
sqdiffBetter n = do
  let s = (n * (n + 1)) `div` 2 -- good ol' Gauss formula
  let ss = (s * (2 * n + 1)) `div` 3 -- same as (2n+1)(n+1)(n)/6
  (s * s) - ss

-------------------------------------main-------------------------------------
main :: IO ()
main = do
  putStrLn "Naive Answer:"
  print (sqdiffNaive 100)

  putStrLn "Better Answer:"
  print (sqdiffBetter 100)

-------------------------------------note-------------------------------------

-- | For the naive answer:
-- | given [a, b, c], we can find (a+b+c)^2 - (a^2 + b^2 + c^2) as
-- | 2 * (a*b + a*c + b*c)

-- | The faster answer is given by a formula. If n is given, then
-- | sum = n * (n + 1) / 2  and
-- | sum of squares = (2n + 1) * (n + 1) * (n) / 6
-- | the answer is then sum^2 - sum of squares