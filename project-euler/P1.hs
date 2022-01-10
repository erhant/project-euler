module P1 where

-- | Simple filter based method
ansNaive :: Int -> Int
ansNaive n
  | n > 3 = sum (filter (\x -> (x `rem` 3 == 0) || (x `rem` 5 == 0)) [1 .. (n - 1)])
  | otherwise = 0

-- | There are floor((n - 1) / m) numbers that are multiples of m up to n (exclusive).
-- | Say there are k numbers that are multiple of 3. Then their sum is 3 * (k * (k + 1)) / 2
-- | We find this for 3 and 5 but need to exclude 15 once, because it is going to be counted twice.
-- | Better solution
ansBetter :: Int -> Int
ansBetter n
  | n > 3 = sumMultiples n 3 + sumMultiples n 5 - sumMultiples n 15
  | otherwise = 0

sumMultiples :: Int -> Int -> Int
sumMultiples n m = do
  let k = (n - 1) `div` m
  (m * k * (k + 1)) `div` 2 -- guaranteed to be an int due to k * (k+1) being even

-------------------------------------main-------------------------------------
main :: IO ()
main = do
  putStrLn "Naive Answer:"
  print (ansNaive 1000)

  putStrLn "Better Answer:"
  print (ansBetter 1000)