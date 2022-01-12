module P10 where

-- | Infinite list of primes up to N
primes :: [Int]
primes = 2 : 3 : filter isPrime [5, 7 ..]
  where
    isPrime n =
      all
        (\p -> n `mod` p /= 0) -- division test
        (takeWhile (\p -> p * p <= n) (tail primes)) -- prime candidates <= sqrt(n)

sumPrimes :: Int -> Int
sumPrimes n = sum (takeWhile (< n) primes)

-------------------------------------main-------------------------------------
main :: IO ()
main = do
  putStrLn "Answer:"
  print (sumPrimes 2000000)

-------------------------------------note-------------------------------------

{-
"Sum of all primes less than n" is equivalent to
"Sum of all numbers less than n" - "Sum of all composite numbers less than n"

For the second one, we can say if there are K numbers that are divisible by M up to N,
then K-1 of these are prime.

The inclusion-exclusion method here could be a bit complicado though.

-}