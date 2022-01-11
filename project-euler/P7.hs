module P7 where

-- | Infinite list of primes
primes :: [Int]
primes = 2 : 3 : filter isPrime [5, 7 ..]
  where
    isPrime n =
      all
        (\p -> n `mod` p /= 0) -- division test
        (takeWhile (\p -> p * p <= n) (tail primes)) -- prime candidates <= sqrt(n)

nthPrime :: Int -> Int
nthPrime n = primes !! (n - 1)

-------------------------------------main-------------------------------------
main :: IO ()
main = do
  putStrLn "Answer:"
  print (nthPrime 10001)

-------------------------------------note-------------------------------------

-- | The prime list here is created with a simple sieve over odd numbers that
-- | try divisibility until the square root of the given number.