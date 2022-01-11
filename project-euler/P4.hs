module P4 where

-- | The naive method brute forces by starting with two factors 999 x 999, and then decreasing them one by one.

-- | Largest Palindrome Product, the parameter is the number of digits of the factors
lpp :: Int -> Int
lpp 1 = 9 -- 1 x 9 = 9, no other palindrome product of two digits
lpp d = _lpp d ((10 ^ d) - 1) ((10 ^ d) - 1) 0

-- TODO: Use higher-order function to avoid recomputing digits and stuff

-- | Returns the factors of the largest palindrome product (in case we want to see them)
_lpp :: Int -> Int -> Int -> Int -> Int
_lpp d a b max = do
  let p = a * b -- product
  if p < 10 ^ ((2 * d) - 1) -- if the product has less than 2*d digits, reset a but decrement b
    then
      if p < 1
        then max -- finish recursion
        else
          _lpp
            d
            ((10 ^ d) - 1) -- reset a
            (b - 1) -- decrement b
            max -- store passed max
    else
      _lpp
        d
        (a - 1) -- decrement a
        b -- b stays constant
        (if isPalindrome (2 * d) p && (p > max) then p else max) -- store passed max

-- | Returns True if the given number p (with d digits) is a palindrome.
isPalindrome :: Int -> Int -> Bool
isPalindrome d p
  | d < 2 = True
  | otherwise = do
    let l = p `div` (10 ^ (d - 1)) -- leftmost digit
    let r = p `rem` 10 -- rightmost digit
    (l == r) && isPalindrome (d - 2) (div (p - l * 10 ^ (d - 1)) 10) -- abcba becomes bcb

-------------------------------------main-------------------------------------
main :: IO ()
main = do
  putStrLn "Answer:"
  print (lpp 3)

-------------------------------------note-------------------------------------

-- | We could generate palindromes and then factorize them too.
-- | For D digits, there are V = (div (D+1) 2) variables that can be used to construct a palindrome number.
-- |      a = a x 1                               D = 1 -> V = 1
-- |     aa = a x 11                              D = 2 -> V = 1
-- |    aba = a x 101    + b x 10                 D = 3 -> V = 2
-- |   abba = a x 1001   + b x 110                D = 4 -> V = 2
-- |  abcba = a x 10001  + b x 1010  + c x 1      D = 5 -> V = 3
-- | abccba = a x 100001 + b x 10010 + c x 10     D = 6 -> V = 3
-- | ...
-- |
-- | While constructing the number, a = 1..9, b = 0..9, c = 0..9 (a cant be zero)
-- | If we are looking for the palindrome product of two K digit numbers, we can expect the result to have 2K digits