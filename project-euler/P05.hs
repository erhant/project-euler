module P5 where

-- | In other words, the question asks for the `least common multiple` of all these numbers.

-- | Returns the smallest positive integer that is evenly divisible by all number 1..n.
ans :: Int -> Int
ans 1 = 1
ans n = foldr lcm 1 [1 .. n]

-------------------------------------main-------------------------------------
main :: IO ()
main = do
  putStrLn "Answer:"
  print (ans 20)

-------------------------------------note-------------------------------------

-- | foldr lcm 1 [1, 2, 3, 4, 5] basically does:
-- | 1 lcm (2 lcm (3 lcm (4 lcm (5 lcm 1))))
-- | The innermost '1' is the one we provide

-- | I could perhaps implement LCM myself, maybe later.