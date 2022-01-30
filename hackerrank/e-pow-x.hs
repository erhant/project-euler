f :: Double -> Double
f x =
  1.0 + _f x x 1.0 2.0
  where
    _f :: Double -> Double -> Double -> Double -> Double
    _f x num denom 10.0 = num / denom
    _f x num denom step = num / denom + _f x (x * num) (denom * step) (step + 1.0)

-- | Code below is given as part of the boilerplate
main :: IO ()
main = do
  n <- readLn :: IO Int

  forM_ [1 .. n] $ \n_itr -> do
    x <- readLn :: IO Double
    print (f x)