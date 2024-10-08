isPrime :: Int -> Bool
isPrime n = tmp (floor (sqrt (fromIntegral n)))
  where
    tmp 1 = True
    tmp idx
      | n `mod` idx == 0 = False
      | otherwise = tmp n (idx - 1)

-- main :: IO ()
-- main = do

--   let res :: Bool = isPrime 5
--   putStrLn $ show res
