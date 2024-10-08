isPrime :: Int -> Bool
isPrime n = isPrime_ n (n - 1)

isPrime_ :: Int -> Int -> Bool
isPrime_ n 1 = True
isPrime_ n idx
  | n `mod` idx == 0 = False
  | otherwise = isPrime_ n (idx - 1)

main :: IO ()
main = do
  let res :: Bool = isPrime 5
  putStrLn $ show res
