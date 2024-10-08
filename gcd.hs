gcd' :: Int -> Int -> Int
gcd' a b
  | b == 0 = a
  | otherwise = gcd' b (a `mod` b)

main :: IO ()
main = do
  let res :: Int = gcd' 30 13
  putStrLn $ show res
