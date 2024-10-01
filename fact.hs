fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

fact2 :: Int -> Int
fact2 n
  | n == 0 = 1
  | otherwise = n * fact2 (n - 1)

fact3 :: Int -> Int
fact3 n = if n == 0 then 1 else n * fact3 (n - 1)

main :: IO ()
main = do
  let res :: Int = fact 5
  let res2 :: Int = fact 5
  let res3 :: Int = fact 5
  putStrLn $ show res ++ show res2 ++ show res3
