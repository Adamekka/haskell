mult :: Int -> Int -> Int
mult a 0 = 0
mult a b = a + mult a (b - 1)

main :: IO ()
main = do
  let res :: Int = mult 4 5
  putStrLn $ show res
