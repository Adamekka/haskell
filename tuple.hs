zipThem :: [a] -> [b] -> [(a, b)]
zipThem (x : xs) (y : ys) = (x, y) : zipThem xs ys
zipThem _ _ = []

dotProduct :: [a] -> [b] -> [(a, b)]
dotProduct xs ys = [(x, y) | x <- xs, y <- ys]

main :: IO ()
main = do
  let res :: [(Int, Char)] = zipThem [1, 2, 3] "ABCD"
  let res2 :: [(Int, Char)] = dotProduct [1 .. 4] "ABC"
  putStrLn $ show res ++ show res2
