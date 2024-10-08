sumList :: [Int] -> Int
sumList [] = 0
sumList (x : xs) = x + sumList xs

getHead :: [a] -> a
getHead (x : xs) = x

getTail :: [a] -> Maybe a
getTail [] = Nothing
getTail [x] = Just x
getTail (_ : xs) = getTail xs

main :: IO ()
main = do
  let res :: Int = sumList [5, 5]
  let res2 :: Int = getHead [1, 2, 3]
  let res3 :: Maybe Int = getTail [1, 2, 3]
  putStrLn $ show res ++ show res2 ++ show res3
