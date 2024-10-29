quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort [x] = [x]
quicksort (pivot : xs) =
  let ls = filter (< pivot) xs
      rs = filter (> pivot) xs
   in quicksort ls ++ [pivot] ++ quicksort rs

main :: IO ()
main = do
  let res :: [Int] = quicksort [3, 2, 1, 4]
  putStrLn $ show res
