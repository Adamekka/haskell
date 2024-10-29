import Data.Char

allToUpper :: String -> String
allToUpper = map toUpper

oddList :: Int -> Int -> [Int]
oddList x y = filter odd [x .. y]

removeAllUpper :: String -> String
removeAllUpper = filter isLower

union :: (Eq a) => [a] -> [a] -> [a]
union xs ys = xs ++ [y | y <- ys, notElem y xs]

intersection :: (Eq a) => [a] -> [a] -> [a]
intersection xs ys = [y | y <- ys, elem y xs]

unique :: String -> String
unique [] = []
unique (x : xs) = x : unique (filter (/= x) xs)

main :: IO ()
main = do
  let res :: String = allToUpper "abcd"
  let res2 :: [Int] = oddList 1 10
  let res3 :: String = removeAllUpper "ABCabcABC"
  let res4 :: [Int] = union [1 .. 5] [3 .. 10]
  let res5 :: [Int] = intersection [1 .. 5] [3 .. 10]
  let res6 :: String = "aabbcd"
  putStrLn $ show res ++ show res2 ++ show res3 ++ show res4 ++ show res5 ++ show res6
