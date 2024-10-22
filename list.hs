import GHC.IO.Exception (assertError)

sumList :: [Int] -> Int
sumList [] = 0
sumList (x : xs) = x + sumList xs

getHead :: [a] -> a
getHead (x : xs) = x

getLast :: [a] -> Maybe a
getLast [] = Nothing
getLast [x] = Just x
getLast (_ : xs) = getLast xs

isElement :: (Eq a) => a -> [a] -> Bool
isElement _ [] = False
isElement a (x : xs)
  | a == x = True
  | otherwise = isElement a xs

getInit :: [a] -> [a]
getInit [] = []
getInit [x] = []
getInit (x : xs) = x : getInit xs

combine :: [a] -> [a] -> [a]
combine [] [] = []
combine [] (y : ys) = y : combine [] ys
combine (x : xs) y = x : combine xs y

combine2 :: [a] -> [a] -> [a]
combine2 [] [] = []
combine2 [] y = y
combine2 (x : xs) y = x : combine xs y

combine3 :: [a] -> [a] -> [a]
combine3 x y = x ++ y

combine4 :: [a] -> [a] -> [a]
combine4 = (++)

cycle :: Int -> Int
cycle x = lp 0
  where
    lp y
      | x == y = 0
      | otherwise = lp (y + 1)

maxInt :: (Ord a) => [a] -> a
maxInt [x] = x
maxInt (x : y : xs) = maxInt (max x y : xs)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x : xs) = x : take' (n - 1) xs

drop' :: Int -> [a] -> [a]
drop' 0 a = a
drop' n (_ : xs) = drop' (n - 1) xs

minimum' :: (Ord a) => [a] -> Maybe a
minimum' [] = Nothing
minimum' [x] = Just x
minimum' (x : y : xs)
  | x < y = minimum' ([x] ++ xs)
  | otherwise = minimum' ([y] ++ xs)

divisors :: Int -> [Int]
divisors 0 = []
divisors n = tmp 1
  where
    tmp i
      | i == n = [n]
      | mod n i == 0 = i : tmp (i + 1)
      | otherwise = tmp (i + 1)

main :: IO ()
main = do
  let res :: Int = sumList [5, 5]
  let res2 :: Int = getHead [1, 2, 3]
  let res3 :: Maybe Int = getLast [1, 2, 3]
  let res4 :: Bool = isElement 3 [1, 2, 3]
  let res5 :: Bool = isElement 4 [1, 2, 3]
  let res6 :: [Int] = getInit [1, 2, 3]
  let res7 :: [Int] = combine [1, 2] [3, 4]
  let res8 :: Int = maxInt [1, 2, 3]
  let res9 :: [Int] = reverse' [1, 2, 3]
  let res10 :: [Int] = take' 2 [1, 2, 3]
  let res11 :: [Int] = drop' 2 [1, 2, 3]
  let res12 :: Maybe Int = minimum' [1, 3, 4, 0]
  let res13 :: [Int] = divisors 32
  putStrLn $ show res ++ show res2 ++ show res3 ++ show res4 ++ show res5 ++ show res6 ++ show res7 ++ show res8 ++ show res9 ++ show res10 ++ show res11 ++ show res12 ++ show res13
