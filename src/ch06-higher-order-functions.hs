compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

subtractByTen :: Num a => a -> a
subtractByTen = subtract 10

applyTwice :: (a -> a) -> a -> a
applyTwice f a = f (f a)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = (f x) : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
  | f x = x : (filter' f xs)
  | otherwise = filter' f xs

quickSort' :: Ord a => [a] -> [a]
quickSort' [] = []
quickSort' (x:xs) =
  let smaller = quickSort' (filter (<= x) xs)
      larger = quickSort' (filter (> x) xs)
  in smaller ++ [x] ++ larger

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999 ..])
  where
    p x = x `mod` 3829 == 0

sumOddSquares :: Integer
sumOddSquares = sum . takeWhile (< 10000) . filter odd . map (^ 2) $ [1 ..]

sumOddSquares' =
  let oddSquares = filter odd $ map (^ 2) [1 ..]
      belowLimit = takeWhile (< 10000) oddSquares
  in sum belowLimit

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n : chain (n `div` 2)
  | odd n = n : chain (n * 3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1 .. 100]))
  where
    isLong xs = length xs > 15

greaterThanSix xs = filter (\x -> x > 6) xs

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' y =
  foldl
    (\acc x ->
       if x == y
         then True
         else acc)
    False

map'' :: (a -> b) -> [a] -> [b]
map'' _ [] = []
map'' f xs = foldr (\x acc -> f x : acc) [] xs

maximum' :: Ord a => [a] -> a
maximum' =
  foldl1
    (\acc x ->
       if x > acc
         then x
         else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f =
  foldr
    (\x acc ->
       if f x
         then x : acc
         else acc)
    []

head' :: [a] -> a
head' = foldl1 (\acc _ -> acc)

last' :: [a] -> a
last' = foldr1 (\_ acc -> acc)

sqrtSums :: Int
sqrtSums = (length $ takeWhile (< 1000) $ scanl1 (+) $ map sqrt [1 ..]) + 1

turnToNegative :: (Num a) => [a]
turnToNegative = map (negate . abs) [5, -3, -6]
