maximum' :: Ord a => [a] -> a
maximum' [] = error "Maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x < maxTail = maxTail
    | otherwise = x
    where maxTail = maximum' xs

maximum'' :: Ord a => [a] -> a
maximum'' [] = error "Maximum of empty list"
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum'' xs)


replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n xs
    | n <= 0 = []
    | otherwise = xs:(replicate' (n - 1) xs)

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x:(take' (n - 1) xs)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y):(zip' xs ys)

elem' :: Eq a => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = elem' a xs

quickSort' :: (Ord a) => [a] -> [a]
quickSort' [] = []
quickSort' (x:xs) =
    let smallerSorted = quickSort' [a | a <- xs, a <= x]
        largerSorted = quickSort' [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ largerSorted
