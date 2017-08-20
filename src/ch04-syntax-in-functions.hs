lucky :: (Integral a) => a -> String
lucky 7 = "L7"
lucky x = "sorry"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName x = "Something"

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, y, _) = x

head' :: [a] -> a
head' [] = error "WTF"
head' (x:_) = x

head'' xs =
  case xs of
    [] -> error "No head"
    (x:_) -> x

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "Empty string"
capital all@(x:xs) = "THe first letter of " ++ all ++ " is " ++ [x]

{- bmiTell :: (RealFloat a) => a -> String -}
{- bmiTell bmi -}
{-   | bmi <= 18.5 = "small" -}
{-   | bmi <= 25.0 = "med" -}
{-   | bmi <= 30 = "large" -}
{-   | otherwise = "Fat" -}
max' :: (Ord a) => a -> a -> a
a `max'` b
  | a > b = a
  | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a > b = GT
  | a == b = EQ
  | otherwise = LT

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= skinny = "Underwight"
  | bmi <= normal = "ormal"
  | bmi <= fat = "Fat"
  | otherwise = "YOU WHIALE"
  where
    bmi = weight / height ^ 2
    (skinny, normal, fat) = (18.5, 25.0, 30.0)

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where
    (f:_) = firstname
    (l:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where
    bmi weight height = weight / height ^ 2

cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + 2 * topArea

cylinder' r h = sideArea + 2 * topArea
  where
    sideArea = 2 * pi * r * h
    topArea = pi * r ^ 2

calcFatBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

describeList xs =
  "The List is " ++
  case xs of
    [] -> "empty"
    [a] -> "a singleton list"
    xs -> "a longer list"

describeList' xs = "The list is " ++ what xs
  where
    what [] = "empty"
    what [a] = "a singleton list"
    what xs = "a longer list"
