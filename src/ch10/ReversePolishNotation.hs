module ReversePolishNotation
  ( solveRPN
  ) where

solveRPN :: (Num a, Read a, Fractional a) => String -> a
solveRPN = calculate . toList

toList :: String -> [String]
toList = words

calculate :: (Num a, Read a, Fractional a) => [String] -> a
calculate = head . foldl foldFn []

foldFn :: (Num a, Read a, Fractional a) => [a] -> String -> [a]
foldFn (x:y:xs) "+" = y + x : xs
foldFn (x:y:xs) "-" = y - x : xs
foldFn (x:y:xs) "*" = y * x : xs
foldFn (x:y:xs) "/" = y / x : xs
foldFn acc x = read x : acc
{- this requires some Fractional Typeclass -}
{- operatorMapping "/" = (/) -}
