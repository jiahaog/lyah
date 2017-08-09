import Data.List (nub, tails)
import Data.Char (ord, chr)
import Data.Map as Map

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
  let nlen = length needle
  in foldl (\acc x -> if take nlen x == needle
                      then True
                      else acc)
           False
           (tails haystack)

encode :: Int -> String -> String
encode shift msg =
  let ords = map ord msg
      shifted = map (+ shift) ords
  in map chr shifted

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k, v):xs) = if key == k
                          then Just v
                          else findKey key xs

findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey' key = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing
