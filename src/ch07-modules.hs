import Data.List (nub, tails)
import Data.Char (ord, chr)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Geometry
import Geometry.Cube as Cube

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

fromList' :: Ord k => [(k, v)] -> Map.Map k v
fromList' = foldr (\(k, v) acc -> Map.insert k v acc) Map.empty

phoneBook =
    [("betty","555-2938")
    ,("betty","342-2492")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("patsy","943-2929")
    ,("patsy","827-9162")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ,("penny","555-2111")
    ]

phoneBookToMap :: Ord k => [(k, a)] -> Map.Map k [a]
phoneBookToMap = Map.fromListWith (++) . map (\(k, v) -> (k, [v]))

testGeometry = cubeArea 2

testGeoCube = Cube.area 2
