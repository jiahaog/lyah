data Node =
  Node Road
       (Maybe Road)

data Road =
  Road Int
       Node

data Section = Section
  { getA :: Int
  , getB :: Int
  , getC :: Int
  } deriving (Show)

type RoadSystem = [Section]

heathrowToLondon :: RoadSystem
heathrowToLondon =
  [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

data Label
  = A
  | B
  | C
  deriving (Show)

type Path = [(Label, Int)]

optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
  let (bestAPath, bestBPath) = foldl roadStep ([], []) roadSystem
  in if sum (map snd bestAPath) <= sum (map snd bestBPath)
       then reverse bestAPath
       else reverse bestBPath

bestPath :: (Path, Path) -> Section -> (Path, Path)
bestPath (toA, toB) sec =
  let pathToA =
        if getA sec < (getB sec + getC sec)
          then [(A, getA sec)]
          else [(B, getB sec), (C, getC sec)]
      pathToB =
        if getB sec < (getA sec + getC sec)
          then [(B, getB sec)]
          else [(A, getA sec), (C, getC sec)]
  in (pathToA ++ toA, pathToB ++ toB)

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
  let priceA = sum $ map snd pathA
      priceB = sum $ map snd pathB
      forwardPriceToA = priceA + a
      crossPriceToA = priceB + b + c
      forwardPriceToB = priceB + b
      crossPriceToB = priceA + a + c
      newPathToA =
        if forwardPriceToA <= crossPriceToA
          then (A, a) : pathA
          else (C, c) : (B, b) : pathB
      newPathToB =
        if forwardPriceToB <= crossPriceToB
          then (B, b) : pathB
          else (C, c) : (A, a) : pathA
  in (newPathToA, newPathToB)

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

main = do
  contents <- getContents
  let threes = groupsOf 3 (map read $ lines contents)
      roadSystem = map (\[a, b, c] -> Section a b c) threes
      path = optimalPath roadSystem
      pathString = concat $ map (show . fst) path
      pathPrice = sum $ map snd path
  putStrLn $ "The best path to take is: " ++ pathString
  putStrLn $ "The price is: " ++ show pathPrice
