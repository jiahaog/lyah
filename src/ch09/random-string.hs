import System.Random (getStdGen, randomRs)

main = do
  gen <- getStdGen
  putStr $ take 20 (randomRs ('a', 'z') gen)
