import System.Environment (getArgs, getProgName)

main = do
  args <- getArgs
  progName <- getProgName
  putStrLn "The arguments are:"
  mapM putStrLn args
  putStrLn "The prog name is:"
  putStrLn progName
