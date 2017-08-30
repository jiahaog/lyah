import qualified Data.List as List
import qualified System.Directory as Dir
import qualified System.IO as IO

type Tasks = [String]

main = do
  (tasks, handle) <- loadTasks "todo.txt"
  let numberedTasks = numberTasks $ tasks
  lineToDelete <- promptForLine numberedTasks
  let newTasks = removeTask tasks lineToDelete
  IO.hClose handle
  saveTasks newTasks

loadTasks :: FilePath -> IO (Tasks, IO.Handle)
loadTasks path = do
  handle <- IO.openFile path IO.ReadMode
  contents <- IO.hGetContents handle
  return (lines contents, handle)

promptForLine :: Tasks -> IO (Int)
promptForLine tasks = do
  putStrLn "These are your todo items:"
  mapM putStrLn tasks
  {- putStr $ unlines tasks -}
  putStrLn "Which one do you want to delete?"
  numberString <- getLine
  return (read numberString)

saveTasks :: Tasks -> IO ()
saveTasks tasks = do
  (tempName, tempHandle) <- IO.openTempFile "." "temp"
  IO.hPutStr tempHandle $ unlines tasks
  Dir.removeFile "todo.txt"
  Dir.renameFile tempName "todo.txt"
  IO.hClose tempHandle

removeTask :: Eq a => [a] -> Int -> [a]
removeTask tasks number = List.delete (tasks !! number) tasks

numberTasks = zipWith (\n line -> show n ++ " - " ++ line) [0 ..]
