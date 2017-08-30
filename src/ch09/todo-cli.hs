import Data.List (delete, lookup)
import System.Directory (removeFile, renameFile)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO
       (IOMode(ReadMode), hClose, hGetContents, hPutStr, openFile,
        openTempFile)

dispatch :: [(String, [String] -> IO ())]
dispatch = [("add", add), ("view", view), ("remove", remove)]

main = do
  args <- getArgs
  case args of
    (command:args) ->
      let action = lookup command dispatch
      in handleAction action args
    _ -> do
      putStr helpText
      exitFailure

handleAction :: Maybe ([String] -> IO ()) -> [String] -> IO ()
handleAction m args =
  case m of
    (Just action) -> action args
    _ -> do
      putStr helpText
      exitFailure

helpText :: String
helpText = "Todo CLI\nOptions:\n" ++ (unlines $ map fst dispatch)

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks =
        zipWith (\n line -> show n ++ " - " ++ line) [0 ..] todoTasks
  putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
  handle <- openFile fileName ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let number = read numberString
      todoTasks = lines contents
      newTodoItems = delete (todoTasks !! number) todoTasks
  hPutStr tempHandle $ unlines newTodoItems
  hClose handle
  hClose tempHandle
  removeFile fileName
  renameFile tempName fileName
