import Control.Exception (catch)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO (readFile)
import System.IO.Error (ioeGetFileName, isDoesNotExistError)

{- import System.IO.Error (catch) -}
{- main = do -}
{-   (fileName:_) <- getArgs -}
{-   fileExists <- doesFileExist fileName -}
{-   if fileExists -}
{-     then do -}
{-       contents <- readFile fileName -}
{-       putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines! " -}
{-     else do -}
{-       putStrLn "the file doesn't exist" -}
main :: IO ()
main = catch toTry handler

toTry :: IO ()
toTry = do
  (fileName:_) <- getArgs
  contents <- readFile fileName
  putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines! "

handler :: IOError -> IO ()
handler e
  | isDoesNotExistError e =
    case ioeGetFileName e of
      (Just path) -> putStrLn $ "File does not exist at: " ++ path
      Nothing -> putStrLn "file does not exist at unknown location"
  | otherwise = ioError e
{- handler e = putStrLn $ "Whoops, had some trouble" ++ show e -}
