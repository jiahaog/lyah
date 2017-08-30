import Control.Monad (forM, forever, when)
import qualified Data.Char as Char
import System.Exit (exitFailure)

{- main = do -}
{-   putStrLn "What's your first name?" -}
{-   firstName <- getLine -}
{-   putStrLn "What's your last name?" -}
{-   lastName <- getLine -}
{-   let bigFirstName = map Char.toUpper firstName -}
{-       bigLastName = map Char.toUpper lastName -}
{-   putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?" -}
{- main = do -}
{-   line <- getLine -}
{-   if null line -}
{-     then do -}
{-       exitFailure -}
{-     else do -}
{-       putStrLn $ reverseWords line -}
{-       main -}
{- reverseWords :: String -> String -}
{- reverseWords = unwords . map reverse . words -}
{- main = do -}
{-   c <- getChar -}
{-   when (c /= ' ') $ do -}
{-     putChar c -}
{-     main -}
{- main = -}
{-   forever $ do -}
{-     putStr "Give me some input: " -}
{-     l <- getLine -}
{-     putStrLn $ map Char.toUpper l -}
main = do
  colors <-
    forM
      [1, 2, 3, 4]
      (\a -> do
         putStrLn $
           "Which color do you associate with the number " ++ show a ++ "?"
         color <- getLine
         return color)
  putStrLn "The colors that you associate with 1, 2, 3, and 4 are: "
  mapM putStrLn colors
