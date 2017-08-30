import System.IO
       (Handle, IOMode(ReadMode), hClose, hGetContents, openFile,
        withFile)

{- main = do -}
{-   handle <- openFile "haiku.txt" ReadMode -}
{-   contents <- hGetContents handle -}
{-   putStr contents -}
{-   hClose handle -}
{- main = do -}
{-   withFile' -}
{-     "haiku.txt" -}
{-     ReadMode -}
{-     (\handle -> do -}
{-        contents <- hGetContents handle -}
{-        putStr contents) -}
{- withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a -}
{- withFile' path mode f = do -}
{-   handle <- openFile path mode -}
{-   result <- f handle -}
{-   hClose handle -}
{-   return result -}
main = do
  contents <- readFile "haiku.txt"
  putStr contents
