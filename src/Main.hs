module Main where

main :: IO ()
main = do
  putStrLn "hello world"
  putStrLn (greet "bobby")

greet name = "Hello " ++ name ++ "!"
