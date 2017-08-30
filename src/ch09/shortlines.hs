import Control.Monad (forever)
import Data.Char (toUpper)

main = interact shortLinesOnly

lessThanTen :: [a] -> Bool
lessThanTen = (< 10) . length

shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter lessThanTen . lines
