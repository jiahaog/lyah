main = interact respondPalindrome

isPalindrome :: String -> Bool
isPalindrome xs = (==) xs $ reverse xs

respondPalindrome :: String -> String
respondPalindrome =
  unlines .
  map
    (\l ->
       if isPalindrome l
         then "palindrome"
         else "not palindrome") .
  lines
