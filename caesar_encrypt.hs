import Data.Char
import Data.Ix

caesar_encrypt :: String -> Int -> String
caesar_encrypt string offset = map encode string
  where
    transform base symbol offset = chr $ ord base + mod (ord symbol - ord base + offset) 26
    encode symbol
      | inRange ('a','z') symbol = transform 'a' symbol offset
      | inRange ('A','Z') symbol = transform 'A' symbol offset
      | otherwise = symbol