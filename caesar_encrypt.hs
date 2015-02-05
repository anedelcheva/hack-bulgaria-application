{- 
we import Data.Char in order to use the functions ord and chr and Data.Ix to use the function inRange
 ord :: Char -> Int
 chr :: Int -> Char
 inRange :: Ix a => (a, a) -> a -> Bool
-}
import Data.Char
import Data.Ix

caesar_encrypt :: Int -> String -> String
caesar_encrypt offset = map encode
  where
    encode symbol
      | inRange ('a','z') symbol = transform 'a' offset symbol
      | inRange ('A','Z') symbol = transform 'A' offset symbol
      | otherwise = symbol

transform base offset symbol = chr $ ord base + mod (ord symbol - ord base + offset) 26


