-- Informatics 1 - Functional Programming
-- Tutorial 2
--
-- Week 4 - due: 9/10 Oct.

import Data.Char
import Data.List
import Test.QuickCheck


-- 1.
rotate :: Int -> [Char] -> [Char]
rotate n xs = (drop n xs) ++ (take n xs)

-- 2.
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

-- 3.
makeKey :: Int -> [(Char, Char)]
makeKey n = zip ['A'..'Z'] (rotate n ['A'..'Z'])

-- 4.
lookUp :: Char -> [(Char, Char)] -> Char
lookUp char xs
  | null charList = char
  | otherwise     = head charList
    where
      charList = [ b | (a,b) <- xs, a == char ]

lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec char [] = char
lookUpRec char (x:xs)
  | (fst x) == char = (snd x)
  | otherwise = lookUpRec char xs

prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp char xs = (lookUp char xs) == (lookUpRec char xs)

-- 5.
encipher :: Int -> Char -> Char
encipher n ch = lookUpRec ch (makeKey n)

-- 6.
normalize :: String -> String
normalize str = [ toUpper s | s <- str, isAlphaNum s ]

-- 7.
encipherStr :: Int -> String -> String
encipherStr n str = [ encipher n s | s <- (normalize str) ]

-- 8.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey = undefined

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec = undefined

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey = undefined
-- 9.
decipher :: Int -> Char -> Char
decipher = undefined

decipherStr :: Int -> String -> String
decipherStr = undefined

-- 10.
contains :: String -> String -> Bool
contains = undefined

-- 11.
candidates :: String -> [(Int, String)]
candidates = undefined



-- Optional Material

-- 12.
splitEachFive :: String -> [String]
splitEachFive = undefined

-- 13.
prop_transpose :: String -> Bool
prop_transpose = undefined

-- 14.
encrypt :: Int -> String -> String
encrypt = undefined

-- 15.
decrypt :: Int -> String -> String
decrypt = undefined

-- Challenge (Optional)

-- 16.
countFreqs :: String -> [(Char, Int)]
countFreqs = undefined

-- 17
freqDecipher :: String -> [String]
freqDecipher = undefined
