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
makeKey n = zip alpha (rotate n alpha)
  where alpha = ['A'..'Z']

-- 4.
lookUp :: Char -> [(Char, Char)] -> Char
lookUp char xs
  | null charList = char
  | otherwise     = head charList
    where
      charList = [ b | (a,b) <- xs, a == char ]

lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec char [] = char
lookUpRec char ((a,b):xs)
  | a == char = b
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
reverseKey xs = [ (y, x) | (x, y) <- xs ]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec [] = []
reverseKeyRec ((a,b):xs) = (b,a) : reverseKeyRec xs

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey xs = (reverseKey xs) == (reverseKeyRec xs)

-- 9.
decipher :: Int -> Char -> Char
decipher n ch = lookUpRec ch (reverseKeyRec (makeKey n))

decipherStr :: Int -> String -> String
decipherStr n str = [ decipher n s | s <- str, isDigit s || isUpper s]

-- 10.
contains :: String -> String -> Bool
contains [] sub        = False
contains (s:str) sub = isPrefixOf sub str || contains str sub

-- 11.
candidates :: String -> [(Int, String)]
candidates str = [ pair | pair <- pairs, contains (snd pair) "AND" || contains (snd pair) "THE" ]
  where
    options str = [ decipherStr n str | n <- [1..26] ]
    pairs = [ pair | pair <- zip [1..26] (options str) ]



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
