-- Informatics 1 - Functional Programming
-- Tutorial 1
--
-- Due: the tutorial of week 3 (2/3 Oct.)

import Data.Char
import Data.List
import Test.QuickCheck



-- 1. halveEvens

-- List-comprehension version
halveEvens :: [Int] -> [Int]
halveEvens xs = [ x `div` 2 | x <- xs, (x `mod` 2) == 0 ]

-- Recursive version
halveEvensRec :: [Int] -> [Int]
halveEvensRec [] = []
halveEvensRec (x:xs)
  | (x `mod` 2) == 0 = (x `div` 2) : halveEvensRec xs
  | otherwise        = halveEvensRec xs

-- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = (halveEvens xs) == (halveEvensRec xs)



-- 2. inRange

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [ x | x <- xs, x >= lo, x <= hi ]

-- Recursive version
inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec lo hi [] = []
inRangeRec lo hi (x:xs)
  | (x >= lo && x <= hi) = x : inRangeRec lo hi xs
  | otherwise            = inRangeRec lo hi xs

-- Mutual test
prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = (inRange lo hi xs) == (inRangeRec lo hi xs)



-- 3. countPositives: count all the positive numbers in a list

-- List-comprehension version
countPositives :: [Int] -> Int
countPositives xs = length [ x | x <- xs, x > 0 ]

-- Recursive version
countPositivesRec :: [Int] -> Int
countPositivesRec []     = 0
countPositivesRec (x:xs)
  | x > 0                = 1 + countPositivesRec xs
  | otherwise            = countPositivesRec xs

-- Mutual test
prop_countPositives :: [Int] -> Bool
prop_countPositives xs = (countPositives xs) == (countPositivesRec xs)



-- 4. pennypincher

-- Helper function
discount :: Int -> Int
discount x = round (0.9 * fromIntegral x)

-- List-comprehension version
pennypincher :: [Int] -> Int
pennypincher xs = sum [ discount x | x <- xs, discount x <= 19900 ]

-- Recursive version
pennypincherRec :: [Int] -> Int
pennypincherRec []      = 0
pennypincherRec (x:xs)
  | discount x <= 19900 = discount x + pennypincherRec xs
  | otherwise           = pennypincherRec xs

-- Mutual test
prop_pennypincher :: [Int] -> Bool
prop_pennypincher xs = (pennypincher xs) == (pennypincherRec xs)



-- 5. sumDigits

-- List-comprehension version
multDigits :: String -> Int
multDigits xs = product [ digitToInt x | x <- xs, isDigit x ]

-- Recursive version
multDigitsRec :: String -> Int
multDigitsRec []     = 1
multDigitsRec (x:xs)
  | isDigit x        = digitToInt x * multDigitsRec xs
  | otherwise        = multDigitsRec xs

-- Mutual test
prop_multDigits :: String -> Bool
prop_multDigits xs = (multDigits xs) == (multDigitsRec xs)



-- 6. capitalise

-- List-comprehension version
capitalise :: String -> String
capitalise []     = []
capitalise (x:xs) = toUpper x : [ toLower x | x <- xs ]

-- Recursive helper

downcase :: String -> String
downcase []     = []
downcase (x:xs) = toLower x : downcase xs

-- Recursive version
capitaliseRec :: String -> String
capitaliseRec []     = []
capitaliseRec (x:xs) = toUpper x : downcase xs

-- Mutual test
prop_capitalise :: String -> Bool
prop_capitalise xs = (capitalise xs) == (capitaliseRec xs)



-- 7. title

-- List-comprehension version
title :: [String] -> [String]
title []     = []
title (x:xs) = capitalise x : [ if length x > 3 then capitalise x else downcase x | x <- xs ]

-- Recursive version
titleRec :: [String] -> [String]
titleRec []     = []
titleRec (x:xs) = capitalise x : titleRecCont xs

titleRecCont :: [String] -> [String]
titleRecCont []     = []
titleRecCont (x:xs)
  | length x > 3      = capitalise x : titleRecCont xs
  | otherwise       = downcase x : titleRecCont xs

-- mutual test
prop_title :: [String] -> Bool
prop_title xs = (title xs) == (titleRec xs)




-- Optional Material

-- 8. crosswordFind

-- List-comprehension version
crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind ch iP len ws = [ w | w <- ws, length w == len, length w < iP, (abs iP) <= len, w !! (abs iP) == ch ]

-- Recursive version
crosswordFindRec :: Char -> Int -> Int -> [String] -> [String]
crosswordFindRec ch iP len []                          = []
crosswordFindRec ch iP len (w:ws)
  | length w < (abs iP) || (abs iP) >= len = []
  | length w == len && w !! (abs iP) == ch = w : crosswordFindRec ch iP len ws
  | otherwise                              = crosswordFindRec ch iP len ws

-- Mutual test
prop_crosswordFind :: Char -> Int -> Int -> [String] -> Bool
prop_crosswordFind ch iP len ws = (crosswordFind ch iP len ws) == (crosswordFindRec ch iP len ws)



-- 9. search

-- List-comprehension version

search :: String -> Char -> [Int]
search ws ch = zsearch (zip [0..] ws) ch

zsearch :: [(Int, Char)] -> Char -> [Int]
zsearch ws ch = [ fst w | w <- ws, snd w == ch ]

-- Recursive non-library zip
zzip :: [Int] -> [String] -> [(Int, Char)]
zzip [] ys = []
zzip xs [] = []
zzip (x:xs) (y:ys) = (x, y) : zzip xs ys

-- Recursive version
searchRec :: String -> Char -> [Int]
searchRec = undefined

-- Mutual test
prop_search :: String -> Char -> Bool
prop_search = undefined


-- 10. contains

-- List-comprehension version
contains :: String -> String -> Bool
contains = undefined

-- Recursive version
containsRec :: String -> String -> Bool
containsRec = undefined

-- Mutual test
prop_contains :: String -> String -> Bool
prop_contains = undefined

