-- Informatics 1 Functional Programming
-- Tutorial 6
--
-- Due: 6/7 November

import System.Random


-- Importing the keymap module

import KeymapList


-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item


-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]


-- Exercise 1

longestProductLen :: [(Barcode, Item)] -> Int
longestProductLen xs = frank xs 0
  where
    frank [] l = l
    frank ((code,(name, unit)):xs) longest
      | length name > longest = frank xs (length name)
      | otherwise = frank xs longest

formatLine :: Int -> (Barcode, Item) -> String
formatLine len (barcode, (name, unit)) = barcode ++ dots ++ (take (len + 3) (name ++ (replicate len '.'))) ++ unit
  where
    dots = "..."

showCatalogue :: Catalogue -> String
showCatalogue xs = printing (toList xs) (longestProductLen (toList xs))
  where
    printing [] _ = []
    printing (x:xs) len = formatLine len x ++ "\n" ++ printing xs len

-- Exercise 2
maybeToList :: Maybe a -> [a]
maybeToList = undefined

listToMaybe :: [a] -> Maybe a
listToMaybe = undefined

catMaybes :: [Maybe a] -> [a]
catMaybes = undefined

-- Exercise 3

getItems :: [Barcode] -> Catalogue -> [Item]
getItems = undefined






-- Input-output ------------------------------------------

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine $ lines dbl)
            putStrLn (size db >= 0 `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

getSample :: Catalogue -> IO Barcode
getSample db = do g <- newStdGen
                  return $ fst $ toList db !! fst (randomR (0,size db - 1) g)
