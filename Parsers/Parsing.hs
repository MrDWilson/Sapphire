{-
- Version: 1.1
- Author: Danny Wilson
- Description: The main functions to parse the files.
-}

module Parsers.Parsing where
  
import Data.List.Split
import Data.List

--Main initial parse function to call the others
initialParse :: String -> [String]
initialParse x = removeEmpty $ removeWhite $ inlineComments $ removeComments $ newlineParse $ semicolonParse x
  
--Splits the string into lines (e.g. on each new line)
newlineParse :: String -> [String]
newlineParse x = splitOn "\n" x

--Removes the inline comments
inlineComments :: [String] -> [String]
inlineComments xs = removeComments (concat (map (split (keepDelimsL $ oneOf "~")) xs))

--Removes the semicolons
semicolonParse :: String -> String
semicolonParse xs = [x | x <- xs, not (x `elem` ";")]

--Removes single line comments
removeComments :: [String] -> [String]
removeComments xs = [x | x <- xs, not (isComment x "~")]

--Checks if the current elements is a comment
isComment :: String -> String -> Bool
isComment (x:_) y = x `elem` y
isComment _ _= False

--Checks if it is a block comments
isBlockComment :: [Char] -> Bool
isBlockComment (x:xs) = first x && second xs
  where first '~' = True
        first _ = False
        second ('/':xs) = True
        second _ = False
isBlockComment _ = False

--Will remove block comments when complete
blockComments :: [String] -> [[String]]
blockComments (x:xs) = groupBy (\x y -> isBlockComment x) xs

--Formatting function
removeWhite :: [String] -> [String]
removeWhite xs = map (dropWhileEnd (== ' '))(map (dropWhile (== ' ')) xs)

--Remove empty elements
removeEmpty :: [String] -> [String]
removeEmpty xs = [x | x <- xs, x /= ""]