{-
- Version: 1.1
- Author: Danny Wilson
- Description: The main functions to parse the files.
-}

module Parsers.Parsing where
  
import Data.List.Split

--Main initial parse function to call the others
initialParse :: String -> [String]
initialParse x = (inlineComments (removeComments (newlineParse(semicolonParse x))))
  
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
removeComments xs = [x | x <- xs, not (isComment x)]

--Checks if the current elements is a comment
isComment :: String -> Bool
isComment (x:_) = x `elem` "~"
isComment _ = False

--Will remove block comments when complete
--blockComments :: [String] -> [String]
--blockComments xs = (takeWhile (\x -> x /= "~/") xs)