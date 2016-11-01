{-
- Version: 1.1
- Author: Danny Wilson
- Description: The main functions to parse the files
-}

module Parsers.Parsing where
  
import Data.List.Split

initialParse :: String -> [String]
initialParse x = (inlineComments (removeComments (newlineParse(semicolonParse x))))
  
newlineParse :: String -> [String]
newlineParse x = splitOn "\n" x

inlineComments :: [String] -> [String]
inlineComments xs = removeComments (concat (map (split (keepDelimsL $ oneOf "~")) xs))

semicolonParse :: String -> String
semicolonParse xs = [x | x <- xs, not (x `elem` ";")]

removeComments :: [String] -> [String]
removeComments xs = [x | x <- xs, not (isComment x)]

isComment :: String -> Bool
isComment (x:_) = x `elem` "~"
isComment _ = False

