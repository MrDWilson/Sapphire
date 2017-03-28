{-
- Version: 1.2
- Author: Danny Wilson
- Description: The main functions to parse the files.
-}

--IMPORTANT:: Change the list (when comments are removed etc) to a tuple list, with (line, line number, number of indentations

module Parsers.Parsing where

import Data.List
import Data.List.Split

--Main initial parse function to call the others
initialParse :: String -> String
initialParse x = concat $ addNewLines $ parseTime (removeEmpty $ removeWhite $ inlineComments $ removeComments $ newlineParse $ removeBlockComment $ semicolonParse x)

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

--Formatting function
removeWhite :: [String] -> [String]
removeWhite xs = map (dropWhileEnd (== ' '))(map (dropWhile (== ' ')) xs)

--Remove empty elements
removeEmpty :: [String] -> [String]
removeEmpty xs = [x | x <- xs, x /= ""]

--Remove block comments
removeBlockComment :: String -> String
removeBlockComment xs
  |'$' `notElem` xs = xs
  |otherwise = takeWhile (/= '$') xs ++ (removeBlockComment $ removeBlockComment' xs)

removeBlockComment' :: String -> String
removeBlockComment' xs = tail $ dropWhile (/= '$') $ tail $ snd $ break ('$'==) xs

--Parsing into Scala begins here
parseLine :: String -> String
parseLine x
 |isInfixOf "start" x = parseStart x
 |x == "end" = parseEnd
 |x == "fi" = ""
 |x == "od" = "}"
 |isInfixOf "do" x = parseDo x
 |isInfixOf "if" (head $ splitOn ("->") x) = parseIf x
 |isInfixOf "|" x = parseElse x
 |isInfixOf "show" x = parseShow x
 |isInfixOf "as" x = parseV $ map (parseVariable') (parseVariable x) --This could possibly not work, test!!
 |isInfixOf "=" x = x
 |otherwise = ""


--Parsing variables
parseV :: [String] -> String
parseV xs = "var " ++ concat xs --Unhard code the indentation

parseVariable :: String -> [String]
parseVariable x = splitOn " " x

parseVariable' :: String -> String
parseVariable' x
  |x == "as" = " : "
  |x == "=" = " = "
  |otherwise = x

--Parsing for simple statements
parseStart x = "object " ++ (concat $ tail $ splitOn (" ") x) ++ " extends App {"

parseEnd = "}"

--Parsing for if statements
parseIf :: String -> String
parseIf x = readyIf $ checkShow $ removeWhite $ map formatIf $ splitOn "->" $ drop 3 x

--Remove the :
formatIf :: String -> String
formatIf xs = [x | x <- xs, x /= ':']

--Get the Scala statement ready
readyIf :: [String] -> String
readyIf xs = "if(" ++ head xs ++ ") {" ++ last xs ++ "}"

--Parsing for else statements
parseElse :: String -> String
parseElse x = readyElse $ checkShow $ removeWhite $ map formatIf $ splitOn "->" $ tail x

readyElse :: [String] -> String
readyElse xs
  | isInfixOf "otherwise" (head xs) = "else {" ++ last xs ++ "}"
  | otherwise = "else if(" ++ head xs ++ ") {" ++ last xs ++ "}"

parseDo :: String -> String
parseDo x = doReady $ drop 2 x

doReady :: String -> String
doReady x = "while(" ++ (dropWhite x) ++ "){"

checkShow :: [String] -> [String]
checkShow xs
  |isInfixOf "show" (last xs) = head xs : parseShow (last xs) : []
  |otherwise = xs

parseShow x = "println(" ++ parseShow' x ++ ")" --Unhard code the indentation

dropWhite :: String -> String
dropWhite xs = [x | x <- xs, x /= ' ']

--Code for parsing show statements

--Potentially not needed
takeShow :: String -> String
takeShow xs
  |'(' `notElem` xs = error "Show statements must be enclosed in ()"
  |otherwise = dropWhile(/= '(') xs ++ (takeShow $ takeShow' xs)

--Potentially not needed
takeShow' :: String -> String
takeShow' xs = tail $ takeWhile (/= ')') $ tail $ snd $ break (')' ==) xs

parseShow' :: String -> String
parseShow' xs = takeWhile (/= ')') $ tail $ dropWhile (/= '(') xs

--Rename please
parseTime :: [String] -> [String]
parseTime xs = map (parseLine) xs

--Can maybe do this without a dedicated function?
addNewLines :: [String] -> [String]
addNewLines xs = map (++ "\n") xs
