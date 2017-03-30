{-
- Version: 1.1
- Author: Danny Wilson
- Description: The main functions to error check the files.
- Note: All error checks will have an equvilant H function.
- E.g. test will have testH, which is it's handler. This
- calls the error check function and handles the result.
-}

module ErrorChecker.ErrorChecker where

import Data.List.Split
import Control.Monad
import Data.List
import Data.Char

--Main function to call the error checkers
errorCheck :: [String] -> IO()
errorCheck x
  |(controller x) == x = putStrLn "Complete"
  |otherwise = putStrLn "Failed"

--Controller to call all error check functions
controller :: [String] -> [String]
controller xs = fileNameCheckH $ endCheckH $ startCheckH xs

--Handler for file name check
fileNameCheckH :: [String] -> [String]
fileNameCheckH xs
  |fileNameCheck xs = xs
  |otherwise = error("Error 1: File name does not match start name.")

--File name check
fileNameCheck :: [String] -> Bool
fileNameCheck xs
  | (concat $ tail $ splitOn (" ") $ trim $ head xs) == (last xs) = True
  | otherwise = False

--Handler for start check
startCheckH :: [String] -> [String]
startCheckH xs
  |startCheck xs = xs
  |otherwise = error("Error 2: Start is missing from beginning of file.")

--Check that start is present
startCheck :: [String] -> Bool
startCheck xs
  |isInfixOf "start" $ head xs = True
  |otherwise = False

--Handler for end check
endCheckH :: [String] -> [String]
endCheckH xs
  |endCheck xs = xs
  |otherwise = error("Error 3: End is missing from the end of the file.")

--Check that end is present
endCheck :: [String] -> Bool
endCheck xs
  |isInfixOf "end" $ (last $ init xs) = True
  |otherwise = False

errorCheckLine :: String -> String
errorCheckLine x
  |isInfixOf "fun" (head $ splitOn(" ") x) = parseFun x
  |isInfixOf "nuf" x = "}"
  |isInfixOf "result" (head $ splitOn (" ") x) = parseResult x
  |isInfixOf "fi" x = ""
  |"od" == (take 2 x) = parseEnd
  |isInfixOf "do" x = parseDo x
  |isInfixOf "if" (head $ splitOn ("->") x) = parseIf x
  |isInfixOf "|" x = parseElse x
  |isInfixOf "show" x = parseShow x
  |isInfixOf "as" x = parseV $ map (parseVariable') (parseVariable x) --This could possibly not work, test!!
  |isInfixOf "=" x = x
  |otherwise = error("Error 4: Unrecognised line.")

--trim whitespace
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace
