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
controller xs = map errorCheckLine $ fileNameCheckH $ endCheckH $ startCheckH xs

--Handler for file name check
fileNameCheckH :: [String] -> [String]
fileNameCheckH xs
  |fileNameCheck xs = xs
  |otherwise = error("Error 1: File name does not match start name.")

--File name check
fileNameCheck :: [String] -> Bool
fileNameCheck xs = (concat $ tail $ splitOn (" ") $ trim $ head xs) == (last xs)

--Handler for start check
startCheckH :: [String] -> [String]
startCheckH xs
  |startCheck xs = xs
  |otherwise = error("Error 2: Start is missing from beginning of file.")

--Check that start is present
startCheck :: [String] -> Bool
startCheck xs = isInfixOf "start" $ head xs

--Handler for end check
endCheckH :: [String] -> [String]
endCheckH xs
  |endCheck xs = xs
  |otherwise = error("Error 3: End is missing from the end of the file.")

--Check that end is present
endCheck :: [String] -> Bool
endCheck xs = isInfixOf "end" $ (last $ init xs)


errorCheckLine :: String -> String
errorCheckLine x
  |isInfixOf "fun" (head $ splitOn(" ") x) = funCheckH x
  |isInfixOf "nuf" x = x
  |isInfixOf "result" (head $ splitOn (" ") x) = resultCheckH x
  |isInfixOf "fi" x = x
  |"od" == (take 2 x) = x
  |isInfixOf "do" x = doCheckH x
  |isInfixOf "if" (head $ splitOn ("->") x) = ifCheckH x
  |isInfixOf "|" x = elseCheckH x
  |isInfixOf "show" x = showCheckH x
  |isInfixOf "as" x = asCheckH x
  |isInfixOf "=" x = x
  |otherwise = x --error("Error 4: Unrecognised line.")

--Function checker handler
funCheckH :: String -> String
funCheckH x
  |funCheck x = x
  |otherwise = error("Error 5: Error in a function definition.")

--Check that all function syntax is present
funCheck :: String -> Bool
funCheck x = isInfixOf ":" x && isInfixOf "(" x && isInfixOf ")" x

--Result statement check handler
resultCheckH :: String -> String
resultCheckH x
  |resultCheck x = x
  |otherwise = error("Error 6: Error in result statement.")

--Checks the result statements abide to syntax rules
resultCheck :: String -> Bool
resultCheck x = isInfixOf "result" x && (all isPrint $ last $ splitOn (" ") $ trim x)

--Do check handler
doCheckH :: String -> String
doCheckH x
  |doCheck x = x
  |otherwise = error("Error 7: Error in do statement.")

--Checks that do statements abide to syntax rules
doCheck :: String -> Bool
doCheck x = isInfixOf "do" x && (all isPrint $ last $ splitOn (" ") $ trim x)

--Handler for if statements
ifCheckH :: String -> String
ifCheckH x
    |ifCheck x = x
    |otherwise = error("Error 8: Error in if statement.")

--Makes sure if statements abide to syntax rules
ifCheck :: String -> Bool
ifCheck x = isInfixOf ("->") x && isInfixOf (":=") x

--Handler for else statements
elseCheckH :: String -> String
elseCheckH x
  |elseCheck x = x
  |otherwise = error("Error 9: Error in else statement.")

--Makes sure else starements abide to syntax rules
elseCheck :: String -> Bool
elseCheck x = isInfixOf ("->") x && (isInfixOf (":=") x || isInfixOf ("otherwise") x) && isInfixOf ("|") x

--Handler for show statement checks
showCheckH :: String -> String
showCheckH x
  |showCheck x = x
  |otherwise = error("Error 10: Error in show statement.")

--Check that show statements are enclosed in brackets
showCheck :: String -> Bool
showCheck x = (head $ drop 4 $ trim x) == '(' && (last $ trim x) == ')'

--Handler for variable declarations
asCheckH :: String -> String
asCheckH x
  |asCheck x = x
  |otherwise = error("Error 11: Error in variable declaration.")

--Check that variable decs conform to syntax rules
asCheck :: String -> Bool
asCheck x = (isInfixOf ("=") $ trim x) && (isInfixOf ("as") $ trim x)

--trim whitespace
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace
