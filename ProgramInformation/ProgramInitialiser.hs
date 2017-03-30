{-
- Version: 1.3
- Author: Danny Wilson
- Description: This file is used to house the version identifiers of the main program.
-}

module ProgramInformation.ProgramInitialiser where

-- getVersion will return the current version of the parsing program
getVersion :: IO()
getVersion = putStrLn "Sapphire: Version 2.4"

-- getDateBegun will return the date the current version was begun
getDateBegun :: IO()
getDateBegun = putStrLn "Date begun: Sunday 6th November, 2016"

-- getErrorCheckMessage will be printed to console when checking begins
getErrorCheckMessage :: IO()
getErrorCheckMessage = putStrLn "Checking for syntax errors within program..."

-- getErrorCheckComplete will be printed to console when checking finished
getErrorCheckComplete :: IO()
getErrorCheckComplete = putStrLn "Error checking completed."

-- getParsingMessage will be printed to console when parsing begins
getParsingMessage :: IO()
getParsingMessage = putStrLn "Parsing the program..."

-- getParsingComplete will be printed to console when parsing finishes
getParsingComplete :: IO()
getParsingComplete = putStrLn "Parsing complete."
