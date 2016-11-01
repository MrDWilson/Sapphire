{-
- Version: 1.0
- Author: Danny Wilson
- Description: The main functions to parse the files
-}

module Parsers.Parsing where
  
--initialParse :: String -> [String]
initialParse xs = [x | x <- xs, x /= ";"]