{-
- Version: 1.3
- Author: Danny Wilson
- Description: Calls the required functions to parse the file.
-}

import qualified ProgramInformation.ProgramInitialiser as Info
import qualified Parsers.Parsing as Parse
import qualified ErrorChecker.ErrorChecker as Error
import System.Environment
import Data.List.Split

main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile ("SapphireExecutables/" ++ fileName)
  Info.getVersion
  Info.getDateBegun
  --Import given program
  --Error check given program
  Info.getErrorCheckMessage
  Error.errorCheck $ (Parse.removeEmpty $ Parse.removeWhite $ Parse.inlineComments $ Parse.removeComments $ Parse.newlineParse $ Parse.removeBlockComment $ Parse.semicolonParse (contents ++ (head $ splitOn(".") fileName)))
  Info.getErrorCheckComplete
  Info.getParsingMessage
  writeFile ("ScalaExecutables/" ++ (head $ splitOn(".") fileName) ++ ".scala") $ Parse.initialParse contents
  --Complete parsing
  Info.getParsingComplete
