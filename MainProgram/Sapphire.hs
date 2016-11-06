{-
- Version: 1.1
- Author: Danny Wilson
- Description: Calls the required functions to parse the file.
-}

import qualified ProgramInformation.ProgramInitialiser as Info
import qualified Parsers.Parsing as Parse
import System.Environment

main = do
  --(fileName1:fileName2:_) <- getArgs
  Info.getVersion
  Info.getDateBegun
  --Import given program
  --Error check given program
  Info.getErrorCheckMessage
  --Complete error check
  Info.getErrorCheckComplete
  contents <- readFile "TestingFiles/InitialTesting.txt"
  print $ Parse.initialParse contents
  Info.getParsingMessage
  --Complete parsing
  Info.getParsingComplete