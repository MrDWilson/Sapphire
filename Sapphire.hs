{-
- Version: 1.1
- Author: Danny Wilson
- Description: Calls the required functions to parse the file.
-}

import qualified ProgramInformation.ProgramInitialiser as Info
import qualified Parsers.Parsing as Parse
import System.Environment
import Data.List.Split

main = do
  --(fileName1:fileName2:_) <- getArgs
  args <- getArgs
  let fileName = head args
  Info.getVersion
  Info.getDateBegun
  --Import given program
  --Error check given program
  Info.getErrorCheckMessage
  --Complete error check
  Info.getErrorCheckComplete
  Info.getParsingMessage
  contents <- readFile ("SapphireExecutables/" ++ fileName)
  writeFile ("ScalaExecutables/" ++ (head $ splitOn(".") fileName) ++ ".scala") $ Parse.initialParse contents
  --Complete parsing
  Info.getParsingComplete