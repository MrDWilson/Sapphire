import qualified ProgramInformation.ProgramInitialiser as Info
import qualified Parsers.Parsing as Parse

main = do
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