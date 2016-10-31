import qualified ProgramInformation.ProgramInitialiser as Info

main = do
  Info.getVersion
  Info.getDateBegun
  --Import given program
  --Error check given program
  Info.getErrorCheckMessage
  --Complete error check
  Info.getErrorCheckComplete
  --Begin Parsing
  Info.getParsingMessage
  --Complete parsing
  Info.getParsingComplete