module ProtoFmt (runFormatting, Error, Result) where

import Formatting (formatProto)
import Parsing (parseProto)

type Error = String

type Result = String

runFormatting :: String -> String -> Either Error Result
runFormatting file s = case parseProto file s of
  Left error -> Left error
  Right result -> Right $ formatProto result
