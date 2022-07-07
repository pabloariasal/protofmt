import qualified FormattingSpec (spec)
import qualified ParsingSpec (spec)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Parsing" ParsingSpec.spec
  describe "Formatting" FormattingSpec.spec
