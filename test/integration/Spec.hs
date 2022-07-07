import ProtoFmt (runFormatting)
import Test.Hspec

main :: IO ()
main = do
  input <- readFile "test/integration/input.proto"
  expected <- readFile "test/integration/expected.proto"
  hspec $ spec input expected

spec :: String -> String -> Spec
spec input expected = do
  describe "Integration" $ do
    it "formatting succeeds on a valid proto file" $ do
      runFormatting "" input `shouldBe` Right expected
    it "formatting is idempotent" $ do
      runFormatting "" expected `shouldBe` Right expected
