import Relude
import Test.Hspec (SpecWith, hspec)
import qualified Test.Lib

main :: IO ()
main = hspec tests

tests :: SpecWith ()
tests = do
  Test.Lib.tests
