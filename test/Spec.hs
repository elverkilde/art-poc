import Protolude

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "tree" []

main :: IO ()
main = defaultMain tests
