import Test.Tasty
import Test.Tasty.HUnit
import Parser

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ 
   testCase "Simple splitLines test" $ assertEqual "Comparison is wrong."
     ["abc", "def"] (splitLines "abc\ndef")
  ]
