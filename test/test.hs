import Test.Tasty
import Test.Tasty.HUnit
import qualified Parser as P

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ 
   testCase "Simple splitLines test" $ assertEqual "Comparison is wrong."
     ["abc", "def"] (P.splitLines "abc\ndef"),
   testCase "Simple splitLines test spaces" $ assertEqual "Comparison is wrong."
     ["abc", "def"] (P.splitLines "  \tabc\n\t    def"), 
   testCase "Simple splitLines test comment I" $ assertEqual "Comparison is wrong."
     ["ab", "no_h"] (P.splitLines " ab#\nno_h"),
   testCase "Simple splitLines test comment II" $ assertEqual "Comparison is wrong."
     ["ab", "no_h"] (P.splitLines " ab#foo\nno_h"),
   testCase "Simple splitLines test linebreak I" $ assertEqual "Comparison is wrong."
     ["ab"] (P.splitLines " ab#foo\\\nno_h"),
   testCase "Simple splitLines test linebreak II" $ assertEqual "Comparison is wrong."
     ["ab","no_h"] (P.splitLines " ab#foo\\\\\nno_h"),
   testCase "Simple splitLines test linebreak III" $ assertEqual "Comparison is wrong."
     ["abno_h"] (P.splitLines " ab\\\nno_h"),
   testCase "Simple splitLines test linebreak IV" $ assertEqual "Comparison is wrong."
     ["ab\\", "no_h"] (P.splitLines " ab\\\\\nno_h"),
   testCase "Simple splitLines test linebreak V" $ assertEqual "Comparison is wrong."
     ["a\\\\"] (P.splitLines "a\\\\\\\n\\")        
  ]
