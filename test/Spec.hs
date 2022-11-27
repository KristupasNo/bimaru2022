{-# OPTIONS_GHC -Wno-unused-imports #-}
import Test.Tasty
import Test.Tasty.HUnit

import Lib2 (renderDocument, gameStart, hint)
import Types (Document(..))
import Lib1 (State (..), BoxType (..), emptyState)

main :: IO ()
main = defaultMain (testGroup "Tests" [
  toYamlTests,
  gameStartTests,
  hintTests])

toYamlTests :: TestTree
toYamlTests = testGroup "Document to yaml"
  [   testCase "null" $
        renderDocument DNull @?= nullTest
    , testCase "int" $
        renderDocument (DInteger 5) @?= intTest
    , testCase "list of ints" $
        renderDocument (DList [DInteger 5, DInteger 6]) @?= listOfInts
    , testCase "DMap Int" $
        renderDocument (DMap [("row", DInteger 6)]) @?= mapOfInt
    , testCase "DMap List" $
        renderDocument (DMap [("row", DList [DInteger 1, DInteger 2])]) @?= mapOfList
    , testCase "String" $
        renderDocument (DString "labas") @?= stringTest
    , testCase "list of Strings" $
        renderDocument (DList [DString "labas", DString "sveikas"]) @?= listOfStrings
    , testCase "list of DMap" $
        renderDocument (DMap [("col", DString "labas"), ("row", DInteger 10)]) @?= listOfDMap
  ]

stringTest :: String
stringTest = unlines [
    "---",
    "labas"
  ]

nullTest :: String
nullTest = unlines [
    "---",
    "null"
  ]

intTest :: String
intTest = unlines [
    "---",
    "5"
  ]

listOfInts :: String
listOfInts = unlines [
    "---",
    "- 5",
    "- 6"
  ]

listOfStrings :: String
listOfStrings = unlines [
    "---",
    "- labas",
    "- sveikas"
  ]

mapOfInt :: String
mapOfInt = unlines [
    "---",
    "row: 6"
  ]

mapOfList :: String
mapOfList = unlines [
    "---",
    "row: ",
    "  - 1",
    "  - 2"
  ]

listOfDMap :: String
listOfDMap = unlines [
    "---",
    "col: labas",
    "row: 10"
  ]


gameStartTests :: TestTree
gameStartTests = testGroup "Test start document" 
  [
    testCase "gameStart all 0'" $
      gameStart emptyState gameStartDoc @?= Right (State (replicate 100 EmptyBox) [0,0,0,0,0,0,0,0,0,0] [0,0,0,0,0,0,0,0,0,0]),
    testCase "gameStart no more than 10 elements" $
      assertBool "Test 'gameStart no more than 10 elements' failed." $
        gameStart emptyState gameStartDoc /= Right (State (replicate 100 EmptyBox) [0,0,0,0,0,0,0,0,0,0,0] [0,0,0,0,0,0,0,0,0,0,0]),
    testCase "gameStart no less than 10 elements" $
      assertBool "Test 'gameStart no less than 10 elements' failed." $
        gameStart emptyState gameStartDoc /= Right (State (replicate 100 EmptyBox) [0,0,0,0,0,0,0,0,0] [0,0,0,0,0,0,0,0,0])
  ]

gameStartDoc :: Document
gameStartDoc = DMap [("number_of_hints",DInteger 10),("occupied_cols",DList [DInteger 0,DInteger 0,DInteger 0,DInteger 0,DInteger 0,DInteger 0,DInteger 0,DInteger 0,DInteger 0,DInteger 0]),("occupied_rows",DList [DInteger 0,DInteger 0,DInteger 0,DInteger 0,DInteger 0,DInteger 0,DInteger 0, DInteger 0,DInteger 0,DInteger 0])]


hintTests :: TestTree
hintTests = testGroup "Test hint document" 
  [
    testCase "0 hints" $
      assertBool "Test '0 hints' failed." $
        hint (State [] [] []) DNull == Right (State [] [] []),
    testCase "0 hints V2" $
      hint (State [] [] []) DNull @?= Right (State [] [] []),
    testCase "1 hint" $
      hint (State [EmptyBox] [] []) hintDoc1 @?= Right (State [FilledBox] [] []),
    testCase "2 hints" $
      hint (State [EmptyBox, EmptyBox, EmptyBox] [] []) hintDoc2 @?= Right (State [FilledBox, FilledBox, EmptyBox] [] []),
    testCase "10 hints" $
      hint (State (replicate 100 EmptyBox) [] []) hintDoc10 @?= Right (State ((replicate 10 FilledBox)++(replicate 90 EmptyBox)) [] [])
  ]

hintDoc1 :: Document
hintDoc1 = DMap [("coords",DMap [("head",DMap [("col",DInteger 0),("row",DInteger 0)]),("tail", DNull)])]

hintDoc2 :: Document
hintDoc2 = DMap [("coords",DMap [("head",DMap [("col",DInteger 0),("row",DInteger 0)]),("tail",DMap [("head",DMap [("col",DInteger 1),("row",DInteger 0)]),("tail", DNull)])])]

hintDoc10 :: Document
hintDoc10 = DMap [("coords",DMap [("head",DMap [("col",DInteger 0),("row",DInteger 0)]),("tail",DMap [("head",DMap [("col",DInteger 1),("row",DInteger 0)]),("tail",DMap [("head",DMap [("col",DInteger 5),("row",DInteger 0)]),("tail",DMap [("head",DMap [("col",DInteger 6),("row",DInteger 0)]),("tail",DMap [("head",DMap [("col",DInteger 8),("row",DInteger 0)]),("tail",DMap [("head",DMap [("col",DInteger 3),("row",DInteger 0)]),("tail",DMap [("head",DMap [("col",DInteger 7),("row",DInteger 0)]),("tail",DMap [("head",DMap [("col",DInteger 9),("row",DInteger 0)]),("tail",DMap [("head",DMap [("col",DInteger 2),("row",DInteger 0)]),("tail",DMap [("head",DMap [("col",DInteger 4),("row",DInteger 0)]),("tail",DNull)])])])])])])])])])])]
