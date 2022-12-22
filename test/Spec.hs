import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.String.Conversions
import Data.Yaml as Y ( encode )

import Lib1 (State(..), BoxType(..), emptyState)
import Lib2 (renderDocument, gameStart, hint)
import Lib3 (parseDocument)
import Types (Document(..))

main :: IO ()
main = defaultMain (testGroup "Tests" [
  toYamlTests,
  fromYamlTests,
  gameStartTests,
  hintTests,
  properties])

properties :: TestTree
properties = testGroup "Properties" [golden, dogfood]

golden :: TestTree
golden = testGroup "Handles foreign rendering"
  [
    testProperty "parseDocument (Data.Yaml.encode doc) == doc" $
      \doc -> parseDocument (cs (Y.encode doc)) == Right doc
  ]

dogfood :: TestTree
dogfood = testGroup "Eating your own dogfood"
  [  
    testProperty "parseDocument (renderDocument doc) == doc" $
      \doc -> parseDocument (renderDocument doc) == Right doc
  ]

fromYamlTests :: TestTree
fromYamlTests = testGroup "Document from yaml"
  [   testCase "null" $
        parseDocument nullTest @?= Right DNull
    , testCase "int" $
        parseDocument intTest @?= Right (DInteger 5)
    , testCase "list of ints" $
        parseDocument listOfInts @?= Right (DList [DInteger 5, DInteger 6])
    , testCase "DMap Int" $
        parseDocument mapOfInt @?= Right (DMap [("row", DInteger 6)])
    , testCase "DMap List" $
        parseDocument mapOfList @?= Right (DMap [("row", DList [DInteger 1, DInteger 2])])
    , testCase "String" $
        parseDocument stringTest @?= Right (DString "labas")
    , testCase "list of Strings" $
        parseDocument listOfStrings @?= Right (DList [DString "labas", DString "sveikas"])
    , testCase "list of DMap" $
        parseDocument listOfDMap @?= Right (DMap [("col", DString "labas"), ("row", DInteger 10)])
    , testCase "list of 3 DMap" $
        parseDocument listOfDMap3 @?= Right (DMap [("col", DMap [("row", DMap [("labas", DInteger 99), ("sveikas",  DNull)])])])
    , testCase "list of 4 DMap" $
        parseDocument listOfDMap4 @?= Right (DMap [("col", DMap [("row", DMap [("labas", DInteger 99), ("labas123", DMap [("sveikas", DList [DInteger 1, DInteger 2])])])])])
    -- IMPLEMENT more test cases:
    --  other primitive types/values
    --  nested types
  ]

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
    , testCase "list of 3 DMap" $
        renderDocument (DMap [("col", DMap [("row", DMap [("labas", DInteger 99), ("sveikas",  DNull)])])]) @?= listOfDMap3
    , testCase "list of 4 DMap" $
        renderDocument (DMap [("col", DMap [("row", DMap [("labas", DInteger 99), ("labas123", DMap [("sveikas", DList [DInteger 1, DInteger 2])])])])]) @?= listOfDMap4
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

stringTest :: String
stringTest = unlines [
    "---",
    "labas"
  ]

listOfStrings :: String
listOfStrings = unlines [
    "---",
    "- labas",
    "- sveikas"
  ]

listOfDMap :: String
listOfDMap = unlines [
    "---",
    "col: labas",
    "row: 10"
  ]

listOfDMap3 :: String
listOfDMap3 = unlines [
    "---",
    "col: ",
    "  row: ",
    "    labas: 99",
    "    sveikas: null"
  ]

listOfDMap4 :: String
listOfDMap4 = unlines [
    "---",
    "col: ",
    "  row: ",
    "    labas: 99",
    "    labas123: ",
    "      sveikas: ",
    "        - 1",
    "        - 2"
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
        gameStart emptyState gameStartDoc /= Right (State (replicate 100 EmptyBox) [0,0,0,0,0,0,0,0,0] [0,0,0,0,0,0,0,0,0]),
    testCase "gameStart bad list" $
      gameStart emptyState badList @?= Left "Error: Cannot return ",
    testCase "gameStart bad key" $
      gameStart emptyState badKey @?= Left "Error: Cannot return ",
    testCase "gameStart bad document" $
      gameStart emptyState (DInteger 8) @?= Left "Error: Wrong parameters ",
    testCase "gameStart bad value of key" $
      gameStart emptyState badKeyValue @?= Left "Error: Cannot return "
  ]

gameStartDoc :: Document
gameStartDoc = DMap [("number_of_hints",DInteger 10),("occupied_cols",DList [DInteger 0,DInteger 0,DInteger 0,DInteger 0,DInteger 0,DInteger 0,DInteger 0,DInteger 0,DInteger 0,DInteger 0]),("occupied_rows",DList [DInteger 0,DInteger 0,DInteger 0,DInteger 0,DInteger 0,DInteger 0,DInteger 0, DInteger 0,DInteger 0,DInteger 0])]

badList :: Document
badList = DMap [("number_of_hints",DInteger 10),("occupied_cols",DList [DString "String",DInteger 0,DInteger 0,DInteger 0,DInteger 0,DInteger 0,DInteger 0,DInteger 0,DInteger 0,DInteger 0]),("occupied_rows",DList [DInteger 0,DInteger 0,DInteger 0,DInteger 0,DInteger 0,DInteger 0,DInteger 0, DInteger 0,DInteger 0,DInteger 0])]

badKey :: Document
badKey = DMap [("number_of_hints",DInteger 10),("occupied_labas",DList [DInteger 0,DInteger 0,DInteger 0,DInteger 0,DInteger 0,DInteger 0,DInteger 0,DInteger 0,DInteger 0,DInteger 0]),("occupied_rows",DList [DInteger 0,DInteger 0,DInteger 0,DInteger 0,DInteger 0,DInteger 0,DInteger 0, DInteger 0,DInteger 0,DInteger 0])]

badKeyValue :: Document
badKeyValue = DMap [("number_of_hints",DInteger 10),("occupied_labas",DInteger 100),("occupied_rows",DList [DInteger 0,DInteger 0,DInteger 0,DInteger 0,DInteger 0,DInteger 0,DInteger 0, DInteger 0,DInteger 0,DInteger 0])]

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