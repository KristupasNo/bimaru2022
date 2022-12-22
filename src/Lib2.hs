{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Lib2(renderDocument, hint, gameStart) where

import Types ( ToDocument(..), Document(..), Check (..), Coord(..) )
import Lib1 (State(..), BoxType(..))

-- IMPLEMENT
-- First, make Check an instance of ToDocument class
instance ToDocument Check where
    toDocument (Check c) = DMap [("coords", DList (map toDocument c))]

instance ToDocument Coord where
  toDocument (Coord x y) = DMap [("row", DInteger y), ("col", DInteger x)]

-- IMPLEMENT
-- Renders document to yaml
renderDocument :: Document -> String
renderDocument doc = "---\n" ++ renderDoc doc 0 ""

-- Render Document.
renderDoc :: Document -> Int -> String -> String
renderDoc doc a sep2 = do
    case doc of
      DMap dmap -> (case length dmap of
            0 -> ""
            _ -> sep2) ++ (renderDMap dmap a)
      DList list -> (case length list of
            0 -> ""
            _ -> sep2) ++ (renderDList list a)
      DNull -> "null" ++ "\n"
      DInteger int -> show int ++ "\n"
      DString "" -> "''" ++ "\n"
      DString str -> str ++ "\n"

-- Render DMap.
renderDMap :: [(String, Document)] -> Int -> String
renderDMap ((key, doc):remain) a
    | key == "" = (space a) ++ "''" ++ ": " ++ (renderDoc doc (a + 1) "\n")
    | null remain = (space a) ++ key ++ ": " ++ (renderDoc doc (a + 1) "\n")
    | otherwise = (space a) ++ key ++ ": " ++ (renderDoc doc (a + 1) "\n") ++ (renderDMap remain a)
renderDMap [] _ = "[]" ++ "\n"

-- Render Dlist.
renderDList :: [Document] -> Int -> String
renderDList (x:remain) a
    | null remain = (space a) ++ "- " ++ (renderDoc x (a + 1) "\n")
    | otherwise = (space a) ++ "- " ++ (renderDoc x (a + 1) "\n") ++ (renderDList remain a)
renderDList [] _ = "[]" ++ "\n"

space :: Int -> String
space n = replicate (n * 2) ' '

-- IMPLEMENT
-- This adds game data to initial state
-- Errors are reported via Either but not error 
gameStart :: State -> Document -> Either String State
gameStart (State board' _ _) doc = do 
    oRows <- fillList doc "occupied_rows" 
    oCols <- fillList doc "occupied_cols" 
    Right  (State board' oRows oCols)
gameStart _ _ = Left "Error: Wrong parameters "


fillList :: Document -> String -> Either String [Int]
fillList (DMap map') pointer = case  getDocument map' pointer of
    Left _ -> Left "Error: Cannot return "
    Right x ->  case getInt x of
        Left _ -> Left "Error: Cannot return "
        Right intList -> Right intList
fillList _ _ = Left "Error: Wrong parameters "

-- Extracts a "Document" type element by String
getDocument :: [(String, Document)] -> String -> Either String Document
getDocument ((s, doc) : remain) pointer
  | s == pointer = Right doc
  | null remain = Right DNull
  | otherwise = case  getDocument remain pointer of
                    Left _  -> Left "Error: Cannot return "
                    Right d -> Right d
getDocument _ _ = Left "Error: Wrong parameters "

-- Converts a DList to an Int list.
getInt :: Document -> Either String [Int]
getInt (DList list) =  case sep list [] of
  Left _ -> Left "Wrong List"
  Right a -> Right a
getInt _ = Left "Error: Wrong parameters "

sep :: [Document] -> [Int] -> Either String [Int]
sep ((DInteger int):x) acc = sep1 x (Right (acc ++ [int]))
sep _ _ = Left "Wrong Integer"

sep1 :: [Document] -> Either String [Int] -> Either String [Int]
sep1 [] (Right b) = Right b
sep1 ((DInteger int):x) (Right b) = sep1 x (Right (b ++ [int]))
sep1 _ _ = Left "Wrong Integer"

-- IMPLEMENT
-- Adds hint data to the game state
-- Errors are reported via Either but not error 
hint :: State -> Document -> Either String State
hint (State b r c) d = Right $ State (funn (hintPosition (initcollist d) (initrowlist d)) b) r c
  where
    fun1 x hh2 = fst $ (splitAt ((x)-1) hh2)
    fun2 x hh2 = [FilledBox] ++ (drop 1 (snd (splitAt ((x)-1) hh2)))
    funn [] hh1 = hh1
    funn (x:xs) hh1 = funn xs (fun1 x hh1 ++ fun2 x hh1)
hint _ _ = Left "Error: Wrong data"

initcollist :: Document -> [Int]
initcollist d = case (parseCol d []) of 
  Left _ -> []
  Right a -> getIntData a []

initrowlist :: Document -> [Int]
initrowlist d = case (parseRow d []) of
  Left _ -> []
  Right a -> getIntData a []

--Parse Document for cols.
parseCol :: Document -> [Document] -> Either String [Document]
parseCol DNull d = Right d
parseCol (DMap ((str, doc):remain)) d = case str of
    "coords" -> parseCol doc d
    "head" -> case (parseCol doc d) of 
      Left s -> Left s
      Right h -> parseCol (DMap remain) h
    "tail" -> parseCol doc d
    "col" -> Right ([doc] ++ d)
    _ -> Left "Bad key"
parseCol _ _ = Left "Bad document"

--Parse Document for rows.
parseRow :: Document -> [Document] -> Either String [Document]
parseRow DNull d = Right d
parseRow (DMap ((str, doc):remain)) d = case str of
    "coords" -> parseRow doc d
    "head" -> case (ttt2 doc d) of
      Left s -> Left s
      Right h -> parseRow (DMap remain) h
    "tail" -> parseRow doc d
    _ -> Left "Bad key"
parseRow _ _ = Left "Bad document"

--Additional function for parsing rows.
ttt2 :: Document -> [Document] -> Either String [Document]
ttt2 (DMap (_:(str, doc):_)) d = case str of
    "row" -> Right ([doc] ++ d)
    _ -> Left "Bad key"
ttt2 _ _ = Left "Bad document"

-- Parse list of DInteger to list of Integers.
getIntData :: [Document] -> [Int] -> [Int]
getIntData [] acc = acc
getIntData ((DInteger x):xs) acc = getIntData xs ([x] ++ acc)
getIntData (DNull:xs) acc = getIntData xs acc
getIntData _ _ = []

-- Make hint positions on board from hint coords. Example: [1, 2, 5] [3, 4, 5] -> [32, 43, 56]
hintPosition :: [Int] -> [Int] -> [Int]
hintPosition c r = [x+1+y*10 | (x,y) <- zip c r]
