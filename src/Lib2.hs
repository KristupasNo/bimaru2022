{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Lib2(renderDocument, hint, gameStart) where

import Types ( ToDocument(..), Document(..), Check (..), Coord(..) )
import Lib1 (State(..), BoxType(..))

-- IMPLEMENT
-- First, make Check and Coord an instance of ToDocument class
instance ToDocument Coord where
  toDocument (Coord x y) = DMap [("row", DInteger y), ("col", DInteger x)]

instance ToDocument Check where
    toDocument (Check x) = DMap [("coords", DList (map toDocument x))]

-- IMPLEMENT
-- Renders document to yaml
renderDocument :: Document -> String
renderDocument dcmnt = "---\n" ++ render dcmnt 0 ""

-- Render Document.
render :: Document -> Int -> String -> String
render dcmnt a sep2 = do
    case dcmnt of
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
renderDMap ((key, dcmnt):remain) a
    | null remain = (replicate (a * 2) ' ') ++ key ++ ": " ++ (render dcmnt (a + 1) "\n")
    | key == "" = (replicate (a * 2) ' ') ++ "''" ++ ": " ++ (render dcmnt (a + 1) "\n")
    | otherwise = (replicate (a * 2) ' ') ++ key ++ ": " ++ (render dcmnt (a + 1) "\n") ++ (renderDMap remain a)
renderDMap [] _ = "[]" ++ "\n"

-- Render Dlist.
renderDList :: [Document] -> Int -> String
renderDList (x:remain) a
    | null remain = (replicate (a * 2) ' ') ++ "- " ++ (render x (a + 1) "\n")
    | otherwise = (replicate (a * 2) ' ') ++ "- " ++ (render x (a + 1) "\n") ++ (renderDList remain a)
renderDList [] _ = "[]" ++ "\n"

-- IMPLEMENT
-- This adds game data to initial state
-- Errors are reported via Either but not error 
gameStart :: State -> Document -> Either String State
gameStart (State board' _ _) dcmnt = do 
    oRows <- fillList dcmnt "occupied_rows" 
    oCols <- fillList dcmnt "occupied_cols" 
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
getDocument ((s, dcmnt) : remain) pointer
  | s == pointer = Right dcmnt
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
hint (State b r c) d = Right $ State (funn (hintPosition (initializeHintList d "col" []) (initializeHintList d "row" [])) b) r c
  where
    fun1 x hh2 = fst $ (splitAt ((x)-1) hh2)
    fun2 x hh2 = [FilledBox] ++ (drop 1 (snd (splitAt ((x)-1) hh2)))
    funn [] hh1 = hh1
    funn (x:xs) hh1 = funn xs (fun1 x hh1 ++ fun2 x hh1)
hint _ _ = Left "Error: Wrong data"

-- Parse Document.
tt :: Document -> String -> Either String Document
tt (DMap ((str, d):_)) key
  | str == "coords" = tt d key
  | str == "head" = tt d key
  | str == "tail" = tt d key
  | str == key = Right d
tt (DMap (_:(str, d):_)) key
  | str == key = Right d
  | otherwise = Left "Error: Wrong key"
tt DNull _ = Right DNull
tt _ _ = Left "Error: Parse error"

-- Parse Document.
tt1 :: Document -> String -> Either String Document
tt1 (DMap ((str, d):remain)) key
  | str == "head" = Right $ DMap remain
  | str == "tail" = tt1 d key
  | otherwise = tt1 d key
tt1 DNull _ = Right DNull
tt1 _ _ = Left "Error: Parse error"

-- Parse integer and add to the list.
getIntData :: Document -> [Int] -> [Int]
getIntData (DInteger x) acc = [x] ++ acc
getIntData DNull acc = acc
getIntData _ _ = []

-- Make hint positions on board from hint coords. Example: [1, 2, 5] [3, 4, 5] -> [32, 43, 56]
hintPosition :: [Int] -> [Int] -> [Int]
hintPosition c r = [x+1+y*10 | (x,y) <- zip c r]

-- Make a list of hints' positions from Document.
initializeHintList :: Document -> String -> [Int] -> [Int]
initializeHintList a b c = case tt1 a b of
  Left _ -> []
  Right h10s1 -> case tt1 h10s1 b of
    Left _ -> []
    Right h10s2 -> case tt1 h10s2 b of
      Left _ -> []
      Right h10s3 -> case tt1 h10s3 b of
        Left _ -> []
        Right h10s4 -> case tt1 h10s4 b of
          Left _ -> []
          Right h10s5 -> case tt1 h10s5 b of
            Left _ -> []
            Right h10s6 -> case tt1 h10s6 b of
              Left _ -> []
              Right h10s7 -> case tt1 h10s7 b of
                Left _ -> []
                Right h10s8 -> case tt1 h10s8 b of
                  Left _ -> []
                  Right h10s9 -> case tt h10s9 b of
                    Left _ -> []
                    Right h10s10 -> getIntData h10s10 $ case tt1 a b of
                      Left _ -> []
                      Right h9s1 -> case tt1 h9s1 b of
                        Left _ -> []
                        Right h9s2 -> case tt1 h9s2 b of
                          Left _ -> []
                          Right h9s3 -> case tt1 h9s3 b of
                            Left _ -> []
                            Right h9s4 -> case tt1 h9s4 b of
                              Left _ -> []
                              Right h9s5 -> case tt1 h9s5 b of
                                Left _ -> []
                                Right h9s6 -> case tt1 h9s6 b of
                                  Left _ -> []
                                  Right h9s7 -> case tt1 h9s7 b of
                                    Left _ -> []
                                    Right h9s8 -> case tt h9s8 b of
                                      Left _ -> []
                                      Right h9s9 -> getIntData h9s9 $ case tt1 a b of
                                        Left _ -> []
                                        Right h8s1 -> case tt1 h8s1 b of
                                          Left _ -> []
                                          Right h8s2 -> case tt1 h8s2 b of
                                            Left _ -> []
                                            Right h8s3 -> case tt1 h8s3 b of
                                              Left _ -> []
                                              Right h8s4 -> case tt1 h8s4 b of
                                                Left _ -> []
                                                Right h8s5 -> case tt1 h8s5 b of
                                                  Left _ -> []
                                                  Right h8s6 -> case tt1 h8s6 b of
                                                    Left _ -> []
                                                    Right h8s7 -> case tt h8s7 b of
                                                      Left _ -> []
                                                      Right h8s8 -> getIntData h8s8 $ case tt1 a b of
                                                        Left _ -> []
                                                        Right h7s1 -> case tt1 h7s1 b of
                                                          Left _ -> []
                                                          Right h7s2 -> case tt1 h7s2 b of
                                                            Left _ -> []
                                                            Right h7s3 -> case tt1 h7s3 b of
                                                              Left _ -> []
                                                              Right h7s4 -> case tt1 h7s4 b of
                                                                Left _ -> []
                                                                Right h7s5 -> case tt1 h7s5 b of
                                                                  Left _ -> []
                                                                  Right h7s6 -> case tt h7s6 b of
                                                                    Left _ -> []
                                                                    Right h7s7 -> getIntData h7s7 $ case tt1 a b of
                                                                      Left _ -> []
                                                                      Right h6s1 -> case tt1 h6s1 b of
                                                                        Left _ -> []
                                                                        Right h6s2 -> case tt1 h6s2 b of
                                                                          Left _ -> []
                                                                          Right h6s3 -> case tt1 h6s3 b of
                                                                            Left _ -> []
                                                                            Right h6s4 -> case tt1 h6s4 b of
                                                                              Left _ -> []
                                                                              Right h6s5 -> case tt h6s5 b of
                                                                                Left _ -> []
                                                                                Right h6s6 -> getIntData h6s6 $ case tt1 a b of
                                                                                  Left _ -> []
                                                                                  Right h5s1 -> case tt1 h5s1 b of
                                                                                    Left _ -> []
                                                                                    Right h5s2 -> case tt1 h5s2 b of
                                                                                      Left _ -> []
                                                                                      Right h5s3 -> case tt1 h5s3 b of
                                                                                        Left _ -> []
                                                                                        Right h5s4 -> case tt h5s4 b of
                                                                                          Left _ -> []
                                                                                          Right h5s5 -> getIntData h5s5 $ case tt1 a b of
                                                                                            Left _ -> []
                                                                                            Right h4s1 -> case tt1 h4s1 b of
                                                                                              Left _ -> []
                                                                                              Right h4s2 -> case tt1 h4s2 b of
                                                                                                Left _ -> []
                                                                                                Right h4s3 -> case tt h4s3 b of
                                                                                                  Left _ -> []
                                                                                                  Right h4s4 -> getIntData h4s4 $ case tt1 a b of
                                                                                                    Left _ -> []
                                                                                                    Right h3s1 -> case tt1 h3s1 b of
                                                                                                      Left _ -> []
                                                                                                      Right h3s2 -> case tt h3s2 b of
                                                                                                        Left _ -> []
                                                                                                        Right h3s3 -> getIntData h3s3 $ case tt1 a b of
                                                                                                          Left _ -> []
                                                                                                          Right h2s1 -> case tt h2s1 b of
                                                                                                            Left _ -> []
                                                                                                            Right h2s2 -> getIntData h2s2 $ case tt a b of
                                                                                                              Left _ -> []
                                                                                                              Right h1s1 -> getIntData h1s1 c