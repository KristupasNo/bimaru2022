{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Lib1(
    State, emptyState, gameStart, render, mkCheck, toggle, hint
) where

import Types
import Data.List (intercalate)
import Data.List (elemIndices)

-- 10x10 grid size
boardSize :: Int
boardSize = 10

-- "Toggle" assigns "PredictBox" to a box
-- "Hint" assigns "FilledBox" to a box 
data BoxType = EmptyBox | PredictBox | FilledBox deriving (Show, Eq)

-- This is a state of your game.
-- It must contain all values you might need during a game:
-- number of occupied rows/cols, hints, occupied cells,..
-- You can change the right hand side as you wish but please
-- keep the type name as is
data State = State
  { board :: [BoxType],
    occRows :: [Int],
    occCols :: [Int]
  }
  deriving (Show)


-- This is very initial state of your program
emptyState :: State
emptyState = State (replicate 100 EmptyBox) [] []


-- This adds game data to initial state
gameStart :: State -> Document -> State
gameStart (State board' _ _) d = State board' (fillList d "occupied_rows") (fillList d "occupied_cols") 

-- Extracts the data from the document by String and fills a Int list
fillList :: Document -> String -> [Int]
fillList (DMap map') pointer = getInt (getDocument map' pointer)
fillList _ _ = []

-- Extracts a "Document" type element by String
getDocument :: [(String, Document)] -> String -> Document
getDocument ((s, d) : rem) pointer
  | s == pointer = d
  | null rem = DNull
  | otherwise = getDocument rem pointer
getDocument _ _ = undefined

-- Converts a DList to an Int list.
getInt :: Document -> [Int]
getInt (DList list) = map sep list
  where
    sep (DInteger int) = int
    sep _ = undefined
getInt _ = []


-- renders your game board
render :: State -> String
render (State b occRows occCols) =
  "| " ++ foldr foldBoard "" (zip b [1 ..]) ++ "\n" ++ horDiv ++ "  " ++ intercalate "   " (map show occCols)
  where
    horDiv = replicate (4 * boardSize) '~' ++ "\n"
    foldBoard (box, index) acc = renderBox box ++ " | " ++ endRow index occRows ++ acc
    renderBox box = case box of
      EmptyBox -> " "
      PredictBox -> "O"
      FilledBox -> "X"
    endRow index occRows' =
      if mod index boardSize == 0
        then
          show (occRows' !! (div index boardSize - 1))
            ++ ( if index /= boardSize * boardSize
                   then "\n" ++ horDiv ++ "| "
                   else ""
               )
        else ""


-- IMPLEMENT
-- Make check from current state
-- In order to check this function, there has to be the right solution for the game.
-- To achieve that execute these commands: hint 10; toggle A0 A9 F1 F4 F5 G1 H6 I1 I8 J1
mkCheck :: State -> Check
mkCheck (State b _ _) = Check [Coord { col = (mod x 10), row = (div x 10)}  | x <- (elemIndices FilledBox b) ++ (elemIndices PredictBox b)]

-- IMPLEMENT
-- Toggle state's value
-- Receive raw user input tokens
-- For input use string of [t, u] for toggle (t) or untoggle (u), [A, B, C, D, E, F, G, H, I, J] for row and [1, 2, 3, 4, 5, 6, 7, 8, 9, 0] for column.
-- Examples: toggle tA6, toggle tD0, toggle tJ9, toggle uH8, toggle uA0
toggle :: State -> [String] -> State
toggle (State b r c) strs = State (funn (addGuess strs) b) r c
  where
    fun1 x hh2 
      | x > 100 = fst $ (splitAt ((x)-1-100) hh2)
      | x <= 100 = fst $ (splitAt ((x)-1) hh2)
      | otherwise = error "Wrong number of position"
    fun2 x hh2 
      | x > 100 = [EmptyBox] ++ (drop 1 (snd (splitAt ((x)-1-100) hh2)))
      | x <= 100 = [PredictBox] ++ (drop 1 (snd (splitAt ((x)-1) hh2)))
      | otherwise = error "Wrong number of position"
    funn [] hh1 = hh1
    funn (x:xs) hh1 = funn xs (fun1 x hh1 ++ fun2 x hh1)

    addGuess :: [String] -> [Int]
    addGuess ((a:b:c:_):strs) = (checkUn a) + (checkLet b) + (checkNum c) : addGuess strs
    addGuess _ = []
    
    checkLet:: Char -> Int 
    checkLet 'A' = 0
    checkLet 'B' = 10
    checkLet 'C' = 20
    checkLet 'D' = 30
    checkLet 'E' = 40
    checkLet 'F' = 50
    checkLet 'G' = 60
    checkLet 'H' = 70
    checkLet 'I' = 80
    checkLet 'J' = 90
    checkLet _ = error "Wrong input"

    checkNum:: Char -> Int 
    checkNum '0' = 1
    checkNum '1' = 2
    checkNum '2' = 3
    checkNum '3' = 4
    checkNum '4' = 5
    checkNum '5' = 6
    checkNum '6' = 7
    checkNum '7' = 8
    checkNum '8' = 9
    checkNum '9' = 10
    checkNum _ = error "Wrong input"

    checkUn:: Char -> Int
    checkUn 't' = 0
    checkUn 'u' = 100
    checkUn _ = error "Wrong input"

-- IMPLEMENT
-- Adds hint data to the game state
hint :: State -> Document -> State
hint (State b r c) d = State (funn (hintPosition (initializeHintList d "col" []) (initializeHintList d "row" [])) b) r c
  where
    fun1 x hh2 = fst $ (splitAt ((x)-1) hh2)
    fun2 x hh2 = [FilledBox] ++ (drop 1 (snd (splitAt ((x)-1) hh2)))
    funn [] hh1 = hh1
    funn (x:xs) hh1 = funn xs (fun1 x hh1 ++ fun2 x hh1)

-- Make a list of hints' positions from Document.
initializeHintList :: Document -> String -> [Int] -> [Int]
initializeHintList a b c = getIntData (tt(tt1(tt1(tt1(tt1(tt1(tt1(tt1(tt1 (tt1 (tt1 a b) b) b) b)b)b)b)b)b)b)b) $ getIntData (tt(tt1(tt1(tt1(tt1(tt1(tt1(tt1 (tt1 (tt1 a b) b) b) b)b)b)b)b)b)b)$ getIntData (tt(tt1(tt1(tt1(tt1(tt1(tt1 (tt1 (tt1 a b) b) b) b)b)b)b)b)b) $ getIntData (tt(tt1(tt1(tt1(tt1(tt1 (tt1 (tt1 a b) b) b) b)b)b)b)b) $ getIntData (tt(tt1(tt1(tt1(tt1 (tt1 (tt1 a b) b) b) b)b)b)b) $ getIntData (tt(tt1(tt1(tt1 (tt1 (tt1 a b) b) b) b)b)b) $ getIntData (tt(tt1(tt1 (tt1 (tt1 a b) b) b) b)b) $ getIntData (tt(tt1 (tt1 (tt1 a b) b) b) b) $ getIntData (tt (tt1 (tt1 a b) b) b) $ getIntData (tt (tt1 a b) b) $ getIntData (tt a b) c

-- Parse Document.
tt :: Document -> String -> Document
tt (DMap ((str, d):rem)) key
  | str == "coords" = tt d key
  | str == "head" = tt d key
  | str == "tail" = tt d key
  | str == key = d
tt (DMap (_:(str, d):_)) key
  | str == key = d
  | otherwise = error "Wrong key"
tt DNull _ = DNull
tt _ _ = error "Parse error"

-- Parse Document.
tt1 :: Document -> String -> Document
tt1 (DMap ((str, d):rem)) key
  | str == "head" = (DMap rem)
  | str == "tail" = tt1 d key
  | otherwise = tt1 d key
tt1 DNull _ = DNull
tt1 _ _ = error "Parse error"

-- Parse integer and add to the list.
getIntData :: Document -> [Int] -> [Int]
getIntData (DInteger x) acc = [x] ++ acc
getIntData DNull acc = acc
getIntData _ _ = []

-- Make hint positions on board from hint coords. Example: [1, 2, 5] [3, 4, 5] -> [32, 43, 56]
hintPosition :: [Int] -> [Int] -> [Int]
hintPosition c r = [x+1+y*10 | (x,y) <- zip c r]
