{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Lib3(hint, gameStart, parseDocument, GameStart, Hint) where

import Types ( Document(..), FromDocument(..), fromDocument )
import Lib1 (State(..), BoxType(..))
import Data.Char ( isDigit )
import Data.List ( elemIndices )


-- IMPLEMENT
-- Parses a document from yaml
parseDocument :: String -> Either String Document
parseDocument str = case dropP str of
    Left e -> Left e
    Right a -> Right $ fst a

dropP :: String -> Either String (Document, String)
dropP str = case parse (drop 4 str) of
    Left a -> Left a
    Right b -> Right (fst b, drop (length (snd b)) (snd b))


parse :: String -> Either String (Document, String)
parse str = or3 (dlistP str) (dmapP str) (parsePr str) 

parsePr :: String -> Either String (Document, String)
parsePr str = case stringP str of
    Left s -> Left s
    Right a -> case fst a of
        "null" -> Right (DNull, drop 4 str)
        _ -> case integerP (fst a) of
                Left _ -> case stringP str of
                    Left s -> Left s
                    Right c -> Right (DString (fst c), drop (length (fst c)) str)
                Right b -> Right (DInteger (fst b), drop (length (fst a)) str)

dmapP :: String -> Either String (Document, String)
dmapP str = 
    let 
        x = stringP str
    in
        if elem ':' str == True then
            case x of
                Left s -> Left s
                Right a -> case parse (drop (length ((takeWhile (/=':') (fst a))++": ")) (fst a)) of
                    Left p -> Left p
                    Right b -> Right (DMap [((takeWhile (/=':') (fst a)), (fst b))], drop (length (fst a)) str)

        else Left "Not a DMap"

optional :: String -> (String -> Either String (a, String)) -> Either String (Maybe a, String)
optional str parser =
    case parser str of
        Left _ -> Right (Nothing, str)
        Right (i, r) -> Right (Just i, r)


charP :: Char -> String -> Either String (Char, String)
charP ch [] = Left $ "Empty input: '" ++ [ch] ++ "' expected"
charP ch (x:xs) | ch == x = Right (x, xs)
                | otherwise = Left $ ch :" expected"


integerP :: String -> Either String (Int, String)
integerP str =
    let 
        prefix = takeWhile isDigit str
    in
        case prefix of
            [] -> Left "Empty integer"
            _ -> Right (read prefix, drop (length prefix) str)

dintegerP :: String -> Either String (Document, String)
dintegerP str = case integerP str of
    Left e -> Left e
    Right s -> Right (DInteger (fst s), snd s)

dstringP :: String -> Either String (Document, String)
dstringP str = case stringP str of
    Left e -> Left e
    Right s -> Right (DString (fst s), snd s)


stringP :: String -> Either String (String, String)
stringP [] = Right ("","")
stringP str = Right (takeWhile (/='\n') str, drop (length(takeWhile (/='\n') str)+1) str)
stringP _ = Left "Bad string"

many1 :: String -> (String -> Either String (a, String)) -> Either String ([a], String)
many1 str parser = many1' str []
    where
        many1' s [] =
             case parser s of
                Left e -> Left e
                Right (i, r) -> many1' r [i]
        many1' s acc =
            case parser s of
                Left _ -> Right (reverse acc, s)
                Right (i, r) -> many1' r (i:acc)


or2 :: Either String (a, String) -> Either String (a, String) -> Either String (a, String)
or2 parser1 parser2 = case parser1 of
        Right a -> Right a
        Left _ -> parser2


or3 :: Either String (a, String) -> Either String (a, String) -> Either String (a, String) -> Either String (a, String)
or3 parser1 parser2 parser3 = case parser1 of
        Right a -> Right a
        Left _ -> case parser2 of
            Right b -> Right b
            Left _ -> parser3

dlistP :: String -> Either String (Document, String)
dlistP str = do
    (_, r1) <- charP '-' str
    (_, r2) <- charP ' ' r1
    (l, r3) <- optional r2 elems
    case l of
        Just l' -> return (DList l', r3)
        Nothing -> return (DList [], r3)

elems :: String -> Either String ([Document], String)
elems str = do
    (i, r) <- or2 (lenGt2List str) (len1List str)
    return (i, r)

len1List :: String -> Either String ([Document], String)
len1List str = do
    (i, r) <- parsePr str
    return ([i], r)

lenGt2List :: String -> Either String ([Document], String)
lenGt2List str = do
    (i1, r1) <- parsePr str
    (i2, r2) <- many1 r1 fromSecond
    return (i1:i2, r2)

fromSecond :: String -> Either String (Document, String)
fromSecond str = do
    (_, r1) <- charP '\n' str
    (_, r2) <- charP '-' r1
    (_, r3) <- charP ' ' r2
    (i, r4) <- parsePr r3
    return (i, r4)










-- IMPLEMENT
-- Change right hand side as you wish
-- You will have to create an instance of FromDocument
data GameStart = GameStart State
    deriving (Show, Eq)

instance FromDocument GameStart where
    fromDocument a = case gameStart1 emptyState a of
        Left b -> Left b
        Right r -> Right $ GameStart r
    fromDocument _ = Left "Wrong GameStart FromDocument"

-- This adds game data to initial state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
gameStart :: State -> GameStart -> State
gameStart (State b _ _) (GameStart (State _ r c)) = (State b r c)

gameStart1 :: State -> Document -> Either String State
gameStart1 (State board' _ _) doc = do 
    oRows <- fillList doc "occupied_rows" 
    oCols <- fillList doc "occupied_cols" 
    Right  (State board' oRows oCols)
gameStart1 _ _ = Left "Error: Wrong parameters "


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

emptyState :: State
emptyState = State (replicate 100 EmptyBox) [] []



-- IMPLEMENT
-- Change right hand side as you wish
-- You will have to create an instance of FromDocument
data Hint = Hint State
    deriving (Show, Eq)

instance FromDocument Hint where
    fromDocument a = case hint1 emptyState a of
        Left b -> Left b
        Right r -> Right $ Hint r
    fromDocument _ = Left "Wrong Hint FromDocument"

-- Adds hint data to the game state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
hint :: State -> Hint -> State
hint (State b1 r c) (Hint (State b _ _)) = State (funn ((elemIndices FilledBox b)) b1) r c
  where
    fun1 x hh2 = fst $ (splitAt ((x)) hh2)
    fun2 x hh2 = [FilledBox] ++ (drop 1 (snd (splitAt ((x)) hh2)))
    funn [] hh1 = hh1
    funn (x:xs) hh1 = funn xs (fun1 x hh1 ++ fun2 x hh1)-- there is a bug: after hint every guess dissapears


hint1 :: State -> Document -> Either String State
hint1 (State b r c) d = Right $ State (funn (hintPosition (initcollist d) (initrowlist d)) b) r c
  where
    fun1 x hh2 = fst $ (splitAt ((x)-1) hh2)
    fun2 x hh2 = [FilledBox] ++ (drop 1 (snd (splitAt ((x)-1) hh2)))
    funn [] hh1 = hh1
    funn (x:xs) hh1 = funn xs (fun1 x hh1 ++ fun2 x hh1)
hint1 _ _ = Left "Error: Wrong data"

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
