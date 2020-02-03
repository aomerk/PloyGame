{-# LANGUAGE DoAndIfThenElse #-}
module Main
where

-- Diese Datei ist nicht Teil der Abgabe, sondern
-- nur als Hilfe fuer das Erstellen eines ausfuehrbaren Bots

-- import PloyBot
import System.Environment
import Data.Char
import Util

import Data.Foldable
import Data.Array
import Data.Bits
import Data.List


--- external signatures (NICHT AENDERN!)
getMove :: String -> String


--- YOUR IMPLEMENTATION STARTS HERE ---




data Row = Row {rowIndex :: Int, columns :: String} deriving(Show)
data Column = Column { colIndex :: Int, element :: String, columnRowIndex :: Int } deriving (Show)

-- TODO push figures into a list
figureList :: [Figure]


data Figure = Figure {isWhite :: Bool, position :: Int, aimIndices :: [Int], maxDistance :: Int, isNull :: Bool, directions :: [Int] } deriving(Show)



stringToArray s = listArray (0, length s - 1) s

figureList = []

rowToLetterDictionary = ['a','b','c','d','e','f','g','h','i']

directionToDistanceDictionary = [-9, -8, 1, 10 ,9 ,8 , -1, -10]

-- -- TODO 9 rows from board string
boardToRows :: String -> [Row]
boardToRows boardStr = let rows = splitOn "/" boardStr in let indexedRows = zip [0..] rows in
 [Row  {rowIndex = i, columns = row}| (i,row) <- indexedRows]

-- TODO 9 elements from Row
rowToElements :: Row -> [Column]
rowToElements row = let cols = columns(row) in let everyElement = splitOn "," cols in let indexedColumns = zip [0..] everyElement in [ Column {colIndex = colI, element =  elem, columnRowIndex = rowIndex(row) } | (colI, elem) <- indexedColumns ]
--
enumerate = zip [0..]

-- TODO turn board into list of strings for elements
boardToElements :: String -> [Column]
boardToElements board = let rows = boardToRows board in let elems = concat([ rowToElements row  | row <- rows ]) in elems

figureOwner :: String -> (String, Bool, Bool)
figureOwner "" =  ("", False, True)
figureOwner str = let owner = ((str !! 0) == 'w') in let rest = drop 1 str in (rest,owner, False)

readFigureNum ::  String -> Int
readFigureNum "" = 0
readFigureNum rest =(if(rest == "" || rest == "," || rest == "/") then 0 else read rest :: Int)

figureMaxDistance :: Int -> Int
figureMaxDistance a =if(popCount a >= 4) then 1 else popCount a


setDirections :: Int-> Int -> [Int] -> [Int]
setDirections num 8 directions = directions
setDirections num n directions = if testBit num n
  then setDirections num (n+1) (directions ++ [n])
  else setDirections num (n+1) directions


-- -- TODO turn element string into a Figure Data
elemToFigure :: Column -> Figure -- create figures, but not their possible moves
elemToFigure col =
  let elem = element(col) :: String in
  let (rest,owner, nullcheck) = figureOwner (elem) in --
  let idx = (colIndex(col) + (columnRowIndex(col) * 9))  in  -- Figure position
  let figureNum = readFigureNum rest in -- e.g 84 part of w84 element
  let mDistance = figureMaxDistance figureNum in -- how far can it go? also tells which type of figure is it (Shield,Commander etc.)
  let dirs = setDirections figureNum 0 [] in
  Figure { -- create figure
  isWhite = owner, position = idx, aimIndices = [], directions = dirs,
  maxDistance = mDistance , isNull = nullcheck }



boardToFigureList :: [Column] -> [Figure]
boardToFigureList cols = [elemToFigure elem | elem <-cols]  -- figures set with positions and possible(!) moves



setAllFigureTargets :: [Figure] -> [Figure]
setAllFigureTargets figures = [ figure{aimIndices = setFigureTargets figures figure dist [] }  | figure <- figures, let dist = maxDistance(figure) ]


-- position , maxDistance
setFigureTargets :: [Figure] -> Figure -> Int -> [Int] -> [Int]
  -- setAimIndices position maxDistance directions = 3
setFigureTargets figures figure 0 aimIns = aimIns -- REKURSIONSANKER


setFigureTargets figures fig dist aimIns =
  let positionInd = position(fig) in -- figure position
  let dirs = directions(fig) in
  let removedOutOfBorderDirections = [dir | dir <- dirs, let targetIdx = positionInd + (dist * directionToDistanceDictionary!!dir), targetIdx >= 0 , targetIdx <= 80] in
  let removedSameColorDirections = [dir | dir <- removedOutOfBorderDirections, let targetIdx = positionInd + (dist * directionToDistanceDictionary!!dir), isNull(figures!!targetIdx) ] in
  let ways = directionToDistance (removedSameColorDirections) in -- which ways can it go? -10, -9 , +1 index etc..
  let allPossibleIndices = [ (targetIdx) |a <- ways, let targetIdx = positionInd + (dist * a) ] in -- how many tiles in that way => all possible indices
  let legalMoves = filterAims allPossibleIndices positionInd in -- legal in means of board rules
  setFigureTargets figures fig (dist - 1) (aimIns ++ legalMoves) -- add to possible aims and recurse


directionToDistance dirs = [directionToDistanceDictionary!!x | x <- dirs]
indexToString index =   let (col, row) = (index `mod` 9, index `div` 9 ) in [rowToLetterDictionary!!row] ++ show (9-col)


filterAims :: [Int] -> Int -> [Int]
filterAims aims current  =
  let (col, row) = (current `mod` 9, current `div` 9 ) in
  [aim |  aim<- aims , aim >= 0, aim<=80,
  let aimCol = aim `mod` 9 , let aimRow = (aim `div` 9) :: Int,
  abs (aimCol - col ) >= 0 , abs (aimRow - row) >= 0     ]



createOtherMoves :: Figure -> [String]
createOtherMoves figure = let aims = aimIndices(figure) in let from = indexToString (position(figure)) in let moves = [from ++ "-" ++ aimstr ++ "-" ++ "0" | aimIdx <- aims, let aimstr = indexToString aimIdx] in let withRotations = [move ++ show (i) | move <- moves, i <- [0..7]] in withRotations

-- createOtherMoves figure = let from = indexToString position(figure)
-- in let moves = [from ++ "-" ++ aimstr ++ "-" ++ "0" | aimIdx <- aimIndices(figure), let aimstr = indexToString aimIdx ]
-- in  [from ++ "-" ++ from ++ "-" ++ show (i)  | i <- [0..7]]

createShieldMoves figure = let aims = aimIndices(figure) in let from = indexToString (position(figure)) in let moves = [from ++ "-" ++ aimstr ++ "-" ++ "0" | aimIdx <- aims, let aimstr = indexToString aimIdx] in let withRotations = [move ++ show (i) | move <- moves, i <- [0..7]] in withRotations

createMoves :: Figure -> [String]
createMoves figure = if length (directions(figure)) == 1 then createShieldMoves figure else createOtherMoves figure

aimsToMoves figures =  [createMoves figure | figure <- figures]

-- getMove str = generateMoveList str !! 0
getMove str = str

charToString :: Char -> String
charToString c = [c]

generateMoveList :: String -> Bool -> [String]
generateMoveList board whitePlays =
  let elements = boardToElements board in
  let parsedFigures = boardToFigureList elements in
  let a = [figure |  figure <- parsedFigures, isNull(figure) == False] in
  let allfig = setAllFigureTargets a in
  let legalFigures = [figure |figure <- allfig, let whiteFigure = isWhite(figure), whiteFigure == whitePlays] in
  concat [createMoves figure | figure <- legalFigures]

-- listMoves :: String -> String
-- listMoves input = let arr = generateMoveList input in concat (intersperse ","  arr)
listMoves input =
  let parseInput = splitOn " " input in
  let isWhite = (last parseInput) == "w" in --split get last element of array
  let arr = generateMoveList (head parseInput) isWhite  in
  arr
  -- concat (intersperse "," arr)

-- Wer hier mehr erfahren will: Im naechsten Schritt (nicht Teil des Stoffs) kann in Haskell
-- mit Monaden impliziter ein Zustand definiert und in sequentieller Ausfuehrung mitgefuehrt werden
-- Dadurch wird auch Interaktion mit der Umgebung moeglich, wie im Beispiel der IO-Monade.
main :: IO ()
main = do
  args <- getArgs
  let oneString = foldr (\x y -> if y == "" then x else x ++ " " ++ y) "" args
  -- print(((boardToElements (oneString))))
  -- print(generateMoveList oneString True)
  -- print(setDirections 84 0 [])
  print( head $splitOn " " oneString)
  print (listMoves oneString)

  -- putStrLn ( getMove oneString )
