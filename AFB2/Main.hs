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


--- external signatures (NICHT AENDERN!)
getMove :: String -> String
listMoves :: String -> String


--- YOUR IMPLEMENTATION STARTS HERE ---




data Row = Row {rowIndex :: Int, columns :: String} deriving(Show)
data Column = Column { colIndex :: Int, element :: String, columnRowIndex :: Int } deriving (Show)

-- TODO push figures into a list
figureList :: [Figure]


data Figure = Figure {isWhite :: Bool, position :: Int, aimIndices :: [Int], maxDistance :: Int, isNull :: Bool, directions :: [Int] } deriving(Show)



stringToArray s = listArray (0, length s - 1) s

figureList = []



--
-- -- TODO 9 rows from board string
boardToRows :: String -> [Row]
boardToRows boardStr = let rows = splitOn "/" boardStr in let indexedRows = zip [0..] rows in
 [Row  {rowIndex = i, columns = row}| (i,row) <- indexedRows]
--
-- -- TODO 9 elements from Row
rowToElements :: Row -> [Column]
rowToElements row = let cols = columns(row) in let everyElement = splitOn "," cols in let indexedColumns = zip [0..] everyElement in [ Column {colIndex = colI, element =  elem, columnRowIndex = rowIndex(row) } | (colI, elem) <- indexedColumns ]
--
enumerate = zip [0..]




-- -- TODO turn board into list of strings for elements
boardToElements :: String -> [Column]

boardToElements board = let rows = boardToRows board in let elems = concat([ rowToElements row  | row <- rows ]) in elems

boardToFigureList cols = [elemToFigure elem | elem <-cols]
--

figuresToField figures = [isWhite(elem) | elem <- figures]
--

directionToDistanceDictionary = [-9, -8, 1, 10 ,9 ,8 , -1, -10]

-- -- TODO turn element string into a Figure Data
elemToFigure :: Column -> Figure
elemToFigure col =
  let elem = element(col) :: String in
  let (rest,owner, nullcheck) = figureOwner (elem) in
  let idx = (colIndex(col) + (columnRowIndex(col) * 9))  in
  let figureNum = readFigureNum rest in
  Figure {
  isWhite = owner, position = idx, aimIndices = [], directions = setDirections figureNum 0 [],
  maxDistance = figureMaxDistance figureNum, isNull = nullcheck   }


figureOwner :: String -> (String, Bool, Bool)
figureOwner "" =  ("", False, True)
figureOwner str = let owner = ((str !! 0) == 'w') in let rest = drop 1 str in (rest,owner, False)


readFigureNum ::  String -> Int
readFigureNum "" = 0
readFigureNum rest =(if(rest == "" || rest == "," || rest == "/") then 0 else read rest :: Int)


figureMaxDistance :: Int -> Int
figureMaxDistance a =if(popCount a >= 4) then 1 else popCount a

-- setDirections num 0 [] = setDirections (num, , [])

-- input ===> 84 part of w84, empty array
-- returns ====> array of possible directions
setDirections :: Int-> Int -> [Int] -> [Int]

setDirections num 8 directions = directions
setDirections num n directions = if testBit num n
  then setDirections num (n+1) (directions ++ [n])
  else setDirections num (n+1) directions


-- position , maxDistance
setAimIndices :: Figure -> Int -> [Int] -> [Int]
  -- setAimIndices position maxDistance directions = 3
setAimIndices figure 0 aimIns = aimIns

setAimIndices fig dist aimIns =
  let positionInd = position(fig) in
  let ins = [positionInd + (dist * a) |a <- directionToDistance (directions( fig))] in
  let insideBoard = [x | x <- ins , x >= 0 , x <=80 ] in
  let (col, row) = (positionInd `mod` 9, positionInd `div` 9 ) in
  let aims = [aim |aim <- insideBoard, let aimCol = aim `mod` 9 , let aimRow = (aim `div` 9) :: Int,  abs (aimCol - col ) >= 0 , abs (aimRow - row) >= 0  ] in
  setAimIndices fig (dist - 1) (aimIns ++ aims)


directionToDistance dirs = [directionToDistanceDictionary!!x | x <- dirs]

filterAims aims current  =
  let (col, row) = (current `mod` 9, current `div` 9 ) in
  [aim |  aim<- aims , aim >= 0, aim<=80,
  let aimCol = aim `mod` 9 , let aimRow = (aim `div` 9) :: Int,
  abs (aimCol - col ) >= 0 , abs (aimRow - row) >= 0     ]
--
-- -- TODO board string to figures list
-- boardToFigureList :: String -> [String]
-- boardToFigureList board = let elementsArray = boardToElements board in [elemToFigure show elem | elem <- elementsArray]
--
--
--
--
-- -- boardToFigureList positionList = splitOn "," positionList

getMove str = "getMove"

listMoves lst = lst

-- Wer hier mehr erfahren will: Im naechsten Schritt (nicht Teil des Stoffs) kann in Haskell
-- mit Monaden impliziter ein Zustand definiert und in sequentieller Ausfuehrung mitgefuehrt werden
-- Dadurch wird auch Interaktion mit der Umgebung moeglich, wie im Beispiel der IO-Monade.
main :: IO ()
main = do
  args <- getArgs
  let oneString = foldr (\x y -> if y == "" then x else x ++ " " ++ y) "" args
  -- print((boardToElements (oneString) ) )
  -- print(setDirections 84 0 [])
  print((boardToFigureList (boardToElements (oneString) ) ) )

  putStrLn ( getMove oneString )
