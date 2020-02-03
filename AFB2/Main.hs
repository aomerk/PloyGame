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


--- external signatures (NICHT AENDERN!)
getMove :: String -> String
listMoves :: String -> String


--- YOUR IMPLEMENTATION STARTS HERE ---




data Row = Row {rowIndex :: Int, columns :: String} deriving(Show)
data Column = Column { colIndex :: Int, element :: String, columnRowIndex :: Int } deriving (Show)

-- TODO push figures into a list
figureList :: [Figure]


data Figure = Figure {isWhite :: Bool, position :: Int, aimIndices :: [Int]} deriving(Show)

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
boardToElements :: String -> [Figure]

boardToElements board = let rows = boardToRows board in let elems = concat([ rowToElements row  | row <- rows ]) in  [elemToFigure elem | elem <-elems]

--
--

-- -- TODO turn element string into a Figure Data
elemToFigure :: Column -> Figure
elemToFigure elem = Figure {isWhite = True, position = (colIndex(elem) + (columnRowIndex(elem) * 9)), aimIndices = [] }

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
  print((boardToElements (oneString) ) )
  putStrLn ( getMove oneString )
