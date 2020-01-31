{-# LANGUAGE DoAndIfThenElse #-}
module Main
where

-- Diese Datei ist nicht Teil der Abgabe, sondern
-- nur als Hilfe fuer das Erstellen eines ausfuehrbaren Bots

-- import PloyBot
import System.Environment
import Data.Char
import Util


--- external signatures (NICHT AENDERN!)
getMove :: String -> String
listMoves :: String -> String


--- YOUR IMPLEMENTATION STARTS HERE ---


-- TODO 9 rows from board string
boardToRows :: String -> [String]

-- TODO 9 elements from Row
rowToColumns :: String -> [String]

-- TODO turn element string into a Figure Data
elemToFigure :: String -> Figure

-- TODO push figures into a list
figureArray :: [Figure]

boardToFigureList :: String -> [String]

positionList :: String

positionList = []



boardToFigureList positionList = splitOn "," positionList

getMove str = (boardToFigureList str) !! 1

listMoves lst = lst


-- Wer hier mehr erfahren will: Im naechsten Schritt (nicht Teil des Stoffs) kann in Haskell
-- mit Monaden impliziter ein Zustand definiert und in sequentieller Ausfuehrung mitgefuehrt werden
-- Dadurch wird auch Interaktion mit der Umgebung moeglich, wie im Beispiel der IO-Monade.
main :: IO ()
main = do
  args <- getArgs
  let oneString = foldr (\x y -> if y == "" then x else x ++ " " ++ y) "" args
  putStrLn ( getMove oneString )
