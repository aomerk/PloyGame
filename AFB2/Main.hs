{-# LANGUAGE DoAndIfThenElse #-}
module Main
where

-- Diese Datei ist nicht Teil der Abgabe, sondern
-- nur als Hilfe fuer das Erstellen eines ausfuehrbaren Bots

import PloyBot
import System.Environment
import Data.Char
import Util

-- Wer hier mehr erfahren will: Im naechsten Schritt (nicht Teil des Stoffs) kann in Haskell
-- mit Monaden impliziter ein Zustand definiert und in sequentieller Ausfuehrung mitgefuehrt werden
-- Dadurch wird auch Interaktion mit der Umgebung moeglich, wie im Beispiel der IO-Monade.
main :: IO ()
main = do
  args <- getArgs
  let oneString = foldr (\x y -> if y == "" then x else x ++ " " ++ y) "" args
  -- print(parseInput (oneString))
  -- print(((boardToElements oneString)))
  -- print(generateMoveList oneString)
  -- print(setDirections 84 0 [])
  -- print( head $splitOn " " oneString)
  print (listMoves oneString)

  -- putStrLn ( getMove oneString )
