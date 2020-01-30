module PloyFormat where

import System.IO
import Util
import Data.List
import Control.Monad
import Test.HUnit

import PloyBot

isRow r = r `elem` ['1'..'9']
isCol c = c `elem` ['a'..'i']
isDigit c = c `elem` ['0'..'7']

formatMove (a:b:c:d:e:f:g:[])
    | (isCol a) && (isRow b) && c == '-' && (isCol d) && (isRow e) && f == '-' && (isDigit g) = True
formatMove _ = False

formatList s
    | (head s == '[') && (last s == ']') = foldr (\ sm y -> y && (formatMove sm)) True (Util.splitOn "," (init (tail s)) )
formatList _ = False

assertFormat :: String -> (String -> Bool) -> Assertion
assertFormat actual check =
    unless (check actual) (assertFailure msg)
    where msg = "Wrong format! Looks like: \"" ++ actual ++ "\""

--------------------------------------------------------------------------

format = TestList [  (TestLabel "MOVE FORMAT WRONG!" (TestCase (assertFormat (PloyBot.getMove ",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69, b") formatMove))),
                     (TestLabel "LIST FORMAT WRONG!" (TestCase (assertFormat (PloyBot.listMoves ",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69, w") formatList))) ]

main :: IO (Counts, Int)
main =  runTestText (putTextToHandle stdout False) format
