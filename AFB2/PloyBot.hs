--- module (NICHT AENDERN!)
module PloyBot where

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
-- setAllFigureTargets figures = [figure{aimIndices =  } | figure <- figures, let dist = maxDistance(figure)]
setAllFigureTargets figures = [ figure{aimIndices = setFigureTargets dirs figures figure 0 [] }  | figure <- figures, let dist = maxDistance(figure), let dirs = directions(figure) ]


-- position , maxDistance
-- setFigureTargets :: [Figure] -> Figure -> Int -> [Int] -> [Int]
  -- setAimIndices position maxDistance directions = 3
setFigureTargets dirs figures figure 99 aimIns = aimIns -- REKURSIONSANKER


setFigureTargets dirs figures fig dist aimIns =
  let maxDist = maxDistance(fig) in

  if dist == maxDist then setFigureTargets dirs figures fig 99 aimIns else
  if isNull(fig) then setFigureTargets dirs figures fig 99 [] else
  let positionInd = position(fig) in -- figure position
  let removedOutOfBorderDirections = [dir | dir <- dirs, let targetIdx = positionInd + (dist * directionToDistanceDictionary!!dir), targetIdx >= 0 && targetIdx <= 80] in
  let removedSameColorDirections = [dir | dir <- removedOutOfBorderDirections, let targetIdx = positionInd + (dist * directionToDistanceDictionary!!dir),  (isNull(figures!!targetIdx)) ] in
  -- let ways = directionToDistance (removedSameColorDirections) in -- which ways can it go? -10, -9 , +1 index etc..
  let allPossibleIndices = [ (targetIdx) |dir <- removedSameColorDirections, let targetIdx = positionInd + (dist * directionToDistanceDictionary!!dir)] in -- how many tiles in that way => all possible indices
  let legalMoves = filterAims allPossibleIndices positionInd in -- legal in means of board rules
  setFigureTargets removedSameColorDirections figures fig (dist + 1) (aimIns ++ legalMoves) -- add to possible aims and recurse


directionToDistance dirs = [directionToDistanceDictionary!!x | x <- dirs]
indexToString index =   let (col, row) = (index `mod` 9, index `div` 9 ) in [rowToLetterDictionary!!col] ++ show (9-row)


filterAims :: [Int] -> Int -> [Int]
filterAims aims current  =
  let (col, row) = (current `mod` 9, current `div` 9 ) in
  [aim |  aim<- aims , aim >= 0 && aim<=80,
  let aimCol = aim `mod` 9 , let aimRow = (aim `div` 9) :: Int,
  abs (aimCol - col ) >= 0 , abs (aimRow - row) >= 0     ]


filterAim :: Int -> Int -> Bool
filterAim aim current  =
  let (col, row) = (current `mod` 9, current `div` 9 ) in
  let aimCol = aim `mod` 9 in
  let aimRow = (aim `div` 9) in
  aim >= 0 && aim<=80  && abs (aimCol - col ) >= 0 && abs (aimRow - row) >= 0





createOtherMoves :: Figure -> [String]
-- createOtherMoves figure = let aims = aimIndices(figure) in let from = indexToString (position(figure)) in let moves = [from ++ "-" ++ aimstr ++ "-" ++ "0" | aimIdx <- aims, let aimstr = indexToString aimIdx] in let withRotations = [move ++ show (i) | move <- moves, i <- [0..7]] in withRotations

createOtherMoves figure =
  let from = indexToString (position(figure)) in
  let aims = aimIndices(figure) in
  let moves = [from ++ "-" ++ aimstr ++ "-" ++ "0" | aimIdx <- aims, let aimstr = indexToString aimIdx ] in
  moves ++ [from ++ "-" ++ from ++ "-" ++ show (i)  | i <- [1..7]]

createShieldMoves figure =
  let from = indexToString (position(figure)) in
  let aims = aimIndices(figure) ++ [position(figure)] in

  let moves = [from ++ "-" ++ aimstr ++ "-" | aimIdx <- aims, let aimstr = indexToString aimIdx] in
  let withRotations = [move ++ show (i) | move <- moves, i <- [0..7]] in withRotations ++ [from ++ "-" ++ from ++ "-" ++ show(i) | i <- [1..7]]

createMoves :: Figure -> [String]
createMoves figure  = if isNull(figure)
  then []
  else
    if length (directions(figure)) == 1 then createShieldMoves figure else createOtherMoves figure


aimsToMoves figures =  [createMoves figure | figure <- figures]


charToString :: Char -> String
charToString c = [c]

generateMoveList :: String -> [String]
generateMoveList input =
  let (parsedFigures, whitePlays) = parseInput input in
  -- let a = [figure |  figure <- parsedFigures, isNull(figure) == False] in
  let allfig = setAllFigureTargets parsedFigures in
  -- let legalFigures = [figure |figure <- allfig, let whiteFigure = isWhite(figure), whiteFigure == whitePlays] in
  concat [createMoves figure | figure <- allfig, isWhite(figure) == whitePlays]


parseInput :: String -> ([Figure], Bool)
parseInput input =
  let parsedInput = splitOn " " input in
  let isWhite = (last parsedInput) == "w" in --split get last element of array
  let elements = boardToElements (head parsedInput) in
  let parsedFigures = boardToFigureList elements in
  (parsedFigures, isWhite)


getMove str = generateMoveList str !! 0
-- getMove str = "a9-a8-0"

-- listMoves :: String -> String
-- listMoves input = let arr = generateMoveList input in concat (intersperse ","  arr)
listMoves input =
  -- let (figures, isWhite) = parseInput input in
  let arr = generateMoveList (input)  in
  let moves = concat (intersperse "," arr) in
  ("[" ++ moves ++ "]")
