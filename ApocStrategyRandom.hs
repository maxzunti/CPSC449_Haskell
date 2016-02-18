module ApocStrategyRandom where

--import System.Random
import Data.List
import ApocTools
import GameRules

-- Need to check move legality here
-- Gets input from user and return
randomStrat    :: Chooser
randomStrat g Normal        p = if (p == White) then do return (Just (getWPMove 0 0 Normal g)) else do return (Just (getBPMove 0 0 Normal g))
randomStrat g PawnPlacement p = if (p == White) then do return (Just (getWPMove 0 0 PawnPlacement g)) else do return (Just (getBPMove 0 0 PawnPlacement g))

--get coodinates for all specified pieces INT HAS TO BE 0!!!
getAllPieceCoor :: Int -> Cell -> Board -> [(Int, Int)]
getAllPieceCoor y pi [] = []
getAllPieceCoor y pi (x:xs) = (getAllX y pi x) ++ (getAllPieceCoor (y+1) pi xs)

--Get coordinates of all specified pieces
getAllX :: Int -> Cell -> [Cell] -> [(Int,Int)]
getAllX y pi bo = mapCoorALLX y (elemIndices pi bo)

--Maps a y value and a list of x values to the tuple that is suppose to represent a move
mapCoorALLX :: Int -> [Int] -> [(Int, Int)]
mapCoorALLX y [] = []
mapCoorALLX y (x:xs) = (x,y):mapCoorALLX y xs

--Lists all the moves of a white pawn
wMoves :: (Int, Int) -> [(Int, Int)]
wMoves (x, y) = [(x-1, y+1), (x+1, y+1), (x, y+1)] --diagonal left, diagonal right, forward

--Lists all the moves of a black pawn
bMoves :: (Int, Int) -> [(Int, Int)]
bMoves (x, y) = [(x-1, y-1), (x+1, y-1), (x, y-1)] --diagonal left, diagonal right, forward

--Gets element at provided index
getNext :: Int -> [(Int, Int)] -> (Int, Int)
getNext _ [] = (-1, -1)
getNext 0 list = head list
getNext i (x:xs) = getNext (i-1) xs

--Gives the first valid movement of a white pawn
getWPMove :: Int -> Int -> PlayType -> GameState -> [(Int, Int)] --pawn index, possible moves Index, GameState
getWPMove n m pt (GameState bplay bpenalty wplay wpenalty board) =  
          if ((isValidMove (pos:mov:[]) gs pt White) == VALID || (isValidMove (pos:mov:[]) gs pt White) == CAPTURE)
          then if (pt == Normal)then [pos, mov] else [mov]
          else if (m < length wmList)
               then getWPMove n (m+1) pt gs
               else if (n > length wpList)
               then []
               else getWPMove (n+1) 0 pt gs

    where wpList = getAllPieceCoor 0 WP board
          wmList = wMoves pos
          pos    = getNext n wpList
          mov    = getNext m wmList
          gs     = (GameState bplay bpenalty wplay wpenalty board)


--Gives the first valid movement of a black pawn
getBPMove :: Int -> Int -> PlayType -> GameState -> [(Int, Int)] --pawn index, possible moves Index, GameState
getBPMove n m pt (GameState bplay bpenalty wplay wpenalty board) =  
          if ((isValidMove (pos:mov:[]) gs pt Black) == VALID || (isValidMove (pos:mov:[]) gs pt Black) == CAPTURE)
          then if (pt == Normal)then [pos, mov] else [mov]
          else if (m < length bmList)
               then getBPMove n (m+1) pt gs
               else if (n > length bpList)
               then []
               else getBPMove (n+1) 0 pt gs

    where bpList = getAllPieceCoor 0 BP board
          bmList = bMoves pos
          pos    = getNext n bpList
          mov    = getNext m bmList
          gs     = (GameState bplay bpenalty wplay wpenalty board)


--test function
testFunc :: [(Int, Int)]
testFunc = getWPMove 0 0 Normal initBoard
