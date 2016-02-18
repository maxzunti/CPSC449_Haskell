module ApocStrategyGreedy where

--import System.Random
import Data.List
import ApocTools
import GameRules

-- Need to check move legality here
-- Gets input from user and return
greedyStrat    :: Chooser
greedyStrat (GameState bplay bpenalty wplay wpenalty board) Normal p = 
            if (p == White) 
            then do return (Just (if (length (getAllPieceCoor 0 WK board) == 0) 
                                  then getWPMove 0 0 Normal (GameState bplay bpenalty wplay wpenalty board) 
                                  else getWKMove 0 0 (GameState bplay bpenalty wplay wpenalty board) )) 
            else do return (Just (if (length (getAllPieceCoor 0 BK board) == 0) 
                                  then getBPMove 0 0 Normal (GameState bplay bpenalty wplay wpenalty board) 
                                  else getBKMove 0 0 (GameState bplay bpenalty wplay wpenalty board) ))

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
wpMoves :: (Int, Int) -> [(Int, Int)]
wpMoves (x, y) = [(x-1, y+1), (x+1, y+1), (x, y+1)] --diagonal left, diagonal right, forward

--Lists all the moves of a black pawn
bpMoves :: (Int, Int) -> [(Int, Int)]
bpMoves (x, y) = [(x-1, y-1), (x+1, y-1), (x, y-1)] --diagonal left, diagonal right, forward

--Lists all the moves of a knight
kMoves :: (Int, Int) -> [(Int, Int)]
kMoves (x, y) = [(x-1, y+2), (x+1, y+2), (x+1, y-2), (x-1, y-2), (x-2, y+1), (x+2, y+1), (x+2, y-1), (x-2, y-1)]

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
          wmList = wpMoves pos
          pos    = getNext n wpList
          mov    = getNext m wmList
          gs     = (GameState bplay bpenalty wplay wpenalty board)



--Gives the first valid movement of a white knight
getWKMove :: Int -> Int -> GameState -> [(Int, Int)] --knight index, possible moves Index, GameState
getWKMove n m (GameState bplay bpenalty wplay wpenalty board) =  
          if ((isValidMove (pos:mov:[]) gs Normal White) == VALID || (isValidMove (pos:mov:[]) gs Normal White) == CAPTURE)
          then [pos, mov]
          else if (m < length wmList)
               then getWKMove n (m+1) gs
               else if (n > length wkList)
               then []
               else getWKMove (n+1) 0 gs

    where wkList = getAllPieceCoor 0 WK board
          wmList = kMoves pos
          pos    = getNext n wkList
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
          bmList = bpMoves pos
          pos    = getNext n bpList
          mov    = getNext m bmList
          gs     = (GameState bplay bpenalty wplay wpenalty board)


--Gives the first valid movement of a black knight
getBKMove :: Int -> Int -> GameState -> [(Int, Int)] --knight index, possible moves Index, GameState
getBKMove n m (GameState bplay bpenalty wplay wpenalty board) = 
          if ((isValidMove (pos:mov:[]) gs Normal Black) == VALID || (isValidMove (pos:mov:[]) gs Normal Black) == CAPTURE)
          then [pos, mov]
          else if (m < length bmList)
               then getBKMove n (m+1) gs
               else if (n > length bkList)
               then []
               else getBKMove (n+1) 0 gs

    where bkList = getAllPieceCoor 0 BK board
          bmList = kMoves pos
          pos    = getNext n bkList
          mov    = getNext m bmList
          gs     = (GameState bplay bpenalty wplay wpenalty board)



testFunc :: [(Int, Int)]
--testFunc = getWPMove 0 0 Normal initBoard
testFunc = getWKMove 0 0 initBoard
