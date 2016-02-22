module ApocStrategyKnightmare where

-- | Import System.Random
import Data.List
import ApocTools
import GameRules
import ApocStrategySimple

-- | Need to check move legality here
-- | Gets input from user and return
knightStrat    :: Chooser
knightStrat (GameState bplay bpenalty wplay wpenalty board) Normal p =
            if (p == White)
            then do return (Just (if (length (getAllPieceCoor 0 WK board) == 0)
                                  then getWPMove 0 0 Normal (GameState bplay bpenalty wplay wpenalty board)
                                  else getWKMove 0 0 (GameState bplay bpenalty wplay wpenalty board) ))
            else do return (Just (if (length (getAllPieceCoor 0 BK board) == 0)
                                  then getBPMove 0 0 Normal (GameState bplay bpenalty wplay wpenalty board)
                                  else getBKMove 0 0 (GameState bplay bpenalty wplay wpenalty board) ))


-- | Lists all the moves of a white pawn
wpMoves :: (Int, Int) -> [(Int, Int)]
wpMoves (x, y) = [(x-1, y+1), (x+1, y+1), (x, y+1)] --diagonal left, diagonal right, forward

-- | Lists all the moves of a black pawn
bpMoves :: (Int, Int) -> [(Int, Int)]
bpMoves (x, y) = [(x-1, y-1), (x+1, y-1), (x, y-1)] --diagonal left, diagonal right, forward

-- | Lists all the moves of a knight
kMoves :: (Int, Int) -> [(Int, Int)]
kMoves (x, y) = [(x-1, y+2), (x+1, y+2), (x+1, y-2), (x-1, y-2), (x-2, y+1), (x+2, y+1), (x+2, y-1), (x-2, y-1)]

-- | Gives the first valid movement of a white knight
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


-- | Gives the first valid movement of a black knight
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


--  | test the strat knightmare 
testFunc :: [(Int, Int)]
--testFunc = getWPMove 0 0 Normal initBoard
testFunc = getWKMove 0 0 initBoard
