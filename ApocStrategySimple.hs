module ApocStrategySimple where

-- | import System.Random
import Data.List
import ApocTools
import GameRules
import System.Random



-- | Need to check move legality here
-- | Chooses which piece to move
-- | Gets input from user and return
simpleStrat    :: Chooser
simpleStrat (GameState bplay bpenalty wplay wpenalty board) Normal p =
            if (p == White)
            then do 
                                  gen <- newStdGen
                                  return (Just (if ((length (getAllPieceCoor 0 WP board)) == 0)
                                                then getWKMove gen 0 0 (GameState bplay bpenalty wplay wpenalty board)
                                                else if ( (length (getAllPieceCoor 0 WP board) > 0 ) && (getWPMove gen 0 0 Normal (GameState bplay bpenalty wplay wpenalty board) == []) )
                                                     then getWKMove gen 0 0 (GameState bplay bpenalty wplay wpenalty board)
                                                     else getWPMove gen 0 0 Normal (GameState bplay bpenalty wplay wpenalty board)))
            else do 
                                  gen <- newStdGen
                                  return (Just (if ((length (getAllPieceCoor 0 BP board)) == 0)
                                                then getBKMove gen 0 0 (GameState bplay bpenalty wplay wpenalty board)
                                                else if ( (length (getAllPieceCoor 0 WP board) > 0 ) && (getBPMove gen 0 0 Normal (GameState bplay bpenalty wplay wpenalty board) == []) )
                                                     then getBKMove gen 0 0 (GameState bplay bpenalty wplay wpenalty board)
                                                     else getBPMove gen 0 0 Normal (GameState bplay bpenalty wplay wpenalty board)))
                                  
simpleStrat g PawnPlacement p = if (p == White) then do 
                                                        gen <- newStdGen
                                                        return (Just (filterWhiteEmpty gen (getEmptyCells g))) 
                                                        else do
                                                        gen <- newStdGen
                                                        return (Just (filterBlackEmpty gen (getEmptyCells g)))


-- | Checks if cell is empty
isEmptyCell :: (Int, Int) -> Board -> Bool
isEmptyCell x board = if ((getFromBoard board x) == E )
                      then True
                      else False

-- | Gets an empty cell
getEmptyCells :: GameState -> [(Int, Int)]
getEmptyCells (GameState _ _ _ _ board) = (getAllPieceCoor 0 E board)

-- | Filters the empty cells on the bottom row
filterWhiteEmpty :: StdGen -> [(Int, Int)] -> [(Int, Int)]
filterWhiteEmpty gen list = randPerm gen (filter func list)
    where func :: (Int, Int) -> Bool
          func (x, y) = if (y == 4) then False else True

-- | Filters the empty cells on the top row
filterBlackEmpty :: StdGen -> [(Int, Int)] -> [(Int, Int)]
filterBlackEmpty gen list = randPerm gen (filter func list)
    where func :: (Int, Int) -> Bool
          func (x, y) = if (y == 0) then False else True

-- | Shuffles the elements of a list into a random order
randPerm :: StdGen -> [a] -> [a]
randPerm _ [] = []
randPerm gen xs = let (n, newGem) =randomR (0, length xs -1) gen
                      front = xs !! n
                  in front : randPerm newGem (take n xs ++ drop (n+1) xs)


--------Helper functions to deal with the board----------------------------



-- | get coodinates for all specified pieces INT HAS TO BE 0!!!
getAllPieceCoor :: Int -> Cell -> Board -> [(Int, Int)]
getAllPieceCoor y pi [] = []
getAllPieceCoor y pi (x:xs) = (getAllX y pi x) ++ (getAllPieceCoor (y+1) pi xs)

-- | Get coordinates of all specified pieces
getAllX :: Int -> Cell -> [Cell] -> [(Int,Int)]
getAllX y pi bo = mapCoorALLX y (elemIndices pi bo)

-- | Maps a y value and a list of x values to the tuple that is suppose to represent a move
mapCoorALLX :: Int -> [Int] -> [(Int, Int)]
mapCoorALLX y [] = []
mapCoorALLX y (x:xs) = (x,y):mapCoorALLX y xs

-- | Lists all the moves of a white pawn
wMoves :: (Int, Int) -> [(Int, Int)]
wMoves (x, y) = [(x-1, y+1), (x+1, y+1), (x, y+1)] --diagonal left, diagonal right, forward

-- | Lists all the moves of a black pawn
bMoves :: (Int, Int) -> [(Int, Int)]
bMoves (x, y) = [(x-1, y-1), (x+1, y-1), (x, y-1)] --diagonal left, diagonal right, forward

-- | Gets element at provided index
getNext :: Int -> [(Int, Int)] -> (Int, Int)
getNext _ [] = (-1, -1)
getNext 0 list = head list
getNext i (x:xs) = getNext (i-1) xs



------Code for moving the Pawns-----------------------------



-- | Gives the first valid movement of a white pawn
getWPMove :: StdGen -> Int -> Int -> PlayType -> GameState -> [(Int, Int)] --pawn index, possible moves Index, GameState
getWPMove gen n m pt (GameState bplay bpenalty wplay wpenalty board) =
          if ((isValidMove (pos:mov:[]) gs pt White) == VALID || (isValidMove (pos:mov:[]) gs pt White) == CAPTURE)
          then if (pt == Normal)then [pos, mov] else [mov]
          else if (m < length wmList)
               then getWPMove gen n (m+1) pt gs
               else if (n >= length wpList)
               then []
               else getWPMove gen (n+1) 0 pt gs

    where wpList = randPerm gen (getAllPieceCoor 0 WP board)
          wmList = wMoves pos
          pos    = getNext n wpList
          mov    = getNext m wmList
          gs     = (GameState bplay bpenalty wplay wpenalty board)


-- | Gives the first valid movement of a black pawn
getBPMove :: StdGen -> Int -> Int -> PlayType -> GameState -> [(Int, Int)] --pawn index, possible moves Index, GameState
getBPMove gen n m pt (GameState bplay bpenalty wplay wpenalty board) =
          if ((isValidMove (pos:mov:[]) gs pt Black) == VALID || (isValidMove (pos:mov:[]) gs pt Black) == CAPTURE)
          then if (pt == Normal)then [pos, mov] else [mov]
          else if (m < length bmList)
               then getBPMove gen n (m+1) pt gs
               else if (n >= length bpList)
               then []
               else getBPMove gen (n+1) 0 pt gs

    where bpList = randPerm gen (getAllPieceCoor 0 BP board)
          bmList = bMoves pos
          pos    = getNext n bpList
          mov    = getNext m bmList
          gs     = (GameState bplay bpenalty wplay wpenalty board)




------Code for moving the Knights-------------------------------



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
getWKMove :: StdGen -> Int -> Int -> GameState -> [(Int, Int)] --knight index, possible moves Index, GameState
getWKMove gen n m (GameState bplay bpenalty wplay wpenalty board) =
          if ((isValidMove (pos:mov:[]) gs Normal White) == VALID || (isValidMove (pos:mov:[]) gs Normal White) == CAPTURE)
          then [pos, mov]
          else if (m < length wmList)
               then getWKMove gen n (m+1) gs
               else if (n > length wkList)
               then []
               else getWKMove gen (n+1) 0 gs

    where wkList = getAllPieceCoor 0 WK board
          wmList = randPerm gen (kMoves pos)
          pos    = getNext n wkList
          mov    = getNext m wmList
          gs     = (GameState bplay bpenalty wplay wpenalty board)


-- | Gives the first valid movement of a black knight
getBKMove :: StdGen -> Int -> Int -> GameState -> [(Int, Int)] --knight index, possible moves Index, GameState
getBKMove gen n m (GameState bplay bpenalty wplay wpenalty board) =
          if ((isValidMove (pos:mov:[]) gs Normal Black) == VALID || (isValidMove (pos:mov:[]) gs Normal Black) == CAPTURE)
          then [pos, mov]
          else if (m < length bmList)
               then getBKMove gen n (m+1) gs
               else if (n > length bkList)
               then []
               else getBKMove gen (n+1) 0 gs

    where bkList = getAllPieceCoor 0 BK board
          bmList = randPerm gen (kMoves pos)
          pos    = getNext n bkList
          mov    = getNext m bmList
          gs     = (GameState bplay bpenalty wplay wpenalty board)



-- | test function
--testFunc :: [(Int, Int)]
--testFunc = getWPMove 0 0 Normal initBoard
