{- | This module is used for CPSC 449 for the Apocalypse assignment.

Feel free to modify this file as you see fit.

Copyright: Copyright 2016, Rob Kremer (rkremer@ucalgary.ca), University of Calgary.
Permission to use, copy, modify, distribute and sell this software
and its documentation for any purpose is hereby granted without fee, provided
that the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation. The University of Calgary makes no representations about the
suitability of this software for any purpose. It is provided "as is" without
express or implied warranty.

-}

module Main(main) where

import System.Random
import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
import Data.List
import System.Environment
import System.IO.Unsafe
import ApocTools
import ApocStrategyHuman
import ApocStrategyKnightmare
import ApocStrategySimple
import GameRules

import StartupMod
import System.Exit (exitSuccess)



---Main-------------------------------------------------------------

-- | The main entry, which just calls 'main'' with the command line arguments.
main = main' (unsafePerformIO getArgs)

{- | We have a main' IO function so that we can either:

     1. call our program from GHCi in the usual way
     2. run from the command line by calling this function with the value from (getArgs)
-}

main'           :: [String] -> IO()
main' args = do
    stratList <- getStrategies args -- stratList has type [Strat], stores "BlkStrat:WhtStrat:[]"
    checkInvalid stratList -- exits if invalid

    putStrLn (show $ GameState (blackPlay initBoard) (blackPen initBoard) (whitePlay initBoard) (whitePen initBoard) (theBoard initBoard))
    finalGameState <- mainLoop stratList initBoard
    putStrLn " "

-- | Function for main loop
-- mainLoop calls itself, passing Strats and the current state
-- before returning the new GameState
mainLoop :: [Strat] -> GameState -> IO GameState
-- At this point, assume the GameState and [Strat] is valid
-- Perform error checking for new GameStates during update calls
mainLoop (b:w:[]) g =
                        if ((checkForWinner g) == NONE)
                        then do -- GAME NOT OVER

                        --Get Normal move

                        bMove <- getStratMove b g Black
                        wMove <- getStratMove w g White

                        newg <- findPlayed (fromJust bMove) (fromJust wMove) g

                        --Does a normal move
                        putStrLn (show $ GameState (blackPlay newg) (blackPen newg) (whitePlay newg) (whitePen newg) (theBoard newg))

                        bpp <- findPawnPlay' (fromJust bMove) b Black newg
                        wpp <- findPawnPlay' (fromJust wMove) w White newg

                        -- Print board
                        if ((fromJust bMove) == []) && ((fromJust wMove) == [])
                        then do
                          printWinnerDoublePass b w g
                          exitSuccess
                        else putStrLn " "
                        
                        --checks if a pawnplacement or upgrade has to happen 
                        newNewg <- getGameStatePawnPlace bpp wpp newg

                        --Loops back to the begining for another turn
                        newGs <- mainLoop (b:w:[]) newNewg   -- TODO: Need to pass a NEW GameState as arg
                        return $ newGs

                        else do
                        if (checkForWinner g == DRAW)
                        then putStrLn $ "It's a draw! You're all losers!"
                        else
                            if (checkForWinner g == WHITE)
                            then putStrLn $ "White Wins!"
                            else putStrLn $ "Black Wins!"
                        return $ initBoard

mainLoop _ _ = do putStrLn "Something broke"
                  exitSuccess
                  return $ initBoard

--Prints the winner when two players passes on the same turn----------------
printWinnerDoublePass :: Strat -> Strat -> GameState -> IO()
printWinnerDoublePass b w (GameState _ _ _ _ board) = if (bp == wp)
                    then putStrLn $"Both Win! Black (" ++ (stratToString b) ++ "): " ++ (show bp) ++ "  White (" ++ (stratToString w) ++ "): " ++ (show wp)
                    else if (bp > wp)
                        then putStrLn $"Black Wins! Black (" ++ (stratToString b) ++ "): " ++ (show bp) ++ "  White (" ++ (stratToString w) ++ "): " ++ (show wp)
                        else putStrLn $"White Wins! Black (" ++ (stratToString b) ++ "): " ++ (show bp) ++ "  White (" ++ (stratToString w) ++ "): " ++ (show wp)
            where bp = length (getAllPieceCoor 0 BP board)
                  wp = length (getAllPieceCoor 0 WP board)


----------- PawnPlacement codes -----------------

-- | Given two played move for PawnPlacement, if they are given none (no one has a pawn placement)
--then return the orginal gamestate, else update the gamestate with the new played moves and return it
getGameStatePawnPlace :: Played -> Played -> GameState -> IO(GameState)
getGameStatePawnPlace bpp wpp (GameState bplay bpen wplay wpen board) =
 if ((checkForWinner gs) /= NONE)
 then 
  return gs 
 else if (wpp /= None || bpp /= None)
    then do
          x <- updateMoves bpp wpp (GameState bplay bpen wplay wpen board)
          putStrLn (show $ x)
          return x
    else return gs
 where gs = (GameState bplay bpen wplay wpen board)

-- | Checks for pawnplacements or knight upgrades and return the correct played type
-- this checks for the a pawn to be at the end of the board and checks for passes
--then calls findPawnPlay to contiue the checks
findPawnPlay' :: [(Int,Int)] -> Strat -> Player -> GameState -> IO(Played)
findPawnPlay' [] _ _ _ = return None
findPawnPlay' (x:y:[]) s p (GameState a b c d board) = do
                    if (((getFromBoard board y) == BP) || ((getFromBoard board y) == WP))
                    then if (pawnPlacementCheck (x:y:[]) p)
                        then findPawnPlay (x:y:[]) s p (GameState a b c d board)
                        else return None
                    else return None

-- | Continues the pawn placement checks, and returns the correct pawnplacement move ()
findPawnPlay :: [(Int,Int)] -> Strat -> Player -> GameState -> IO(Played)
findPawnPlay (x:y:[]) s p (GameState a b c d board) = do
                      if (pawnPlacementCheck (x:y:[]) p)
                      then
                        if (p == White)
                          then if (length (getAllPieceCoor 0 WK board) < 2)
                            then return $UpgradedPawn2Knight y
                            else if (isEmptyCell x board )
                              then do
                                des <-  (getPawnPlaceMove s (GameState a b c d board) p) -- get pawn placement move
                                if (fromJust des == [])
                                then return NullPlacedPawn
                                else return $PlacedPawn (y, ((fromJust des) !! 0))
                              else do
                                des <-  (getPawnPlaceMove s (GameState a b c d board) p) -- get pawn placement move
                                if (fromJust des == [])
                                then return NullPlacedPawn
                                else return $BadPlacedPawn (y, ((fromJust des) !! 0))
                          else if (length (getAllPieceCoor 0 BK board) < 2)
                            then return $UpgradedPawn2Knight y
                            else if (isEmptyCell x board )
                              then do
                                des <-  (getPawnPlaceMove s (GameState a b c d board) p) -- get pawn placement move
                                if (fromJust des == [])
                                then return NullPlacedPawn
                                else return $PlacedPawn (y, ((fromJust des) !! 0))
                              else do
                                des <-  (getPawnPlaceMove s (GameState a b c d board) p) -- get pawn placement move
                                if (fromJust des == [])
                                then return NullPlacedPawn
                                else return $BadPlacedPawn (y, ((fromJust des) !! 0))
                      else return None

-- | Checks that the pawn is at the end
pawnPlacementCheck :: [(Int,Int)] -> Player -> Bool
pawnPlacementCheck (_:(x, y):[]) p = if (p == White)
                                     then if (y == 4)
                                          then True
                                          else False
                                     else if (y == 0)
                                          then True
                                          else False

-- | Get the moves for pawn placement based on strat
getPawnPlaceMove :: Strat -> GameState ->  Player -> IO (Maybe [(Int,Int)])
getPawnPlaceMove s g p  | (s == HUMAN) = do
                                        x <- human g PawnPlacement p
                                        return x
                    | (s == SIMPLE) = do
                                        x <- simpleStrat g PawnPlacement p
                                        return x
                    | (s == KNIGHT ) = do
                                        x <- knightStrat g PawnPlacement p
                                        return x

-- | Get the moves for a normal turn based on strat
getStratMove :: Strat -> GameState ->  Player -> IO (Maybe [(Int,Int)])
getStratMove s g p   | (s == HUMAN) = do
                                        x <- human g Normal p
                                        return x
                     | (s == SIMPLE) = do
                                        x <- simpleStrat g Normal p
                                        return x
                     | (s == KNIGHT ) = do
                                        x <- knightStrat g Normal p
                                        return x

-- EXAMPLE
-- Dummy function showing how to access type members
-- (I definitely didn't know how to do this - Max)
printGameState :: GameState -> IO()
printGameState a = do
                   putStrLn "GameState blackPlay: "
                   putStrLn $ show $ blackPlay a

-- | The 'findPlayed' function takes the black and white moves and updates the gamestate.
findPlayed :: [(Int,Int)] -> [(Int, Int)] -> GameState -> IO(GameState)
findPlayed b w g = do
        x <- findPlayed' b w g (mapMoveToPT b) (mapMoveToPT w)
        return x

-- | The 'findPlayed'' function determines the validity of moves the updates the gamestate based on those moves.
findPlayed' :: [(Int,Int)] -> [(Int, Int)] -> GameState -> PlayType -> PlayType -> IO(GameState)
findPlayed' b w g bPt wPt | (((isValidMove b g bPt Black)== VALID) || ((isValidMove b g bPt Black)== CAPTURE)) && (((isValidMove w g wPt White) == VALID) || ((isValidMove w g wPt White) == CAPTURE))= bwValid b w g bPt wPt
                          | (((isValidMove b g bPt Black)== VALID) || ((isValidMove b g bPt Black)== CAPTURE))  && ((isValidMove w g wPt White) == INVALID) = bValidOnly b w g bPt wPt
                          | ((isValidMove b g bPt Black)== INVALID) && (((isValidMove w g wPt White) == VALID) || ((isValidMove w g wPt White) == CAPTURE)) = wValidOnly b w g bPt wPt
                          | otherwise = bwInvalid b w g bPt wPt

-- | The 'bwValid' function updates the gamestate based on both black and white having valid moves.
bwValid :: [(Int,Int)] -> [(Int, Int)] -> GameState -> PlayType -> PlayType -> IO(GameState)
bwValid [] [] (GameState bPlay bPen wPlay wPen board) bPt wPt = do
                                                                x <- updateMoves (Passed) (Passed) (GameState bPlay bPen wPlay wPen board)
                                                                return x
bwValid [] w (GameState bPlay bPen wPlay wPen board) bPt wPt = do
  x <- updateMoves (Passed) (Played (w !! 0,w !! 1)) (GameState bPlay bPen wPlay wPen board)
  return x
bwValid b [] (GameState bPlay bPen wPlay wPen board) bPt wPt = do
  x <- updateMoves (Played (b !! 0,b !! 1)) (Passed) (GameState bPlay bPen wPlay wPen board)
  return x
bwValid b w (GameState bPlay bPen wPlay wPen board) bPt wPt = do
  x <- updateMoves (Played (b !! 0,b !! 1)) (Played (w !! 0,w !! 1)) (GameState bPlay bPen wPlay wPen board)
  return x

-- | The 'wValidOnly' function updates the gamstate based on only the white move being valid.
wValidOnly :: [(Int,Int)] -> [(Int, Int)] -> GameState -> PlayType -> PlayType -> IO(GameState)
wValidOnly b [] (GameState bPlay bPen wPlay wPen board) bPt wPt = do
  x <- updateMoves (Goofed (b !! 0,b !! 1)) (Passed) (GameState bPlay (bPen + 1) wPlay wPen board)
  return x
wValidOnly b w (GameState bPlay bPen wPlay wPen board) bPt wPt = do
  x <- updateMoves (Goofed (b !! 0,b !! 1)) (Played (w !! 0,w !! 1)) (GameState bPlay (bPen + 1) wPlay wPen board)
  return x

-- | The 'bValidOnly' function updates the gamstate based on only the black move being valid.
bValidOnly :: [(Int,Int)] -> [(Int, Int)] -> GameState -> PlayType -> PlayType -> IO(GameState)
bValidOnly [] w (GameState bPlay bPen wPlay wPen board) bPt wPt = do
  x <- updateMoves (Passed) (Goofed (w !! 0,w !! 1)) (GameState bPlay bPen wPlay (wPen + 1) board)
  return x
bValidOnly b w (GameState bPlay bPen wPlay wPen board) bPt wPt = do
  x <- updateMoves (Played (b !! 0,b !! 1)) (Goofed (w !! 0,w !! 1)) (GameState bPlay bPen wPlay (wPen + 1) board)
  return x

-- | The 'bwInvalid' function updates the gamestate based on both black and white having invalid moves.
bwInvalid :: [(Int,Int)] -> [(Int, Int)] -> GameState -> PlayType -> PlayType -> IO(GameState)
bwInvalid b w (GameState bPlay bPen wPlay wPen board) bPt wPt = do
  x <- updateMoves (Goofed (b !! 0,b !! 1)) (Goofed (w !! 0,w !! 1)) (GameState bPlay (bPen + 1) wPlay (wPen + 1) board)
  return x

-- | The 'mapMoveToPT' function extracts the PlayType from a move.
mapMoveToPT :: [a] -> PlayType
mapMoveToPT [] = Normal
mapMoveToPT x = if (length x == 2)
                then Normal
                else PawnPlacement

-- | The 'updateMoves' function takes the black and white moves, and updates the gamestate.
updateMoves :: Played -> Played -> GameState -> IO(GameState)
updateMoves b w (GameState x bP y wP bd) = do return (updateGamestate (GameState b bP w wP bd))

-- | The 'updateGamestate' function updates the values of the gamestate for a turn.
updateGamestate :: GameState -> GameState
updateGamestate (GameState bPlay bPen wPlay wPen b) | (moveType bPlay wPlay == MISSCAPTURE) = GameState bPlay bPen wPlay wPen (misscapture bPlay wPlay b)
                                                    | (moveType bPlay wPlay == DOUBLECAPTURE) = GameState bPlay bPen wPlay wPen (doublecapture bPlay wPlay b)
                                                    | (moveType bPlay wPlay == SWAP) = GameState bPlay bPen wPlay wPen (swap bPlay wPlay (getFromBoard b (playedToCood bPlay)) b)
                                                    | ((pawnPenalty bPlay) && (pawnPenalty wPlay)) = GameState bPlay (bPen+1) wPlay (wPen+1) (updateBoard wPlay (updateBoard bPlay b))
                                                    | (pawnPenalty bPlay) = GameState bPlay (bPen+1) wPlay wPen (updateBoard wPlay (updateBoard bPlay b))
                                                    | (pawnPenalty wPlay) = GameState bPlay bPen wPlay (wPen+1) (updateBoard wPlay (updateBoard bPlay b))
                                                    | otherwise = GameState bPlay bPen wPlay wPen (updateBoard wPlay (updateBoard bPlay b))

-- | The 'updateBoard' function applys a move to a board.
updateBoard :: Played -> Board -> Board
updateBoard (Played (x,y)) b = replace2 (replace2 b y (getFromBoard b x)) x E
updateBoard (Passed) b = b
updateBoard (Goofed (x,y)) b = b
updateBoard (Init) b = b
updateBoard (UpgradedPawn2Knight x) b = if ((playerOf (ourPieceOf (getFromBoard b x))) == Black )
                                        then replace2 b x BK
                                        else replace2 b x WK
updateBoard (PlacedPawn (x,y)) b = replace2 (replace2 b y (getFromBoard b x)) x E
updateBoard (BadPlacedPawn (x,y)) b = b
updateBoard (NullPlacedPawn) b = b
updateBoard (None) b = b

{- | This type is used by 'moveType' to tell the 'moveType' function whether the move is
of type normal, a miss capture, a double capture, or a swap.
-}
data MoveType = NORMALMOVE | MISSCAPTURE | DOUBLECAPTURE | SWAP deriving (Eq)

-- | The 'moveType' function determines what kind of move was made.
moveType :: Played -> Played -> MoveType
moveType (Played (x1,y1)) (Played (x2,y2)) | ((x1 == y2) && (y1 == x2)) = SWAP
                                           | ((x1 == y2) || (x2 == y1)) = MISSCAPTURE
                                           | (y1 == y2) = DOUBLECAPTURE
                                           | otherwise = NORMALMOVE
moveType b w = NORMALMOVE

-- | The 'misscapture' function updates the board based on a misscature move.
misscapture :: Played -> Played -> Board -> Board
misscapture (Played (sb,db)) (Played (sw,dw)) b | (sb == dw) = replace2 (replace2 (replace2 b db (getFromBoard b sb)) dw (getFromBoard b sw)) sw E
                                                | (sw == db) = replace2 (replace2 (replace2 b dw (getFromBoard b sw)) db (getFromBoard b sb)) sb E

-- | The 'doublecapture' function updates the board based on a doublecapture move.
doublecapture :: Played -> Played -> Board -> Board
doublecapture (Played (x1,y1)) (Played (x2,y2)) b  | (((getFromBoard b x1) == BP) && ((getFromBoard b x2) == WP)) = replace2 (replace2 b x1 E) x2 E
                                                   | (((getFromBoard b x1) == BK) && ((getFromBoard b x2) == WK)) = replace2 (replace2 b x1 E) x2 E
                                                   | ((getFromBoard b x1) == BK) = replace2 (replace2 (replace2 b y1 (getFromBoard b x1)) x1 E) x2 E
                                                   | ((getFromBoard b x2) == WK) = replace2 (replace2 (replace2 b y2 (getFromBoard b x2)) x2 E) x1 E

-- | The 'swap' function updates the board based on a swap move.
swap :: Played -> Played -> Cell -> Board -> Board
swap (Played (x1,y1)) (Played (x2,y2)) c b = replace2 (replace2 b x1 (getFromBoard b x2)) x2 c

-- | The 'playedToCood' function finds the coodinates of the source of a move.
playedToCood :: Played -> (Int,Int)
playedToCood (Played (x,y)) = x

-- | The 'pawnPenalty' function determines if a penalty based on pawnplacement should be added.
pawnPenalty :: Played -> Bool
pawnPenalty (BadPlacedPawn (x,y)) = True
pawnPenalty (NullPlacedPawn) = True
pawnPenalty p = False


---2D list utility functions-------------------------------------------------------

-- | Replaces the nth element in a row with a new element.
replace         :: [a] -> Int -> a -> [a]
replace xs n elem = let (ys,zs) = splitAt n xs
                     in (if null zs then (if null ys then [] else init ys) else ys)
                        ++ [elem]
                        ++ (if null zs then [] else tail zs)

-- | Replaces the (x,y)th element in a list of lists with a new element.
replace2        :: [[a]] -> (Int,Int) -> a -> [[a]]
replace2 xs (x,y) elem = replace xs y (replace (xs !! y) x elem)
