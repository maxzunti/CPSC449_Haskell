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

--Custom Types------------------------------------------------------
--None, apparently


---Main-------------------------------------------------------------

-- | The main entry, which just calls 'main'' with the command line arguments.
main = main' (unsafePerformIO getArgs)

{- | We have a main' IO function so that we can either:

     1. call our program from GHCi in the usual way
     2. run from the command line by calling this function with the value from (getArgs)
-}

--testFun :: GameState -> GameState
--testFun g = updateMoves (Played ((0, 3), ( 0, 2))) (Played ((4, 1), ( 4, 2))) g

main'           :: [String] -> IO()
main' args = do
    stratList <- getStrategies args -- stratList has type [Strat], stores "BlkStrat:WhtStrat:[]"
    checkInvalid stratList -- exits if invalid
    --putStrLn "Begin actual loop: "
    --Print the board
    putStrLn (show $ GameState (blackPlay initBoard) (blackPen initBoard) (whitePlay initBoard) (whitePen initBoard) (theBoard initBoard))
    finalGameState <- mainLoop stratList initBoard
    putStrLn("It's overrrr!");

    {-
    printGameState initBoard -- dummy call
    putStrLn "\nThe initial board:"
    print initBoard

    putStrLn $ "\nThe initial board with back human (the placeholder for human) strategy having played one move\n"
               ++ "(clearly illegal as we must play in rounds!):"
    move <- human (initBoard) Normal Black
    putStrLn (show $ GameState (if move==Nothing
                                then Passed
                                else Played (head (fromJust move), head (tail (fromJust move))))
                               (blackPen initBoard)
                               (Passed)
                               (whitePen initBoard)
                               (replace2 (replace2 (theBoard initBoard)
                                                   ((fromJust move) !! 1)
                                                   (getFromBoard (theBoard initBoard) ((fromJust move) !! 0)))
                                         ((fromJust move) !! 0)
                                         E))
    -}


-- takes Gamestate -> PawnPlacement and returns winner
{-mainloop :: GameState -> Int -> Int
mainloop g p = if (gameOver g)
             then winner g
             else mainloop (nextTurn g p) p
-}

-- mainLoop calls itself, passing Strats and the current state
-- before returning the new GameState
mainLoop :: [Strat] -> GameState -> IO GameState
-- At this point, assume the GameState and [Strat] is valid
-- Perform error checking for new GameStates during update calls
mainLoop (b:w:[]) g =
                        if ((checkForWinner g) == NONE)
                        then do -- GAME NOT OVER

                        --These calls automatically return the correct type of [(Int,Int)] list
                        -- depending on whether or not we should upgrade
                        -- **** If the returned list has TWO entries, then this is a NORMAL turn
                        -- If the returned list has ONE entry, this is a PAWNPLACEMENT
                        bMove <- getStratMove b g Black
                        wMove <- getStratMove w g White

                        -- End game if both players pass
                        if ((bMove == Nothing) && (wMove == Nothing))
                        then do
                          putStrLn "Both Pass, ending game"
                          exitSuccess
                        else putStrLn " " --TODO: print actual ending message here

                        newg <- findPlayed (fromJust bMove) (fromJust wMove) g

                        --TODO: Check move legality, update board / penalty accordingly

                        {-if (isValidMove (fromJust bMove) == VALID || CAPTURE)
                          then newg <- updateMoves (Played (fromJust bMove !! 0,fromJust bMove !! 1)) (Played (fromJust wMove !! 0,fromJust wMove !! 1)) g
                          else newg <- updateMoves (Goofed (fromJust bMove !! 0,fromJust bMove !! 1)) (Played (fromJust wMove !! 0,fromJust wMove !! 1)) (updatePenalty g Black)

                          if (isValidMove (fromJust wMove) == VALID || CAPTURE)
                            then newg <- updateMoves (Played (fromJust bMove !! 0,fromJust bMove !! 1)) (Played (fromJust wMove !! 0,fromJust wMove !! 1)) g
                            else newg <- newg-}
                        --TODO: newGS <- mainLoop (b:w:[]) updateGameState g

                        --newg <- updateMoves (Played (fromJust bMove !! 0,fromJust bMove !! 1)) (Played (fromJust wMove !! 0,fromJust wMove !! 1)) g

                        --Print the board
                        putStrLn (show $ GameState (blackPlay newg) (blackPen newg) (whitePlay newg) (whitePen newg) (theBoard newg))

                        bpp <- findPawnPlay' (fromJust bMove) b Black newg
                        wpp <- findPawnPlay' (fromJust wMove) w White newg

                        {-bpp <- do
                          if (pawnPlacementCheck (fromJust bMove) Black) --checks top
                          then findPawnPlay bMove b Black newg
                          else None

                        wpp <- do
                        if (pawnPlacementCheck (fromJust wMove) White) --chekcs bottom
                          then findPawnPlay wMove w White newg
                          else None
                            -}
                        newNewg <- getGameStatePawnPlace bpp wpp newg
                        --if (bpp != None || wpp != None)
                        --  then do newNewg <- (GameState bpp (blackPen newg) wpp (whitePen newg) (theBoard newg))
                        --  else do newNewg <- (returnGameState newg)

                        newGs <- mainLoop (b:w:[]) newNewg   -- TODO: Need to pass a NEW GameState as arg
                        return $ newGs
                        -- but for some reason,
                        --return $ mainLoop (b:w:[]) initBoard
                        -- doesn't
                        else do
                         -- GAMEOVER, PRINT WINNER
                        if (checkForWinner g == WHITE)
                        then putStrLn $ "have winrar White"
                        else putStrLn $ "have winrar Black"
                        return $ initBoard
                        -- TODO: if (winner) then end, else
                        --
mainLoop _ _ = do putStrLn "Something broke"
                  exitSuccess
                  return $ initBoard

getGameStatePawnPlace :: Played -> Played -> GameState -> IO(GameState)
getGameStatePawnPlace bpp wpp (GameState bplay bpen wplay wpen board) =
  if (wpp /= None || bpp /= None)
    then do
          x <- updateMoves bpp wpp (GameState bplay bpen wplay wpen board)
          putStrLn (show $ x)
          return x
    else return gs
      where gs = (GameState bplay bpen wplay wpen board)

findPawnPlay' :: [(Int,Int)] -> Strat -> Player -> GameState -> IO(Played)
findPawnPlay' (x:y:[]) s p (GameState a b c d board) = do
                    if (pawnPlacementCheck (x:y:[]) p)
                    then findPawnPlay (x:y:[]) s p (GameState a b c d board)
                    else return None

findPawnPlay :: [(Int,Int)] -> Strat -> Player -> GameState -> IO(Played)
findPawnPlay (x:y:[]) s p (GameState a b c d board) = do

                      des <-  (getPawnPlaceMove s (GameState a b c d board) p) -- get pawn placement move
                      if (pawnPlacementCheck (x:y:[]) p)
                      then
                        if (p == White)
                          then if (length (getAllPieceCoor 0 WK board) < 2)
                            then return $UpgradedPawn2Knight y
                            else if (isEmptyCell x board )
                              then return $PlacedPawn (y, ((fromJust des) !! 0))
                              else return $BadPlacedPawn (y, ((fromJust des) !! 0))
                          else if (length (getAllPieceCoor 0 BK board) < 2)
                            then return $UpgradedPawn2Knight y
                            else if (isEmptyCell x board )
                              then return $PlacedPawn (y, ((fromJust des) !! 0))
                              else return $BadPlacedPawn (y, ((fromJust des) !! 0))
                      else return None

pawnPlacementCheck :: [(Int,Int)] -> Player -> Bool
pawnPlacementCheck (_:(x, y):[]) p = if (p == White)
                                     then if (y == 4)
                                          then True
                                          else False
                                     else if (y == 0)
                                          then True
                                          else False


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

-- Invoke "human" or "knoight" Chooser based on passed Strat
-- Also determines if it should be a normal move or a pawn placement

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

findPlayed :: [(Int,Int)] -> [(Int, Int)] -> GameState -> IO(GameState)
findPlayed b w g = do
        x <- findPlayed' b w g (mapMoveToPT b) (mapMoveToPT w)
        return x

findPlayed' :: [(Int,Int)] -> [(Int, Int)] -> GameState -> PlayType -> PlayType -> IO(GameState)
findPlayed' b w g bPt wPt | (((isValidMove b g bPt Black)== VALID) || ((isValidMove b g bPt Black)== CAPTURE)) && (((isValidMove w g wPt White) == VALID) || ((isValidMove w g wPt White) == CAPTURE))= bwValid b w g bPt wPt
                          | (((isValidMove b g bPt Black)== VALID) || ((isValidMove b g bPt Black)== CAPTURE))  && ((isValidMove w g wPt White) == INVALID) = bValidOnly b w g bPt wPt
                          | ((isValidMove b g bPt Black)== INVALID) && (((isValidMove w g wPt White) == VALID) || ((isValidMove w g wPt White) == CAPTURE)) = wValidOnly b w g bPt wPt
                          | otherwise = bwInvalid b w g bPt wPt

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

wValidOnly :: [(Int,Int)] -> [(Int, Int)] -> GameState -> PlayType -> PlayType -> IO(GameState)
wValidOnly b [] (GameState bPlay bPen wPlay wPen board) bPt wPt = do
  x <- updateMoves (Goofed (b !! 0,b !! 1)) (Passed) (GameState bPlay (bPen + 1) wPlay wPen board)
  return x
wValidOnly b w (GameState bPlay bPen wPlay wPen board) bPt wPt = do
  x <- updateMoves (Goofed (b !! 0,b !! 1)) (Played (w !! 0,w !! 1)) (GameState bPlay (bPen + 1) wPlay wPen board)
  return x

bValidOnly :: [(Int,Int)] -> [(Int, Int)] -> GameState -> PlayType -> PlayType -> IO(GameState)
bValidOnly [] w (GameState bPlay bPen wPlay wPen board) bPt wPt = do
  x <- updateMoves (Passed) (Goofed (w !! 0,w !! 1)) (GameState bPlay bPen wPlay (wPen + 1) board)
  return x
bValidOnly b w (GameState bPlay bPen wPlay wPen board) bPt wPt = do
  x <- updateMoves (Played (b !! 0,b !! 1)) (Goofed (w !! 0,w !! 1)) (GameState bPlay bPen wPlay (wPen + 1) board)
  return x

bwInvalid :: [(Int,Int)] -> [(Int, Int)] -> GameState -> PlayType -> PlayType -> IO(GameState)
bwInvalid b w (GameState bPlay bPen wPlay wPen board) bPt wPt = do
  x <- updateMoves (Goofed (b !! 0,b !! 1)) (Goofed (w !! 0,w !! 1)) (GameState bPlay (bPen + 1) wPlay (wPen + 1) board)
  return x

mapMoveToPT :: [a] -> PlayType
mapMoveToPT [] = Normal
mapMoveToPT x = if (length x == 2)
                then Normal
                else PawnPlacement

-- Must be a valid move
-- Move must be in the form of played
updateMoves :: Played -> Played -> GameState -> IO(GameState)
updateMoves b w (GameState x bP y wP bd) = do return (updateGamestate (GameState b bP w wP bd))

-- Must be a legal move
updateGamestate :: GameState -> GameState
updateGamestate (GameState bPlay bPen wPlay wPen b) | (moveType bPlay wPlay == MISSCAPTURE) = GameState bPlay bPen wPlay wPen (misscapture bPlay wPlay b)
                                                    | (moveType bPlay wPlay == DOUBLECAPTURE) = GameState bPlay bPen wPlay wPen (doublecapture bPlay wPlay b)
                                                    | (moveType bPlay wPlay == SWAP) = GameState bPlay bPen wPlay wPen (swap bPlay wPlay (getFromBoard b (playedToCood bPlay)) b)
                                                    | otherwise = GameState bPlay bPen wPlay wPen (updateBoard wPlay (updateBoard bPlay b))

updateBoard :: Played -> Board -> Board
updateBoard (Played (x,y)) b = replace2 (replace2 b y (getFromBoard b x)) x E --Does not work for pawn missed capture
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

data MoveType = NORMALMOVE | MISSCAPTURE | DOUBLECAPTURE | SWAP deriving (Eq)

moveType :: Played -> Played -> MoveType
moveType (Played (x1,y1)) (Played (x2,y2)) | ((x1 == y2) && (y1 == x2)) = SWAP
                                           | ((x1 == y2) || (x2 == y1)) = MISSCAPTURE
                                           | (y1 == y2) = DOUBLECAPTURE
                                           | otherwise = NORMALMOVE
moveType b w = NORMALMOVE

misscapture :: Played -> Played -> Board -> Board
misscapture (Played (sb,db)) (Played (sw,dw)) b | (sb == dw) = replace2 (replace2 (replace2 b db (getFromBoard b sb)) dw (getFromBoard b sw)) sw E
                                                | (sw == db) = replace2 (replace2 (replace2 b dw (getFromBoard b sw)) db (getFromBoard b sb)) sb E

doublecapture :: Played -> Played -> Board -> Board
doublecapture (Played (x1,y1)) (Played (x2,y2)) b  | (((getFromBoard b x1) == BP) && ((getFromBoard b x2) == WP)) = replace2 (replace2 b x1 E) x2 E
                                                   | (((getFromBoard b x1) == BK) && ((getFromBoard b x2) == WK)) = replace2 (replace2 b x1 E) x2 E
                                                   | ((getFromBoard b x1) == BK) = replace2 (replace2 (replace2 b y1 (getFromBoard b x1)) x1 E) x2 E
                                                   | ((getFromBoard b x2) == WK) = replace2 (replace2 (replace2 b y2 (getFromBoard b x2)) x2 E) x1 E

swap :: Played -> Played -> Cell -> Board -> Board
swap (Played (x1,y1)) (Played (x2,y2)) c b = replace2 (replace2 b x1 (getFromBoard b x2)) x2 c

playedToCood :: Played -> (Int,Int)
playedToCood (Played (x,y)) = x

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
