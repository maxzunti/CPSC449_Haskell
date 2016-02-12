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
import ApocStrategyGreedy
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
main'           :: [String] -> IO()
main' args = do
    stratList <- getStrategies args -- stratList has type [Strat], stores "BlkStrat:WhtStrat:[]"
    checkInvalid stratList -- exits if invalid
    --putStrLn "Begin actual loop: "
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
                        then exitSuccess
                        else putStrLn "Both players passed, game over" --TODO: print actual ending message here

                        --TODO: Check move legality, update board / penalty accordingly
                        --
                        --TODO: newGS <- mainLoop (b:w:[]) updateGameState g
                       
                        --Print the board 
                        putStrLn (show $ GameState (blackPlay g) (blackPen g) (whitePlay g) (whitePen g) (theBoard g))
                        -- The below works:
                        newGs <- mainLoop (b:w:[]) g    -- TODO: Need to pass a NEW GameState as arg
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

-- Invoke "human" or "greedy" Chooser based on passed Strat
-- Also determines if it should be a normal move or a pawn placement
-- FIXME: CURRENTLY DOESN'T WORK FOR GREEDY OR RANDOM
getStratMove :: Strat -> GameState ->  Player -> IO (Maybe [(Int,Int)])
getStratMove s g p = if (checkPawnUpgrade g p == Nothing)
                       -- then if (s == HUMAN) 
                       then human g Normal p   --TODO: IF (s == HUMAN) then
                       --TODO: else if (s == GREEDY)
                       else human g PawnPlacement p


-- EXAMPLE
-- Dummy function showing how to access type members
-- (I definitely didn't know how to do this - Max)
printGameState :: GameState -> IO()
printGameState a = do
                   putStrLn "GameState blackPlay: "
                   putStrLn $ show $ blackPlay a


-- Must be a valid move
-- Move must be in the form of played
updateMoves :: Played -> Played -> GameState -> GameState
updateMoves b w (GameState x bP y wP bd) = updateGamestate (GameState b bP w wP bd)
        
-- Must be a legal move
updateGamestate :: GameState -> GameState
--updateGamestate :: Played -> Int -> Played -> Int -> Board -> GameState
updateGamestate (GameState bPlay bPen wPlay wPen b) = GameState bPlay bPen wPlay wPen (updateBoard wPlay (updateBoard bPlay b))

updateBoard :: Played -> Board -> Board
updateBoard (Played (x,y)) b = replace2 (replace2 b y (getFromBoard b x)) x E --Does not work for pawn missed capture
updateBoard (Passed) b = b
updateBoard (Goofed (x,y)) b = replace2 (replace2 b y (getFromBoard b x)) x E --Does not work for pawn missed capture
updateBoard (Init) b = b
updateBoard (UpgradedPawn2Knight x) b = if ((playerOf (ourPieceOf (getFromBoard b x))) == Black )
                                        then replace2 b x BK
                                        else replace2 b x WK  
updateBoard (PlacedPawn (x,y)) b = replace2 (replace2 b y (getFromBoard b x)) x E
updateBoard (BadPlacedPawn (x,y)) b = b
updateBoard (NullPlacedPawn) b = b
updateBoard (None) b = b



--updatePenalty :: Played -> Bool
--updatePenalty (Goofed (x,y)) = True
--updatePenalty a = False


--tfunc :: Maybe a -> Int
--tfunc a = 0

-- Determines what stratagy white and black have and then sends it to moveWB
-- It returns the altered GameState and the pawn placement status
-- TODO
nextTurn :: GameState -> Int -> GameState
nextTurn g p = g
--nextTurn g p = if (white is human)
--               then if (black is human) 
--                    then moveWB white_human black_human 
--			          else moveWB white_statagy, black_stratagy
--			     else moveWB white_statagy black_stratagy



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

