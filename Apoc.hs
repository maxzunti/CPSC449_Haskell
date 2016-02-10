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

import StartupMod
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
mainLoop (b:w:[]) a = do
                        putStrLn "This is a test"
                        return $ initBoard
        
--mainLoop _ _ = do putStrLn "Something broke"



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

-- moves the pieces by updating the gamestate
-- needs to check is it was a legal move
-- TODO
moveWB :: Chooser -> Chooser -> GameState -> Int -> GameState 
moveWB w b g p = g

--checks GameState to see if game is over			 
--TODO
gameOver :: GameState -> Bool
gameOver _ = False

--checks GameState to find winner
--0 = tie, 1 = white, 2 = black
--MAX: If gameOver needs to verify the game is complete, why not
--     make it return the winner, too?
--TODO
winner :: GameState -> Int
winner _ = 0


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

