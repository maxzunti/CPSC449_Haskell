{- |
Module      : ApocStrategyHuman
Description : Template for a game-playing strategy definition.
Copyright   : Copyright 2016, Rob Kremer (rkremer@ucalgary.ca), University of Calgary.
License     : Permission to use, copy, modify, distribute and sell this software
              and its documentation for any purpose is hereby granted without fee, provided
              that the above copyright notice appear in all copies and that both that
              copyright notice and this permission notice appear in supporting
              documentation. The University of Calgary makes no representations about the
              suitability of this software for any purpose. It is provided "as is" without
              express or implied warranty.
Maintainer  : rkremer@ucalgary.ca
Stability   : experimental
Portability : ghc 7.10.2 - 7.10.3

This module is used for CPSC 449 for the Apocalypse assignment.

This is merely a skeleton to get you started on creating a strategy for playing the
Apocalypse game.  It has VERY little functionality.
-}

module ApocStrategyHuman where


import Data.Char
import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
import System.IO.Unsafe
import ApocTools

{- | This is just a placeholder for the human strategy: it always chooses to play
     (0,0) to (2,1).
-}
-- Need to check move legality here

human    :: Chooser
human b Normal        c = do
      x <- getInput Normal
      return (Just x)

human b PawnPlacement c = do
  x <- getInput PawnPlacement
  return (Just x)


getInput :: PlayType -> IO[(Int, Int)]
getInput p = do
  putStrLn "Enter the move coordinates for player ___ in the form"
  input <- getLine
  pos <- stringToList input p
  return pos


stringToList :: String -> PlayType -> IO[(Int, Int)]
stringToList x p = if (p == Normal)
                  then if (length (words x) /= 4)
                        then do return [(20,20),(20,20)]
                        else do return [(readNumbers x !! 0,readNumbers x !! 1),(readNumbers x !! 2,readNumbers x !! 3)]
                  else if (length (words x) /= 2)
                        then do return [(20,20)]
                        else do return [(readNumbers x !! 0, readNumbers x !! 1)]

--takes a string
readNumbers :: String -> [Int]
readNumbers s = if (checkInHum (words s))
                then map toInt (words s)
                else [20, 20, 20, 20]

checkInHum :: [String] -> Bool
checkInHum [] = False
checkInHum (x:xs) | length (x:xs) == 4 && checkStrsIsInt (x:xs) = True
                  | length (x:xs) == 2 && checkStrsIsInt (x:xs) = True
                  | otherwise = False

checkStrIsInt :: String -> Bool
checkStrIsInt []     = True
checkStrIsInt (x:xs) = if (isNumber x)
                      then checkStrIsInt xs
                      else False

checkStrsIsInt :: [String] -> Bool
checkStrsIsInt []     = True
checkStrsIsInt (x:xs) = if (checkStrIsInt x)
                      then checkStrsIsInt xs
                      else False

toInt :: String -> Int
toInt x = read x :: Int
