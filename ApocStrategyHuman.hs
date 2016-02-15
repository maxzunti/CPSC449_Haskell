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

-- Need to check move legality here
-- Gets input from user and return
human    :: Chooser
human b Normal        c = do
      x <- getInput Normal c
      return (Just x)
human b PawnPlacement c = do
  x <- getInput PawnPlacement c
  return (Just x)

-- make char into String
charToString :: Char -> String
charToString c = [c]

-- Get string of player for print
playerToStr :: Player -> String
playerToStr p | p == White = "White"
              | p == Black = "Black"
              | otherwise = "notValidPlayer"

--gets the input of the player
getInput :: PlayType -> Player -> IO[(Int, Int)]
getInput pt pl = do
  if (pt == Normal)
  then putStrLn ("Enter the move coordinates for player " ++ (playerToStr pl) ++ " in the form 'srcX srcY destX destY' [0 >= n >= 4, or just enter return for a 'pass'] " ++ (charToString ((playerToStr pl) !! 0)) ++ "2:")
  else putStrLn ("Enter the coordinates to place the pawn for player " ++ (playerToStr pl) ++ " in the form 'destX destY': [0 >= n >= 4] " ++ (charToString ((playerToStr pl) !! 0)) ++ "1:")
  input <- getLine
  if (checkEnoughVar (words input) pt && checkVarIsNum (words input) pt)
  then do pos <- convertMoveType (words input) pt
          return pos
  else do
    putStrLn "You have entered an invalid input! Try again" 
    getInput pt pl

--Check that their is Enough varibles for the move
checkEnoughVar :: [String] -> PlayType -> Bool
checkEnoughVar [] pt = False
checkEnoughVar (x:xs) pt | length (x:xs) < 4 && (pt == Normal) = False
                      | length (x:xs) < 2 && (pt == PawnPlacement) = False
                      | otherwise = True

--Checks that the varibles can be converted into a integer
--Checks if the integer is in the bound of the board
checkVarIsNum :: [String] -> PlayType -> Bool
checkVarIsNum x pt = if (pt == Normal)
  then if checkStrsIsInt (fst (splitAt 4 x) )
      then checkIntsBound (readStrNumbers (fst (splitAt 4 x) ))
      else False
  else if checkStrsIsInt (fst (splitAt 2 x) )
      then checkIntsBound (readStrNumbers (fst (splitAt 2 x) ))
      else False

--turn String into a integer
readStrNumbers :: [String] -> [Int]
readStrNumbers s = map toInt s

--check if a list of int is in the bound of the board
checkIntsBound :: [Int] -> Bool
checkIntsBound [] = True
checkIntsBound (x:xs) = if (checkIntBound x)
                      then checkIntsBound xs
                      else False

--check if a int is in the bound of the board
checkIntBound :: Int -> Bool
checkIntBound x = if (x >= 0 && x <= 4)
  then True
  else False

--Turn a list of strings into the type of a move
convertMoveType :: [String] -> PlayType -> IO[(Int, Int)]
convertMoveType x p = if (p == Normal)
                  then do return [(toInt (x !! 0), toInt (x !! 1)),(toInt (x !! 2), toInt (x !! 3))]
                  else do return [(toInt (x !! 0), toInt (x !! 1))]

--Checks if one String contains on numbers
checkStrIsInt :: String -> Bool
checkStrIsInt []     = True
checkStrIsInt (x:xs) = if (isNumber x)
                      then checkStrIsInt xs
                      else False

--Check if an entire String conntains only numbers
checkStrsIsInt :: [String] -> Bool
checkStrsIsInt []     = True
checkStrsIsInt (x:xs) = if (checkStrIsInt x)
                      then checkStrsIsInt xs
                      else False

--Turns a String into an Int
toInt :: String -> Int
toInt x = read x :: Int
