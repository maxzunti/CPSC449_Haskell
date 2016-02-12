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

module ApocStrategyHuman (
   human
   ) where



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
      x <- getInput
      return (Just x)
--human b PawnPlacement c = return (Just getInput)
human b PawnPlacement c = return (Just [(2,2)])

getInput :: IO[(Int, Int)]
getInput = do
  putStrLn "Enter the move coordinates for player ___ in the form"
  input <- getLine
  pos <- stringToList input
  return pos


stringToList :: String -> IO[(Int, Int)]
stringToList x = do
        return [(head (readNumbers x), toInt "1")]

readNumbers :: String -> [Int]
readNumbers s = map toInt (words s)

toInt :: String -> Int
toInt x = read x :: Int
