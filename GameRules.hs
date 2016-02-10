-- Also empty!
--
module GameRules where

import ApocTools

--Custom Types------------------------------------------------------
data Result = VALID | INVALID | CAPTURE deriving (Eq)
data WinState = BLACK | WHITE | DRAW | NONE deriving (Eq) 

-- MOVE LEGALITY CHECKERS
-- Filter move to Normal or PawnPlacement checkers
isValidMove :: [(Int,Int)] -> [(Int,Int)] -> GameState -> PlayType -> Player -> Result
isValidMove i o g t p =
                     if (checkBounds o)
                        then if (pieceOwned i g p)
                            then if (t == Normal)
                            then isValidNormalMove i o g p
                            else isValidPawnPlacement i g
                        else INVALID
                     else INVALID

-- First, check output boundaries
checkBounds :: [(Int,Int)] -> Bool
checkBounds _ = True

-- Check if input position corresponds to a player's piece
pieceOwned :: [(Int,Int)] -> GameState -> Player -> Bool
pieceOwned _ _ _ = True


-- TODO
isValidNormalMove :: [(Int,Int)] -> [(Int,Int)] -> GameState -> Player -> Result
isValidNormalMove _ _ _ _ = VALID

-- TODO
isValidPawnPlacement :: [(Int,Int)] -> GameState -> Result
isValidPawnPlacement _ _ = INVALID

-- Check GameState to see if we have a winner
checkForWinner :: GameState -> WinState
checkForWinner  _ = NONE
