-- Also empty!
--
module GameRules where

import ApocTools

--Custom Types------------------------------------------------------
data Result = VALID | INVALID | CAPTURE deriving (Eq)
data WinState = BLACK | WHITE | DRAW | NONE deriving (Eq) 

-- MOVE LEGALITY CHECKERS
-- Filter move to Normal or PawnPlacement checkers
isValidMove :: [(Int,Int)] -> GameState -> PlayType -> Player -> Result
isValidMove i g t p = --Proceed iff none of the entries are out of bounds
                     if (null (filter outOfBounds i))
                        then if (pieceOwned (head i) g p)
                            then if (t == Normal)
                            then isValidNormalMove i g p
                            else isValidPawnPlacement (head i) g   --Otherwise, is PawnPlacement
                        else INVALID
                    else INVALID

-- Return "true" iff the specified tuple is out of bounds
outOfBounds :: (Int,Int) -> Bool
--checkBounds [] = False  -- Shouldn't hit this
outOfBounds l = if (((fst l <= 4) && (fst l >= 0))
                 && ((snd l <= 4) && (snd l >= 0)))
               then False
               else True
                
-- Check if input position corresponds to a player's piece
-- Already know piece is in bounds
pieceOwned :: (Int,Int) -> GameState -> Player -> Bool
pieceOwned l g p = if (getFromBoard (theBoard g) l == E)
                   then False
                   else playerOf (pieceOf (getFromBoard (theBoard g) l)) == p


-- At this point, the first list is certain to have TWO 2-tuples which
-- are both in bounds (and owned by the appropriate player)
isValidNormalMove :: [(Int,Int)] ->  GameState -> Player -> Result
isValidNormalMove (b:a:[]) g p = if  ((pieceOf (getFromBoard (theBoard g) b) == WhitePawn) ||
                                      (pieceOf (getFromBoard (theBoard g) b) == BlackPawn))
                                 then isValidPawnMove (b:a:[]) g p
                                 else isValidKnightMove (b:a:[]) g p
isValidNormalMove _ _ _ = INVALID -- Should not hit

-- Check specific knight/pawn movements
isValidKnightMove :: [(Int,Int)] -> GameState -> Player -> Result
isValidKnightMove (b:a:[]) g p = if (((fst a == fst b + 2) && (snd a == snd b + 1))  ||
                                     ((fst a == fst b + 2) && (snd a == snd b - 1 )) ||
                                     ((fst a == fst b - 2) && (snd a == snd b + 1 )) ||
                                     ((fst a == fst b - 2) && (snd a == snd b - 1 )) ||
                                     ((fst a == fst b + 1) && (snd a == snd b + 2 )) ||
                                     ((fst a == fst b + 1) && (snd a == snd b - 2 )) ||
                                     ((fst a == fst b - 1) && (snd a == snd b + 2 )) ||
                                     ((fst a == fst b - 1) && (snd a == snd b - 2 )))
                                 then checkDest a g p
                                 else INVALID

isValidPawnMove :: [(Int,Int)] -> GameState -> Player -> Result
-- Assuming White moves down (and therefore INCREASES)
isValidPawnMove (b:a:[]) g p = if (p == White)
                                   then if (snd a == snd b + 1) -- Must move 'down'
                                       then if (fst a == fst b) 
                                       then checkDest a g p
                                       else if ((checkDest a g p == CAPTURE) &&
                                               ((fst a == fst b + 1) || (fst a == fst b - 1)))
                                       then CAPTURE
                                       else INVALID
                                   else INVALID
                               else if (p == Black)
                                   then if (snd a == snd b - 1) -- Must move 'up'
                                       then if (fst a == fst b) 
                                       then checkDest a g p
                                       else if ((checkDest a g p == CAPTURE) &&
                                               ((fst a == fst b + 1) || (fst a == fst b - 1)))
                                       then CAPTURE
                                       else INVALID
                                   else INVALID
                               else INVALID --Really, REALLY shouldn't be reachable
                                   

-- See who owns the targeted cell
checkDest :: (Int,Int) -> GameState -> Player -> Result
checkDest l g p = if (getFromBoard (theBoard g) l == E)
                  then VALID    -- Empty square
                  else if (playerOf (pieceOf (getFromBoard (theBoard g) l)) == p) --Nonempty
                  then INVALID  -- Friendly square
                  else CAPTURE  -- Enemy square


--XXX: What happens if we pick and invalid square?
-- Apparently have BadPlacedPawn; will need to check against in calling function (i.e. mainLoop)
isValidPawnPlacement :: (Int,Int) -> GameState -> Result
isValidPawnPlacement l g = if (getFromBoard (theBoard g) l == E)
                           then VALID
                           else INVALID


-- Check GameState to see if we have a winner
checkForWinner :: GameState -> WinState
checkForWinner  _ = NONE


