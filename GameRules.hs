-- Also empty!
--
module GameRules where

import ApocTools

{-
 - IMPORTANT NOTES:
 - The only functions that (I think) need to be called from OUTSIDE this
 - module are:
 -
 - isValidMove :: [(Int,Int)] -> GameState -> PlayType -> Player -> Result
 -      --> Takes a list of coordinates, a GameState, and a PlayType.
 -          Returns either VALID, INVALID, or CAPTURE if the PlayType is Normal,
 -          and returns either VALID or INVALID if the PlayType is PawnPlacement.
 -
 - checkPawnUpgrade :: GameState -> Player -> Maybe((Int,Int))
 -      --> Checks if any pawns owned by the specified Player have reached the
 -          furthest Rank and should upgraded. Returns a Maybe((Int,Int)) which
 -          either contains Nothing (if no pawns are eligible) or contains the
 -          coordinates of the pawn to be upgraded.
 -
 - upgradeOrReplace :: GameState -> Player -> UpgradeResult
 -      --> If checkPawnUpgrade returns a location (i.e. is not Nothing), then
 -          call upgradeOrReplace to count Knights and determine if we should either
 -          replace the pawn or upgrade it
 -
 - checkForWinner -> GameState -> WinState
 -      --> Checks if either player has won the game; returns BLACK, WHITE, or NONE. DRAW
 -          is currently unused.
 -          NOTE THAT THIS DOESNT CHECK IF BOTH PLAYERS PASS, that needs to be done after
 -          both players take their turns.
 -
 - (MAYBE? I'm not sure if you guys will need this one)
 - countPieces -> GameState -> Piece -> Int
 -      --> Returns the number of pieces of a given type.
 -
 - If it turns out you guys do need some of the other functions in here, that should be fine,
 - just be careful with the recursive ones (i.e. match the numbers initally passed by the "wrappers")
 - -}

--Custom Types------------------------------------------------------
data Result = VALID | INVALID | CAPTURE deriving (Eq)
-- This is a different type than Result because we need to get the result for a pawn move,
-- perform a capture if appropriate, and THEN check if we reach the last rank
-- (Example case: pawn CAPTURES a unit in the last rank, then needs to capture AND upgrade)
data UpgradeResult = UPGRADE | REPLACE  deriving (Eq)
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
                   else playerOf (ourPieceOf (getFromBoard (theBoard g) l)) == p


-- At this point, the first list is certain to have TWO 2-tuples which
-- are both in bounds (and owned by the appropriate player)
isValidNormalMove :: [(Int,Int)] ->  GameState -> Player -> Result
isValidNormalMove (b:a:[]) g p = if  ((ourPieceOf (getFromBoard (theBoard g) b) == WhitePawn) ||
                                      (ourPieceOf (getFromBoard (theBoard g) b) == BlackPawn))
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
                  else if (playerOf (ourPieceOf (getFromBoard (theBoard g) l)) == p) --Nonempty
                  then INVALID  -- Friendly square
                  else CAPTURE  -- Enemy square


--XXX: What happens if we pick and invalid square?
-- Apparently have BadPlacedPawn; will need to check against in calling function (i.e. mainLoop)
isValidPawnPlacement :: (Int,Int) -> GameState -> Result
isValidPawnPlacement l g = if (getFromBoard (theBoard g) l == E)
                           then VALID
                           else INVALID

-- See if any pawns should be upgraded; if so, return the location of the pawn to upgrade
checkPawnUpgrade :: GameState -> Player -> Maybe((Int,Int))
checkPawnUpgrade g p = if (p == White) --White Far rank == 4
                       then checkPawnInRank g WhitePawn 4 4
                       else checkPawnInRank g BlackPawn 4 0 -- Black far rank = 0

checkPawnInRank :: GameState -> Piece -> Int -> Int -> Maybe((Int,Int))
checkPawnInRank g p 0 y = if (ourPieceOf (getFromBoard (theBoard g) (0,y)) == p)
                          then Just (0,y)
                          else Nothing
checkPawnInRank g p x y = if (ourPieceOf (getFromBoard (theBoard g) (x,y)) == p)
                          then Just (x,y)
                          else checkPawnInRank g p (x-1) y

upgradeOrReplace :: GameState -> Player -> UpgradeResult
upgradeOrReplace g p = if (p == White)
                       then if (countPieces g WhiteKnight < 2)
                           then UPGRADE
                           else REPLACE
                       else if (countPieces g BlackKnight < 2)
                           then UPGRADE
                           else REPLACE



-- Check GameState to see if we have a winner
-- NOTE: Doesn't check if both players pass
checkForWinner :: GameState -> WinState
checkForWinner g = if ((countPieces g WhitePawn == 0) || (whitePen g >= 2))
                   then BLACK
                   else if ((countPieces g BlackPawn == 0) || (blackPen g >= 2))
                   then WHITE
                   else NONE


-- Counts the number of specific pieces on the board
-- (Note that Piece = BlackKnight | WhiteKnight | BlackPawn | WhitePawn)
countPieces :: GameState ->  Piece -> Int
countPieces g p = countPieceHelper g p 4 4 -- 4 4 corresponds to max (x,y) pos

-- Recursive function to go through the entire board
countPieceHelper :: GameState -> Piece -> Int -> Int -> Int
countPieceHelper g p x y = if (x > 0)
                               then if (y > 0)
                               then (matchingPiece g p (x,y)) + (countPieceHelper g p x (y-1))
                               else (matchingPiece g p (x,y)) + (countPieceHelper g p (x-1) 4)
                           -- x == 0
                           else if (y > 0)
                           then (matchingPiece g p (x,y)) + (countPieceHelper g p x (y-1))
                           else matchingPiece g p (x,y) -- x == y == 0

matchingPiece :: GameState -> Piece -> (Int,Int) -> Int
matchingPiece g p l = if (ourPieceOf (getFromBoard (theBoard g) l) == p)
                      then 1
                      else 0

-- Overridden functions from ApocTools.hs
-- These functions are only used as clones of functions in ApocTools which hopefully circumvent a few errors
ourPieceOf :: Cell -> Piece
ourPieceOf BK = BlackKnight 
ourPieceOf WK = WhiteKnight 
ourPieceOf BP = BlackPawn 
ourPieceOf WP = WhitePawn
ourPieceOf _ = BlackKnight -- Bad result, but we shouldn't hit it
