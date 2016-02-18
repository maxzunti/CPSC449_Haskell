module StartupMod where

import System.Exit (exitSuccess)

listOfStrategy :: [String]
listOfStrategy = ["human", "greedy", "random"]
data Strat = HUMAN | GREEDY | RANDOMSTRAT | INVALID deriving (Eq)



-- Return a list of 2 strategies
getStrategies :: [String] -> IO [Strat]
getStrategies [] = interactiveMode
-- "do return" needed for IO [Strat] signature
getStrategies (x:y:[]) = do return $ (stringToStrat x):(stringToStrat y):[]
getStrategies s = do return $ INVALID:INVALID:[]


-- Translate a string to an elem of type Strat
stringToStrat :: String -> Strat
stringToStrat s | (s == "human") = HUMAN
                | (s == "greedy") = GREEDY
                | (s == "random") = RANDOMSTRAT
                | otherwise = INVALID -- Shouldn't be able to hit this


--interactive mode
interactiveMode :: IO [Strat]
interactiveMode = do
                    putStrLn "\nWelcome to Apocalypse!\n"
                    putStrLn "Apocalypse is a variant on the classic chess game. The players each start with two horsemen and five footmen on a 5×5 board. The two sides make their moves simultaneously."
                    putStrLn "\nThe Rules:"
                    putStrLn "- If they moved to the same square, a horseman captures a footman. Same-type pieces are both removed from the board."
                    putStrLn "- If a capture was declared using a footman, but the piece to be captured moved from its square, the footman move still stands. (The move converts to a diagonal step instead of a capture.)"
                    putStrLn "- If a declared move is illegal, the player incurs a penalty point."
                    putStrLn "- A footman promotes to horseman when reaching the last rank, but only when the player has less than two horsemen. Otherwise the player must redeploy the footman to any vacant square."
                    putStrLn "A player wins by being first to eliminate all of the opponent's footmen. Accumulating two penalty points forfeits the game. A stalemate is a draw."
                    putStrLn "\nHere is a list of strategies"
                    printStrategyNames listOfStrategy
                    putStrLn "Choose a Strategy for the Black Side:"
                    blkStrat <- getLine
                    putStrLn "Choose a Strategy for the White Side:"
                    whtStrat <- getLine
                    putStrLn ""
                    return $ (stringToStrat blkStrat):(stringToStrat whtStrat):[]


invalidStrategyNames :: IO()
invalidStrategyNames = do
      putStrLn "You did not enter valid strategy names"
      printStrategyNames listOfStrategy
      exitSuccess


checkInvalid :: [Strat] -> IO()
checkInvalid [] = do
                putStrLn "I borked" -- shouldn't happen
                exitSuccess
checkInvalid (x:y:[]) = if (x == INVALID) then invalidStrategyNames
                      else if (y == INVALID) then invalidStrategyNames
                      else putStr "" --do nothing
checkInvalid s = do
                putStrLn "I borked different" -- shouldn't happen
                exitSuccess


--print a list of string and add two space at the begining
printStrategyNames :: [String] -> IO()
printStrategyNames [] = return ()
printStrategyNames (x:xs) = do
                              putStrLn ("  " ++ x)
                              printStrategyNames xs
