module StartupMod where

import System.Exit (exitSuccess)

listOfStrategy :: [String]
listOfStrategy = ["human", "greedy"]
data Strat = HUMAN | GREEDY | INVALID deriving (Eq)


-- Return a list of 2 strategies
getStrategies :: [String] -> IO [Strat]
getStrategies [] = interactiveMode
-- "do return" needed for IO [Strat] signature
getStrategies (x:y:[]) = do return $ (stringToStrat x):(stringToStrat y):[]
getStrategies s = do return $ INVALID:INVALID:[]


-- Translate a string to an elem of type Strat
stringToStrat :: String -> Strat
stringToStrat s = if (s == "human")
                  then HUMAN
                  else if (s == "greedy")
                  then GREEDY
                  else INVALID -- Shouldn't be able to hit this


--interactive mode
interactiveMode :: IO [Strat]
interactiveMode = do
                    putStrLn "Welcome to Apocalypse!"
                    printStrategyNames listOfStrategy
                    putStrLn "Black Strategy: "
                    blkStrat <- getLine
                    putStrLn "White Strategy: "
                    whtStrat <- getLine
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
