module StartupMod where

import System.Exit (exitSuccess)

listOfStrategy :: [String]
listOfStrategy = ["human", "greedy"]

--check cmd line arg and go to correct state
cmdLineArgs :: [String] -> IO()
cmdLineArgs [] = interactiveMode --If no cmd go to interactive mode
cmdLineArgs (x:y:[]) = if ((elem x listOfStrategy) && (elem y listOfStrategy)) --if two elements check if they are valid stratigies
                  then putStrLn "Valid strategy names" -- if Valid set strategy NOT DONE
                  else invalidStrategyNames -- not valid go to invalidStrategyNames
cmdLineArgs xs = invalidStrategyNames -- not 0 or tow arg go to invalidStrategyNames

--interactive mode
interactiveMode :: IO()
interactiveMode = do
                    putStrLn "Welcome to Apocalypse!"
                    printStrategyNames listOfStrategy
                    putStrLn "Black Strategy: "
                    blkStrat <- getLine
                    putStrLn "White Strategy: "
                    whtStrat <-getLine
                    if ((elem blkStrat listOfStrategy) && (elem whtStrat listOfStrategy)) --if two elements check if they are valid stratigies
                    then putStrLn "Valid strategy names" -- if Valid set strategy NOT DONE
                    else invalidStrategyNames -- not valid go to invalidStrategyNames

invalidStrategyNames :: IO()
invalidStrategyNames = do
      putStrLn "You did not enter valid strategy names"
      printStrategyNames listOfStrategy
      exitSuccess

--TODO
--setStrategies :: String -> String -> IO()

--print a list of string and add two space at the begining
printStrategyNames :: [String] -> IO()
printStrategyNames [] = return ()
printStrategyNames (x:xs) = do
                              putStrLn ("  " ++ x)
                              printStrategyNames xs
