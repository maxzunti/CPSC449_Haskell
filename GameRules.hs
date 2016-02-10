-- Also empty!
--
module GameRules where

import ApocTools

isValidMove :: GameState -> PlayType -> Player -> IO [(Int,Int)]
isValidMove g t p = do
                    putStrLn "Spaget!"
                    return $ (0,0):[]
