module ApocStrategyKnightmare where

-- | Import System.Random
import Data.List
import ApocTools
import GameRules
import ApocStrategySimple
import System.Random

-- | Need to check move legality here
-- | Gets input from user and return
-- | Most knight code is in the ApocStrategySimple  
knightStrat    :: Chooser
knightStrat (GameState bplay bpenalty wplay wpenalty board) Normal p =
            if (p == White)
            then do 
                                  gen <- newStdGen
                                  return (Just (if (length (getAllPieceCoor 0 WK board) == 0)
                                  then getWPMove gen 0 0 Normal (GameState bplay bpenalty wplay wpenalty board)
                                  else getWKMove gen 0 0 (GameState bplay bpenalty wplay wpenalty board) ))
            else do 
                                  gen <- newStdGen
                                  return (Just (if (length (getAllPieceCoor 0 BK board) == 0)
                                  then getBPMove gen 0 0 Normal (GameState bplay bpenalty wplay wpenalty board)
                                  else getBKMove gen 0 0 (GameState bplay bpenalty wplay wpenalty board) ))


--  | test the strat knightmare 
--testFunc :: [(Int, Int)]
--testFunc = getWPMove 0 0 Normal initBoard
--testFunc = getWKMove 0 0 initBoard
