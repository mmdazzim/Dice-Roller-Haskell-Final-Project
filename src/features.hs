module Src.Features where

import Src.Misc (pressAnyKey, rollingAesthethic)
import Src.ErrorChecking (promptUser, checkNumber)


import Control.Monad.Random
import Control.Monad (forM_, replicateM)
import Control.Concurrent ( threadDelay )
import System.IO

-- the main function to roll a dice where n is the number of dice to roll
-- and m is the dice face value
rollDice :: MonadRandom m => Int -> Int -> m [Int]
rollDice n m = replicateM n $ getRandomR (1, m)

-- This function prompt the user two values
-- which then passed into rollDice function for generating the result
-- evalRandIO converts the monad into value Int
customDice :: IO ()
customDice = do
    faceVal <- promptUser "\nEnter dice face value: " checkNumber
    diceNum <- promptUser "\nEnter number of dice:  " checkNumber
    rollingAesthethic "\nRolling your fate"
    executeRoll <- evalRandIO (rollDice (read diceNum :: Int) (read faceVal :: Int))
    putStrLn "++++++++++++++++++++++++++++++++++++++++++++"
    putStr "Each dice result:"
    hFlush stdout
    putStrLn ("\t" ++ diceNum ++ "D" ++ faceVal ++ ": "++ (show executeRoll))
    putStr "++++++++++++++++++++++++++++++++++++++++++++"
    putStrLn ("\n\tTotal Dice Value Result: " ++ (show (sum executeRoll))) -- sum the list of all value received
    putStrLn "++++++++++++++++++++++++++++++++++++++++++++"
    pressAnyKey

-- function that execute multiple dice rolling in which will display
-- sum of total dice result and each of the dice value
multipleDice :: IO ()
multipleDice = do
    let d4 = 4
    let d6 = 6
    let d8 = 8
    let d10 = 10
    let d12 = 12
    let d20 = 20

    putStrLn "\nInput number of dice per type"
    -- this block is prompting the user for integer value
    numD4 <- promptUser "D4: " checkNumber
    valueD4 <- evalRandIO (rollDice (read numD4 :: Int) d4) -- evalRand converts the IO() into Int value
    numD6 <- promptUser "D6: " checkNumber
    valueD6 <- evalRandIO (rollDice (read numD6 :: Int) d6)
    numD8 <- promptUser "D8: " checkNumber
    valueD8 <- evalRandIO (rollDice (read numD8 :: Int) d8)
    numD10 <- promptUser "D10: " checkNumber
    valueD10 <- evalRandIO (rollDice (read numD10 :: Int) d10)
    numD12 <- promptUser "D12: " checkNumber
    valueD12 <- evalRandIO (rollDice (read numD12 :: Int) d12)
    numD20 <- promptUser "D20: " checkNumber
    valueD20 <- evalRandIO (rollDice (read numD20 :: Int) d20)

    rollingAesthethic "\nRolling dice" -- pure aesthethic

    -- display results with the calculation of total value
    putStrLn "++++++++++++++++++++++++++++++++++++++++++++"
    putStr "Each dice result:"
    hFlush stdout
    putStrLn ("\t" ++ numD4 ++ "D4: " ++ (show valueD4) -- each contains list of integer
            ++ "\n\t\t\t" ++ numD6 ++ "D6: " ++ (show valueD6)
            ++ "\n\t\t\t" ++ numD8 ++ "D8: " ++ (show valueD8)
            ++ "\n\t\t\t" ++ numD10 ++ "D10: " ++ (show valueD10)
            ++ "\n\t\t\t" ++ numD12 ++ "D12: " ++ (show valueD12)
            ++ "\n\t\t\t" ++ numD20 ++ "D20: " ++ (show valueD20))
    putStr "++++++++++++++++++++++++++++++++++++++++++++"
    putStrLn ("\n\tTotal Dice Value Result: " ++ (show ((sum valueD4) + (sum valueD6) + (sum valueD8) + (sum valueD10) + (sum valueD12) + (sum valueD20)))) -- sum the list of all value received
    putStrLn "++++++++++++++++++++++++++++++++++++++++++++"
    pressAnyKey -- pure aesthethic

-- Function that takes in two integers and returns boolean for chanceResult function
checkIntRange :: Int -> Int -> Bool
checkIntRange x y
    | x < y = True
    | otherwise = False

-- Converts [Int] to Int
fromDigits = foldl addDigit 0
   where addDigit num d = 10*num + d

-- Pattern matching functions to pass specific arguments
rollWithChance :: String -> IO ()
rollWithChance "1" = chanceResult 1
rollWithChance "3" = chanceResult 3
rollWithChance "5" = chanceResult 5
rollWithChance "8" = chanceResult 8
rollWithChance "9" = chanceResult 9

-- A variation of rollDice function which includes zero as the range
rollChance :: MonadRandom m => Int -> Int -> m [Int]
rollChance n m = replicateM n $ getRandomR (0, m)

-- Function with if else condition checking checking passing chance
chanceResult :: Int -> IO ()
chanceResult x = do
    putStrLn ("\n.:You have selected " ++ (show x) ++ "0% as your lucky chance:.\n")
    threadDelay 1000000
    rollingAesthethic "Brace yourself, the fate is rolling"
    
    executeRoll <- evalRandIO (rollChance 1 10) -- 0 to 10 is equivalent to 0% to 100%
    let passCheck = checkIntRange (fromDigits executeRoll) x -- get boolean based on rolled value compared with chance rate
    if passCheck
        then putStrLn ("PASS! You're LUCKY! \t** You rolled " ++ (show (fromDigits executeRoll)) ++ " **") --if True display this
        else putStrLn ("FAIL! You're not so LUCKY! \t** You rolled " ++ (show (fromDigits executeRoll)) ++ " **") -- if False display this
    pressAnyKey

