module Main where

import Src.Features (rollDice, customDice, multipleDice, rollWithChance)
import Src.ErrorChecking (menuValidation)
import Src.Misc (rollingAesthethic)

-- Display main menu and pass a string to menuLoop function
menu :: IO String
menu = do
    putStrLn "  __________________________________________________________________________"
    putStrLn "/\\                                                                          \\"
    putStrLn "\\_|                  ~~~ Welcome to Dice Roller App ~~~                      |"
    putStrLn "  |                                                                          |"
    putStrLn "  |                            ____                                          |"
    putStrLn "  |                           /\\' .\\    _____                                |"
    putStrLn "  |                          /: \\___\\  / .  /\\                               |"
    putStrLn "  |                          \\' / . / /____/..\\                              |"
    putStrLn "  |                           \\/___/  \\'  '\\  /                              |"
    putStrLn "  |                                    \\'__'\\/                               |"
    putStrLn "  |                                                                          |"
    putStrLn "  |                                                                          |"
    putStrLn "  |      This application allows user to roll dice based on preference.      |"
    putStrLn "  |     Dice representation is nDm, where n = number of dice, D = dice,      |"
    putStrLn "  |    m = dice face value. There is also a bonus function where you can     |"
    putStrLn "  |      test your luck! To begin, please select your choice based on        |"
    putStrLn "  |                         numbered options below                           |"
    putStrLn "  |                                                                          |"
    putStrLn "  |                         [1] Roll Custom Dice                             |"
    putStrLn "  |                         [2] Roll Multiple Dice                           |"
    putStrLn "  |                         [3] Test Your Luck!                              |"
    putStrLn "  |                                                                          |"
    putStrLn "  |                         [0] Exit                                         |"
    putStrLn "  |                                                                          |"
    putStrLn "  |   _______________________________________________________________________|_"
    putStrLn "   \\_/________________________________________________________________________/"
    getLine

customDiceMenu :: IO ()
customDiceMenu = do
    putStrLn "  __________________________________________________________________________"
    putStrLn "/\\                                                                          \\"
    putStrLn "\\_|                       ~~~ Roll Custom Dice ~~~                           |"
    putStrLn "  |                                                                          |"
    putStrLn "  |       The freedom is yours! Here you can create your own dice which      |"
    putStrLn "  |        has your own face value. You will able to set how many dice       |"
    putStrLn "  |     to roll. The program will generate a random result based on your     |"
    putStrLn "  |                            dice preference                               |"
    putStrLn "  |                                                                          |"
    putStrLn "  |                      May the RNGesus be with you!                        |"
    putStrLn "  |                                                                          |"
    putStrLn "  |   _______________________________________________________________________|_"
    putStrLn "   \\_/________________________________________________________________________/"

multipleDiceMenu :: IO ()
multipleDiceMenu = do
    putStrLn "  __________________________________________________________________________"
    putStrLn "/\\                                                                          \\"
    putStrLn "\\_|                     ~~~ Roll Multiple Dice ~~~                           |"
    putStrLn "  |                                                                          |"
    putStrLn "  |        In this section, you will be able to choose how many dice         |"
    putStrLn "  |           to roll per dice type: D4, D6, D8, D10, D12 and D20.           |"
    putStrLn "  |    This will be useful for example when you are playing RPG game that    |"
    putStrLn "  |       requires a set of dice to roll. To begin, enter the number         |"
    putStrLn "  |                    of dice per type when prompted                        |"
    putStrLn "  |                                                                          |"
    putStrLn "  |                      May the RNGesus be with you!                        |"
    putStrLn "  |                                                                          |"
    putStrLn "  |   _______________________________________________________________________|_"
    putStrLn "   \\_/________________________________________________________________________/"

testLuckMenu :: IO String
testLuckMenu = do
    putStrLn "  __________________________________________________________________________"
    putStrLn "/\\                                                                          \\"
    putStrLn "\\_|                      ~~~ Test Your Luck! ~~~                             |"
    putStrLn "  |                                                                          |"
    putStrLn "  |        Choose a chance percentage to determine your passing rate.        |"
    putStrLn "  |     The program will then generate a random value and compare with       |"
    putStrLn "  |    your chance percentage. To begin, select one percentage from below    |"
    putStrLn "  |                                                                          |"
    putStrLn "  |                              [1] 10%                                     |"
    putStrLn "  |                              [2] 30%                                     |"
    putStrLn "  |                              [3] 50%                                     |"
    putStrLn "  |                              [4] 80%                                     |"
    putStrLn "  |                              [5] 90%                                     |"
    putStrLn "  |                                                                          |"
    putStrLn "  |                           [0] Main Menu                                  |"
    putStrLn "  |                                                                          |"
    putStrLn "  |   _______________________________________________________________________|_"
    putStrLn "   \\_/________________________________________________________________________/"
    getLine

-- User selection case function
menuChoices :: String -> IO ()
menuChoices choice =
    case choice of
        "1" -> do
            customDiceMenu
            customDice
            menuLoop
        "2" -> do
            multipleDiceMenu
            multipleDice
            menuLoop
        "3" -> testLuckLoop
        "0" -> rollingAesthethic "\nThank you for using Dice Roller App! Exiting"
        _ -> menuValidation menuLoop

-- User selection case function
testLuckChoices :: String -> IO ()
testLuckChoices choice =
    case choice of
        "1" -> do
            rollWithChance "1"
            testLuckLoop
        "2" -> do
            rollWithChance "3"
            testLuckLoop
        "3" -> do
            rollWithChance "5"
            testLuckLoop
        "4" -> do
            rollWithChance "8"
            testLuckLoop
        "5" -> do
            rollWithChance "9"
            testLuckLoop
        "0" -> menuLoop
        _ -> menuValidation testLuckLoop

-- Recursion function for menu
menuLoop :: IO ()
menuLoop =  do
    menu >>= menuChoices

-- Recursion function for menu
testLuckLoop :: IO ()
testLuckLoop =  do
    testLuckMenu >>= testLuckChoices

main :: IO ()
main = initializeApp

initializeApp = do
    menuLoop
