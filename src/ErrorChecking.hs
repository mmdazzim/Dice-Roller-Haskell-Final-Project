module Src.ErrorChecking where

import Data.Char (isDigit, isSpace)
import Control.Concurrent ( threadDelay )
import System.IO

-- Function to provide getLine with validity checking
promptUser :: [Char] -> (String -> Bool) -> IO String
promptUser labVal checkValid = run 
  where
    run = do
      putStr labVal
      hFlush stdout
      value <- getLine -- User Input
      if checkValid value -- If else output
        then pure value -- Returns original value
        else do -- execute this if bool is FALSE
          putStrLn $ "\nVALUE ERROR! PLEASE ENTER AN INTEGER.."
          run

-- checkNumber is a number only validation
checkNumber :: Foldable t => t Char -> Bool
checkNumber x = (all isDigit x) && not (all isSpace x)

-- This function display an error message to user for wrong input, it will then execute
-- a function 
menuValidation :: IO f -> IO f
menuValidation function = do
  putStr "\nWrong input, please try again."
  hFlush stdout
  threadDelay 750000
  putStr "."
  hFlush stdout
  threadDelay 750000
  putStrLn "."
  threadDelay 1000000
  function -- function to be executed