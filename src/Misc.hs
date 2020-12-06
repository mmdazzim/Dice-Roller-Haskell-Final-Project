module Src.Misc where

import Control.Concurrent ( threadDelay )
import System.IO

-- A function to allow screen pause before a key is entered
pressAnyKey :: IO ()
pressAnyKey = do
    putStr "\nPress ENTER To Continue..."
    hFlush stdout
    _ <- getLine
    putStrLn "\n"

-- Pure aesthethic function to display loading period
rollingAesthethic :: [Char] -> IO ()
rollingAesthethic label = do
    putStr (label)
    hFlush stdout
    threadDelay 1000000
    putStr "."
    hFlush stdout
    threadDelay 1000000
    putStr "."
    hFlush stdout
    threadDelay 1000000
    putStrLn ".\n"
    threadDelay 1000000