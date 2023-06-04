module Main where

import Actions
import Lib
import Types
--import Control.Monad.State
import System.IO

-- testing for rotors
main :: IO ()
main = do
        hSetBuffering stdout NoBuffering
        hSetBuffering stdin  NoBuffering
        rs <- setMachine
        operate 0 rs
       

{-}

testHarness :: IO ()
testHarness = do
               c <- getChar
               case isValidInput c of Just c' -> do 
                                                  go c'
                                                  letter <- get
                                                  putChar $ intToChar letter
                                      Nothing -> main
               where
                go d = runState (pressKey $ charToInt d) (0, [testRotorI])
-}