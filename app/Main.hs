module Main where

import Actions
import Lib
import Types
--import Control.Monad.State
import System.IO
import System.Console.ANSI (clearScreen)

-- testing for rotors
main :: IO ()
main = do
        hSetBuffering stdout NoBuffering
        hSetBuffering stdin  NoBuffering
        rs <- setMachine
        let [_,r2,r1,r0,_,_,_,_,_] = rs
        printMachine 0 [] [] [] [r2,r1,r0]
        operate 0 [] [] rs
       

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