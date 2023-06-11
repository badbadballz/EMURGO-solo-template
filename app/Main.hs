module Main where

import Actions
import Lib
import Types
import System.IO (hSetBuffering, stdout, stdin, BufferMode(NoBuffering))
import Control.Monad.State
import System.Console.ANSI (setTitle, clearScreen)

main :: IO ()
main = do
        clearScreen
        hSetBuffering stdout NoBuffering
        hSetBuffering stdin  NoBuffering
        setTitle "haskell Enigma M3 simulator"
        putStrLn "Check README for more in-depth information"
        ms <- setMachine' 
        runStateT (printMachine' []) ms
        operate' ms
        
       

