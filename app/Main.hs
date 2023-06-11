module Main where

import Actions
import Lib
import Types
import System.IO (hSetBuffering, stdout, stdin, BufferMode(NoBuffering))
import Control.Monad.State

main :: IO ()
main = do
        hSetBuffering stdout NoBuffering
        hSetBuffering stdin  NoBuffering
        putStrLn "Check README for more in-depth information"
        ms <- setMachine' 
        runStateT (printMachine' []) ms
        operate' ms
        
       

