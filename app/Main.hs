module Main where

import Actions
import Lib
import Types
import System.IO
import qualified Data.Sequence as Seq

main :: IO ()
main = do
        hSetBuffering stdout NoBuffering
        hSetBuffering stdin  NoBuffering
        putStrLn "Check README for more in-depth information"
        rs <- setMachine
        let [_,r2,r1,r0,_,_,_,_,_] = rs
        printMachine 0 Seq.empty Seq.empty [] [r2,r1,r0]
        operate 0 Seq.empty Seq.empty rs
       

