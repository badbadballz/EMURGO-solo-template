module Main where

import Actions
import Lib
import Types
import System.IO
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.IO as TO

main :: IO ()
main = do
        hSetBuffering stdout NoBuffering
        hSetBuffering stdin  NoBuffering
        putStrLn "Check README for more in-depth information"
        rs <- setMachine
        let [_,r2,r1,r0,_,_,_,_,_] = rs
        printMachine 0 T.empty T.empty [] [r2,r1,r0]
        operate 0 T.empty T.empty rs
       

