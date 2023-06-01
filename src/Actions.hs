module Actions where

import Lib
import Types
import Data.Char
{-
setMachine :: IO ()
setMachine = do
                chooseRotors 
-}

chooseRotor0 :: IO Rotor
chooseRotor0 = do
                putStrLn ""
                putStrLn "Choose first rotor (1 - 5)"
                c <- getChar
                if isDigit c then 
                 let d = read [c] :: Int
                 in go d
                else chooseRotor0
                where
                    go n 
                     | 1 <= n && n <= 5 = return (Rotor 0 0 0 0 0 testRotorI) 
                     | otherwise = chooseRotor0
              

