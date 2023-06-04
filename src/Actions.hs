module Actions where

import Lib
import Types
import Data.Char
import System.IO

operate :: Steps -> Rotors -> IO ()
operate stps rs = do            
                    putStrLn ""
                    c <- getChar
                    putStrLn ""
                    if isValidChar c
                    then do 
                            let c' = (charToInt . toUpper) c
                                ((stps', output, _), rs') = nextStep c' stps rs
                                alphas = map intToChar output
                            putStrLn (show stps' ++ " " ++ [head alphas] ++ " " ++ tail alphas)    
                            operate stps' rs'
                    else 
                        operate stps rs

-- assumes n = rotorNumber
setRotors :: Int -> IO [(Rotor, Rotor)]
setRotors 0 = return []
setRotors n = do
                tr <- chooseRotor (n - 1)
                trs <- setRotors (n - 1)
                return (tr : trs)

setMachine :: IO Rotors
setMachine = do
              trss <- setRotors rotorNumber
              let (r , ir) = unzip trss 
              return ([plugboard] ++ reverse r ++ [reflectorB] ++ ir ++ [plugboard]) 
              -- later implemenation can include programmable plugboard / reflector

-- i is the position of the rotor to be chosen
chooseRotor :: Int -> IO (Rotor, Rotor)
chooseRotor i = do
                putStrLn ""
                putStrLn ("Choose rotor " ++ show (i + 1) ++ " (1 - 5)")
                c <- getChar
                if isDigit c then 
                 let d = read [c] :: Int
                 in go d
                else chooseRotor i 
                where
                    go n 
                     | n == 1 = do 
                                r <- setRotor i 
                                let r' = r {turnover = r1Turnover, rotorWiring = setRing (ringSetting r) rotorIWiring}
                                    ir = r {turnover = r1Turnover, rotorWiring = setRing (ringSetting r) invRotorIWiring}
                                return (r' , ir)
                     | n == 2 = do 
                                r <- setRotor i
                                let r' = r {turnover = r2Turnover, rotorWiring = setRing (ringSetting r) rotorIIWiring} 
                                    ir = r {turnover = r2Turnover, rotorWiring = setRing (ringSetting r) invRotorIIWiring} 
                                return (r', ir) 
                     | n == 3 = do 
                                r <- setRotor i
                                let r' = r {turnover = r3Turnover, rotorWiring = setRing (ringSetting r) rotorIIIWiring} 
                                    ir = r {turnover = r3Turnover, rotorWiring = setRing (ringSetting r) invRotorIIIWiring} 
                                return (r', ir) 
                     | n == 4 = do 
                                r <- setRotor i
                                let r' = r {turnover = r4Turnover, rotorWiring = setRing (ringSetting r) rotorIVWiring}
                                    ir = r {turnover = r4Turnover, rotorWiring = setRing (ringSetting r) invRotorIVWiring}
                                return (r', ir)
                     | n == 5 = do 
                                r <- setRotor i
                                let r' = r {turnover = r5Turnover, rotorWiring = setRing (ringSetting r) rotorVWiring} 
                                    ir = r {turnover = r5Turnover, rotorWiring = setRing (ringSetting r) invRotorVWiring}
                                return (r', ir)       
                     | otherwise = chooseRotor i 

isValidChar :: Char -> Bool
isValidChar = isAlpha  

isValidNumber :: String -> Bool
isValidNumber s     
    | all isDigit s = let result = read s :: Int
                              in 1 <= result && result <= rotorSize
    | otherwise = False

convertNumber :: String -> Int
convertNumber s 
    | all isDigit s = read s :: Int 
    | otherwise = (-1)

convertChar :: Char -> Int
convertChar c 
    | isAlpha c = (charToInt . toUpper) c 
    | otherwise = (-1)  



setRotor :: Int -> IO Rotor
setRotor i = do
              putStrLn ""
              putStrLn ("Choose rotor " ++ show (i + 1) ++ " start position (A - Z)")
              c <- getChar
              if isValidChar c
              then do 
                    putStrLn ""
                    putStrLn ("Choose rotor " ++ show (i + 1) ++ " ring position (1 - 26)")
                    s <- getLine 
                    if isValidNumber s
                    then do
                          let sp = convertChar c 
                              rs = convertNumber s
                              r = Rotor i sp (rs - 1) sp 0 rotorIdWiring
                              --ir = Rotor i sp (rs - 1) sp 0 rotorIdWiring
                          return r
                    else setRotor i
              else setRotor i 
                          



              
