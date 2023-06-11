{-# LANGUAGE FlexibleContexts #-}

module Lib where

import Types
import Data.Char
import Control.Monad.State
import Control.Monad
import Data.List (foldl')

-- found this on reddit!
iterateM' :: (Monad m) => (a -> m a) -> m a -> Int -> m [a]
iterateM' f mx n = sequence . take n . iterate (>>= f) $ mx 


nextRotorsTurns :: Rotors -> Rotors
nextRotorsTurns rss = go rss False
                            where
                            go [] _ = []
                            go (r:rs) notch
                                | rotorPos r == 0 = let (r0, notch') = setRotor0 (r, notch) 
                                                    in r0 : go rs notch'
                                | rotorPos r == 1 = let (r1, notch') = setRotor1 (r, notch) 
                                                    in r1 : go rs notch'
                                | rotorPos r == 2 = let (r2, notch') = setRotor2 (r, notch) 
                                                    in r2 : go rs notch'
                                | otherwise = r : go rs False


setRotor0 :: (Rotor, Bool) -> (Rotor, Bool)
setRotor0 (r, n) 
  | turns r == turnover r = let r' = r{turns = (turns r + 1) `mod` rotorSize}
                            in  (r', True)
  | otherwise = let r' = r{turns = (turns r + 1) `mod` rotorSize}
                in  (r', False)

setRotor1 :: (Rotor, Bool) -> (Rotor, Bool)
setRotor1 (r, n)
  | turns r == turnover r = let r' = r{turns = (turns r + 1) `mod` rotorSize}
                            in  (r', True)
  | n = let r' = r{turns = (turns r + 1) `mod` rotorSize}
        in  (r', False)
  | otherwise = (r, False)

setRotor2 :: (Rotor, Bool) -> (Rotor, Bool)
setRotor2 (r, n) 
  | n = let r' = r{turns = (turns r + 1) `mod` rotorSize}
        in  (r', False)
  | otherwise = (r, False)

encrypt' :: (MonadIO m, MonadState MachineState' m) => Letter -> m Letter
encrypt' l = state $ \ms -> 
                              let fr = head $ getRotors ms
                                  tr = tail $ getRotors ms
                                  result = passRotor l fr
                              in (result, ms {getRotors = tr ++ [fr]})

                      


passRotor :: Letter -> Rotor -> Letter
passRotor l r 
  | rotorPos r == (-1) = rotorWiring r !! l -- plugboards and reflectors
  | otherwise = let offset = turns r
                    l' = rotorWiring r !! ((l + offset) `mod` rotorSize)
                in (l' - offset) `mod` rotorSize


configPlugboard :: RotorWiring -> ([Int], [Int]) -> RotorWiring 
configPlugboard rw ([], _) = rw
configPlugboard rw (_, []) = rw
configPlugboard rw (s1:ss1, s2:ss2) = let (h, t) = splitAt s1 rw
                                          rw' = h ++ [s2] ++ tail t
                                          (h', t') = splitAt s2 rw'
                                          rw'' = h' ++ [s1] ++ tail t'
                                    in configPlugboard rw'' (ss1, ss2)
                                           

printRotorTypes :: [Int] -> String
printRotorTypes [] = []
printRotorTypes (t:ts) = case t of 1 -> "I " ++ printRotorTypes ts
                                   2 -> "II " ++ printRotorTypes ts 
                                   3 -> "III " ++ printRotorTypes ts 
                                   4 -> "IV " ++ printRotorTypes ts 
                                   5 -> "V " ++ printRotorTypes ts
                                   _ -> " " ++ printRotorTypes ts

printRotorTurnsHelper :: Rotors -> String
printRotorTurnsHelper [_,r0,r1,r2,_,_,_,_,_] = map intToChar $ foldl' (\acc r -> turns r : acc) [] [r0,r1,r2]
printRotorTurnsHelper _ = ""

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
