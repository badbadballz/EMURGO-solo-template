module Sandbox where

import Data.Char
import System.Random.Stateful (uniformM, globalStdGen, randomRIO)
import Control.Monad.State
import Data.List (foldl', nub)

{-
Old and testing functions, not used in main program
-}

-- representation of the rotor wiring in a rotor. The index represents the input, and the corresponding item number is the output index

data RotorType = Rotor | Reflector | Plugboard

type Rotor = [Int]

data Rotor' = Rotor' RotorPos RotorWiring RotorWiring

type RotorPos = Int
type RotorWiring = [Int]
--data Rotor = Rotor Steps RotorWiring Int

--instance Monad Rotor 

-- first rotor from left to right ie. head is the first rotor
type Rotors = [Rotor]
-- represents the steps taken for each rotor, mod rotor size
type AllRSteps = [RSteps]

type Steps = Int
type RSteps = Int
type Letter = Int

type MachineState = (Steps, Rotors)

-- represents the plugboard
type PlugBoard = [Int]

type Reflector = [Int]


isValidInput :: Char -> Maybe Char
isValidInput c = if isAlpha c then Just (toUpper c)
                    else Nothing

pressKey :: Letter -> State MachineState Letter
pressKey l = state (\(stps, rs) -> let nextStps = stps + 1
                                    in (passRotorsRToL l nextStps rs, (nextStps, rs) ))
              


stepsToRotorSteps :: Steps -> AllRSteps
stepsToRotorSteps stps = let rs = [0..(rotorNumber - 1)]
                         in [(stps `div` rotorSize^n) `mod` rotorSize | n <- rs]

-- direction from fixed rotor to reflector
-- this could be wrong...but i tried fixing it already, better test this!
passRotor :: Letter -> (RSteps, Rotor) -> Letter
passRotor l (stps, r) = let offsetStps = (l + stps) `mod` rotorSize
                          in ((r !! offsetStps) - stps) `mod` rotorSize


passRotorsRToL :: Letter -> Steps -> Rotors -> Letter
passRotorsRToL l stps rs = let stpsRs = zip (stepsToRotorSteps stps) rs
                        in foldl' passRotor l stpsRs

passRotorsLToR :: Letter -> Steps -> Rotors -> Letter
passRotorsLToR l stps rs = let stpsRs = zip (reverse $ stepsToRotorSteps stps) rs
                        in foldl' passRotor l stpsRs

upperAlphaOffset = 65

rotorNumber :: Int 
rotorNumber = 3

alphabet :: String
alphabet = ['A' .. 'Z']

rotorSize :: Int 
rotorSize = 26

-- converts an upper alphabet letter to an int, starting from 0 for A
charToInt :: Char -> Int 
charToInt c = ord c - upperAlphaOffset 

intToChar :: Int -> Char
intToChar n = chr $ n + upperAlphaOffset

-- code to generate random test rotors
randomBool :: IO Bool
randomBool = uniformM globalStdGen

randomChar :: IO Char
randomChar = randomRIO ('A', 'Z')

randomInt :: IO Int 
randomInt = randomRIO (0,25)

emptyList :: [Int]
emptyList = replicate 25 (-1)
{-}
makeReflector :: [Int]
makeReflector = makeReflector' emptyList
                where
                    makeReflector' y@(x:xs) = case x of -1 -> let result = randomInt
                                                                in if 
-}
shuffle :: Int -> String -> IO String
shuffle 0 s = return s
shuffle n t@(_:_:_) = do
                       s_result <- shuffle' t
                       shuffle (n-1) s_result 
                        where 
                            shuffle' (a:b:ys) = do
                                                b_result <- randomBool
                                                if b_result 
                                                then do
                                                        s <- shuffle' (a:ys) 
                                                        return $ b:s
                                                else do
                                                        s <- shuffle' (b:ys) 
                                                        return $ a:s
                            shuffle' s = return s

shuffle _ s = return s 


inverseRotor :: Rotor -> Rotor
inverseRotor rss = let nullRotor = take 26 $ cycle [-1]
                    in go rss 0 nullRotor 
                    where
                     go [] _ ns = ns
                     go (r:rs) i ns = let (f, e) = splitAt r ns 
                                          result = f ++ [i] ++ tail e  
                                        in go rs (i+1) result

testState = runState (mapM ((\x -> modify (x+)))  [1,2,3,4]) 10

testRotorI :: Rotor      --"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
testRotorI = map charToInt "EKMFLGDQVZNTOWYHXUSPAIBRCJ"

invTestRotorI :: Rotor 
invTestRotorI = inverseRotor testRotorI
--"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
--"UWYGADFPVZBECKMTHXSLRINQOJ"
testRotorII :: Rotor      --"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
testRotorII = map charToInt "AJDKSIRUXBLHWTMCQGZNPYFVOE"

invTestRotorII :: Rotor
invTestRotorII = inverseRotor testRotorII

testRotorIII :: Rotor      --"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
testRotorIII = map charToInt "BDFHJLCPRTXVZNYEIWGAKMUSQO"

invTestRotorIII :: Rotor
invTestRotorIII = inverseRotor testRotorIII

testReflectorB :: Rotor      --"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
testReflectorB = map charToInt "YRUHQSLDPXNGOKMIEBFZCWVJAT"

test321 :: Rotors
test321 = [testRotorIII, testRotorII, testRotorI]

invTest123 :: Rotors
invTest123 = [invTestRotorI, invTestRotorII, invTestRotorIII]

testRotorHackADay :: Rotor
testRotorHackADay = map charToInt "GETNDHQZUPBRCOXMKYAWFILSVJ"

testRotorsHackADay :: Rotors
testRotorsHackADay = [testRotorHackADay]

testRotor0 :: Rotor
testRotor0 = map charToInt ['A'..'Z']

testRotor1 :: Rotor
testRotor1 = map charToInt "PFBUEKDIHOXRANJYLSGZMWQVTC"

--iTestRotor1 :: Rotor
--iTestRotor1 = 

testRotor2 :: Rotor
testRotor2 = map charToInt "BOAPMFQGKJWZIDRLYHCSVUTXEN"

testRotor3 :: Rotor
testRotor3 = map charToInt "EWPZLXYAUOVCTGKBMIHDFRSJQN"

testRotors0 :: Rotors
testRotors0 = replicate rotorNumber testRotor0

testRotors1 :: Rotors
testRotors1 = [testRotor1, testRotor2]

testRotors2 :: Rotors
testRotors2 = [testRotor0]