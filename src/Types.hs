module Types where

import Data.Char
import System.Random.Stateful (uniformM, globalStdGen, randomRIO)
import Control.Monad.State
import Data.List (foldl')

data Rotor = Rotor RotorPos RotorWiring RotorWiring deriving (Eq, Show)

type Steps = Int

type RotorPos = Int
type RotorWiring = [Int]

type Letter = Int 
type Offset = Int -- modified with mod rotorSize

{-
stepsToRotorSteps :: Steps -> AllRSteps
stepsToRotorSteps stps = let rs = [0..(rotorNumber - 1)]
                         in [(stps `div` rotorSize^n) `mod` rotorSize | n <- rs]
-}
-- direction from fixed rotor to reflector
-- this could be wrong...but i tried fixing it already, better test this!
{-
passRotor' :: Letter -> (RSteps, Rotor) -> Letter
passRotor' l (stps, r) = let offsetStps = (l + stps) `mod` rotorSize
                          in ((r !! offsetStps) - stps) `mod` rotorSize
-}
passRotorRToL :: Letter -> Steps -> Rotor -> Letter
passRotorRToL l stps (Rotor (-1) r _) = let rStps = stps `mod` rotorSize
                                            offsetStps = (l + rStps) `mod` rotorSize
                                        in ((r !! offsetStps) - rStps) `mod` rotorSize
passRotorRToL l stps (Rotor n r _) = let rStps = (stps `div` rotorSize^n) `mod` rotorSize
                                         offsetStps = (l + rStps) `mod` rotorSize
                                     in ((r !! offsetStps) - rStps) `mod` rotorSize

passRotorLToR :: Letter -> Steps -> Rotor -> Letter
passRotorLToR l stps (Rotor (-1) _ r') = let rStps = stps `mod` rotorSize
                                             offsetStps = (l + rStps) `mod` rotorSize
                                         in ((r' !! offsetStps) - rStps) `mod` rotorSize
passRotorLToR l stps (Rotor n _ r') = let rStps = (stps `div` rotorSize^n) `mod` rotorSize
                                          offsetStps = (l + rStps) `mod` rotorSize
                                      in ((r' !! offsetStps) - rStps) `mod` rotorSize



makeTestRotor :: Rotor
makeTestRotor = Rotor 0 testRotor0 invTestRotor0

testRotor :: Rotor -> Bool 
testRotor (Rotor _ r l) = let l' = inverseRotorWiring r 
                            in l == l'

inverseRotorWiring :: RotorWiring -> RotorWiring
inverseRotorWiring rss = let nullRotor = take 26 $ cycle [-1]
                         in go rss 0 nullRotor 
                         where
                             go [] _ ns = ns
                             go (r:rs) i ns = let (f, e) = splitAt r ns 
                                                  result = f ++ [i] ++ tail e  
                                               in go rs (i+1) result


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

testRotor0 :: RotorWiring
testRotor0 = map charToInt ['A'..'Z']

invTestRotor0 :: RotorWiring
invTestRotor0 = inverseRotorWiring testRotor0

-- 0 is just a placeholder
rotorI :: Rotor
rotorI = Rotor 0 testRotorI invTestRotorI

rotorII :: Rotor
rotorII = Rotor 1 testRotorII invTestRotorII

rotorIII :: Rotor
rotorIII = Rotor 2 testRotorIII invTestRotorIII

testRotorI :: RotorWiring --"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
testRotorI = map charToInt  "EKMFLGDQVZNTOWYHXUSPAIBRCJ"

invTestRotorI :: RotorWiring 
invTestRotorI = inverseRotorWiring testRotorI
--"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
--"UWYGADFPVZBECKMTHXSLRINQOJ"
testRotorII :: RotorWiring --"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
testRotorII = map charToInt  "AJDKSIRUXBLHWTMCQGZNPYFVOE"

invTestRotorII :: RotorWiring
invTestRotorII = inverseRotorWiring testRotorII

testRotorIII :: RotorWiring --"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
testRotorIII = map charToInt  "BDFHJLCPRTXVZNYEIWGAKMUSQO"

invTestRotorIII :: RotorWiring
invTestRotorIII = inverseRotorWiring testRotorIII

testReflectorB :: RotorWiring      --"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
testReflectorB = map charToInt "YRUHQSLDPXNGOKMIEBFZCWVJAT"
