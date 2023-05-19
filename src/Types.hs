module Types where

import Data.Char
import System.Random.Stateful (uniformM, globalStdGen, randomRIO)
import Control.Monad.State
import Control.Monad
import Data.List (foldl')
import Test.QuickCheck 

--import qualified Sandbox as S 

data Rotor = Rotor RotorPos RotorWiring deriving (Eq, Show)
type Rotors = [Rotor]

instance Arbitrary Rotor where
    arbitrary = do
                 n <- choose (-1, 10)
                 rs <- shuffle [0..25] -- this needs to change for different rotor inputs/outputs
                 return $ Rotor n rs

--sToG :: IO String -> Gen String
--sToG s = s >>= (\s' -> return s')


type Steps = Int

type RotorPos = Int
type RotorWiring = [Int]

type Letter = Int 
type Outputs = [Letter]
type MachineState = (Steps, Outputs, Rotors)

-- type Offset = Int -- modified with mod rotorSize

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

-- encrypt is the same as decrypt
encrypt :: Letter -> State MachineState Letter
encrypt l = state (\s@(stps, outputs, rs) -> 
                    if null rs then (l, s) else
                    let result = passRotor l stps (head rs)
                    in (result, (stps, result:outputs, tail rs)))

pressKey :: Letter -> MachineState -> MachineState
pressKey l (stps, _ ,rs) = go l (stps+1, [], rs)
                            where
                              go _ s@(_, _, []) = s 
                              go l' s@(stps', output, rs') = let (result, s') = runState (encrypt l') s
                                                              in go result s'

makeState :: MachineState
makeState = (0, [], makeRotors)

makeState1 :: MachineState
makeState1 = (0, [], makeRotors1)

stepsToNthRSteps :: Int -> Int -> Int
stepsToNthRSteps stps n = (stps `div` rotorSize^n) `mod` rotorSize

passRotor :: Letter -> Steps -> Rotor -> Letter
passRotor l _ (Rotor (-1) r) = r !! l
passRotor l stps (Rotor n r) = let rStps = stepsToNthRSteps stps n
                                   offsetStps = (l + rStps) `mod` rotorSize
                                     in ((r !! offsetStps) - rStps) `mod` rotorSize

{-
passRotorLToR :: Letter -> Steps -> Rotor -> Letter
passRotorLToR l stps (Rotor (-1) _ r') = let rStps = stepsToNthRSteps stps 0
                                             offsetStps = (l + rStps) `mod` rotorSize
                                         in ((r' !! offsetStps) - rStps) `mod` rotorSize
passRotorLToR l stps (Rotor n _ r') = let rStps = stepsToNthRSteps stps n
                                          offsetStps = (l + rStps) `mod` rotorSize
                                      in ((r' !! offsetStps) - rStps) `mod` rotorSize
-}


-- future function to make rotors from user input
makeRotors :: Rotors
makeRotors = [rotorI, reflectorB, invRotorI] 

makeRotors1 :: Rotors
makeRotors1 = [rotorI, rotorII, rotorIII, reflectorB, invRotorIII, invRotorII, invRotorI]

{-
testRotor :: Rotor -> Bool 
testRotor (Rotor _ r l) = let l' = inverseRotorWiring r 
                            in l == l'
-}

inverseRotorWiring :: RotorWiring -> RotorWiring
inverseRotorWiring rss = let nullRotor = replicate 26 (-1)
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


-- 0 is just a placeholder
rotorI :: Rotor
rotorI = Rotor 0 testRotorI 

invRotorI :: Rotor
invRotorI = Rotor 0 invTestRotorI

rotorII :: Rotor
rotorII = Rotor 1 testRotorII 

invRotorII :: Rotor
invRotorII = Rotor 1 invTestRotorII

rotorIII :: Rotor
rotorIII = Rotor 2 testRotorIII 

invRotorIII :: Rotor
invRotorIII = Rotor 2 invTestRotorIII

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

testReflectorB :: RotorWiring --"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
testReflectorB = map charToInt  "YRUHQSLDPXNGOKMIEBFZCWVJAT"

reflectorB :: Rotor
reflectorB = Rotor (-1) testReflectorB 