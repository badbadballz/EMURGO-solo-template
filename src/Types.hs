module Types where


import Data.Char
--import System.Random.Stateful (uniformM, globalStdGen, randomRIO)
--import Control.Monad.State
--import Control.Monad
--import Data.List (foldl')
import Test.QuickCheck 
--import GHC.Base (VecElem(Int16ElemRep))

--import qualified Sandbox as S 

data Rotor = Rotor { rotorPos :: Int
                   , startPos :: Int
                   , ringSetting :: Int 
                   , turns :: Int 
                   , turnover :: Int 
                   , rotorWiring :: RotorWiring} deriving (Eq, Show) 

--data Rotor = Rotor RotorPos StartPos RingSetting Turns Turnover RotorWiring deriving (Eq, Show)

type Rotors = [Rotor]
type Reflector = Rotor
type Plugboard = Rotor
type Steps = Int
type RotorWiring = [Int]
type Letter = Int 
type Outputs = [Letter]
type MachineState = (Steps, Outputs, Rotors)

upperAlphaOffset = 65

rotorNumber :: Int 
rotorNumber = 3

alphabet :: String
alphabet = ['A' .. 'Z']

rotorSize :: Int 
rotorSize = 26

rotorTypes = 5

instance Arbitrary Rotor where
    arbitrary = do
                 n <- choose (-1, 10)
                 st <- choose (0, 25)
                 rs <- choose (0, 25)
                 turnover <- choose (0,25)
                 rw <- shuffle [0..25] -- this needs to change for different rotor inputs/outputs
                 return $ Rotor n st rs st turnover (setRing rs rw)


charToInt :: Char -> Int 
charToInt c = ord c - upperAlphaOffset 

intToChar :: Int -> Char
intToChar n = chr $ n + upperAlphaOffset

inverseRotorWiring :: RotorWiring -> RotorWiring
inverseRotorWiring rss = let nullRotor = replicate 26 (-1)
                         in go rss 0 nullRotor 
                         where
                             go [] _ ns = ns
                             go (r:rs) i ns = let (f, e) = splitAt r ns 
                                                  result = f ++ [i] ++ tail e  
                                               in go rs (i+1) result

setRing :: Int -> RotorWiring -> RotorWiring
setRing n rw = let c = cycleList (n `mod` rotorSize) rw
               in map (\i -> (i + n) `mod` rotorSize) c
                   

cycleList :: Int -> RotorWiring -> RotorWiring
cycleList 0 xs = xs
cycleList _ [] = []
cycleList n xs = cycleList (n - 1) $ last xs : init xs

--sToG :: IO String -> Gen String
--sToG s = s >>= (\s' -> return s')


-- Start position, the inital offset of each rotor when step is zero
-- Ring position, the internal offset/ difference of the letters moved back or fore
-- Can the rotor step at different stages apart from the usual 26?
-- Rotor notch, each rotor sets the next rotor at a different notch ie. I when Q to R
-- Also double stepping, which i don't completely understand yet





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
-- encrypt assumes that rotors stack has been created properly (with correct turns)

{-
pressKey :: Letter -> MachineState -> MachineState
pressKey l (stps, _ ,rs) = go l (stps + 1, [], rs)
                                where
                                go _ s@(_, _, []) = s 
                                go l' s@(stps', output, rs') = let (result, s') = runState (encrypt l') s
                                                               in go result s'
-}





--stepsToNthRSteps :: Int -> Int -> Int
--stepsToNthRSteps stps n = (stps `div` rotorSize^n) `mod` rotorSize

--stepsTurnoverCheck :: Int




{-
passRotor :: Letter -> Rotor -> Letter
passRotor l _ (Rotor (-1) _ _ r) = r !! l -- plugboards and reflectors
passRotor l stps (Rotor n st _ r) = let rStps = stepsToNthRSteps (stps + st) n 
                                        offsetStps = (l + rStps) `mod` rotorSize
                                    in ((r !! offsetStps) - rStps) `mod` rotorSize
-}
-- processes the next step, ie assumes the next key has been presses. Stps has already been 
-- advanced once


-- stps - 1 == turnover r = let offsetStps = (startPos r + stps) `mod` rotorSize
-- in r{turns = offsetStps} : go stps rs True


-- stps - 1 == turnover r &&
-- notch = let offsetStps = (startPos r + 1) `mod` rotorSize
-- in r{turns = offsetStps} : go stps rs True


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


{-
testRotor :: Rotor -> Bool 
testRotor (Rotor _ r l) = let l' = inverseRotorWiring r 
                            in l == l'
-}






-- converts an upper alphabet letter to an int, starting from 0 for A


{-
data Rotor = Rotor { rotorPos :: Int
                   , startPos :: Int
                   , ringSetting :: Int 
                   , turns :: Int 
                   , turnover :: Int 
                   , rotorWiring :: [Int]} deriving (Eq, Show) -}

-- 0 is just a placeholder
-- rotorPos is the relative position in the enigma machine, not the rotor number
-- startPos has to be initialised to turns when starting rotor


r1Turnover :: Int
r1Turnover = 16 

rotorI :: Rotor
rotorI = Rotor 0 0 0 0 16 (setRing 0 rotorIWiring) 
--Rotor 0 3 1 (ringSetting 1 testRotorI) 

invRotorI :: Rotor
invRotorI = Rotor 0 0 0 0 16 (setRing 0 invRotorIWiring)

r2Turnover :: Int
r2Turnover = 4

rotorII :: Rotor
rotorII = Rotor 1 0 0 0 4 (setRing 0 rotorIIWiring) 

invRotorII :: Rotor
invRotorII = Rotor 1 0 0 0 4 (setRing 0 invRotorIIWiring)

r3Turnover :: Int
r3Turnover = 21

rotorIII :: Rotor
rotorIII = Rotor 2 0 0 0 21 (setRing 0 rotorIIIWiring) 

invRotorIII :: Rotor
invRotorIII = Rotor 2 0 0 0 21 (setRing 0 invRotorIIIWiring)

r4Turnover :: Int
r4Turnover = 9

rotorIV :: Rotor
rotorIV = Rotor 0 0 0 0 9 (setRing 0 rotorIVWiring)

invRotorIV :: Rotor
invRotorIV = Rotor 0 25 0 25 9 (setRing 0 invRotorIVWiring)

r5Turnover :: Int
r5Turnover = 25

rotorV :: Rotor
rotorV = Rotor 1 8 0 8 25 (setRing 0 rotorVWiring)

invRotorV :: Rotor
invRotorV = Rotor 1 8 0 8 25 (setRing 0 invRotorVWiring)



rotorIWiring :: RotorWiring --"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
rotorIWiring = map charToInt  "EKMFLGDQVZNTOWYHXUSPAIBRCJ"


invRotorIWiring :: RotorWiring 
invRotorIWiring = inverseRotorWiring rotorIWiring
--"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
--"UWYGADFPVZBECKMTHXSLRINQOJ"
rotorIIWiring :: RotorWiring --"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
rotorIIWiring = map charToInt  "AJDKSIRUXBLHWTMCQGZNPYFVOE"

invRotorIIWiring :: RotorWiring
invRotorIIWiring = inverseRotorWiring rotorIIWiring

rotorIIIWiring :: RotorWiring --"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
rotorIIIWiring = map charToInt  "BDFHJLCPRTXVZNYEIWGAKMUSQO"

invRotorIIIWiring :: RotorWiring
invRotorIIIWiring = inverseRotorWiring rotorIIIWiring

rotorIVWiring :: RotorWiring --"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
rotorIVWiring = map charToInt  "ESOVPZJAYQUIRHXLNFTGKDCMWB"

invRotorIVWiring :: RotorWiring
invRotorIVWiring = inverseRotorWiring rotorIVWiring 

rotorVWiring :: RotorWiring --"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
rotorVWiring = map charToInt  "VZBRGITYUPSDNHLXAWMJQOFECK"

invRotorVWiring :: RotorWiring
invRotorVWiring = inverseRotorWiring rotorVWiring

reflectorBWiring :: RotorWiring --"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
reflectorBWiring = map charToInt  "YRUHQSLDPXNGOKMIEBFZCWVJAT"

reflectorCWiring :: RotorWiring
reflectorCWiring = map charToInt "FVPJIAOYEDRZXWGCTKUQSBNMHL"

makeReflector :: RotorWiring -> Reflector
makeReflector = Rotor (-1) 0 0 0 0 

-- reflectors and plugboards are modelled as non rotating rotors

-- plugboard with A-B, C-D pairings
--"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
--"BADCEFGHIJKLMNOPQRSTUVWXYZ"

plugboardWiring :: RotorWiring --"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
plugboardWiring = map charToInt  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

reflectorB :: Reflector
reflectorB = Rotor (-1) 0 0 0 0 reflectorBWiring 

makePlugboard :: RotorWiring -> Plugboard
makePlugboard = Rotor (-1) 0 0 0 0 

rotorIdWiring :: RotorWiring
rotorIdWiring = map charToInt ['A'..'Z']