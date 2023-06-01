module Types where


import Data.Char
--import System.Random.Stateful (uniformM, globalStdGen, randomRIO)
--import Control.Monad.State
--import Control.Monad
--import Data.List (foldl')
import Test.QuickCheck 

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

makeState :: MachineState
makeState = (0, [], makeRotors')

makeState1 :: MachineState
makeState1 = (0, [], makeRotors1')

makeRotors' :: Rotors
makeRotors' = [rotorI, reflectorB, invRotorI] 

makeRotors1' :: Rotors
makeRotors1' = [plugboard, rotorI, rotorII, rotorIII, reflectorB, invRotorIII, invRotorII, invRotorI, plugboard]

makeRotors2' :: Rotors
makeRotors2' = [plugboard, rotorI, rotorII, rotorIII, reflectorB]

makeRotors3' :: Rotors
makeRotors3' = [plugboard, rotorIV, rotorV, rotorI, reflectorB]


rotorI :: Rotor
rotorI = Rotor 0 0 0 0 16 (setRing 0 testRotorI) 
--Rotor 0 3 1 (ringSetting 1 testRotorI) 

invRotorI :: Rotor
invRotorI = Rotor 0 0 0 0 16 (setRing 0 invTestRotorI)

rotorII :: Rotor
rotorII = Rotor 1 0 0 0 4 (setRing 0 testRotorII) 

invRotorII :: Rotor
invRotorII = Rotor 1 0 0 0 4 (setRing 0 invTestRotorII)

rotorIII :: Rotor
rotorIII = Rotor 2 0 0 0 21 (setRing 0 testRotorIII) 

invRotorIII :: Rotor
invRotorIII = Rotor 2 0 0 0 21 (setRing 0 invTestRotorIII)

rotorIV :: Rotor
rotorIV = Rotor 0 25 0 25 9 (setRing 0 testRotorIV)

invRotorIV :: Rotor
invRotorIV = Rotor 0 25 0 25 9 (setRing 0 testInvRotorIV)

rotorV :: Rotor
rotorV = Rotor 1 8 0 8 25 (setRing 0 testRotorV)

invRotorV :: Rotor
invRotorV = Rotor 1 8 0 8 25 (setRing 0 testInvRotorV)



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

testRotorIV :: RotorWiring --"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
testRotorIV = map charToInt  "ESOVPZJAYQUIRHXLNFTGKDCMWB"

testInvRotorIV :: RotorWiring
testInvRotorIV = inverseRotorWiring testRotorIV 

testRotorV :: RotorWiring --"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
testRotorV = map charToInt  "VZBRGITYUPSDNHLXAWMJQOFECK"

testInvRotorV :: RotorWiring
testInvRotorV = inverseRotorWiring testRotorV

testReflectorB :: RotorWiring --"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
testReflectorB = map charToInt  "YRUHQSLDPXNGOKMIEBFZCWVJAT"

-- reflectors and plugboards are modelled as non rotating rotors

-- plugboard with A-B, C-D pairings
--"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
--"BADCEFGHIJKLMNOPQRSTUVWXYZ"

testPlugboard :: RotorWiring --"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
testPlugboard = map charToInt  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

reflectorB :: Reflector
reflectorB = Rotor (-1) 0 0 0 0 testReflectorB 

plugboard :: Plugboard
plugboard = Rotor (-1) 0 0 0 0 testPlugboard