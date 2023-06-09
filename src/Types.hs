module Types where


import Data.Char
import Test.QuickCheck 
import qualified Data.Sequence as Seq

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

type Tinput = Seq.Seq Char
type Toutput = Seq.Seq Char
type Eoutput = String

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
                 rw <- shuffle [0..25] 
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

-- rotor type, rotorpos, startpos, ringsetting
makeRotor :: Int -> Int -> Int -> Int -> Rotor
makeRotor n rp sp rs 
    | n == 1 = Rotor rp sp rs sp r1Turnover (setRing rs rotorIWiring)
    | n == 2 = Rotor rp sp rs sp r2Turnover (setRing rs rotorIIWiring)
    | n == 3 = Rotor rp sp rs sp r3Turnover (setRing rs rotorIIIWiring)
    | n == 4 = Rotor rp sp rs sp r4Turnover (setRing rs rotorIVWiring)
    | n == 5 = Rotor rp sp rs sp r5Turnover (setRing rs rotorVWiring)
    | otherwise = error "Impossibru!"

makeInvRotor :: Int -> Int -> Int -> Int -> Rotor
makeInvRotor n rp sp rs 
    | n == 1 = Rotor rp sp rs sp r1Turnover (setRing rs invRotorIWiring)
    | n == 2 = Rotor rp sp rs sp r2Turnover (setRing rs invRotorIIWiring)
    | n == 3 = Rotor rp sp rs sp r3Turnover (setRing rs invRotorIIIWiring)
    | n == 4 = Rotor rp sp rs sp r4Turnover (setRing rs invRotorIVWiring)
    | n == 5 = Rotor rp sp rs sp r5Turnover (setRing rs invRotorVWiring)
    | otherwise = error "Impossibru!"

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