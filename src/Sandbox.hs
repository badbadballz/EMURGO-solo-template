module Sandbox where

import Data.Char
import System.Random.Stateful (uniformM, globalStdGen, randomRIO)
import Control.Monad.State
import Data.List (foldl', nub)
--import Actions
--import Lib
import qualified Types 
import Text.Read (readMaybe)
import Control.Monad
--import Data.Colour.SRGB 
import System.Console.ANSI
import System.IO
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as TO
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

type MachineState = (Steps, Outputs, Rotors)
type Inputs' = T.Text
type Outputs' = T.Text
type Outputs = [Letter]
type Tinput = T.Text --Seq.Seq Char
type Toutput = T.Text --Seq.Seq Char
type Eoutput = String
type RotorTypes = [Int]
type StartPositions = [Int]
type RingSettings = [Int]
--type EOutputs' = [Letter] -- the intermediate outputs from the encryption of the letter
--import Test

--import Types (rotorIdWiring)

--import Test.QuickCheck (shrinkNothing)


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

test_runStateT :: Int -> IO ((), Int)
test_runStateT = runStateT (addStateT >> addStateT >> addStateT) 


addStateT :: StateT Int IO ()
addStateT = do
                c <- liftIO getChar
                if isDigit c 
                then do
                        let n = read [c] :: Int
                        i <- get
                        put (i + n)
                else 
                        addStateT 


--x = iterateM (encrypt') (encrypt' 5) 9
--runState x testMachineState2

maybeAdd :: Int -> Int -> Maybe Int
maybeAdd n m = if n > 10 then Just (n + m)
                else Nothing

cursorMovementExample :: IO ()
cursorMovementExample = do
  putStrLn "Line One"
  putStr "Line Two"
  --pause
  -- Line One
  -- Line Two

  cursorUp 1
  putStr " - Extras"
  --pause
  -- Line One - Extras
  -- Line Two

  cursorBackward 2
  putStr "zz"
  --pause
  -- Line One - Extrzz
  -- Line Two

  cursorForward 2
  putStr "- And More"
  --pause
  -- Line One - Extrzz  - And More
  -- Line Two

  cursorDown 3
  putStr "Disconnected"
  --pause
  -- Line One - Extrzz  - And More
  -- Line Two                     Disconnected

test_byteString :: IO ()
test_byteString = do 
                   s <- getLine
                   let t = T.pack s 
                   TO.putStrLn t 


test_getChar :: IO ()
test_getChar = do
                --hSetBuffering stdout NoBuffering
                --hSetBuffering stdin  NoBuffering
                clearScreen
                putStrLn "hi----"
                setSGR [SetColor Foreground Vivid Blue]
                cursorDownLine 10
                putStrLn "hi"
                setSGR [Reset]
                --test_getChar

test_ansi :: IO ()
test_ansi = do 
                setSGR [SetColor Foreground Vivid Green]
                putStr "Enter your name: "
                setSGR [SetColor Foreground Dull Yellow]
                hFlush stdout  -- flush the output buffer before getLine
                name <- getLine
                setSGR [SetColor Foreground Dull Blue]
                putStrLn $ "Hello, " ++ name ++ "!"
                setSGR [Reset]  -- reset to default colour scheme

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
passRotor' :: Letter -> (RSteps, Rotor) -> Letter
passRotor' l (stps, r) = let offsetStps = (l + stps) `mod` rotorSize
                          in ((r !! offsetStps) - stps) `mod` rotorSize


passRotorsRToL :: Letter -> Steps -> Rotors -> Letter
passRotorsRToL l stps rs = let stpsRs = zip (stepsToRotorSteps stps) rs
                        in foldl' passRotor' l stpsRs

passRotorsLToR :: Letter -> Steps -> Rotors -> Letter
passRotorsLToR l stps rs = let stpsRs = zip (reverse $ stepsToRotorSteps stps) rs
                        in foldl' passRotor' l stpsRs

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

getStartPos :: MaybeT IO [Int]
getStartPos = do
                liftIO $ putStrLn "Enter the start position of each rotor correspondingly"
                liftIO $ putStrLn "(ABC means the leftmost rotor has start position A, the middle rotor has start position B"
                liftIO $ putStrLn "and so on) Choose from A - Z"
                s <- liftIO getLine
                check s
                where 
                 check s' 
                  | checkIsValidAlphas s' = MaybeT $ return $ Just (map (charToInt.toUpper) s')
                  | otherwise = MaybeT $ return Nothing

getRingPos :: MaybeT IO [Int]
getRingPos = do  
                liftIO $ putStrLn "Enter the ring setting of each rotor correspondingly"
                liftIO $ putStrLn "(1 25 0 means the leftmost rotor has ring setting of 1, the middle rotor has ring setting of 25"
                liftIO $ putStrLn "and so on) Choose from 1 - 26"
                s <- liftIO getLine
                check s
                where 
                  check s'
                   | checkIsValidNumbers s' = MaybeT $ return $ Just $ map (\x -> (read x - 1) ::  Int) $ words s'
                   | otherwise = MaybeT $ return Nothing


getRotorsInfo :: IO (Maybe ([Int], [Int], [Int]))
getRotorsInfo =  runMaybeT $ do 
                                 rt <- getRotorTypes
                                 sp <- getStartPos
                                 rs <- getRingPos
                                 return (rt, sp, rs)


getRotorTypes :: MaybeT IO [Int]
getRotorTypes = MaybeT $ do 
                  liftIO $ putStrLn "Choose 3 rotor types from 1 - 5 from the leftmost to rightmost (153 = Rotor I, V, III)"
                  liftIO $ putStrLn "position in the machine"
                  s <- liftIO getLine
                  check s
                  where
                   checkRotorTypes = foldr (\x acc -> let x' = read [x] in 1 <= x' && x' <= rotorTypes && acc) True
                   check s' 
                    | length s' == rotorNumber && all isDigit s' = if checkRotorTypes s' 
                                                        then return $ Just $ map (\n -> read [n] :: Int) s'
                                                        else return Nothing
                    | otherwise = return Nothing

printMachine :: Steps -> Tinput -> Toutput -> Eoutput -> Rotors -> IO (Tinput, Toutput) 
printMachine stps input output eoutput rs = do
                                             clearScreen
                                             putStr "Steps - " 
                                             putStr $ show stps 
                                             cursorForward 5
                                             setSGR [SetColor Foreground Vivid Green]
                                             putStr "Rotors - " 
                                             putStrLn (intersperse '-' $ map intToChar $ foldl' (\acc r -> turns r : acc) [] rs)
                                             setSGR [Reset]
                                             cursorDownLine 1
                                             putStr "Input  - " 
                                             TO.putStrLn input
                                             cursorDownLine 1
                                             putStrLn $ intersperse '>' $ reverse eoutput
                                             cursorDownLine 1
                                             setSGR [SetColor Foreground Vivid Yellow]
                                             putStr "Output - " 
                                             TO.putStrLn output
                                             setSGR [Reset]
                                             return (spaceText input, spaceText output)

operate :: Steps -> Tinput -> Toutput -> Rotors -> IO ()
operate stps input output rs = do            
                    putStrLn ""
                    c <- getChar
                    putStrLn ""
                    if isValidChar c
                    then do 
                            let c' = (charToInt . toUpper) c
                                ((stps', eoutput, _), rs') = nextStep c' stps rs
                                alphas = map intToChar eoutput
                                input' = T.snoc input (intToChar c')   --input Seq.|> intToChar c'  
                                output' = T.snoc output (head alphas)  --output Seq.|> head alphas
                                [_,r2,r1,r0,_,_,_,_,_] = rs'
                            (input'', output'') <- printMachine stps' input' output' alphas [r2,r1,r0]
                            operate stps' input'' output'' rs'
                    else 
                        operate stps input output rs

setReflector :: IO Reflector
setReflector = do
                putStrLn "Choose reflector (B or C)"
                c <- getChar
                go (toUpper c) 
                where 
                    go 'B' = do 
                                putStrLn "" 
                                return $ makeReflector reflectorBWiring
                    go 'C' = do 
                                putStrLn "" 
                                return $ makeReflector reflectorCWiring
                    go _ = do
                            putStrLn "Invalid input!"
                            setReflector

setPlugboard :: (String, String) -> IO Plugboard
setPlugboard p@(p1, p2) = do
                           putStrLn ("Enter plugboard letter pair(s) (AB for A-B pair, SE for S-E pair etc.) one pair at a time, type QQ to finish - " ++ show (zip p1 p2 ))
                           s <- getLine
                           if length s == 2 then go (map toUpper s)
                           else do 
                                putStrLn "Invalid input!"
                                setPlugboard p
                           where 
                            go "QQ" = do
                                        let p = configPlugboard rotorIdWiring (map charToInt p1, map charToInt p2)
                                        return $ makePlugboard p
                            go [c1,c2] = if isValidChar c1 && isValidChar c2 &&
                                                     notElem c1 p1 && notElem c1 p2 &&
                                                     notElem c2 p1 && notElem c2 p2 && c1 /= c2 then
                                                     setPlugboard (toUpper c1:p1, toUpper c2:p2)
                                                     else do
                                                            putStrLn "Invalid input!"
                                                            setPlugboard p
                            go _ = do
                                    putStrLn "Invalid input!"
                                    setPlugboard p

setMachine :: IO Rotors
setMachine = do
              (r, ir) <- setRotors 
              reflector <- setReflector
              plugboard <- setPlugboard ("","") 
              return ([plugboard] ++ r ++ [reflector] ++ ir ++ [plugboard]) 

setRotors :: IO (Rotors, Rotors)
setRotors = do
                rsInfo <- getRotorsInfo' 
                let n = rotorNumber - 1
                return (reverse $ makeRotors n rsInfo, makeInvRotors n rsInfo)

encrypt :: Letter -> State MachineState Letter
encrypt l = state (\s@(stps, outputs, rs) -> 
                    if null rs then (l, s) else
                    let result = passRotor l (head rs)
                    in (result, (stps, result:outputs, tail rs)))

pressKey :: Letter -> MachineState -> MachineState
pressKey _ s@(_, _, []) = s
pressKey l s = let (result, s') = runState (encrypt l) s
               in pressKey result s'

nextStep :: Letter -> Steps -> Rotors -> (MachineState, Rotors)
nextStep l stps [p0,r0,r1,r2,rf,ir2,ir1,ir0,p1] = 
                                  let fr = nextRotorsTurns [r0,r1,r2] 
                                      sr = reverse $ nextRotorsTurns [ir0,ir1,ir2]
                                      rs = [p0] ++ fr ++ [rf] ++ sr ++ [p1]
                                  in (pressKey l (stps + 1, [] , rs), rs) 
nextStep _ _ _ = error "Impossibru!"