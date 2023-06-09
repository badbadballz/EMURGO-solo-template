module Actions where

import Lib
import Types
import Data.Char
import System.IO
import System.Console.ANSI
import Data.List (intersperse, foldl')
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import qualified Data.Sequence as Seq
import Data.Foldable (toList)

printMachine :: Steps -> Tinput -> Toutput -> Eoutput -> Rotors -> IO (Tinput, Toutput) 
printMachine stps input output eoutput rs = do
                                             clearScreen
                                             putStr ("Steps - " ++ show stps) 
                                             cursorForward 5
                                             setSGR [SetColor Foreground Vivid Green]
                                             putStrLn $ "Rotors - " ++ (intersperse '-' $ map intToChar $ foldl' (\acc r -> turns r : acc) [] rs)
                                             setSGR [Reset]
                                             cursorDownLine 1
                                             putStrLn $ "Input  - " ++ toList input
                                             cursorDownLine 1
                                             putStrLn $ intersperse '>' $ reverse eoutput
                                             cursorDownLine 1
                                             setSGR [SetColor Foreground Vivid Blue]
                                             putStrLn $ "Output - " ++ toList output
                                             setSGR [Reset]
                                             return (spaceText input, spaceText output)


spaceText :: Seq.Seq Char -> Seq.Seq Char
spaceText s 
    | length (Seq.filter (not.isSpace) s) `mod` 5 == 0 = s Seq.|> ' '
    | otherwise = s

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
                                input' = input Seq.|> intToChar c'  
                                output' = output Seq.|> head alphas
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
                           putStrLn ("Enter plugboard letter pair(s) (AB for A-B pair, SE for S-E pair etc.), type QQ to finish - " ++ show (zip p1 p2 ))
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

checkIsValidAlphas :: String -> Bool
checkIsValidAlphas s = length s == rotorNumber && all isAlpha s 

checkIsValidNumbers :: String -> Bool
checkIsValidNumbers s = let wss = words s
                            ok = length wss == rotorNumber && checkRingSetting wss
                       in ok
                        where 
                          checkRingSetting [] = True
                          checkRingSetting (w:ws) 
                                | all isDigit w = let w' = read w :: Int
                                                   in 1 <= w' && w' <= rotorSize && checkRingSetting ws                       
                                | otherwise = False

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
getRotorsInfo = runMaybeT $ do 
                                 rt <- getRotorTypes
                                 sp <- getStartPos
                                 rs <- getRingPos
                                 return (rt, sp, rs)
setRotors :: IO (Rotors, Rotors)
setRotors = do
                rsInfo <- getRotorsInfo
                let n = rotorNumber - 1
                case rsInfo of Just info -> return (reverse $ makeRotors n info, makeInvRotors n info)
                               Nothing -> do 
                                            putStrLn "Invalid input!"
                                            setRotors

makeRotors _ ([], [], []) = []
makeRotors n ((rt:rts), (sp:sps), (rs:rss)) = makeRotor rt n sp rs : makeRotors (n - 1) (rts, sps, rss) 
makeRotors _ _ = error "Impossibru!"
                 
makeInvRotors _ ([], [], []) = []
makeInvRotors n ((rt:rts), (sp:sps), (rs:rss)) = makeInvRotor rt n sp rs : makeInvRotors (n - 1) (rts, sps, rss) 
makeInvRotors _ _ = error "Impossibru!"


setMachine :: IO Rotors
setMachine = do
              (r, ir) <- setRotors 
              reflector <- setReflector
              plugboard <- setPlugboard ("","") 
              return ([plugboard] ++ r ++ [reflector] ++ ir ++ [plugboard]) 
              
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




              
