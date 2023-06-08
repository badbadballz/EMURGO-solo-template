module Actions where

import Lib
import Types
import Data.Char
import System.IO
import System.Console.ANSI
import Data.List (intersperse, foldl')
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

--import Sandbox (PlugBoard, RotorWiring)

printMachine :: Steps -> Tinput -> Toutput -> Eoutput -> Rotors -> IO () 
printMachine stps input output eoutput rs = do
                                             clearScreen
                                             putStr ("Steps - " ++ show stps) 
                                             cursorForward 5
                                             putStrLn $ intersperse '-' $ map intToChar $ foldl' (\acc r -> turns r : acc) [] rs
                                             cursorDownLine 1
                                             putStrLn $ reverse input
                                             cursorDownLine 1
                                             putStrLn $ intersperse '>' $ reverse eoutput
                                             cursorDownLine 1
                                             putStrLn $ reverse output


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
                                input' = intToChar c' : input 
                                output' = head alphas : output
                                [_,r2,r1,r0,_,_,_,_,_] = rs'
                            --putStrLn (show stps' ++ " " ++ [head alphas] ++ " " ++ tail alphas) 
                            printMachine stps' input' output' alphas [r2,r1,r0]
                            operate stps' input' output' rs'
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
                           putStrLn ("Enter plugboard letter combination (eg. AB), QQ to finish - " ++ show (zip p1 p2 ))
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



{-                        
-- assumes n = rotorNumber
setRotors' :: Int -> IO [(Rotor, Rotor)]
setRotors' n = do
                putStrLn ""
                putStrLn "Choose rotor type (1-5) with 3 digit number"
                putStrLn "ie. 123 means rotor 1, 2, then 3 in the rightmost to leftmost postions -"
                s <- getLine
                if length s == rotorNumber && and $ map isDigit s
                then do
                      let n = convertNumber n 
-}

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
                  liftIO $ putStrLn "Enter 3 rotor types from rightmost to leftmost (123 = Rotor I, II, III)"
                  liftIO $ putStrLn "Choose from 1 - 5"
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
                liftIO $ putStrLn "(ABC means rotor I has start position A, II has start position B"
                liftIO $ putStrLn "and so on) Choose from A - Z"
                s <- liftIO getLine
                check s
                where 
                 check s' 
                  | checkIsValidAlphas s' = MaybeT $ return $ Just (map (charToInt.toUpper) s')
                  | otherwise = MaybeT $ return Nothing
{-
getStartPos :: String -> Maybe [Int]
getStartPos s 
        | checkIsValidAlphas s = Just (map (charToInt.toUpper) s)
        | otherwise = Nothing
-}


getRingPos :: MaybeT IO [Int]
getRingPos = do  
                liftIO $ putStrLn "Enter the ring setting of each rotor correspondingly"
                liftIO $ putStrLn "(1 25 0 means rotor I has ring setting of 1, II has ring setting of 25"
                liftIO $ putStrLn "and so on) Choose from 1 - 26"
                s <- liftIO getLine
                check s
                where 
                  check s'
                   | checkIsValidNumbers s' = MaybeT $ return $ Just $ map (\x -> (read x - 1) ::  Int) $ words s'
                   | otherwise = MaybeT $ return Nothing

{-        
getRingPos :: String -> Maybe [Int]
getRingPos s  
        | checkIsValidNumbers s = Just $ map (\x -> read x ::  Int) $ words s
        | otherwise = Nothing                          

-}
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


-- assumes n = rotorNumber
{-
setRotors' :: Int -> IO [(Rotor, Rotor)]
setRotors' 0 = return []
setRotors' n = do
                tr <- chooseRotor (n - 1)
                trs <- setRotors' (n - 1)
                return (tr : trs)
-}
setMachine :: IO Rotors
setMachine = do
              (r, ir) <- setRotors 
              reflector <- setReflector
              plugboard <- setPlugboard ("","") 
              return ([plugboard] ++ r ++ [reflector] ++ ir ++ [plugboard]) 
              -- later implemenation can include programmable plugboard / reflector

-- i is the position of the rotor to be chosen
{-
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
                                r <- setRotor' i 
                                let r' = r {turnover = r1Turnover, rotorWiring = setRing (ringSetting r) rotorIWiring}
                                    ir = r {turnover = r1Turnover, rotorWiring = setRing (ringSetting r) invRotorIWiring}
                                return (r' , ir)
                     | n == 2 = do 
                                r <- setRotor' i
                                let r' = r {turnover = r2Turnover, rotorWiring = setRing (ringSetting r) rotorIIWiring} 
                                    ir = r {turnover = r2Turnover, rotorWiring = setRing (ringSetting r) invRotorIIWiring} 
                                return (r', ir) 
                     | n == 3 = do 
                                r <- setRotor' i
                                let r' = r {turnover = r3Turnover, rotorWiring = setRing (ringSetting r) rotorIIIWiring} 
                                    ir = r {turnover = r3Turnover, rotorWiring = setRing (ringSetting r) invRotorIIIWiring} 
                                return (r', ir) 
                     | n == 4 = do 
                                r <- setRotor' i
                                let r' = r {turnover = r4Turnover, rotorWiring = setRing (ringSetting r) rotorIVWiring}
                                    ir = r {turnover = r4Turnover, rotorWiring = setRing (ringSetting r) invRotorIVWiring}
                                return (r', ir)
                     | n == 5 = do 
                                r <- setRotor' i
                                let r' = r {turnover = r5Turnover, rotorWiring = setRing (ringSetting r) rotorVWiring} 
                                    ir = r {turnover = r5Turnover, rotorWiring = setRing (ringSetting r) invRotorVWiring}
                                return (r', ir)       
                     | otherwise = chooseRotor i 
-}
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


{-
setRotor' :: Int -> IO Rotor
setRotor' i = do
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
                    else setRotor' i
              else setRotor' i 
-}


              
