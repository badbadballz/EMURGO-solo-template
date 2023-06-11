{-# LANGUAGE FlexibleContexts #-}

module Actions where

import Lib
import Types
import Data.Char
import System.Console.ANSI
import Data.List (intersperse, foldl')
import Control.Monad.State
import qualified Data.Text as T (snoc, filter, length, Text)
import qualified Data.Text.IO as TO (putStrLn)

printMachine' :: (MonadIO m, MonadState MachineState' m) => [Letter] -> m ()
printMachine' encrypted = do
                    ms <- get
                    liftIO clearScreen
                    liftIO $ setSGR [SetColor Foreground Dull Magenta] 
                    liftIO $ cursorBackward 1
                    liftIO $ putStrLn "Settings"
                    liftIO $ putStr "Rotor types - "
                    liftIO $ putStrLn $ printRotorTypes $ getRotorT ms
                    liftIO $ putStr "Start positions - "
                    liftIO $ print $ intersperse ' ' $ map intToChar $ getStartPositions ms
                    liftIO $ putStr "Ring settings - "
                    liftIO $ print $ map (+1) $ getRingSettings ms
                    liftIO $ putStr "Reflector - "
                    liftIO $ putChar $ getReflector ms
                    liftIO $ putStrLn ""
                    liftIO $ putStr "Plugboard settings - "
                    liftIO $ print $ getPlugboard ms
                    liftIO $ setSGR [Reset]
                    liftIO $ putStr "Steps - " 
                    liftIO $ putStr $ show $ getSteps ms 
                    liftIO $ cursorForward 5
                    liftIO $ setSGR [SetColor Foreground Vivid Green]
                    liftIO $ putStr "Rotors - " 
                    liftIO $ putStrLn $ intersperse '-' $ printRotorTurnsHelper (getRotors ms)
                    liftIO $ setSGR [Reset]
                    liftIO $ cursorDownLine 1
                    liftIO $ putStr "Input  - " 
                    liftIO $ TO.putStrLn (getInputs' ms)
                    liftIO $ cursorDownLine 1
                    liftIO $ putStrLn $ intersperse '>' $ map intToChar encrypted
                    liftIO $ cursorDownLine 1
                    liftIO $ setSGR [SetColor Foreground Vivid Red]
                    liftIO $ putStr "Output - " 
                    liftIO $ TO.putStrLn $ getOutputs' ms
                    liftIO $ setSGR [Reset]
                    

spaceText :: T.Text -> T.Text
spaceText s 
    | T.length (T.filter (not.isSpace) s) `mod` 5 == 0 = T.snoc s ' '
    | otherwise = s


operate' :: MachineState' -> IO ()
operate' ms = do
                (_, ms') <- runStateT (pressKey' >>= printMachine') ms
                operate' ms'

pressKey' :: (MonadIO m, MonadState MachineState' m, MonadFail m) => m [Letter]
pressKey' = do            
               c <- liftIO getChar
               if isValidChar c
                      then do     
                             [p0,r0,r1,r2,rf,ir2,ir1,ir0,p1] <- gets getRotors -- not very extendable
                             let c' = (charToInt . toUpper) c
                                 rt = nextRotorsTurns [r0,r1,r2]
                                 irt = reverse $ nextRotorsTurns [ir0,ir1,ir2]
                                 rs = [p0] ++ rt ++ [rf] ++ irt ++ [p1]
                             modify (\s -> s {getSteps = getSteps s + 1, getRotors = rs, 
                                                getInputs' = spaceText $ T.snoc (getInputs' s) (intToChar c')})
                             encrypted <- iterateM' encrypt' (encrypt' c') (length rs) 
                             ms <- get
                             put (ms {getOutputs' = spaceText $ T.snoc (getOutputs' ms) (intToChar $ last encrypted)})
                             return encrypted
                      else 
                        pressKey'

setReflector' :: IO (Reflector, Char)
setReflector' = do
                putStrLn "Choose reflector (B or C)"
                c <- getChar
                go (toUpper c) 
                where 
                    go 'B' = do 
                                putStrLn "" 
                                return (makeReflector reflectorBWiring, 'B')
                    go 'C' = do 
                                putStrLn "" 
                                return (makeReflector reflectorCWiring, 'C')
                    go _ = do
                            putStrLn "Invalid input!"
                            setReflector'


setPlugboard' :: (String, String) -> IO (Plugboard, (String, String))
setPlugboard' p@(p1, p2) = do
                           putStrLn ("Enter plugboard letter pair(s) (AB for A-B pair, SE for S-E pair etc.) one pair at a time, type QQ to finish - " ++ show (zip p1 p2 ))
                           s <- getLine
                           if length s == 2 then go (map toUpper s)
                           else do 
                                putStrLn "Invalid input!"
                                setPlugboard' p
                           where 
                            go "QQ" = do
                                        let p = configPlugboard rotorIdWiring (map charToInt p1, map charToInt p2)
                                        return (makePlugboard p, (p1, p2))
                            go [c1,c2] = if isValidChar c1 && isValidChar c2 &&
                                                     notElem c1 p1 && notElem c1 p2 &&
                                                     notElem c2 p1 && notElem c2 p2 && c1 /= c2 then
                                                     setPlugboard' (toUpper c1:p1, toUpper c2:p2)
                                                     else do
                                                            putStrLn "Invalid input!"
                                                            setPlugboard' p
                            go _ = do
                                    putStrLn "Invalid input!"
                                    setPlugboard' p

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


getRotorTypes' :: IO [Int]
getRotorTypes' = do 
                  putStrLn "Choose 3 rotor types from 1 - 5 from the leftmost to rightmost (153 = Rotor I, V, III)"
                  putStrLn "position in the machine"
                  s <- getLine
                  check s
                  where
                   checkRotorTypes = foldr (\x acc -> let x' = read [x] in 1 <= x' && x' <= rotorTypes && acc) True
                   check s' 
                    | length s' == rotorNumber && all isDigit s' = if checkRotorTypes s' 
                                                        then return $ map (\n -> read [n] :: Int) s'
                                                        else getRotorTypes'
                    | otherwise = do
                                    putStrLn "Invalid input!" 
                                    getRotorTypes'

getStartPos' :: IO [Int]
getStartPos' = do
                putStrLn "Enter the start position of each rotor correspondingly"
                putStrLn "(ABC means the leftmost rotor has start position A, the middle rotor has start position B"
                putStrLn "and so on) Choose from A - Z"
                s <- getLine
                check s
                where 
                 check s' 
                  | checkIsValidAlphas s' = return $ map (charToInt.toUpper) s'
                  | otherwise = do
                                putStrLn "Invalid input!"
                                getStartPos'

getRingPos' :: IO [Int]
getRingPos' = do  
                putStrLn "Enter the ring setting of each rotor correspondingly"
                putStrLn "(1 25 0 means the leftmost rotor has ring setting of 1, the middle rotor has ring setting of 25"
                putStrLn "and so on) Choose from 1 - 26"
                s <- getLine
                check s
                where 
                  check s'
                   | checkIsValidNumbers s' = return $ map (\x -> (read x - 1) ::  Int) $ words s'
                   | otherwise = do
                                  putStrLn "Invalid input!" 
                                  getRingPos'



getRotorsInfo' :: IO ([Int], [Int], [Int])
getRotorsInfo' =  do 
                                 rt <- getRotorTypes'
                                 sp <- getStartPos'
                                 rs <- getRingPos'
                                 return (rt, sp, rs)
            

makeRotors _ ([], [], []) = []
makeRotors n ((rt:rts), (sp:sps), (rs:rss)) = makeRotor rt n sp rs : makeRotors (n - 1) (rts, sps, rss) 
makeRotors _ _ = error "Impossibru!"
                 
makeInvRotors _ ([], [], []) = []
makeInvRotors n ((rt:rts), (sp:sps), (rs:rss)) = makeInvRotor rt n sp rs : makeInvRotors (n - 1) (rts, sps, rss) 
makeInvRotors _ _ = error "Impossibru!"


setRotors'' :: IO (Rotors, Rotors, ([Int], [Int], [Int]))
setRotors'' = do
                rsInfo <- getRotorsInfo' 
                let n = rotorNumber - 1
                return (reverse $ makeRotors n rsInfo, makeInvRotors n rsInfo, rsInfo)


setMachine' :: IO MachineState' 
setMachine' = do
                 (r, ir, (tys, sps, rss)) <- setRotors'' 
                 (reflector, ref) <- setReflector'
                 (plugboard, (p1, p2)) <- setPlugboard' ("","") 
                 let rs = [plugboard] ++ r ++ [reflector] ++ ir ++ [plugboard]
                     ems = emptyMachineState
                 return ems {getRotors = rs, getRotorT = tys, getStartPositions = sps,
                             getRingSettings = rss, getReflector = ref, getPlugboard = zip p1 p2 }
              




              
