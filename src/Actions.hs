module Actions where

import Lib
import Types
import Data.Char
import System.IO
import System.Console.ANSI
import Data.List (intersperse, foldl')
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Trans.State.Lazy as ST 
--import Control.Monad.State.Class
import Control.Monad.State
--import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import qualified Data.Text as T
import qualified Data.Text.IO as TO
--import Control.Monad.Trans.Except
--import Test
import Control.Monad
import Data.Functor.Identity


printMachine' :: [Letter] -> StateT MachineState' IO ()
printMachine' encrypted = do
                    ms <- get
                    liftIO clearScreen
                    
                    liftIO $ setSGR [SetColor Foreground Dull Magenta] 
                    liftIO $ cursorBackward 1
                    liftIO $ putStrLn "Settings"
                    liftIO $ putStr "Rotor types - "
                    liftIO $ putStrLn $ printRotorTypes $ getRotorT ms
                    liftIO $ putStr "Start positions - "
                    liftIO $ print $ map intToChar $ getStartPositions ms
                    liftIO $ putStr "Ring settings - "
                    liftIO $ print $ map (+1) $ getRingSettings ms
                    liftIO $ putStr "Reflector -  "
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
                    --map intToChar $ foldl' (\acc r -> turns r : acc) [] (getRotors ms))
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
                    --return (spaceText input, spaceText output)


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


spaceText :: T.Text -> T.Text
spaceText s 
    | T.length (T.filter (not.isSpace) s) `mod` 5 == 0 = T.snoc s ' '
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
                                input' = T.snoc input (intToChar c')   --input Seq.|> intToChar c'  
                                output' = T.snoc output (head alphas)  --output Seq.|> head alphas
                                [_,r2,r1,r0,_,_,_,_,_] = rs'
                            (input'', output'') <- printMachine stps' input' output' alphas [r2,r1,r0]
                            operate stps' input'' output'' rs'
                    else 
                        operate stps input output rs

operate' :: MachineState' -> IO ()
operate' ms = do
                (_, ms') <- runStateT (pressKey' >>= printMachine') ms
                operate' ms'

pressKey' :: StateT MachineState' IO [Letter]
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
                             --ms' <- get
                             --put (ms' {getInputs' = spaceText $ getInputs' ms', getOutputs' = spaceText $ getOutputs' ms' })
                             return encrypted
                      else 
                        pressKey'


-- don't know how this works, this is the only way I can get this to type check!??!?
{-
pressKey'' :: Letter -> State MachineState' [Letter]
pressKey'' l =  do
                rs <- gets getRotors
                modify (\s -> s {getRotors = nextRotorsTurns rs, getInputs' = T.snoc (getInputs' s) (intToChar l)})
                encrypted <- iterateM' encrypt' (encrypt' l) (length rs)
                ms <- get
                put (ms {getOutputs' = T.snoc (getOutputs' ms) (intToChar $ last encrypted)})
                return encrypted

-}

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
getRotorsInfo =  runMaybeT $ do 
                                 rt <- Actions.getRotorTypes
                                 sp <- getStartPos
                                 rs <- getRingPos
                                 return (rt, sp, rs)

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

setRotors :: IO (Rotors, Rotors)
setRotors = do
                rsInfo <- getRotorsInfo' 
                let n = rotorNumber - 1
                return (reverse $ makeRotors n rsInfo, makeInvRotors n rsInfo)
                {-
                case rsInfo of Just info -> return (reverse $ makeRotors n info, makeInvRotors n info)
                               Nothing -> do 
                                            putStrLn "Invalid input!"
                                            setRotors
                -}

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



setRotors'' :: IO (Rotors, Rotors, ([Int], [Int], [Int]))
setRotors'' = do
                rsInfo <- getRotorsInfo' 
                let n = rotorNumber - 1
                return (reverse $ makeRotors n rsInfo, makeInvRotors n rsInfo, rsInfo)

{-data MachineState' = MachineState' {
                       getSteps :: Int
                     , getInputs' :: T.Text
                     , getOutputs' :: T.Text
                     , getRotors :: Rotors
                     , getRotorTypes :: [Int]
                     , getStartPositions :: [Int]
                     , getRingSettings :: [Int] 
                     , getReflector :: Char 
                     , getPlugboard :: [(Char,Char)]}
-}

setMachine' :: IO MachineState' 
setMachine' = do
                 (r, ir, (tys, sps, rss)) <- setRotors'' 
                 (reflector, ref) <- setReflector'
                 (plugboard, (p1, p2)) <- setPlugboard' ("","") 
                 let rs = [plugboard] ++ r ++ [reflector] ++ ir ++ [plugboard]
                     ems = emptyMachineState
                 return ems {getRotors = rs, getRotorT = tys, getStartPositions = sps,
                             getRingSettings = rss, getReflector = ref, getPlugboard = zip p1 p2 }
              
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




              
