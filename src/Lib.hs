module Lib where

import Types
import Data.Char
import Control.Monad.State


nextRotorsTurns :: Rotors -> Rotors
nextRotorsTurns rss = go rss False
                            where
                            go [] _ = []
                            go (r:rs) notch
                                | rotorPos r == 0 = let (r0, notch') = setRotor0 (r, notch) 
                                                    in r0 : go rs notch'
                                | rotorPos r == 1 = let (r1, notch') = setRotor1 (r, notch) 
                                                    in r1 : go rs notch'
                                | rotorPos r == 2 = let (r2, notch') = setRotor2 (r, notch) 
                                                    in r2 : go rs notch'
                                | otherwise = r : go rs False


setRotor0 :: (Rotor, Bool) -> (Rotor, Bool)
setRotor0 (r, n) 
  | turns r == turnover r = let r' = r{turns = (turns r + 1) `mod` rotorSize}
                            in  (r', True)
  | otherwise = let r' = r{turns = (turns r + 1) `mod` rotorSize}
                in  (r', False)

setRotor1 :: (Rotor, Bool) -> (Rotor, Bool)
setRotor1 (r, n)
  | turns r == turnover r = let r' = r{turns = (turns r + 1) `mod` rotorSize}
                            in  (r', True)
  | n = let r' = r{turns = (turns r + 1) `mod` rotorSize}
        in  (r', False)
  | otherwise = (r, False)

setRotor2 :: (Rotor, Bool) -> (Rotor, Bool)
setRotor2 (r, n) 
  | n = let r' = r{turns = (turns r + 1) `mod` rotorSize}
        in  (r', False)
  | otherwise = (r, False)

passRotor :: Letter -> Rotor -> Letter
passRotor l r 
  | rotorPos r == (-1) = rotorWiring r !! l -- plugboards and reflectors
  | otherwise = let offset = turns r
                    l' = rotorWiring r !! ((l + offset) `mod` rotorSize)
                in (l' - offset) `mod` rotorSize


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

configPlugboard :: RotorWiring -> ([Int], [Int]) -> RotorWiring 
configPlugboard rw ([], _) = rw
configPlugboard rw (_, []) = rw
configPlugboard rw (s1:ss1, s2:ss2) = let (h, t) = splitAt s1 rw
                                          rw' = h ++ [s2] ++ tail t
                                          (h', t') = splitAt s2 rw'
                                          rw'' = h' ++ [s1] ++ tail t'
                                    in configPlugboard rw'' (ss1, ss2)
                                           


