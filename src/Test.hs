module Test where

import Test.QuickCheck
import Types
import Lib
import Data.Char
--import Test (run_nextRotorsTurns)
--import GHC.RTS.Flags (GCFlags(ringBell))


runNextRotorsTurns = foldr (\f acc -> f acc) makeRotors2' (replicate 100 nextRotorsTurns) 



prop_ValidLetterPassRotor :: Rotor -> Gen Bool
prop_ValidLetterPassRotor rs = do
                                 a <- choose (0, 25) :: Gen Int
                                 let result = passRotor a rs 
                                 return $ isLetter (intToChar result)


prop_passRotor :: Rotor -> Gen Bool
prop_passRotor r = do
                     a <- choose (0, 25) :: Gen Int 
                     let result = passRotor a r 
                     return $ passRotor result r{rotorWiring = inverseRotorWiring $ rotorWiring r} == a

prop_pRotorI :: Int -> Gen Bool
prop_pRotorI stps = do
                     a <- choose (0, 25) :: Gen Int 
                     let result = passRotor a rotorI 
                     return $ passRotor result invRotorI == a



prop_IdRotor :: Int -> Gen Bool
prop_IdRotor stps = do
                        a <- choose (0, 25) :: Gen Int
                        n <- choose (-1, 10) :: Gen Int
                        let result = passRotor a (makeTestRotorId n) 
                        return $ result == a


prop_Reflector :: Int -> Gen Bool
prop_Reflector stps = do
                        a <- choose (0, 25) :: Gen Int
                        let result = passRotor a reflectorB
                        return $ a == passRotor result reflectorB

prop_PressKey :: Int -> Gen Bool
prop_PressKey stps = do
                        a <- choose (0, 25) :: Gen Int
                        let (stps', output, _) = pressKey a (stps, [], makeRotors1')
                            result = head output 
                            (stps'', output', _) = pressKey result (stps, [], makeRotors1')
                            result' = head output'
                        return $ a == result' && stps + 1 == stps' && stps + 1 == stps''                  



{-
test_PressKey :: IO ()
test_PressKey = go 0
                where
                    go stps = do
                                c <- getChar
                                let (result, _, _)
-}

makeTestRotorId :: Int -> Rotor
makeTestRotorId n = Rotor n 0 0 0 0 testRotorId 

testRotorId :: RotorWiring
testRotorId = map charToInt ['A'..'Z']

invTestRotorId :: RotorWiring
invTestRotorId = inverseRotorWiring testRotorId

{-
prop_CorrectDiffRToL :: Int -> Rotor -> Gen Bool
prop_CorrectDiffRToL stps r@(Rotor n r' _) = do
                                             a <- choose (0, 25) :: Gen Int 
                                             let result = passRotorRToL a stps r
                                                 rStps = (stps `div` rotorSize^n) `mod` rotorSize 
                                                 diff = (r' !! (a + rStps) `mod` rotorSize) - (a + rStps) `mod` rotorSize
                                             return $ (result - diff) `mod` rotorSize == (a + rStps) `mod` rotorSize 
                                             


-- rotor stepping test every mod rotorSize
{-
prop_StepsToNthRSteps :: Int -> Gen Bool
prop_StepsToNthRSteps stps = do
                                let result = listStepsToNthRSteps stps 
                                return $ result == 
-}

listStepsToNthRSteps :: Steps -> [Int]
listStepsToNthRSteps stps = let rs = [0..(rotorNumber - 1)]
                          in [stepsToNthRSteps stps n | n <- rs]




sum' :: Int -> Int -> Int 
sum' n m = n + m

prop_sum' n = do
                x <- choose (1, 1) :: Gen Int
                return $ sum' x n === x + n


-}
-- nextStep :: Letter -> Steps -> Rotors -> (MachineState, Rotors)
-- type MachineState = (Steps, Outputs, Rotors)

test_NextStep :: [(Steps, Letter)] -> Rotors -> String
test_NextStep [] _ = []
test_NextStep ((stps, l) : sls) rs = let ((_, output, _), rs') = nextStep l stps rs 
                                     in intToChar (head output) : test_NextStep sls rs'
-- for makeRotors1'
aOneThousand = zip [0 :: Steps ..] $ map (charToInt . toUpper) $ filter (not . isSpace) "ftzmg isxip jwgdn jjcoq tyrig dmxfi esrwz gtoiu iekkd cshtp yoepv xnhvr wwesf ruxdg wozdm nkizw nczdu coblt uyhdz govbu ypkoj wbows eemtz fwygk odtbz dqrcz cifdi dxcqz ookvi iomll egmso jxhnf hbofd zctzq powvo mqnwq quozu fmsdx mjxiy zkozd ewged jxsmy hkjkr iqxwb itwly usthz qmgtx xwihd obtkc gzuve kyeky rewly wfmhl qjqjw cvtks nhzeg wzkve xktdz xlchr yjqqd zhyyp zoryg fkkkg ufdcu tkrjq gzwjd lmtyy igdox oigqd wqgou yupew dwcin gpdob rkxtj lkqjs rbimx vgzme bfzkl owxuk tdfnf nyyyo wzyjw origo khhln gbpuy xfdcq lpxsc hhslj lsyfs lcmmb knglv kwvqv djgoi quuhq xokdp icpey cmhko kedzd tjvsy ekpow mcrzg rvfwg fekew tpmzt vbxmk iihhh myxjn jvjil lvqbx eqyho mtnzr fdbst ekfir qhyoi zdmbt sverb nhjpi joufl jtnul rzzcb wswex nrhfg kjlud pxzji qtlnz fkylr ugebh ruksy gqkpr clkyq bpbhd vlosr zfbru ngqyd wsley mypsn rwmhy rglvr uptfu pucne omqhb ecbnj xvzfs qmzbu sefxw fcpli prqlk pmumk hvkbo xbkun ixhbh dvqgd pjgjc scemm ztfwj lbdsc porey pzgex jnmlv zmyzv brzui rgjtm fhgew erplp mjpxp gyyei editg mcqwq ndqjr dojpo bflyk iidwj lqllj zjddd tpsml xipkc mzyln pnzxn tdzto toruh hkgyu ftrnz cvrzx pxjuy qnksg hqcwd pksom hmlqv bwrmb ptcih jsled cvjrv bxvne xexit htexx xlxkk ckkht lxwey hjuxv dowlb"
-- for makeRotors1'
zOneThousand = zip [0 :: Steps ..] $ map (charToInt . toUpper) $ filter (not . isSpace) "tvaoo yxili grjsi ikjhm ogbcd ilwlk jgtna mvbcy mnxfp bfily tdqms txoms fdqgh ncgbk buaqv kwgaq cxabb gnlss ymdga cgwsh ljscm yqvnb ttjma ifthn mjrna kikna gsmeg hrxya vcnow hpkxc jcqqn wgixg sdcdk asdav swdng ctxtd bfeaw hhvtk puydl atgag kmlmw leion lmqhw cmgxt sxmuu wrbua fkuww ixgks iddcm bavsq evkdn mmyth qiiji ywdmn ikqvo xfamh uapuc sbvfa opwid ityes abluc anjek uswef ypysr cpepv fakdh cdoxb ddfls wohhc xppnb jotcx mcito lwbee ljsxh bqlpp gohfi dtapm tkaof njwbh bybiq rpgpt yaset neete rokno qynjo oumjg kgyyj xvpws gvucb pdqgp wvwhb cbqht uyhqt kprtm nbcvm sngri gnksv mihaf wwnvg cbito wphap edcbd stmyk wffau dtdbf neeuc rrgde lpgrh qcyro lnrkh sihak uxili gmkpy morbc aidvl cjmpi qionp smquw hhsei taadk mvkfn joomn fgwkl mrago owwga xcjpu xlljp fqudi wctrb wugke vyjnw tetun akttf kvymk vthmk vvcbq ttopi wwmmc bowcs ibdhr jlpyk pwxut elaub tgasx glrbd ivkxt elfsi ersjv udjwi fpitd skbre inoic fderq jjvkx abhgy yceen ovgwq jaibi kuxsp ajpai ntafl tsepy nyyri borvf lefud donbf fojut pocsh trerb wvkos etonb ufhjr ijmrf atxuj qioew kwwbg kasqm srawc hxars yeonm cbdke dijca bumab ojheh kyeww ovium dsnmp itwlu tgbnv wflry mgxlo wixpd summd kxnms podgc nebeg obfvr ytdqq sriog ggvxo"

-- for makeRotors4'
pOneThousand = zip [0 :: Steps ..] $ map (charToInt . toUpper) $ filter (not . isSpace) "ywcko kcaha cjrco uhbrk mjhgd fuvjz ffury fayiz ldqwx snmtz jthtb bqaoh hthof dslvk sjxst earwc rxuxs xaaov tydfb jhtlo ufycf cvxem romgv djhuw asbwj evvgd ojtgv iqomw qcleo uyqlw fainc xnyih hvonc whgly wlvkm nlcga hcdor auxnq tovoe rsoac grgqc yakgn yvvbb vljuc gwtkt yuiua sqcju lonjq bbiyf nfbrk sriyt soxbk rllns rvgbb lrfic yglgq asefo snuee xjygs uqkhi utycf qaech mxyuy ehbsa vdlyr crfgb bkmox fjxjh jyxfe vnaud xklzt wiiii jxgth hbyja qtqly xyeou iquof zlsvn ixisl vkesd tohts hlnbv bvfca xjkzm ylxhe tzeka hbwnf jrvku ulvsz ralbi udyay mgkim ffouw lsyjl qnlcy zekry bkltf zoail jmjts zafzu xsurc tkagb htexr wakqa qmwqf sirxn mbonb ezzys wbgte edgao ruabb jwyqw nscyi yunwx jgfdo wgqtv lvuiw mzmtn biovx lkjzb zijlf utqmo jlsrz fkdzm vyaug gdorw cublw ofnsi czldw sosao byahg uydyz azvox dqxxz dhkgx lrwsk qgebh kbusm vzkwu youxl tvhzb fnagw gelfx xqcgz lchbn enrty ahxus niynl uvwfe cbnyo chioy kymib vbuov zmigy axfya qegxi ctsbv gvryi xrvux ymgjs kteuo swwnk ketxj onivd emejs txqgo kodav limkl ygqou fshyd egcga earms grhhb xvbns zkjke jrtim jwzfz lvnim fvjlu sfyfr tzeil saszc eaalj umtzj fjgke nsrto iyjgd unvlz sroom uhksx qlvez whdtg mvltx buygo mexug kfcmd fwitj ftnnx gskhx nndmz"

-- for makeRotors5'
hOneThousand = zip [0 :: Steps ..] $ map (charToInt . toUpper) $ filter (not . isSpace) "slvpg gppiq waeop celeq owxtp fltbf bbtaz yrscc uaiss vxrwd dbcdq fqvzx mxkkk czqaf xnxrt busqf xooyd sezjd bayab yyvut sckew gbmzq flztv yvzut iepjb cbszq xskrq xpcox fawpx knded deikl llvmv bltzz dauzc fcafg pxddt gorxi bbttf pwdxt djbqj njlll nyjys bplbi uoqrs qjufi dfupj ivaas placb nwttr smfif oxnnz ljmav xsbpl lltoq afdju cidqw meitq unext wqlec zvddt yorlf tpvpi kzujs ganco gglos royok lfeey zfyoc tuatf zkwdz tqlta fsmca ddfov pcqjr zypuc xquae xvcaz wxlvp qoyel ciyfk tzkjd cafma fwxtu pfkxn aaiam ybdxl yuzmd bvpmb fbdco ubotb quzwp ebnty wzkda qgdzt lnfsw cttep jairk lybso bellx loifs izqwe pedbr tiwzs odnso zqaoe evtyl mfyxn ppios uuwpt ucrva vsnpd gecbm pycpv kiocr iftrm afsng moepf sfzaf veroz bdimw qcwvv yxukl omzeg ycawv opopt cfulq pyzvv aajqj mjeal mpidp lqiol uckkf qocjj bpsxx uafjc tadzy xexix jmkox wuyon ielyf xgdpz jikfk kjnng ywtqp ukzak gbtyr gzvaz awybz gykfa fvnve cwabq nqciv ovmuk vkizs gjuel itbuw mvace avqsd brikn fgwxe vftpa mmyaj esjgm meqpb nlltr eroab jimoe bmjyx lpotl acqaf yqjsl xisjd vytsa pozek zwpws dipao bukfb zgpkz lnqqy zdtkj rrwnk mvwmk esnau pncri geqdt mfato txinl lsqnt mydez ijwlr tppat fmdsq gzwcu ubbtb ksrsn erwwc pegaq zzdmi kbxyc ixfjo"
-- test states and rotors

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

makeRotors4' :: Rotors
makeRotors4' = [plugboard, testRotorI, testRotorII, testRotorIII, reflectorB, testInvRotorIII, testInvRotorII, testInvRotorI, plugboard]

makeRotors5' :: Rotors
makeRotors5' = [plugboard, testRotorIV, testRotorV, testRotorIII, reflectorB, testInvRotorIII, testInvRotorV, testInvRotorIV, plugboard]


-- startpos 'K', ring setting 4
testRotorI :: Rotor
testRotorI = Rotor 0 10 3 10 r1Turnover (setRing 3 rotorIWiring)  

testInvRotorI :: Rotor
testInvRotorI = Rotor 0 10 3 10 r1Turnover (setRing 3 invRotorIWiring)

-- startpos 'D', ring setting 8
testRotorII :: Rotor
testRotorII = Rotor 1 3 7 3 r2Turnover (setRing 7 rotorIIWiring)  

testInvRotorII :: Rotor
testInvRotorII = Rotor 1 3 7 3 r2Turnover (setRing 7 invRotorIIWiring)

-- startpos 'X', ring setting 26
testRotorIII :: Rotor
testRotorIII = Rotor 2 23 25 23 r3Turnover (setRing 25 rotorIIIWiring)  

testInvRotorIII :: Rotor
testInvRotorIII = Rotor 2 23 25 23 r3Turnover (setRing 25 invRotorIIIWiring)

-- pos 0, startpos 'I', ringsetting 11
testRotorIV :: Rotor
testRotorIV = makeRotor 4 0 8 10

testInvRotorIV :: Rotor
testInvRotorIV = makeInvRotor 4 0 8 10

-- pos 1, startpos 'R', ringSetting 3
testRotorV :: Rotor
testRotorV = makeRotor 5 1 17 2

testInvRotorV :: Rotor
testInvRotorV = makeInvRotor 5 1 17 2



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