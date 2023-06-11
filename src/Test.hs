module Test where

import Test.QuickCheck
import Types
import Lib
import Data.Char
import Actions
import System.Console.ANSI
import Data.List (intersperse, foldl')
import qualified Data.Text as T
import Control.Monad
--import Control.Monad.Extra
import Control.Monad.State

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

prop_passRotorI :: Int -> Gen Bool
prop_passRotorI stps = do
                         a <- choose (0, 25) :: Gen Int 
                         let result = passRotor a rotorI 
                         return $ passRotor result invRotorI == a



prop_IdRotor :: Int -> Gen Bool
prop_IdRotor stps = do
                        a <- choose (0, 25) :: Gen Int
                        n <- choose (-1, 10) :: Gen Int
                        let result = passRotor a (makeTestRotorId n) 
                        return $ result == a


prop_ReflectorB :: Int -> Gen Bool
prop_ReflectorB stps = do
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


prop_configPlugboard :: Gen Bool
prop_configPlugboard = do
                        pc <- shuffle [0..25]
                        let (h, t) = splitAt 13 pc 
                            result = configPlugboard rotorIdWiring (h, t)
                        return $ result == inverseRotorWiring result 

prop_inverseRotorWiring :: Gen Bool
prop_inverseRotorWiring = do
                            pc <- shuffle [0..25]
                            let result = inverseRotorWiring pc
                                result' = inverseRotorWiring result 
                            return $ pc == result' 


makeTestRotorId :: Int -> Rotor
makeTestRotorId n = Rotor n 0 0 0 0 testRotorId 

testRotorId :: RotorWiring
testRotorId = map charToInt ['A'..'Z']

invTestRotorId :: RotorWiring
invTestRotorId = inverseRotorWiring testRotorId


test_NextStep :: [(Steps, Letter)] -> Rotors -> String
test_NextStep [] _ = []
test_NextStep ((stps, l) : sls) rs = let ((_, output, _), rs') = nextStep l stps rs 
                                     in intToChar (head output) : test_NextStep sls rs'

test_printMachine :: Steps -> Tinput -> Toutput -> Eoutput -> Rotors -> IO (Tinput, Toutput) 
test_printMachine stps input output eoutput rs = do
                                             clearScreen
                                             putStr "Steps - " 
                                             putStr $ show stps 
                                             cursorForward 5
                                             setSGR [SetColor Foreground Vivid Green]
                                             putStr "Rotors - " 
                                             putStrLn (intersperse '-' $ map intToChar $ foldl' (\acc r -> turns r : acc) [] rs)
                                             setSGR [Reset]
                                             {-
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
                                             -}
                                             return (input, output)


-- for makeRotors1'
aOneThousand = zip [0 :: Steps ..] $ map (charToInt . toUpper) $ filter (not . isSpace) "ftzmg isxip jwgdn jjcoq tyrig dmxfi esrwz gtoiu iekkd cshtp yoepv xnhvr wwesf ruxdg wozdm nkizw nczdu coblt uyhdz govbu ypkoj wbows eemtz fwygk odtbz dqrcz cifdi dxcqz ookvi iomll egmso jxhnf hbofd zctzq powvo mqnwq quozu fmsdx mjxiy zkozd ewged jxsmy hkjkr iqxwb itwly usthz qmgtx xwihd obtkc gzuve kyeky rewly wfmhl qjqjw cvtks nhzeg wzkve xktdz xlchr yjqqd zhyyp zoryg fkkkg ufdcu tkrjq gzwjd lmtyy igdox oigqd wqgou yupew dwcin gpdob rkxtj lkqjs rbimx vgzme bfzkl owxuk tdfnf nyyyo wzyjw origo khhln gbpuy xfdcq lpxsc hhslj lsyfs lcmmb knglv kwvqv djgoi quuhq xokdp icpey cmhko kedzd tjvsy ekpow mcrzg rvfwg fekew tpmzt vbxmk iihhh myxjn jvjil lvqbx eqyho mtnzr fdbst ekfir qhyoi zdmbt sverb nhjpi joufl jtnul rzzcb wswex nrhfg kjlud pxzji qtlnz fkylr ugebh ruksy gqkpr clkyq bpbhd vlosr zfbru ngqyd wsley mypsn rwmhy rglvr uptfu pucne omqhb ecbnj xvzfs qmzbu sefxw fcpli prqlk pmumk hvkbo xbkun ixhbh dvqgd pjgjc scemm ztfwj lbdsc porey pzgex jnmlv zmyzv brzui rgjtm fhgew erplp mjpxp gyyei editg mcqwq ndqjr dojpo bflyk iidwj lqllj zjddd tpsml xipkc mzyln pnzxn tdzto toruh hkgyu ftrnz cvrzx pxjuy qnksg hqcwd pksom hmlqv bwrmb ptcih jsled cvjrv bxvne xexit htexx xlxkk ckkht lxwey hjuxv dowlb"
-- for makeRotors1'
zOneThousand = zip [0 :: Steps ..] $ map (charToInt . toUpper) $ filter (not . isSpace) "tvaoo yxili grjsi ikjhm ogbcd ilwlk jgtna mvbcy mnxfp bfily tdqms txoms fdqgh ncgbk buaqv kwgaq cxabb gnlss ymdga cgwsh ljscm yqvnb ttjma ifthn mjrna kikna gsmeg hrxya vcnow hpkxc jcqqn wgixg sdcdk asdav swdng ctxtd bfeaw hhvtk puydl atgag kmlmw leion lmqhw cmgxt sxmuu wrbua fkuww ixgks iddcm bavsq evkdn mmyth qiiji ywdmn ikqvo xfamh uapuc sbvfa opwid ityes abluc anjek uswef ypysr cpepv fakdh cdoxb ddfls wohhc xppnb jotcx mcito lwbee ljsxh bqlpp gohfi dtapm tkaof njwbh bybiq rpgpt yaset neete rokno qynjo oumjg kgyyj xvpws gvucb pdqgp wvwhb cbqht uyhqt kprtm nbcvm sngri gnksv mihaf wwnvg cbito wphap edcbd stmyk wffau dtdbf neeuc rrgde lpgrh qcyro lnrkh sihak uxili gmkpy morbc aidvl cjmpi qionp smquw hhsei taadk mvkfn joomn fgwkl mrago owwga xcjpu xlljp fqudi wctrb wugke vyjnw tetun akttf kvymk vthmk vvcbq ttopi wwmmc bowcs ibdhr jlpyk pwxut elaub tgasx glrbd ivkxt elfsi ersjv udjwi fpitd skbre inoic fderq jjvkx abhgy yceen ovgwq jaibi kuxsp ajpai ntafl tsepy nyyri borvf lefud donbf fojut pocsh trerb wvkos etonb ufhjr ijmrf atxuj qioew kwwbg kasqm srawc hxars yeonm cbdke dijca bumab ojheh kyeww ovium dsnmp itwlu tgbnv wflry mgxlo wixpd summd kxnms podgc nebeg obfvr ytdqq sriog ggvxo"

-- for makeRotors4'
pOneThousand = zip [0 :: Steps ..] $ map (charToInt . toUpper) $ filter (not . isSpace) "ywcko kcaha cjrco uhbrk mjhgd fuvjz ffury fayiz ldqwx snmtz jthtb bqaoh hthof dslvk sjxst earwc rxuxs xaaov tydfb jhtlo ufycf cvxem romgv djhuw asbwj evvgd ojtgv iqomw qcleo uyqlw fainc xnyih hvonc whgly wlvkm nlcga hcdor auxnq tovoe rsoac grgqc yakgn yvvbb vljuc gwtkt yuiua sqcju lonjq bbiyf nfbrk sriyt soxbk rllns rvgbb lrfic yglgq asefo snuee xjygs uqkhi utycf qaech mxyuy ehbsa vdlyr crfgb bkmox fjxjh jyxfe vnaud xklzt wiiii jxgth hbyja qtqly xyeou iquof zlsvn ixisl vkesd tohts hlnbv bvfca xjkzm ylxhe tzeka hbwnf jrvku ulvsz ralbi udyay mgkim ffouw lsyjl qnlcy zekry bkltf zoail jmjts zafzu xsurc tkagb htexr wakqa qmwqf sirxn mbonb ezzys wbgte edgao ruabb jwyqw nscyi yunwx jgfdo wgqtv lvuiw mzmtn biovx lkjzb zijlf utqmo jlsrz fkdzm vyaug gdorw cublw ofnsi czldw sosao byahg uydyz azvox dqxxz dhkgx lrwsk qgebh kbusm vzkwu youxl tvhzb fnagw gelfx xqcgz lchbn enrty ahxus niynl uvwfe cbnyo chioy kymib vbuov zmigy axfya qegxi ctsbv gvryi xrvux ymgjs kteuo swwnk ketxj onivd emejs txqgo kodav limkl ygqou fshyd egcga earms grhhb xvbns zkjke jrtim jwzfz lvnim fvjlu sfyfr tzeil saszc eaalj umtzj fjgke nsrto iyjgd unvlz sroom uhksx qlvez whdtg mvltx buygo mexug kfcmd fwitj ftnnx gskhx nndmz"

-- for makeRotors5'
hOneThousand = zip [0 :: Steps ..] $ map (charToInt . toUpper) $ filter (not . isSpace) "slvpg gppiq waeop celeq owxtp fltbf bbtaz yrscc uaiss vxrwd dbcdq fqvzx mxkkk czqaf xnxrt busqf xooyd sezjd bayab yyvut sckew gbmzq flztv yvzut iepjb cbszq xskrq xpcox fawpx knded deikl llvmv bltzz dauzc fcafg pxddt gorxi bbttf pwdxt djbqj njlll nyjys bplbi uoqrs qjufi dfupj ivaas placb nwttr smfif oxnnz ljmav xsbpl lltoq afdju cidqw meitq unext wqlec zvddt yorlf tpvpi kzujs ganco gglos royok lfeey zfyoc tuatf zkwdz tqlta fsmca ddfov pcqjr zypuc xquae xvcaz wxlvp qoyel ciyfk tzkjd cafma fwxtu pfkxn aaiam ybdxl yuzmd bvpmb fbdco ubotb quzwp ebnty wzkda qgdzt lnfsw cttep jairk lybso bellx loifs izqwe pedbr tiwzs odnso zqaoe evtyl mfyxn ppios uuwpt ucrva vsnpd gecbm pycpv kiocr iftrm afsng moepf sfzaf veroz bdimw qcwvv yxukl omzeg ycawv opopt cfulq pyzvv aajqj mjeal mpidp lqiol uckkf qocjj bpsxx uafjc tadzy xexix jmkox wuyon ielyf xgdpz jikfk kjnng ywtqp ukzak gbtyr gzvaz awybz gykfa fvnve cwabq nqciv ovmuk vkizs gjuel itbuw mvace avqsd brikn fgwxe vftpa mmyaj esjgm meqpb nlltr eroab jimoe bmjyx lpotl acqaf yqjsl xisjd vytsa pozek zwpws dipao bukfb zgpkz lnqqy zdtkj rrwnk mvwmk esnau pncri geqdt mfato txinl lsqnt mydez ijwlr tppat fmdsq gzwcu ubbtb ksrsn erwwc pegaq zzdmi kbxyc ixfjo"
-- test states and rotors

testMachineState' = MachineState' {getSteps = 0, getInputs' = T.empty, getOutputs' = T.empty, getRotors = [Rotor {rotorPos = -1, startPos = 0, ringSetting = 0, turns = 0, turnover = 0, rotorWiring = [1,0,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]},Rotor {rotorPos = 0, startPos = 0, ringSetting = 0, turns = 0, turnover = 16, rotorWiring = [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9]},Rotor {rotorPos = 1, startPos = 0, ringSetting = 10, turns = 0, turnover = 4, rotorWiring = [0,16,9,23,25,8,15,5,24,14,10,19,13,20,2,18,1,4,7,11,21,17,6,3,22,12]},Rotor {rotorPos = 2, startPos = 0, ringSetting = 0, turns = 0, turnover = 21, rotorWiring = [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]},Rotor {rotorPos = -1, startPos = 0, ringSetting = 0, turns = 0, turnover = 0, rotorWiring = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]},Rotor {rotorPos = 2, startPos = 0, ringSetting = 0, turns = 0, turnover = 21, rotorWiring = [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12]},Rotor {rotorPos = 1, startPos = 0, ringSetting = 10, turns = 0, turnover = 4, rotorWiring = [0,16,14,23,17,7,22,18,5,2,10,19,25,12,9,6,1,21,15,11,13,20,24,3,8,4]},Rotor {rotorPos = 0, startPos = 0, ringSetting = 0, turns = 0, turnover = 16, rotorWiring = [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9]},Rotor {rotorPos = -1, startPos = 0, ringSetting = 0, turns = 0, turnover = 0, rotorWiring = [1,0,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]}], getRotorT = [3,2,1], getStartPositions = [0,0,0], getRingSettings = [0,10,0], getReflector = 'B', getPlugboard = [('A','B')]}

-- B III II I AAA 111 
testMachineState1 = MachineState' {getSteps = 0, getInputs' = T.empty, getOutputs' = T.empty, getRotors = [Rotor {rotorPos = -1, startPos = 0, ringSetting = 0, turns = 0, turnover = 0, rotorWiring = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]},Rotor {rotorPos = 0, startPos = 0, ringSetting = 0, turns = 0, turnover = 16, rotorWiring = [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9]},Rotor {rotorPos = 1, startPos = 0, ringSetting = 0, turns = 0, turnover = 4, rotorWiring = [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4]},Rotor {rotorPos = 2, startPos = 0, ringSetting = 0, turns = 0, turnover = 21, rotorWiring = [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14]},Rotor {rotorPos = -1, startPos = 0, ringSetting = 0, turns = 0, turnover = 0, rotorWiring = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19]},Rotor {rotorPos = 2, startPos = 0, ringSetting = 0, turns = 0, turnover = 21, rotorWiring = [19,0,6,1,15,2,18,3,16,4,20,5,21,13,25,7,24,8,23,9,22,11,17,10,14,12]},Rotor {rotorPos = 1, startPos = 0, ringSetting = 0, turns = 0, turnover = 4, rotorWiring = [0,9,15,2,25,22,17,11,5,1,3,10,14,19,24,20,16,6,4,13,7,23,12,8,21,18]},Rotor {rotorPos = 0, startPos = 0, ringSetting = 0, turns = 0, turnover = 16, rotorWiring = [20,22,24,6,0,3,5,15,21,25,1,4,2,10,12,19,7,23,18,11,17,8,13,16,14,9]},Rotor {rotorPos = -1, startPos = 0, ringSetting = 0, turns = 0, turnover = 0, rotorWiring = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]}], getRotorT = [3,2,1], getStartPositions = [0,0,0], getRingSettings = [0,0,0], getReflector = 'B', getPlugboard = []}

-- C V III IV XEB AB GC RT SE
testMachineState2 = MachineState' {getSteps = 0, getInputs' = T.empty, getOutputs' = T.empty, getRotors = [Rotor {rotorPos = -1, startPos = 0, ringSetting = 0, turns = 0, turnover = 0, rotorWiring = [1,0,6,3,18,5,2,7,8,9,10,11,12,13,14,15,16,19,4,17,20,21,22,23,24,25]},Rotor {rotorPos = 0, startPos = 1, ringSetting = 17, turns = 1, turnover = 9, rotorWiring = [7,11,25,8,24,14,2,4,22,10,23,1,20,19,3,13,18,21,9,5,12,6,16,0,17,15]},Rotor {rotorPos = 1, startPos = 4, ringSetting = 19, turns = 4, turnover = 21, rotorWiring = [8,10,12,16,14,18,6,17,23,1,15,25,19,3,5,13,11,9,7,20,22,24,0,2,4,21]},Rotor {rotorPos = 2, startPos = 23, ringSetting = 1, turns = 23, turnover = 25, rotorWiring = [11,22,0,2,18,7,9,20,25,21,16,19,4,14,8,12,24,1,23,13,10,17,15,6,5,3]},Rotor {rotorPos = -1, startPos = 0, ringSetting = 0, turns = 0, turnover = 0, rotorWiring = [5,21,15,9,8,0,14,24,4,3,17,25,23,22,6,2,19,10,20,16,18,1,13,12,7,11]},Rotor {rotorPos = 2, startPos = 23, ringSetting = 1, turns = 23, turnover = 25, rotorWiring = [2,17,3,25,12,24,23,5,14,6,20,0,15,19,13,22,10,21,4,11,7,9,1,18,16,8]},Rotor {rotorPos = 1, startPos = 4, ringSetting = 19, turns = 4, turnover = 21, rotorWiring = [22,9,23,13,24,14,6,18,0,17,1,16,2,15,4,10,3,7,5,12,19,25,20,8,21,11]},Rotor {rotorPos = 0, startPos = 1, ringSetting = 17, turns = 1, turnover = 9, rotorWiring = [23,11,6,14,7,19,21,0,3,18,9,1,20,15,5,25,22,24,16,13,12,17,8,10,4,2]},Rotor {rotorPos = -1, startPos = 0, ringSetting = 0, turns = 0, turnover = 0, rotorWiring = [1,0,6,3,18,5,2,7,8,9,10,11,12,13,14,15,16,19,4,17,20,21,22,23,24,25]}], getRotorT = [5,3,4], getStartPositions = [23,4,1], getRingSettings = [1,19,17], getReflector = 'C', getPlugboard = [('S','E'),('R','T'),('G','C'),('A','B')]}



makeRotors' :: Rotors
makeRotors' = [rotorI, reflectorB, invRotorI] 

makeRotors1' :: Rotors
makeRotors1' = [testPlugboard, rotorI, rotorII, rotorIII, reflectorB, invRotorIII, invRotorII, invRotorI, testPlugboard]

makeRotors2' :: Rotors
makeRotors2' = [testPlugboard, rotorI, rotorII, rotorIII, reflectorB]

makeRotors3' :: Rotors
makeRotors3' = [testPlugboard, rotorIV, rotorV, rotorI, reflectorB]

makeRotors4' :: Rotors
makeRotors4' = [testPlugboard, testRotorI, testRotorII, testRotorIII, reflectorB, testInvRotorIII, testInvRotorII, testInvRotorI, testPlugboard]

makeRotors5' :: Rotors
makeRotors5' = [testPlugboard, testRotorIV, testRotorV, testRotorIII, reflectorB, testInvRotorIII, testInvRotorV, testInvRotorIV, testPlugboard]

makeRotors6' :: Rotors
makeRotors6' = [rotorIII, rotorII, rotorI]

-- startpos 'K', ring setting 4, reflector B, null plugboard
testRotorI :: Rotor
testRotorI = Rotor 0 10 3 10 r1Turnover (setRing 3 rotorIWiring)  

testInvRotorI :: Rotor
testInvRotorI = Rotor 0 10 3 10 r1Turnover (setRing 3 invRotorIWiring)

-- startpos 'D', ring setting 8, reflector B, null plugboard
testRotorII :: Rotor
testRotorII = Rotor 1 3 7 3 r2Turnover (setRing 7 rotorIIWiring)  

testInvRotorII :: Rotor
testInvRotorII = Rotor 1 3 7 3 r2Turnover (setRing 7 invRotorIIWiring)

-- startpos 'X', ring setting 26, reflector B, null plugboard
testRotorIII :: Rotor
testRotorIII = Rotor 2 23 25 23 r3Turnover (setRing 25 rotorIIIWiring)  

testInvRotorIII :: Rotor
testInvRotorIII = Rotor 2 23 25 23 r3Turnover (setRing 25 invRotorIIIWiring)

-- pos 0, startpos 'I', ringsetting 11, reflector B, null plugboard
testRotorIV :: Rotor
testRotorIV = makeRotor 4 0 8 10

testInvRotorIV :: Rotor
testInvRotorIV = makeInvRotor 4 0 8 10

-- pos 1, startpos 'R', ringSetting 3
testRotorV :: Rotor
testRotorV = makeRotor 5 1 17 2

testInvRotorV :: Rotor
testInvRotorV = makeInvRotor 5 1 17 2

testPlugboard :: Plugboard
testPlugboard = makePlugboard rotorIdWiring




    -- C, V , II, IV
    --    v    x   e
    --    10   23  15

testText = "DuringthefirstpartofyourlifeyouonlybecomeawareofhappinessonceyouhavelostitThenanagecomesasecondoneinwhichyoualreadyknowatthemomentwhenyoubegintoexperiencetruehappinessthatyouareattheendofthedaygoingtoloseitWhenImetBelleIunderstoodthatIhadjustenteredthissecondageIalsounderstoodthatIhadntreachedthethirdageinwhichanticipationofthelossofhappinesspreventsyoufromliving"
cyphertestText = filter (not . isSpace) "yttww xsoqt emgpt gkzbl lmdyb otclk llkkb hqlbn zpaxm ulvwq ywfvu utwgz hunlo ndrsc hfkgb yxtxz oplak fzbpv clfrm lqwze vqajr hpxwb ayofi uwwgm alwgc rfrfb mcqrm lfstz ztdjr adypf fszzu yonqd enfto ewroc iaxcj jacqw bdxef jnxua hophz lufii ykwjc omkvz jgfyv yyxow vwxxw fbrtz kinwn xjivb wqcfd xtcum boffd qaqur jmovf bhtvn ayqxy fregi fjsux hvoxj xuonh lzatq nbypo xiemb tnlsf gpfxf yolxk ugewj ynowo kwoec covpt tzajx ukaaj edfdq aoabo"

{- C, V, III, IV
      X   E    B
       2  20   18
       AB GC RT SE 
-}

testText2 = "YouronlychanceofsurvivalifyouaresincerelysmittenliesinhidingthisfactfromthewomanyouloveoffeigningacasualdetachmentunderallcircumstancesWhatsadnessthereisinthissimpleobservationWhatanaccusationagainstmanHoweverithadneveroccurredtometocontestthislawnortoimaginedisobeyingitlovemakesyouweakandtheweakerofthetwoisoppressedtorturedandfinallykilledbytheotherwhoinhisorherturnoppressestorturesandkillswithouthavingevilintentionswithoutevengettingpleasurefromitwithcompleteindifferencethatswhatmennormallycalllove"


