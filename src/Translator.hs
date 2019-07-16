module Translator where
import Parser

data MimaInstruction = ILdc Int | IXLdc String | ILdv Int | IXLdv String | IStv Int | IXStv String
                   | IAdd Int | IXAdd String | IAnd Int | IXAnd String | IOr Int | IXOr String
                   | IXor Int | IEql Int | IXEql String | IJmp Int | IXJmp String
                   | IJmn Int | IXJmn String | ILdiv Int | IStiv Int | IXLdiv String | IXStiv String
                   | IJms Int | IJind Int | IXJind String
                   | IHalt | INot | IRar | IXLds String | IXSts String
                   | IXStatic String Int | IXSegment Int | IXLabel String
                   | IXComment String deriving Show

  -- virtual registers
virtregs = 1
lcls = IXStatic "LCL" 0
lcl = "LCL" --lcl = 1
args = IXStatic "ARG" 0
arg = "ARG" --arg = 2
thiss = IXStatic "THIS" 0
this = "THIS" --this = 3
thats = IXStatic "THAT" 0
that = "THAT" --that = 4
temps = [IXStatic "TEMP0" 0, IXStatic "TEMP1" 0, IXStatic "TEMP2" 0, IXStatic "TEMP3" 0,
        IXStatic "TEMP4" 0, IXStatic "TEMP5" 0, IXStatic "TEMP6" 0, IXStatic "TEMP7" 0]
temp = "TEMP" --temp = 5
sps = IXStatic "SP" 0
sp = "SP" --sp = 13
t0s = IXStatic "T0" 0
t0 = "T0" --t0 = 14
t1s = IXStatic "T1" 0
t1 = "T1" --t1 = 15
static = 16
prog = 0x8000
  -- CF for control flow
monecf = "MONECF"
monescf = IXStatic "MONECF" (-1) --0xFFFFFF
mfivecf = "MFIVECF"
mfivescf = IXStatic "MFIVECF" (-5) --0xFFFFFB

mone = "MONE"
mones = IXStatic "MONE" 65535 --0x00FFFF

nflag = "NFLAG"
nflags = IXStatic "NFLAG" 32768 -- bit 15
call = "CALL"
calls = [IXLabel call] ++ spinc ++ [IXLdv lcl, IXStiv sp] ++
        spinc ++ [IXLdv arg, IXStiv sp] ++ spinc ++ [IXLdv this, IXStiv sp] ++ spinc ++ [IXLdv that, IXStiv sp] ++
        spinc ++ [IXLdv t1, INot, IXAdd sp, IXStv arg, IXLdv sp, IXStv lcl, IXJind t0]
ret = "RETURN"
rets = [IXLabel ret, IXLdv lcl, IXStv t0, IXAdd mfivecf, IXStv t1, IXLdiv t1, IXStv t1]
        ++ spdec ++ [IXLdiv sp, IXStiv arg, ILdc 1, IXAdd arg, IXStv sp,
        IXLdv monecf, IXAdd t0, IXStv t0, IXLdiv t0, IXStv that, IXLdv monecf, IXAdd t0, IXStv t0, IXLdiv t0, IXStv this,
        IXLdv monecf, IXAdd t0, IXStv t0, IXLdiv t0, IXStv arg, IXLdv monecf, IXAdd t0, IXStv t0, IXLdiv t0, IXStv lcl,
        IXJind t1]
spinc = [ILdc 1, IXAdd sp, IXStv sp]
spdec = [IXLdv monecf, IXAdd sp, IXStv sp]

getCommands :: VMFile -> [MimaInstruction]
getCommands (File filename vmfs) = foldr getCommandsFromVmf [] vmfs
  where
    getCommandsFromVmf :: VMFunction -> [MimaInstruction] -> [MimaInstruction]
    getCommandsFromVmf (Function l nlcls ls) cs = let (Label symb cs') = l in IXLabel symb : function nlcls ++ tail (getCommandsFromLabel (Label symb (ren cs' symb)) [] ) ++ foldr getCommandsFromLabel [] (renameLables ls l) ++ cs
      where
        getCommandsFromLabel :: Label -> [MimaInstruction] -> [MimaInstruction]
        getCommandsFromLabel (Label symb cs) is = IXLabel symb : translateCommands cs filename symb 0 ++ is
        function nlcls = if nlcls == 0 then [] else [ILdc 0, IXStiv sp] ++ spinc ++ function (nlcls - 1)
        renameLables :: [Label] -> Label -> [Label]
        renameLables ls (Label fn _) = map (\(Label n cs) -> (Label (fn ++ "." ++ n) (ren cs fn))) ls
        ren cs fn = map (ren' fn) cs                             -- translate all labels in goto commands to format vmfName.lableName
          where
            ren' fn (CGoto s) = CGoto (fn ++ "." ++ s)
            ren' fn (CIfGoto s) = CIfGoto (fn ++ "." ++ s)
            ren' fn c = c

translateFiles :: [VMFile] -> [MimaInstruction]
translateFiles [] = error "No files to translate!"
translateFiles fs = optimize (segments ++ bootstrap ++ translateFiles' fs ++ calls ++ rets)
--translateFiles fs = segments ++ bootstrap ++ translateFiles' fs ++ calls ++ rets
  where
    translateFiles' = foldr ((++) . getCommands) []
    segments = [IXSegment 0, IXJmp "START", IXSegment virtregs, lcls, args, thiss, thats] ++ temps ++ [sps, t0s, t1s]
    bootstrap = IXSegment static : getStatics (translateFiles' fs) ++ [IXSegment prog] ++ [IXLabel "START", IXJmp "Setup"] ++ [mones, monescf, mfivescf, nflags] ++ [IXLabel "Setup", ILdc 256, IXStv sp, IXJmp "Sys.init"]
      where
        getStatics = foldr concatStatics []
          where
            concatStatics (IXLds s) stcs = if not (any (\(IXStatic u _) -> s == u ) stcs) then IXStatic s 0 : stcs else stcs
            concatStatics (IXSts s) stcs = if not (any (\(IXStatic u _) -> s == u ) stcs) then IXStatic s 0 : stcs else stcs
            concatStatics _ stcs = stcs

translateCommands :: [Command] -> String -> String -> Int-> [MimaInstruction]
translateCommands [] _ _ _ = []
translateCommands (CPush Constant 0 : CArithLogic Not : cs) sn ln retNum = [IXLdv mone, IXStiv sp] ++ spinc ++ translateCommands cs sn ln (retNum + 1)
translateCommands (CPush seg1 idx1 : CPush seg2 idx2 : CArithLogic al : cs) sn ln retNum
  | Add <- al = caseSeg1 ++ [IXStv t0] ++ caseSeg2 ++ [IXAdd t0, IXAnd mone, IXStiv sp] ++ spinc ++ translateCommands cs sn ln (retNum + 1)
  | Sub <- al = caseSeg2 ++ [IXAdd mone, INot, IXStv t1] ++ caseSeg1 ++ [IXAdd t1, IXAnd mone, IXStiv sp] ++ spinc ++ translateCommands cs sn ln (retNum + 1)
  | Neg <- al = caseSeg1 ++ [IXStiv sp] ++ spinc ++ caseSeg2 ++ [IXAdd mone, INot, IXAnd mone, IXStiv sp] ++ spinc ++ translateCommands cs sn ln (retNum + 1)
  | Eq <- al = caseSeg1 ++ [IXStv t0] ++ caseSeg2 ++ [IXEql t0, IXAnd mone, IXStiv sp] ++ spinc ++ translateCommands cs sn ln (retNum + 1)
  | Gt <- al = caseSeg1 ++ [IXAdd mone, INot, IXStv t0] ++ caseSeg2 ++ [IXAdd t0, IXAnd nflag, IXEql nflag, IXAnd mone, IXStiv sp] ++ spinc ++ translateCommands cs sn ln (retNum + 1)
  | Lt <- al = caseSeg2 ++ [IXAdd mone, INot, IXStv t1] ++ caseSeg1 ++ [IXAdd t1, IXAnd nflag, IXEql nflag, IXAnd mone, IXStiv sp] ++ spinc ++ translateCommands cs sn ln (retNum + 1)
  | And <- al = caseSeg1 ++ [IXStv t0] ++ caseSeg2 ++ [IXAnd t0, IXStiv sp] ++ spinc ++ translateCommands cs sn ln (retNum + 1)
  | Or <- al = caseSeg1 ++ [IXStv t0] ++ caseSeg2 ++ [IXOr t0, IXStiv sp] ++ spinc ++ translateCommands cs sn ln (retNum + 1)
  | Not <- al = caseSeg1 ++ [IXStiv sp] ++ spinc ++ caseSeg2 ++ [INot, IXAnd mone, IXStiv sp] ++ spinc ++ translateCommands cs sn ln (retNum + 1)
  where
    caseSeg1
      | Argument <- seg1 = [ILdc idx1, IXAdd arg, IXStv t0, IXLdiv t0]
      | Local <- seg1 = [ILdc idx1, IXAdd lcl, IXStv t0, IXLdiv t0]
      | Static <- seg1 = [IXLds (sn ++ "." ++ show idx1)]
      | Constant <- seg1 = [ILdc idx1]--, IXAnd mone]
      | This <- seg1 = [ILdc idx1, IXAdd this, IXStv t0, IXLdiv t0]
      | That <- seg1 = [ILdc idx1, IXAdd that, IXStv t0, IXLdiv t0]
      | Pointer <- seg1 = if idx1 == 0 then [IXLdv this] else [IXLdv that]
      | Temp <- seg1 = [IXLdv (temp ++ show idx1)]
    caseSeg2
      | Argument <- seg2 = [ILdc idx2, IXAdd arg, IXStv t1, IXLdiv t1]
      | Local <- seg2 = [ILdc idx2, IXAdd lcl, IXStv t1, IXLdiv t1]
      | Static <- seg2 = [IXLds (sn ++ "." ++ show idx2)]
      | Constant <- seg2 = [ILdc idx2]--, IXAnd mone]
      | This <- seg2 = [ILdc idx2, IXAdd this, IXStv t1, IXLdiv t1]
      | That <- seg2 = [ILdc idx2, IXAdd that, IXStv t1, IXLdiv t1]
      | Pointer <- seg2 = if idx2 == 0 then [IXLdv this] else [IXLdv that]
      | Temp <- seg2 = [IXLdv (temp ++ show idx2)]

translateCommands (c:cs) sn ln retNum = translate c ++ translateCommands cs sn ln (retNum + 1)
  where
    translate :: Command -> [MimaInstruction]
    translate c
      | CPush seg val <- c = ldsegment seg val ++ [IXStiv sp] ++ spinc
      | CPop seg val <- c = spdec ++ strsegment seg val
      | CArithLogic arithlgc <- c = arithlogic arithlgc
      | CGoto symbol <- c = [IXJmp symbol]
      | CIfGoto symbol <- c = spdec ++ [IXLdiv sp, IXAdd monecf, INot, IXJmn symbol]
      | CCall funcName val <- c =  [IXLdc (sn ++ "." ++ ln ++ ".ret." ++ show retNum), IXStiv sp, IXLdc funcName, IXStv t0,
                                    ILdc (val + 4), IXStv t1, IXJmp call, IXLabel (sn ++ "." ++ ln ++ ".ret." ++ show retNum)]
      | CReturn <- c = [IXJmp ret]
      | _ <- c = error "VM Instruction can't be translated"
      where
        ldsegment :: Segment -> Int -> [MimaInstruction]
        ldsegment seg idx
          | Argument <- seg = [ILdc idx, IXAdd arg, IXStv t0, IXLdiv t0]
          | Local <- seg = [ILdc idx, IXAdd lcl, IXStv t0, IXLdiv t0]
          | Static <- seg = [IXLds (sn ++ "." ++ show idx)]
          | Constant <- seg = [ILdc idx]--, IXAnd mone]
          | This <- seg = [ILdc idx, IXAdd this, IXStv t0, IXLdiv t0]
          | That <- seg = [ILdc idx, IXAdd that, IXStv t0, IXLdiv t0]
          | Pointer <- seg = if idx == 0 then [IXLdv this] else [IXLdv that]
          | Temp <- seg = [IXLdv (temp ++ show idx)]

        strsegment :: Segment -> Int -> [MimaInstruction]
        strsegment seg idx
          | Argument <- seg = [ILdc idx, IXAdd arg, IXStv t0, IXLdiv sp, IXStiv t0] --error "Semantic error: It shouldn't be popped into argument."
          | Local <- seg = [ILdc idx, IXAdd lcl, IXStv t0, IXLdiv sp, IXStiv t0]
          | Static <- seg = [IXLdiv sp, IXSts (sn ++ "." ++ show idx)]
          | Constant <- seg = error "Semantic error: It shouldn't be popped into constant."
          | This <- seg = [ILdc idx, IXAdd this, IXStv t0, IXLdiv sp, IXStiv t0]
          | That <- seg = [ILdc idx, IXAdd that, IXStv t0, IXLdiv sp, IXStiv t0]
          | Pointer <- seg = if idx == 0 then [IXLdiv sp, IXStv this] else [IXLdiv sp, IXStv that]
          | Temp <- seg = [IXLdiv sp, IXStv (temp ++ show idx)]

        arithlogic :: ArithLogic -> [MimaInstruction]
        arithlogic al
          | Add <- al = spdec ++ [IXLdiv sp, IXStv t0] ++ spdec ++ [IXLdiv sp, IXAdd t0, IXAnd mone, IXStiv sp] ++ spinc
          | Sub <- al = spdec ++ [IXLdiv sp, IXAdd monecf, INot, IXStv t0] ++ spdec ++ [IXLdiv sp, IXAdd t0, IXAnd mone, IXStiv sp] ++ spinc
          | Neg <- al = spdec ++ [IXLdiv sp, IXAdd monecf, INot, IXAnd mone, IXStiv sp] ++ spinc
          | Eq <- al = spdec ++ [IXLdiv sp, IXStv t0] ++ spdec ++ [IXLdiv sp, IXEql t0, IXAnd mone, IXStiv sp] ++ spinc
          | Gt <- al = spdec ++ [IXLdiv sp, IXStv t0] ++ spdec ++ [IXLdiv sp, IXAdd monecf, INot, IXAdd t0, IXAnd nflag, IXEql nflag, IXAnd mone, IXStiv sp] ++ spinc
          | Lt <- al = spdec ++ [IXLdiv sp, IXAdd monecf, INot, IXStv t0] ++ spdec ++ [IXLdiv sp, IXAdd t0, IXAnd nflag, IXEql nflag, IXAnd mone, IXStiv sp] ++ spinc
          | And <- al = spdec ++ [IXLdiv sp, IXStv t0] ++ spdec ++ [IXLdiv sp, IXAnd t0, IXStiv sp] ++ spinc
          | Or <- al = spdec ++ [IXLdiv sp, IXStv t0] ++ spdec ++ [IXLdiv sp, IXOr t0, IXStiv sp] ++ spinc
          | Not <- al = spdec ++ [IXLdiv sp, INot, IXAnd mone, IXStiv sp] ++ spinc

optimize :: [MimaInstruction] -> [MimaInstruction]
optimize [] = error "Nothing to optimize!"
optimize is = let next = concat (optimize' is) in
              if length is == length next then is else optimize next
  where
    optimize' :: [MimaInstruction] -> [[MimaInstruction]]
    optimize' [] = []
    optimize' is = let (xs, ys) = reduce is in xs : optimize' ys
      where
        reduce is@(i:iis)
          | (IXStv "T0" : IXLdv l : IXAdd "T0" : ins) <- is = ([IXAdd l], ins)
          | (IXStv "T0" : IXLdv l : IXAnd "T0" : ins) <- is = ([IXAnd l], ins)
          | (IXStv "T0" : IXLdv l : IXOr "T0" : ins) <- is = ([IXOr l], ins)
          | (IXStv "T0" : IXLdv l : IXEql "T0" : ins) <- is = ([IXEql l], ins)
          | (IXStv "T0" : IXLds l : IXAdd "T0" : ins) <- is = ([IXAdd l], ins)
          | (IXStv "T0" : IXLds l : IXAnd "T0" : ins) <- is = ([IXAnd l], ins)
          | (IXStv "T0" : IXLds l : IXOr "T0" : ins) <- is = ([IXOr l], ins)
          | (IXStv "T0" : IXLds l : IXEql "T0" : ins) <- is = ([IXEql l], ins)
          | (ILdc 1 : IXAdd a : IXStv b : IXLdv "MONECF" : IXAdd c : IXStv d : ins) <- is =
              if a == b && b == c && c == d then ([], ins) else ([i], iis)
          | (IXStiv "SP" : IXLdiv "SP" : ins) <- is = ([], ins)
          | (ILdc a : ILdc b : ins) <- is = ([ILdc b], ins)
          | (IXLabel s : IXLabel s' : ins) <- is = ([IXLabel s, IXAnd monecf, IXLabel s'], ins) --IMPORTANT!
          | otherwise = ([i], iis)
