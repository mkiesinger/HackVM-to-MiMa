module Parser where
import Data.Char (isDigit)
import Data.List.Split
import Debug.Trace

data VMFile = File Filename [VMFunction] deriving Show

data VMFunction = Function Label NLocals [Label] deriving Show

data Label = Label Symbol [Command] deriving Show

data Command = CPush Segment Int
             | CPop Segment Int
             | CArithLogic ArithLogic
             | CGoto Symbol
             | CIfGoto Symbol
             | CCall FuncName Int
             | CReturn
             deriving Show

data Segment = Argument | Local | Static | Constant | This | That | Pointer | Temp deriving Show
data ArithLogic = Add | Sub | Neg | Eq | Gt | Lt | And | Or | Not deriving Show
type Symbol = String
type FuncName = String
type NLocals = Int
type Filename = String

-- separate string into list of tokens and remove comments
tokenize :: String -> [String]
tokenize [] = error "No input to parse!"
tokenize s  = words $ removeComments s
 where
  removeComments []  = []
  removeComments str = noComment
    ++ removeComments (dropWhile (/= '\n') commentStart)
    where (noComment, commentStart) = break (== '/') str

parseFile :: (String, String) -> VMFile
parseFile ([]   , _      ) = error "Filename can't be empty"
parseFile (fname, []     ) = error $ "File " ++ fname ++ " has no content."
parseFile (fname, content) = File fname (functionize [] $ tokenize content)
 where
  functionize acc [] = acc
  functionize acc ss = functionize (acc ++ [f]) ss'
    where (f, ss') = convertFunction ss 

convertFunction :: [String] -> (VMFunction, [String])
convertFunction [] = error "No input to parse!"
convertFunction (s1:s2:s3:ss) | s1 == "function" && all isDigit s3 && not
  (null ss) =
  (Function l (read s3) ls, ss'')
 where
  (l , ss' ) = convertLabel ("label" : s2 : ss)
  (ls, ss'') = labelize [] ss'
  labelize acc [] = (acc, [])
  labelize acc (s:us) | s == "function" = (acc, s : us)
                      | otherwise       = (is, us'')
   where
    (i , us' ) = convertLabel (s : us)
    (is, us'') = labelize (acc ++ [i]) us'

convertLabel :: [String] -> (Label, [String])
convertLabel [] = error "No label to parse!"
convertLabel (s:ss)
  | s == "label" && not (null ss) = (Label (head ss) is, js)
  | otherwise = error $ "Syntax error: Not a label! " ++ s ++ show ss
 where
  (is, js) = commandize [] $ tail ss
  commandize acc (s:us) = case s of
    "function" -> (acc, s : us)
    "label"    -> (acc, s : us)
    "return"   -> if null us
      then (acc ++ [CReturn], us)
      else commandize (acc ++ [CReturn]) us
    _ -> (cs, us'')
   where
    (c , us' ) = convertCommand $ s : us
    (cs, us'') = commandize (acc ++ [c]) us'
  commandize _ _ =
    error "Syntax error: A label has to end by starting a new label or a return"

convertCommand :: [String] -> (Command, [String])
convertCommand []     = error "No input to parse!"
convertCommand (s:ss) = case s of
  "add"     -> (CArithLogic Add, ss)
  "sub"     -> (CArithLogic Sub, ss)
  "neg"     -> (CArithLogic Neg, ss)
  "eq"      -> (CArithLogic Eq, ss)
  "gt"      -> (CArithLogic Gt, ss)
  "lt"      -> (CArithLogic Lt, ss)
  "and"     -> (CArithLogic And, ss)
  "or"      -> (CArithLogic Or, ss)
  "not"     -> (CArithLogic Not, ss)
  "goto"    -> (CGoto (head ss), tail ss)
  "if-goto" -> (CIfGoto (head ss), tail ss)
  "push"    -> (CPush (segment ss) (value ss), drop 2 ss)
  "pop"     -> (CPop (segment ss) (value ss), drop 2 ss)
  "call"    -> (CCall (funcName ss) (value ss), drop 2 ss)
  _ -> error $ "Syntax error: couldn't convert Command: " ++ s ++ show ss
 where
  segment (s:_) = case s of
    "argument" -> Argument
    "local"    -> Local
    "static"   -> Static
    "constant" -> Constant
    "this"     -> This
    "that"     -> That
    "pointer"  -> Pointer
    "temp"     -> Temp
    _          -> error $ "Syntax Error: Unsupported segment!" ++ s
  funcName (s:ss) = s
  value (_:s:_)
    | all isDigit s = read s
    | otherwise     = error $ "Syntax Error: Argument is not a number!" ++ s
