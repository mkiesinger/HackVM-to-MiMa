module DataflowAnalyzer where
import Data.Graph
import Parser

optimizeDflow :: [VMFile] -> [VMFile]
optimizeDflow [] = error "No files to optimize"
optimizeDflow files =
  preOptimize . fromReachableOutput . reachable graph $ getSysInit "Sys.init"
 where
  (graph, vertexf, keyf) = graphFromEdges $ toGraphInput files
  getSysInit key
    | Just v <- keyf key
    = v
    | otherwise
    = error
      $  "Optimizer error: Vertex with"
      ++ key
      ++ "is not found in callgraph."
  fromReachableOutput :: [Vertex] -> [VMFile]
  fromReachableOutput vertices =
    createFilesFromFuncs $ map (fst3 . vertexf) vertices

toGraphInput :: [VMFile] -> [((VMFunction, String), String, [String])]
toGraphInput = foldr (\file gi -> toGraphInput' file ++ gi) []
 where
  toGraphInput' (File name fs) =
    map (\vmf -> ((vmf, name), nameFunc vmf, calls vmf)) fs

calls :: VMFunction -> [String]
calls (Function l _ ls) = foldr getCalls [] (l : ls)
 where
  getCalls (Label _ cs) cls = foldr getCalls' [] cs ++ cls
   where
    getCalls' c cs | (CCall fname _) <- c = fname : cs
                   | otherwise            = cs

createFilesFromFuncs :: [(VMFunction, String)] -> [VMFile]
createFilesFromFuncs = foldr insert []

insert :: (VMFunction, String) -> [VMFile] -> [VMFile]
insert (f, filename) files = insert' f file : files'
 where
  (file, files') = if filename `notElem` map nameFile files
    then (File filename [], files)
    else
      ( head (filter (\f -> nameFile f == filename) files)
      , filter (\f -> nameFile f /= filename) files
      )
  insert' f file@(File name fs) | not (contains f) = File name (fs ++ [f])
                                | otherwise        = file
    where contains f = nameFunc f `elem` map nameFunc fs

preOptimize :: [VMFile] -> [VMFile]
preOptimize = map optimizeFile
 where
  optimizeFile (File filename vmfs) = File filename (map optimizeFunction vmfs)
   where
    optimizeFunction (Function l nlcls ls) =
      let optls = map optimizeLabel (l : ls)
      in  Function (head optls) nlcls (tail optls)
     where
      optimizeLabel (Label lname cs) = Label lname (optimizeCommands cs)
       where
        optimizeCommands cs = if length cs == length (optimizeCommands' cs)
          then cs
          else optimizeCommands (optimizeCommands' cs)
         where
          optimizeCommands' [] = []
          optimizeCommands' cs
            | (c1@(CPush Static idx1):c2@(CPush seg idx2):c3@(CArithLogic Add):css) <-
              cs
            = c2 : c1 : c3 : optimizeCommands' css
            | (c1@(CPush Static idx1):c2@(CPush seg idx2):c3@(CArithLogic And):css) <-
              cs
            = c2 : c1 : c3 : optimizeCommands' css
            | (c1@(CPush Static idx1):c2@(CPush seg idx2):c3@(CArithLogic Or):css) <-
              cs
            = c2 : c1 : c3 : optimizeCommands' css
            | (c1@(CPush Static idx1):c2@(CPush seg idx2):c3@(CArithLogic Eq):css) <-
              cs
            = c2 : c1 : c3 : optimizeCommands' css
            | (c1@(CPush Pointer idx1):c2@(CPush seg idx2):c3@(CArithLogic Add):css) <-
              cs
            = c2 : c1 : c3 : optimizeCommands' css
            | (c1@(CPush Pointer idx1):c2@(CPush seg idx2):c3@(CArithLogic And):css) <-
              cs
            = c2 : c1 : c3 : optimizeCommands' css
            | (c1@(CPush Pointer idx1):c2@(CPush seg idx2):c3@(CArithLogic Or):css) <-
              cs
            = c2 : c1 : c3 : optimizeCommands' css
            | (c1@(CPush Pointer idx1):c2@(CPush seg idx2):c3@(CArithLogic Eq):css) <-
              cs
            = c2 : c1 : c3 : optimizeCommands' css
            | (c1@(CPush Temp idx1):c2@(CPush seg idx2):c3@(CArithLogic Add):css) <-
              cs
            = c2 : c1 : c3 : optimizeCommands' css
            | (c1@(CPush Temp idx1):c2@(CPush seg idx2):c3@(CArithLogic And):css) <-
              cs
            = c2 : c1 : c3 : optimizeCommands' css
            | (c1@(CPush Temp idx1):c2@(CPush seg idx2):c3@(CArithLogic Or):css) <-
              cs
            = c2 : c1 : c3 : optimizeCommands' css
            | (c1@(CPush Temp idx1):c2@(CPush seg idx2):c3@(CArithLogic Eq):css) <-
              cs
            = c2 : c1 : c3 : optimizeCommands' css
            | (CPush Constant 0:CPush seg idx:CArithLogic Add:css) <- cs
            = CPush seg idx : optimizeCommands' css
            | (CPush seg idx:CPush Constant 0:CArithLogic Add:css) <- cs
            = CPush seg idx : optimizeCommands' css
            | (CPush Constant 0:CPush seg idx:CArithLogic Or:css) <- cs
            = CPush seg idx : optimizeCommands' css
            | (CPush seg idx:CPush Constant 0:CArithLogic Or:css) <- cs
            = CPush seg idx : optimizeCommands' css
            | (CPush Constant 0:CArithLogic Neg:css) <- cs
            = CPush Constant 0 : optimizeCommands' css
            | (CArithLogic Not:CArithLogic Not:css) <- cs
            = optimizeCommands' css
            | (CArithLogic Neg:CArithLogic Neg:css) <- cs
            = optimizeCommands' css
            | (c:css) <- cs
            = c : optimizeCommands' css

nameFunc :: VMFunction -> String
nameFunc (Function (Label s _) _ _) = s

nameFile :: VMFile -> String
nameFile (File s _) = s

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
