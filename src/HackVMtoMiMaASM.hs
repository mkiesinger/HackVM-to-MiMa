module HackVMtoMiMaASM where
import CodeWriter
import Control.DeepSeq
import Data.String.Utils
import Data.Time
import DataflowAnalyzer
import Parser
import System.Directory
import Translator

execute = do
  dirPath  <- getCurrentDirectory
  allFiles <- getDirectoryContents $ dirPath ++ "\\program"
  let vmFiles = filter (endswith ".vm") allFiles
  contents <- mapM (readFile . ((dirPath ++ "/program/") ++)) vmFiles
  let d = zip (map cutlast vmFiles) contents
  t0 <- getCurrentTime
  let u = map parseFile d
  t1 <- u `seq` getCurrentTime
  putStr "parsing: "
  print $ diffUTCTime t1 t0
  t2 <- getCurrentTime
  let a = optimizeDflow u
  t3 <- a `seq` getCurrentTime
  putStr "preoptimize: "
  print $ diffUTCTime t3 t2
  t4 <- getCurrentTime
  let v = translateFiles a
  t5 <- v `seq` getCurrentTime
  putStr "translate: "
  print $ diffUTCTime t5 t4
  t6 <- getCurrentTime
  let txt = write v
  t7 <- txt `deepseq` getCurrentTime
  putStr "write: "
  print $ diffUTCTime t7 t6
  t8 <- getCurrentTime
  writeFile (dirPath ++ "/output.asm") txt
  t9 <- getCurrentTime
  putStr "write to disk: "
  print $ diffUTCTime t9 t8

cutlast = reverse . drop 3  . reverse
