module Data.ReadExam where

import           Numeric.LinearAlgebra.Static

loadExamData :: String -> IO ([R 2], [R 1])
loadExamData fpath = do
  inp <- readFile fpath
  let (inps, outs) = foldr f ([], []) (lines inp)
  return (inps, outs)
  where
    f :: String -> ([R 2], [R 1]) -> ([R 2], [R 1])
    f line (inps, outs) =
      let (test1, line') = readScore line
          (test2, line'') = readScore line'
          adm = readAdm line''
       in (vector [test1, test2] : inps, vector [adm] : outs)
    readScore :: String -> (Double, String)
    readScore line =
      let (val, line') = span (/= ',') line
       in (read val :: Double, drop 1 line')
    readAdm :: String -> Double
    readAdm val = read val :: Double
