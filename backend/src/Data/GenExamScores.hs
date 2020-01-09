module Data.GenExamScores where

import           Control.Monad
import           System.Random

main :: Int -> String -> IO ()
main n fname = do
  results <- genResults n
  writeFile fname $ unlines results

genResult :: IO String
genResult = do
  test1 <- randomRIO (0, 100 :: Double)
  test2 <- randomRIO (0, 100 :: Double)
  let adm = isAdm test1 test2
  return $ show test1 ++ "," ++ show test2 ++ "," ++ show adm
  where
    isAdm :: Double -> Double -> Int
    isAdm test1 test2
      | test1 + test2 >= 120 = 1
      | test1 >= 80 = 1
      | test2 >= 70 = 1
      | otherwise = 0

genResults :: Int -> IO [String]
genResults n = replicateM n genResult
