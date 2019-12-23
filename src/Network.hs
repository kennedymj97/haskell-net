{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network where

import           Control.Monad
import           Control.Monad.Random
import           Data.Foldable
import           Data.List
import           Data.Maybe
import           Numeric.LinearAlgebra
import           System.Environment
import           System.Random
import           Text.Read

data Connections =
  C
    { biases  :: !(Vector Double) --n
    , weights :: !(Matrix Double) --m x n
    , bGrads  :: !(Vector Double)
    , wGrads  :: !(Matrix Double)
    } -- m to n layer

updateGrads :: Vector Double -> Matrix Double -> Connections -> Connections
updateGrads bGrads' wGrads' (C b w bGrads wGrads) =
  C b w (bGrads + bGrads') (wGrads + wGrads')

data Network where
  Output :: !Connections -> Network
  Layer :: !Connections -> !Network -> Network

sigmoid :: Floating a => a -> a
sigmoid x = 1 / (1 + exp (-x))

sigmoid' :: Floating a => a -> a
sigmoid' x = sigmoid x * (1 - sigmoid x)

weightedInput :: Connections -> Vector Double -> Vector Double
weightedInput (C b w _ _) v = w #> v + b

runNet :: Network -> Vector Double -> Vector Double
runNet (Output c) !inp = sigmoid (weightedInput c inp)
runNet (c `Layer` n) !inp =
  let inp' = sigmoid (weightedInput c inp)
   in runNet n inp'

randomConnections :: Int -> Int -> IO Connections
randomConnections i o = do
  seed1 <- randomIO :: IO Int
  seed2 <- randomIO :: IO Int
  let b = randomVector seed1 Uniform o * 2 - 1
      w = uniformSample seed2 o (replicate i (-1, 1))
  return (C b w 0 0)

randomNet :: Int -> [Int] -> Int -> IO Network
randomNet i [] o     = Output <$> randomConnections i o
randomNet i (h:hs) o = Layer <$> randomConnections i h <*> randomNet h hs o

backPropOneInput ::
     Vector Double -> Vector Double -> Network -> (Network, Double)
backPropOneInput !inp !target !net =
  let (net', _, cost) = go inp net
   in (net', cost)
  where
    go :: Vector Double -> Network -> (Network, Vector Double, Double)
    go !inp (Output !c) =
      let (z, a) = feedForward c inp
          (c', dWs) = outputErrs a z inp target c
          cost = norm_2 (target - a) ** 2
       in (Output c', dWs, cost)
    go !inp (c1 `Layer` c2) =
      let (z, a) = feedForward c1 inp
          (c1', c2', cost, dWs) = backProp c1 c2 a z inp
       in (c1' `Layer` c2', dWs, cost)
    feedForward ::
         Connections -> Vector Double -> (Vector Double, Vector Double)
    feedForward c inp =
      let z = weightedInput c inp
          a = sigmoid z
       in (z, a)
    outputErrs ::
         Vector Double
      -> Vector Double
      -> Vector Double
      -> Vector Double
      -> Connections
      -> (Connections, Vector Double)
    outputErrs a z inp target c@(C b w bGrads wGrads) =
      let errs = (a - target) * sigmoid' z
          errWs = errs `outer` inp
          dWs = tr w #> errs
       in (C b w (bGrads + errs) (wGrads + errWs), dWs)
    backProp ::
         Connections
      -> Network
      -> Vector Double
      -> Vector Double
      -> Vector Double
      -> (Connections, Network, Double, Vector Double)
    backProp c1@(C b w bGrads wGrads) c2 a z inp =
      let (c2', dWs', cost) = go a c2
          errs = sigmoid' z * dWs'
          errWs = errs `outer` inp
          dWs = tr w #> errs
       in (C b w (bGrads + errs) (wGrads + errWs), c2', cost, dWs)

backPropagate ::
     [Vector Double]
  -> [Vector Double]
  -> Double
  -> Network
  -> (Network, Double)
backPropagate !inps !targets !lr !net =
  let (netFinal, costSum) =
        foldl'
          (\(net, cost) (inp, target) ->
             let (net', cost') = backPropOneInput inp target net
                 net'' = gradientDescent lr net'
              in (net'', cost + cost'))
          (net, 0)
          (zip inps targets)
   in (netFinal, costSum / fromIntegral (2 * length inps))

gradientDescent :: Double -> Network -> Network
gradientDescent !lr (Output !c) = Output (applyGrads lr c)
gradientDescent !lr (c `Layer` n) = applyGrads lr c `Layer` gradientDescent lr n

applyGrads :: Double -> Connections -> Connections
applyGrads !lr (C !b !w !bGrads !wGrads) =
  C (b - scale lr bGrads) (w - scale lr wGrads) 0 0

runTests :: [Vector Double] -> [Vector Double] -> Double -> Network -> Double
runTests !testInps !testTargets !thresh !net =
  let numCorrect =
        foldl'
          (\n (inp, target) ->
             if cmap
                  (\x ->
                     if x >= thresh
                       then 1
                       else 0)
                  (runNet net inp) ==
                target
               then n + 1
               else n)
          0
          (zip testInps testTargets)
   in numCorrect / fromIntegral (length testInps)

epoch ::
     [Vector Double]
  -> [Vector Double]
  -> [Vector Double]
  -> [Vector Double]
  -> Double
  -> Network
  -> (Network, Double, Double)
epoch !trainInps !trainTargets !testInps !testTargets !lr !net =
  let (net', cost) = backPropagate trainInps trainTargets lr net
      accuracy = runTests testInps testTargets 0.5 net
   in (net', cost, accuracy * 100)

epochIO ::
     [Vector Double]
  -> [Vector Double]
  -> [Vector Double]
  -> [Vector Double]
  -> Double
  -> IO Network
  -> Int
  -> IO Network
epochIO !trainInps !trainTargets !testInps !testTargets !lr !netIO !count = do
  net <- netIO
  putStrLn $ "Running epoch " ++ show count ++ "..."
  let (net', cost, acc) = epoch trainInps trainTargets testInps testTargets lr net
  putStrLn $ "Cost: " ++ show cost ++ "    Accuracy: " ++ show acc
  return net'

readExams :: String -> IO ([Vector Double], [Vector Double])
readExams fpath = do
  inp <- readFile fpath
  let (inps, outs) = foldr f ([], []) (lines inp)
  return (inps, outs)
  where
    f :: String
      -> ([Vector Double], [Vector Double])
      -> ([Vector Double], [Vector Double])
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

examsNet :: IO ()
examsNet = do
  (trainInps, trainOuts) <- readExams "./data/examScoresTrain.txt"
  (testInps, testOuts) <- readExams "./data/examScoresTest.txt"
  let net0 = randomNet 2 [8, 8] 1
  let lr = 0.01
  let epochIO' = epochIO trainInps trainOuts testInps testOuts lr
  putStrLn "Training network..."
  trained <- foldl' epochIO' net0 [0 .. 100]
  putStrLn "Example results from trained net..."
  print $ runNet trained (vector [80, 80])
  print $ runNet trained (vector [20, 30])
  return ()
  {-
  let (trained, cost, acc) = epoch' net0
  print cost
  print acc
  let (trained', cost', acc') = epoch' trained
  print cost'
  print acc'
  let (trained'', cost'', acc'') = epoch' trained'
  print cost''
  print acc''
  -}
