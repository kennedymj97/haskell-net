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
import           ReadMnist
import           System.Environment
import           System.Random
import           Text.Read

data Connections =
  C
    { biases  :: !(Vector Double) --n
    , weights :: !(Matrix Double) --m x n
    , bGrads  :: !(Vector Double)
    , wGrads  :: !(Matrix Double)
    }
  deriving (Show) -- m to n layer

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

relu :: (Element b, Num b, Container c b, Ord b) => c b -> c b
relu = cmap (max 0)

relu' :: (Element b, Num b, Container c b, Ord b) => c b -> c b
relu' =
  cmap
    (\x ->
       if x <= 0
         then 0
         else 1)

softmax :: (Element b, Num b, Container c b, Ord b, Floating b) => c b -> c b
softmax xs = cmap (\x -> exp x / expSum) xs
  where
    expSum = sumElements $ cmap exp xs

weightedInput :: Connections -> Vector Double -> Vector Double
weightedInput (C b w _ _) v = w #> v + b

runNet :: Network -> Vector Double -> Vector Double
runNet (Output c) !inp = sigmoid (weightedInput c inp)
runNet (c `Layer` n) !inp =
  let inp' = relu (weightedInput c inp)
   in runNet n inp'

randomConnections :: Int -> Int -> IO Connections
randomConnections i o = do
  seed1 <- randomIO :: IO Int
  seed2 <- randomIO :: IO Int
  let b = randomVector seed1 Uniform o * 2 - 1
      w = uniformSample seed2 o (replicate i (-1, 1))
  return (C b w 0 0)

kaimingInit :: Int -> Int -> IO Connections
kaimingInit i o = do
  let b = vector (replicate o 0)
  w <- scale (sqrt (2 / fromIntegral i)) <$> randn o i
  return (C b w 0 0)

randomNet :: Int -> [Int] -> Int -> (Int -> Int -> IO Connections) -> IO Network
randomNet i [] o initFunc = Output <$> initFunc i o
randomNet i (h:hs) o initFunc =
  Layer <$> initFunc i h <*> randomNet h hs o initFunc

backPropOneInput ::
     Vector Double -> Vector Double -> Network -> (Network, Double)
backPropOneInput !inp !target !net =
  let (net', _, cost) = go inp net
   in (net', cost)
  where
    go :: Vector Double -> Network -> (Network, Vector Double, Double)
    go !inp (Output !c) =
      let z = weightedInput c inp
          a = sigmoid z
          (c', dWs) = outputErrs a z inp target c
          cost = sumElements $ (target * log a) + ((1 - target) * log (1 - a))
          -- cost = norm_2 (target - a) ** 2
       in (Output c', dWs, cost)
    go !inp (c1 `Layer` c2) =
      let (z, a) = feedForward c1 inp
          (c1', c2', cost, dWs) = backProp c1 c2 a z inp
       in (c1' `Layer` c2', dWs, cost)
    feedForward ::
         Connections -> Vector Double -> (Vector Double, Vector Double)
    feedForward c inp =
      let z = weightedInput c inp
          a = relu z
       in (z, a)
    outputErrs ::
         Vector Double
      -> Vector Double
      -> Vector Double
      -> Vector Double
      -> Connections
      -> (Connections, Vector Double)
    outputErrs a z inp target c@(C b w bGrads wGrads)
          -- errs = sigmoid' z * (a - target)
     =
      let errs = (a - target)
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
    backProp c1@(C b w bGrads wGrads) n@(Output _) a z inp =
      let (n', dWs', cost) = go a n
          errs = dWs' * sigmoid' z
          errWs = errs `outer` inp
          dWs = tr w #> errs
       in (C b w (bGrads + errs) (wGrads + errWs), n', cost, dWs)
    backProp c1@(C b w bGrads wGrads) n a z inp =
      let (n', dWs', cost) = go a n
          errs = dWs' * relu' z
          errWs = errs `outer` inp
          dWs = tr w #> errs
       in (C b w (bGrads + errs) (wGrads + errWs), n', cost, dWs)

backPropagate ::
     [Vector Double] -> [Vector Double] -> Network -> (Network, Double)
backPropagate !inps !targets !net =
  let (netFinal, costSum) =
        foldl'
          (\(net, cost) (inp, target) ->
             let (net', cost') = backPropOneInput inp target net
              in (net', cost + cost'))
          (net, 0)
          (zip inps targets)
   in (netFinal, costSum)

train ::
     [Vector Double]
  -> [Vector Double]
  -> Int
  -> Network
  -> (Connections -> Connections)
  -> (Network, Double)
train !inps !targets !bs !net !optFunc =
  let (_, _, net', costSum) = handleBatching bs inps targets net 0 optFunc
   in (net', -(costSum / fromIntegral (length inps)))

handleBatching ::
     Int
  -> [Vector Double]
  -> [Vector Double]
  -> Network
  -> Double
  -> (Connections -> Connections)
  -> ([Vector Double], [Vector Double], Network, Double)
handleBatching _ [] [] !net !cost _ = ([], [], net, cost)
handleBatching !bs !inps !targets !net !cost !optFunc =
  let (batchInps, inps') = splitAt bs inps
      (batchTargets, targets') = splitAt bs targets
      (net', cost') = backPropagate batchInps batchTargets net
      optimisedNet = optimise net' optFunc
   in handleBatching bs inps' targets' optimisedNet (cost + cost') optFunc

gradientDescent :: Double -> Connections -> Connections
gradientDescent !lr (C !b !w !bGrads !wGrads) =
  C (b - scale lr bGrads) (w - scale lr wGrads) 0 0

stochasticGradientDescent :: Int -> Double -> Connections -> Connections
stochasticGradientDescent !bs !lr (C !b !w !bGrads !wGrads) =
  C (b - scale lr (scale (1 / fromIntegral bs) bGrads))
    (w - scale lr (scale (1 / fromIntegral bs) wGrads))
    0
    0

optimise :: Network -> (Connections -> Connections) -> Network
optimise (Output !c) !opt   = Output (opt c)
optimise (c `Layer` n) !opt = opt c `Layer` optimise n opt

getAccuracy :: [Vector Double] -> [Vector Double] -> Double -> Network -> Double
getAccuracy !testInps !testTargets !thresh !net =
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
  -> Int
  -> Network
  -> (Connections -> Connections)
  -> (Network, Double, Double, Double)
epoch !trainInps !trainTargets !testInps !testTargets !bs !net !optFunc =
  let (net', cost) = train trainInps trainTargets bs net optFunc
      trainAccuracy = getAccuracy trainInps trainTargets 0.5 net'
      testAccuracy = getAccuracy testInps testTargets 0.5 net'
   in (net', cost / 2, trainAccuracy * 100, testAccuracy * 100)

epochIO ::
     [Vector Double]
  -> [Vector Double]
  -> [Vector Double]
  -> [Vector Double]
  -> Int
  -> (Connections -> Connections)
  -> IO Network
  -> Int
  -> IO Network
epochIO !trainInps !trainTargets !testInps !testTargets !bs !optFunc !netIO !count = do
  net <- netIO
  putStrLn $ "Running epoch " ++ show count ++ "..."
  let (net', cost, trainAcc, testAcc) =
        epoch trainInps trainTargets testInps testTargets bs net optFunc
  putStrLn $ "Cost: " ++ show cost
  putStrLn $ "Train Accuracy: " ++ show trainAcc
  putStrLn $ "Test Accuracy: " ++ show testAcc
  putStrLn ""
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
  let trainInps' = map (cmap (/ 100)) trainInps
      testInps' = map (cmap (/ 100)) testInps
  let net0 = randomNet 2 [8, 8] 1 kaimingInit
  let lr = 0.01
  let bs = 64
  let optFunc = stochasticGradientDescent bs lr
  let epochIO' = epochIO trainInps' trainOuts testInps' testOuts bs optFunc
  putStrLn "Training network..."
  trained <- foldl' epochIO' net0 [0 .. 100]
  putStrLn "Example results from trained net..."
  print $ runNet trained (vector [80 / 100, 80 / 100])
  print $ runNet trained (vector [20 / 100, 30 / 100])
  return ()

mnistNet :: IO ()
mnistNet = do
  trainImgs <- loadMnistImages 60000 "./data/train-images-idx3-ubyte.gz"
  trainLbls <- loadMnistLabels 60000 "./data/train-labels-idx1-ubyte.gz"
  testImgs <- loadMnistImages 10000 "./data/t10k-images-idx3-ubyte.gz"
  testLbls <- loadMnistLabels 10000 "./data/t10k-labels-idx1-ubyte.gz"
  let net0 = randomNet 784 [100] 10 kaimingInit
  let lr = 0.01
  let bs = 64
  let optFunc = stochasticGradientDescent bs lr
  let epochIO' = epochIO trainImgs trainLbls testImgs testLbls bs optFunc
  putStrLn "Training network..."
  trained <- foldl' epochIO' net0 [0..100]
  return ()
