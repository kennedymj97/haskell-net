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

weightedInput :: Connections -> Vector Double -> Vector Double
weightedInput (C b w _ _) v = w #> v + b

runNet ::
     Network
  -> Vector Double
  -> (Vector Double -> Vector Double)
  -> Vector Double
runNet (Output c) !inp f = f (weightedInput c inp)
runNet (c `Layer` n) !inp f =
  let inp' = relu (weightedInput c inp)
   in runNet n inp' f

randomNet :: Int -> [Int] -> Int -> (Int -> Int -> IO Connections) -> IO Network
randomNet i [] o initFunc = Output <$> initFunc i o
randomNet i (h:hs) o initFunc =
  Layer <$> initFunc i h <*> randomNet h hs o initFunc

backPropOneInput ::
     Vector Double
  -> Vector Double
  -> ModelFuncs
  -> Network
  -> (Network, Double)
backPropOneInput !inp !target !fs !net =
  let (net', _, cost) = go inp fs net
   in (net', cost)
  where
    go ::
         Vector Double
      -> ModelFuncs
      -> Network
      -> (Network, Vector Double, Double)
    go !inp (F outA costFunc _) (Output !c) =
      let z = weightedInput c inp
          a = outA z
          (c', dWs) = outputErrs inp target a z c
          cost = costFunc target a
       in (Output c', dWs, cost)
    go inp fs (c1 `Layer` n) =
      let (z, a) = feedForward c1 inp
          (c1', c2', cost, dWs) = backProp inp c1 fs n a z
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
    outputErrs inp target a z c@(C b w bGrads wGrads) =
      let errs = (a - target)
          errWs = errs `outer` inp
          dWs = tr w #> errs
       in (updateGrads errs errWs c, dWs)
    backProp ::
         Vector Double
      -> Connections
      -> ModelFuncs
      -> Network
      -> Vector Double
      -> Vector Double
      -> (Connections, Network, Double, Vector Double)
    backProp inp c1@(C b w bGrads wGrads) fs n@(Output _) a z =
      let (n', dWs', cost) = go a fs n
          errs = dWs'
          errWs = errs `outer` inp
          dWs = tr w #> errs
       in (updateGrads errs errWs c1, n', cost, dWs)
    backProp inp c@(C b w bGrads wGrads) fs n a z =
      let (n', dWs', cost) = go a fs n
          errs = dWs' * relu' z
          errWs = errs `outer` inp
          dWs = tr w #> errs
       in (updateGrads errs errWs c, n', cost, dWs)

train :: DataBunch -> ModelFuncs -> Int -> Network -> (Network, Double)
train db@(DB !inps _) !fs !bs !net =
  let (_, net', costSum) = handleBatching db fs net bs 0
   in (net', -(costSum / fromIntegral (length inps)))
  where
    handleBatching ::
         DataBunch
      -> ModelFuncs
      -> Network
      -> Int
      -> Double
      -> (DataBunch, Network, Double)
    handleBatching (DB [] []) _ !net _ !cost = (DB [] [], net, cost)
    handleBatching (DB !inps !targets) fs@(F _ _ !optFunc) !net !bs !cost =
      let (batchInps, inps') = splitAt bs inps
          (batchTargets, targets') = splitAt bs targets
          (net', cost') = backPropagate (DB batchInps batchTargets) fs net
          optimisedNet = optimise net' optFunc
       in handleBatching (DB inps' targets') fs optimisedNet bs (cost + cost')
    backPropagate :: DataBunch -> ModelFuncs -> Network -> (Network, Double)
    backPropagate db@(DB inps targets) !fs !net =
      let (netFinal, costSum) =
            foldl'
              (\(net, cost) (inp, target) ->
                 let (net', cost') = backPropOneInput inp target fs net
                  in (net', cost + cost'))
              (net, 0)
              (zip inps targets)
       in (netFinal, costSum)
    optimise :: Network -> (Connections -> Connections) -> Network
    optimise (Output !c) !opt   = Output (opt c)
    optimise (c `Layer` n) !opt = opt c `Layer` optimise n opt

getAccuracy ::
     DataBunch
  -> Double
  -> (Vector Double -> Vector Double)
  -> Network
  -> Double
getAccuracy (DB !inps !targets) !thresh !f !net =
  let numCorrect =
        foldl'
          (\n (inp, target) ->
             if cmap
                  (\x ->
                     if x >= thresh
                       then 1
                       else 0)
                  (runNet net inp f) ==
                target
               then n + 1
               else n)
          0
          (zip inps targets)
   in numCorrect / fromIntegral (length inps)

epoch ::
     ModelData
  -> ModelFuncs
  -> Int
  -> Network
  -> (Network, Double, Double, Double)
epoch (D !trainData !testData) fs@(F !f _ _) !bs !net =
  let (net', cost) = train trainData fs bs net
      trainAccuracy = getAccuracy trainData 0.5 f net'
      testAccuracy = getAccuracy testData 0.5 f net'
   in (net', cost / 2, trainAccuracy * 100, testAccuracy * 100)

epochIO :: ModelData -> ModelFuncs -> Int -> IO Network -> Int -> IO Network
epochIO !d !f !bs !netIO !count = do
  net <- netIO
  putStrLn $ "Running epoch " ++ show count ++ "..."
  let (net', cost, trainAcc, testAcc) = epoch d f bs net
  putStrLn $ "Cost: " ++ show cost
  putStrLn $ "Train Accuracy: " ++ show trainAcc
  putStrLn $ "Test Accuracy: " ++ show testAcc
  putStrLn ""
  return net'

data DataBunch =
  DB
    { inputs :: [Vector Double]
    , labels :: [Vector Double]
    }

data ModelData =
  D
    { trainingData :: DataBunch
    , testData     :: DataBunch
    }

data ModelFuncs =
  F
    { outputActivation :: Vector Double -> Vector Double
    , costFunc         :: Vector Double -> Vector Double -> Double
    , optFunc          :: Connections -> Connections
    }
      {-
examsNet :: IO ()
examsNet = do
  (trainInps, trainOuts) <- loadExamData "./data/examScoresTrain.txt"
  (testInps, testOuts) <- loadExamData "./data/examScoresTest.txt"
  let trainInps' = map (cmap (/ 100)) trainInps
      testInps' = map (cmap (/ 100)) testInps
  let examsData = D (DB trainInps' trainOuts) (DB testInps' testOuts)
  let net0 = randomNet 2 [8, 8] 1 kaimingInit
  let lr = 0.01
  let bs = 64
  let modelFuncs = F sigmoid sigmoidCE (sgd bs lr)
  let epochIO' = epochIO examsData modelFuncs bs
  putStrLn "Training network..."
  trained <- foldl' epochIO' net0 [0 .. 5]
  return ()

mnistNet :: IO ()
mnistNet = do
  trainImgs <- loadMnistImages 60000 "./data/train-images-idx3-ubyte.gz"
  trainLbls <- loadMnistLabels 60000 "./data/train-labels-idx1-ubyte.gz"
  testImgs <- loadMnistImages 10000 "./data/t10k-images-idx3-ubyte.gz"
  testLbls <- loadMnistLabels 10000 "./data/t10k-labels-idx1-ubyte.gz"
  let mnistData = D (DB trainImgs trainLbls) (DB testImgs testLbls)
  let net0 = randomNet 784 [100] 10 kaimingInit
  let lr = 0.1
  let bs = 64
  let modelFuncs = F softmax softmaxCE (sgd bs lr)
  let epochIO' = epochIO mnistData modelFuncs bs
  putStrLn "Training network..."
  trained <- foldl' epochIO' net0 [0 .. 20]
  return ()
-}
-- INIT FUNCTIONS
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

-- ACTIVATION FUNCTIONS
sigmoid :: Floating a => a -> a
sigmoid x = 1 / (1 + exp (-x))

sigmoid' :: Floating a => a -> a
sigmoid' x = sigmoid x * (1 - sigmoid x)

relu :: (Container c b, Ord b, Num b) => c b -> c b
relu = cmap (max 0)

relu' :: (Container c b, Ord b, Num b) => c b -> c b
relu' =
  cmap
    (\x ->
       if x <= 0
         then 0
         else 1)

softmax :: (Container c b, Ord b, Num b, Floating b) => c b -> c b
softmax xs = cmap (\x -> exp x / expSum) shiftxs
  where
    maxEle = maxElement xs
    shiftxs = cmap (\x -> x - maxEle) xs
    expSum = sumElements $ cmap exp shiftxs

-- COST FUNCTIONS
sigmoidCE :: Vector Double -> Vector Double -> Double
sigmoidCE target a =
  sumElements $ (target * log a) + ((1 - target) * log (1 - a))

softmaxCE :: Vector Double -> Vector Double -> Double
softmaxCE target a = sumElements $ target * log a

-- OPTIMISATION FUNCTIONS
gradientDescent :: Double -> Connections -> Connections
gradientDescent !lr (C !b !w !bGrads !wGrads) =
  C (b - scale lr bGrads) (w - scale lr wGrads) 0 0

sgd :: Int -> Double -> Connections -> Connections
sgd !bs !lr (C !b !w !bGrads !wGrads) =
  C (b - scale lr (scale (1 / fromIntegral bs) bGrads))
    (w - scale lr (scale (1 / fromIntegral bs) wGrads))
    0
    0


