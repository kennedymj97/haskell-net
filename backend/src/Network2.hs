{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE TypeOperators       #-}

module Network2 where

import           Control.Monad
import           Control.Monad.Random
import           Data.Foldable
import           Data.List
import           Data.Maybe
import           Data.Singletons
import           Data.Singletons.Prelude
import           Data.Singletons.TypeLits
import qualified Numeric.LinearAlgebra        as LA
import           Numeric.LinearAlgebra.Static
import           ReadMnist
import           System.Environment
import           System.Random
import           Text.Read
import Data.Binary as B
import GHC.Generics (Generic)

data Connections i o =
  C
    { biases  :: !(R o) --n
    , weights :: !(L o i) --m x n
    , bGrads  :: !(R o)
    , wGrads  :: !(L o i)
    }
  deriving (Show, Generic) -- m to n layer

instance (KnownNat i, KnownNat o) => Binary (Connections i o)

updateGrads ::
     (KnownNat i, KnownNat o)
  => R o
  -> L o i
  -> Connections i o
  -> Connections i o
updateGrads bGrads' wGrads' (C b w bGrads wGrads) =
  C b w (bGrads + bGrads') (wGrads + wGrads')

data Network :: Nat -> [Nat] -> Nat -> * where
  Output :: !(Connections i o) -> Network i '[] o
  Layer
    :: KnownNat h
    => !(Connections i h)
    -> !(Network h hs o)
    -> Network i (h : hs) o

instance (KnownNat i, KnownNat o) => Show (Network i hs o) where
  show (Output c)    = "Output (" ++ show c ++ ") "
  show (c `Layer` n) = "Layer (" ++ show c ++ ") " ++ show n

putNet :: (KnownNat i, KnownNat o) => Network i hs o -> Put
putNet =  \case
  Output w -> put w
  w `Layer` n -> put w *> putNet n

getNet :: (KnownNat i, KnownNat o) => Sing hs -> Get (Network i hs o)
getNet = \case
  SNil -> Output <$> B.get
  SNat `SCons` ss -> Layer <$> B.get <*> getNet ss

instance (KnownNat i, SingI hs, KnownNat o) => Binary (Network i hs o) where
  put = putNet
  get = getNet sing

weightedInput :: (KnownNat i, KnownNat o) => Connections i o -> R i -> R o
weightedInput (C b w _ _) v = w #> v + b

runNet ::
     (KnownNat i, KnownNat o) => Network i hs o -> R i -> (R o -> R o) -> R o
runNet (Output c) !inp outA = outA (weightedInput c inp)
runNet (c `Layer` n) !inp f =
  let inp' = relu (weightedInput c inp)
   in runNet n inp' f

randomNet :: (KnownNat i, SingI hs, KnownNat o) => IO (Network i hs o)
randomNet = go sing
  where
    go :: (KnownNat h, KnownNat o) => Sing hs' -> IO (Network h hs' o)
    go =
      \case
        SNil -> Output <$> kaimingInit
        SNat `SCons` ss -> Layer <$> kaimingInit <*> go ss

backPropOneInput ::
     (KnownNat i, KnownNat o)
  => R i
  -> R o
  -> ModelFuncs o
  -> Network i hs o
  -> (Network i hs o, Double)
backPropOneInput !inp !target !fs !net =
  let (net', _, cost) = go inp target fs net
   in (net', cost)
  where
    go ::
         (KnownNat i, KnownNat o)
      => R i
      -> R o
      -> ModelFuncs o
      -> Network i hs o
      -> (Network i hs o, R i, Double)
    go !inp !target (F outA costFunc) (Output !c) =
      let z = weightedInput c inp
          a = outA z
          (c', dWs) = outputErrs inp target a z c
          cost = costFunc target a
       in (Output c', dWs, cost)
    go inp target fs (c1 `Layer` n) =
      let (z, a) = feedForward c1 inp
          (c1', n', cost, dWs) = backProp inp target c1 fs n a z
       in (c1' `Layer` n', dWs, cost)
    feedForward ::
         (KnownNat i, KnownNat o) => Connections i o -> R i -> (R o, R o)
    feedForward c inp =
      let z = weightedInput c inp
          a = relu z
       in (z, a)
    outputErrs ::
         (KnownNat i, KnownNat o)
      => R i
      -> R o
      -> R o
      -> R o
      -> Connections i o
      -> (Connections i o, R i)
    outputErrs inp target a z c@(C b w bGrads wGrads) =
      let errs = (a - target)
          errWs = errs `outer` inp
          dWs = tr w #> errs
       in (updateGrads errs errWs c, dWs)
    backProp ::
         (KnownNat i, KnownNat o, KnownNat k)
      => R k
      -> R o
      -> Connections k i
      -> ModelFuncs o
      -> Network i hs o
      -> R i
      -> R i
      -> (Connections k i, Network i hs o, Double, R k)
    backProp inp target c@(C b w bGrads wGrads) fs n a z =
      let (n', dWs', cost) = go a target fs n
          errs = dWs' * relu' z
          errWs = errs `outer` inp
          dWs = tr w #> errs
       in (updateGrads errs errWs c, n', cost, dWs)

train ::
     (KnownNat i, KnownNat o)
  => DataBunch i o
  -> ModelFuncs o
  -> Params
  -> Network i hs o
  -> (Network i hs o, Double)
train db@(DB !inps _) !fs !ps !net =
  let (_, net', costSum) = handleBatching db fs net ps 0
   in (net', -(costSum / (2 * fromIntegral (length inps))))
  where
    handleBatching ::
         (KnownNat i, KnownNat o)
      => DataBunch i o
      -> ModelFuncs o
      -> Network i hs o
      -> Params
      -> Double
      -> (DataBunch i o, Network i hs o, Double)
    handleBatching (DB [] []) _ !net _ !cost = (DB [] [], net, cost)
    handleBatching (DB !inps !targets) fs !net ps@(P bs lr) !cost =
      let (batchInps, inps') = splitAt bs inps
          (batchTargets, targets') = splitAt bs targets
          (net', cost') = backPropagate (DB batchInps batchTargets) fs net
          optimisedNet = optimise ps net'
       in handleBatching (DB inps' targets') fs optimisedNet ps (cost + cost')
    backPropagate ::
         (KnownNat i, KnownNat o)
      => DataBunch i o
      -> ModelFuncs o
      -> Network i hs o
      -> (Network i hs o, Double)
    backPropagate db@(DB inps targets) !fs !net =
      let (netFinal, costSum) =
            foldl'
              (\(net, cost) (inp, target) ->
                 let (net', cost') = backPropOneInput inp target fs net
                  in (net', cost + cost'))
              (net, 0)
              (zip inps targets)
       in (netFinal, costSum)
    optimise ::
         (KnownNat i, KnownNat o) => Params -> Network i hs o -> Network i hs o
    optimise (P bs lr) (Output !c)      = Output (sgd bs lr c)
    optimise ps@(P bs lr) (c `Layer` n) = sgd bs lr c `Layer` optimise ps n

getAccuracy ::
     (KnownNat i, KnownNat o)
  => DataBunch i o
  -> Double
  -> (R o -> R o)
  -> Network i hs o
  -> Double
getAccuracy (DB !inps !targets) !thresh !f !net =
  let numCorrect =
        foldl'
          (\n (inp, target) ->
             if LA.cmap
                  (\x ->
                     if x >= thresh
                       then 1
                       else 0)
                  (unwrap $ runNet net inp f) ==
                unwrap target
               then n + 1
               else n)
          0
          (zip inps targets)
   in (numCorrect / fromIntegral (length inps)) * 100

epoch ::
     (KnownNat i, KnownNat o)
  => ModelData i o
  -> ModelFuncs o
  -> Params
  -> IO (Network i hs o)
  -> Int
  -> IO (Network i hs o)
epoch d@(D !trainData !testData) fs@(F !f _) !ps !netIO !count = do
  net <- netIO
  putStrLn $ "Running epoch " ++ show count ++ "..."
  let (net', cost) = train trainData fs ps net
      trainAcc = getAccuracy trainData 0.5 f net'
      testAcc = getAccuracy testData 0.5 f net'
  putStrLn $ "Cost: " ++ show cost
  putStrLn $ "Train Accuracy: " ++ show trainAcc
  putStrLn $ "Test Accuracy: " ++ show testAcc
  putStrLn ""
  return net'

data DataBunch i o =
  DB
    { inputs :: [R i]
    , labels :: [R o]
    }
  deriving (Show)

data ModelData i o =
  D
    { trainingData :: DataBunch i o
    , testData     :: DataBunch i o
    }
  deriving (Show)

data ModelFuncs o =
  F
    { outputActivation :: R o -> R o
    , costFunc         :: R o -> R o -> Double
    }

data Params =
  P
    { bs :: Int
    , lr :: Double
    }

runEpochs ::
     (KnownNat i, KnownNat o)
  => Int
  -> (IO (Network i hs o) -> Int -> IO (Network i hs o))
  -> IO (Network i hs o)
  -> IO (Network i hs o)
runEpochs numEpochs f net = foldl' f net [1 .. numEpochs]

examsNet :: IO ()
examsNet = do
  (trainInps, trainOuts) <- loadExamData "./data/examScoresTrain.txt"
  (testInps, testOuts) <- loadExamData "./data/examScoresTest.txt"
  let trainInps' = map (dvmap (/ 100)) trainInps
      testInps' = map (dvmap (/ 100)) testInps
      examsData = D (DB trainInps' trainOuts) (DB testInps' testOuts)
      net0 = randomNet :: IO (Network 2 '[ 8, 8] 1)
      params = P 64 0.01
      modelFuncs = F sigmoid sigmoidCE
      epoch' = epoch examsData modelFuncs params
  putStrLn "Training network..."
  trained <- runEpochs 10 epoch' net0
  return ()

mnistNet :: IO ()
mnistNet = do
  trainImgs <- loadMnistImages 60000 "./data/train-images-idx3-ubyte.gz"
  trainLbls <- loadMnistLabels 60000 "./data/train-labels-idx1-ubyte.gz"
  testImgs <- loadMnistImages 10000 "./data/t10k-images-idx3-ubyte.gz"
  testLbls <- loadMnistLabels 10000 "./data/t10k-labels-idx1-ubyte.gz"
  let mnistData = D (DB trainImgs trainLbls) (DB testImgs testLbls)
      net0 = randomNet :: IO (Network 784 '[ 100] 10)
      params = P 64 0.5
      modelFuncs = F softmax softmaxCE
      epoch' = epoch mnistData modelFuncs params
  putStrLn "Training network..."
  trained <- runEpochs 20 epoch' net0
  saveNet "./data/mnistNet.txt" trained
  return ()

saveNet :: (KnownNat i, SingI hs, KnownNat o) => String -> Network i hs o -> IO ()
saveNet = encodeFile

loadNet :: (KnownNat i, SingI hs, KnownNat o) => String -> IO (Network i hs o)
loadNet = decodeFile

  {-
randomNet :: (KnownNat i, SingI hs, KnownNat o) => IO (Network i hs o)
randomNet = go sing
  where
    go :: (KnownNat h, KnownNat o) => Sing hs' -> IO (Network h hs' o)
    go =
      \case
        SNil -> Output <$> kaimingInit
        SNat `SCons` ss -> Layer <$> kaimingInit <*> go ss
-}

-- INIT FUNCTIONS
randomConnections :: (KnownNat i, KnownNat o) => IO (Connections i o)
randomConnections = do
  seed1 <- randomIO :: IO Int
  seed2 <- randomIO :: IO Int
  let b = randomVector seed1 Uniform * 2 - 1
      w = uniformSample seed2 (-1) 1
  return (C b w 0 0)

kaimingInit :: (KnownNat i, KnownNat o) => IO (Connections i o)
kaimingInit = do
  let b = 0 * randomVector 0 Uniform
  w <- randn
  let w' = sqrt (2 / fromIntegral (snd $ size w)) * w
  return (C b w' 0 0)

-- ACTIVATION FUNCTIONS
sigmoid :: Floating a => a -> a
sigmoid x = 1 / (1 + exp (-x))

sigmoid' :: Floating a => a -> a
sigmoid' x = sigmoid x * (1 - sigmoid x)

relu :: (KnownNat a) => R a -> R a
relu = dvmap (max 0)

relu' :: (KnownNat a) => R a -> R a
relu' =
  dvmap
    (\x ->
       if x <= 0
         then 0
         else 1)

softmax :: (KnownNat a) => R a -> R a
softmax xs = dvmap (\x -> exp x / expSum) shiftxs
  where
    maxEle = LA.maxElement $ unwrap xs
    shiftxs = dvmap (\x -> x - maxEle) xs
    expSum = sumElements' $ dvmap exp shiftxs

-- COST FUNCTIONS
sigmoidCE :: (KnownNat a) => R a -> R a -> Double
sigmoidCE target a =
  sumElements' $ (target * log a) + ((1 - target) * log (1 - a))

softmaxCE :: (KnownNat a) => R a -> R a -> Double
softmaxCE target a = sumElements' $ target * log a

-- OPTIMISATION FUNCTIONS
gradientDescent ::
     (KnownNat i, KnownNat o) => Double -> Connections i o -> Connections i o
gradientDescent !lr (C !b !w !bGrads !wGrads) =
  C (b - konst lr * bGrads) (w - konst lr * wGrads) 0 0

sgd ::
     (KnownNat i, KnownNat o)
  => Int
  -> Double
  -> Connections i o
  -> Connections i o
sgd !bs !lr (C !b !w !bGrads !wGrads) =
  C (b - (konst (lr / fromIntegral bs) * bGrads))
    (w - (konst (lr / fromIntegral bs) * wGrads))
    0
    0

-- USEFUL VECTOR FUNCTIONS
sumElements' :: KnownNat n => R n -> Double
sumElements' = LA.sumElements . unwrap

-- TEST NETWORK
inpToH :: Connections 2 4
inpToH =
  C (vector [0.1, 0.1, 0.1, 0.1] :: R 4)
    (matrix [0.5, 0.4, 0.3, 0.2, 0.1, 0.2, 0.3, 0.4] :: L 4 2)
    (vector [0, 0, 0, 0] :: R 4)
    (matrix [0, 0, 0, 0, 0, 0, 0, 0] :: L 4 2)

hToOut :: Connections 4 1
hToOut =
  C (vector [0.1] :: R 1)
    (matrix [0.5, 0.4, 0.3, 0.2] :: L 1 4)
    (vector [0.1] :: R 1)
    (matrix [0, 0, 0, 0] :: L 1 4)

testNet :: Network 2 '[ 4] 1
testNet = inpToH `Layer` Output hToOut

testNetIO :: IO (Network 2 '[ 4] 1)
testNetIO = return testNet

testingData :: ModelData 2 1
testingData =
  D (DB
       [vector [0.3, 0.3], vector [0.8, 0.8], vector [0.5, 0.4]]
       [vector [0], vector [1], vector [0]])
    (DB [vector [0.9, 0.9]] [vector [1]])

testFuncs :: ModelFuncs 1
testFuncs = F softmax softmaxCE
