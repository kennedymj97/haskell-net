{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Network2 where

import           Control.Monad
import           Control.Monad.Random
import           Data.Binary                  as B
import           Data.Foldable
import           Data.List
import           Data.Maybe
import           Data.Singletons
import           Data.Singletons.Prelude (Sing(SNil, SCons))
import           Data.Singletons.TypeLits
import           GHC.Generics                 (Generic)
import qualified Numeric.LinearAlgebra        as LA
import           Numeric.LinearAlgebra.Static
import           ReadMnist
import           System.Environment
import           System.Random
import           Text.Read

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
putNet =
  \case
    Output w -> put w
    w `Layer` n -> put w *> putNet n

getNet :: (KnownNat i, KnownNat o) => Sing hs -> Get (Network i hs o)
getNet =
  \case
    SNil -> Output <$> B.get
    SNat `SCons` ss -> Layer <$> B.get <*> getNet ss

instance (KnownNat i, SingI hs, KnownNat o) => Binary (Network i hs o) where
  put = putNet
  get = getNet sing

weightedInput :: (KnownNat i, KnownNat o) => Connections i o -> R i -> R o
weightedInput (C b w _ _) v = w #> v + b

runNet ::
     (KnownNat i, KnownNat o) => Network i hs o -> ModelActivations -> R i -> R o
runNet (Output c) (ModelActivations outA _) !inp = calcActivations outA (weightedInput c inp)
runNet (c `Layer` n) ma@(ModelActivations _ intA) !inp =
  let inp' = calcActivations intA (weightedInput c inp)
   in runNet n ma inp'

randomNet :: (KnownNat i, SingI hs, KnownNat o) => Init -> IO (Network i hs o)
randomNet = go sing
  where
    go :: (KnownNat h, KnownNat o) => Sing hs' -> Init -> IO (Network h hs' o)
    go sing init =
      case sing of
        SNil -> Output <$> genRandomConnections init
        SNat `SCons` ss -> Layer <$> genRandomConnections init <*> go ss init

backPropOneInput ::
     (KnownNat i, KnownNat o)
  => R i
  -> R o
  -> ModelActivations
  -> Network i hs o
  -> (Network i hs o, Double)
backPropOneInput !inp !target !ma !net =
  let (net', _, cost) = go inp target ma net
   in (net', cost)
  where
    go ::
         (KnownNat i, KnownNat o)
      => R i
      -> R o
      -> ModelActivations
      -> Network i hs o
      -> (Network i hs o, R i, Double)
    go !inp !target (ModelActivations outA _) (Output !c) =
      let z = weightedInput c inp
          a = calcActivations outA z
          (c', dWs) = outputErrs inp target a z c
          cost = calcCost outA target a
       in (Output c', dWs, cost)
    go inp target ma@(ModelActivations _ intA) (c `Layer` n) =
      let z = weightedInput c inp
          a = calcActivations intA z
          (c', n', cost, dWs) = backProp inp target c ma n a z
       in (c' `Layer` n', dWs, cost)
    outputErrs ::
         (KnownNat i, KnownNat o)
      => R i
      -> R o
      -> R o
      -> R o
      -> Connections i o
      -> (Connections i o, R i)
    outputErrs inp target a z c@(C _ w _ _) =
      let errs = (a - target)
          errWs = errs `outer` inp
          dWs = tr w #> errs
       in (updateGrads errs errWs c, dWs)
    backProp ::
         (KnownNat i, KnownNat o, KnownNat k)
      => R k
      -> R o
      -> Connections k i
      -> ModelActivations
      -> Network i hs o
      -> R i
      -> R i
      -> (Connections k i, Network i hs o, Double, R k)
    backProp inp target c@(C _ w _ _) ma@(ModelActivations _ intA) net a z =
      let (net', dWs', cost) = go a target ma net
          errs = dWs' * calcGradients intA z
          errWs = errs `outer` inp
          dWs = tr w #> errs
       in (updateGrads errs errWs c, net', cost, dWs)

train ::
     (KnownNat i, KnownNat o)
  => DataBunch i o
  -> ModelActivations
  -> Params
  -> Network i hs o
  -> (Network i hs o, Double)
train db@(DataBunch !inps _) !ma !ps !net =
  let (_, net', costSum) = handleBatching db ma ps net 0
   in (net', -(costSum / (2 * fromIntegral (length inps))))
  where
    handleBatching ::
         (KnownNat i, KnownNat o)
      => DataBunch i o
      -> ModelActivations
      -> Params
      -> Network i hs o
      -> Double
      -> (DataBunch i o, Network i hs o, Double)
    handleBatching (DataBunch [] []) _ _ !net !cost = (DataBunch [] [], net, cost)
    handleBatching (DataBunch !inps !targets) ma ps@(Params bs lr) !net !cost =
      let (batchInps, inps') = splitAt bs inps
          (batchTargets, targets') = splitAt bs targets
          (net', cost') = backPropagate (DataBunch batchInps batchTargets) ma net
          optimisedNet = optimise ps net'
       in handleBatching (DataBunch inps' targets') ma ps optimisedNet (cost + cost')
    backPropagate ::
         (KnownNat i, KnownNat o)
      => DataBunch i o
      -> ModelActivations
      -> Network i hs o
      -> (Network i hs o, Double)
    backPropagate db@(DataBunch inps targets) !ma !net =
      let (netFinal, costSum) =
            foldl'
              (\(net, cost) (inp, target) ->
                 let (net', cost') = backPropOneInput inp target ma net
                  in (net', cost + cost'))
              (net, 0)
              (zip inps targets)
       in (netFinal, costSum)
    optimise ::
         (KnownNat i, KnownNat o) => Params -> Network i hs o -> Network i hs o
    optimise ps (Output !c) = Output (sgd ps c)
    optimise ps (c `Layer` n) = sgd ps c `Layer` optimise ps n

getAccuracy ::
     (KnownNat i, KnownNat o)
  => DataBunch i o
  -> Double
  -> ModelActivations
  -> Network i hs o
  -> Double
getAccuracy (DataBunch !inps !targets) !thresh !ma !net =
  let numCorrect =
        foldl'
          (\n (inp, target) ->
             if LA.cmap
                  (\x ->
                     if x >= thresh
                       then 1
                       else 0)
                  (unwrap $ runNet net ma inp) ==
                unwrap target
               then n + 1
               else n)
          0
          (zip inps targets)
   in (numCorrect / fromIntegral (length inps)) * 100

epoch ::
     (KnownNat i, KnownNat o)
  => ModelData i o
  -> ModelActivations
  -> Params
  -> IO (Network i hs o)
  -> Int
  -> IO (Network i hs o)
epoch md@(ModelData !trainData !testData) ma !ps !netIO !count = do
  net <- netIO
  putStrLn $ "Running epoch " ++ show count ++ "..."
  let (net', cost) = train trainData ma ps net
      trainAcc = getAccuracy trainData 0.8 ma net'
      testAcc = getAccuracy testData 0.8 ma net'
  putStrLn $ "Cost: " ++ show cost
  putStrLn $ "Train Accuracy: " ++ show trainAcc
  putStrLn $ "Test Accuracy: " ++ show testAcc
  putStrLn ""
  return net'

data DataBunch i o =
  DataBunch
    { inputs :: [R i]
    , labels :: [R o]
    }
  deriving (Show)

data ModelData i o =
  ModelData
    { trainingData :: DataBunch i o
    , testData     :: DataBunch i o
    }
  deriving (Show)

data ModelActivations =
  ModelActivations
    { output :: Activation
    , internal :: Activation
    }

data Params =
  Params
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
      examsData = ModelData (DataBunch trainInps' trainOuts) (DataBunch testInps' testOuts)
      net0 = randomNet Kaiming :: IO (Network 2 '[ 8, 8] 1)
      params = Params 64 0.1
      modelFuncs = ModelActivations Relu Sigmoid
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
  let mnistData = ModelData (DataBunch trainImgs trainLbls) (DataBunch testImgs testLbls)
      net0 = randomNet Kaiming :: IO (Network 784 '[ 100] 10)
      params = Params 64 0.25
      modelFuncs = ModelActivations Relu Sigmoid
      epoch' = epoch mnistData modelFuncs params
  putStrLn "Training network..."
  trained <- runEpochs 20 epoch' net0
  saveNet "./data/mnistNetSigmoid.txt" trained
  return ()

saveNet ::
     (KnownNat i, SingI hs, KnownNat o) => String -> Network i hs o -> IO ()
saveNet = encodeFile

loadNet :: (KnownNat i, SingI hs, KnownNat o) => String -> IO (Network i hs o)
loadNet = decodeFile

-- INIT FUNCTIONS

data Init =
  RandomUniform
    | Kaiming

genRandomConnections :: (KnownNat i, KnownNat o) => Init -> IO (Connections i o)
genRandomConnections =
  \case
     RandomUniform -> randomConnections
     Kaiming -> kaimingInit

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

data Activation =
  Sigmoid
    | Relu
    | Softmax

calcActivations :: KnownNat a => Activation -> R a -> R a
calcActivations activation x =
  case activation of
    Sigmoid -> 1 / (1 + exp (-x))
    Relu -> dvmap (max 0) x
    Softmax -> let maxEle = LA.maxElement $ unwrap x
                   shiftx = dvmap (\ele -> ele - maxEle) x
                   expSum = sumElements' $ dvmap exp shiftx
               in dvmap (\ele -> exp ele / expSum) shiftx

calcGradients :: KnownNat a => Activation -> R a -> R a
calcGradients activation x =
  case activation of
    Sigmoid -> let x' = calcActivations Sigmoid x
               in x' * (1 - x')
    Relu -> dvmap (\ele -> if ele < 0 then 0 else 1) x
    Softmax -> error "cannot use softmax for internal layers"

calcCost :: KnownNat a => Activation -> R a -> R a -> Double
calcCost activation target a = 
  case activation of
    Sigmoid -> sumElements' $ (target * log a) + ((1 - target) * log (1 - a))
    Softmax -> sumElements' $ target * log a
    Relu -> error "cannot use relu as an output activation"

-- OPTIMISATION FUNCTIONS

type OptFunc i o = Params -> Connections i o -> Connections i o

gradientDescent :: (KnownNat i, KnownNat o) => OptFunc i o
gradientDescent (Params !bs !lr) (C !b !w !bGrads !wGrads) =
  C (b - konst lr * bGrads) (w - konst lr * wGrads) 0 0

sgd :: (KnownNat i, KnownNat o) => OptFunc i o
sgd (Params !bs !lr) (C !b !w !bGrads !wGrads) =
  C (b - (konst (lr / fromIntegral bs) * bGrads))
    (w - (konst (lr / fromIntegral bs) * wGrads))
    0
    0

-- USEFUL VECTOR FUNCTIONS
sumElements' :: KnownNat n => R n -> Double
sumElements' = LA.sumElements . unwrap
