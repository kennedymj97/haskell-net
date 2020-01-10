module Learner.Network where

import           Control.Monad
import           Data.Binary                  as B
import           Data.Foldable
import           Data.List
import           Data.Maybe
import           Data.ReadExam
import           Data.ReadMnist
import           Data.Singletons
import           Data.Singletons.Prelude      (Sing (SCons, SNil))
import           Data.Singletons.TypeLits
import           Learner.Activation
import           Learner.Connections
import           Learner.Init
import qualified Numeric.LinearAlgebra        as LA
import           Numeric.LinearAlgebra.Static
import           Text.Read

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

saveNet ::
  (KnownNat i, SingI hs, KnownNat o) => String -> Network i hs o -> IO ()
saveNet = encodeFile

loadNet :: (KnownNat i, SingI hs, KnownNat o) => String -> IO (Network i hs o)
loadNet = decodeFile

runNet ::
     (KnownNat i, KnownNat o, Activation outA, Activation intA)
  => Network i hs o
  -> ModelActivations outA intA
  -> R i
  -> R o
runNet (Output c) (ModelActivations outAct _) !inp =
  calcActivations outAct (weightedInput c inp)
runNet (c `Layer` n) ma@(ModelActivations _ intAct) !inp =
  let inp' = calcActivations intAct (weightedInput c inp)
   in runNet n ma inp'

weightedInput :: (KnownNat i, KnownNat o) => Connections i o -> R i -> R o
weightedInput (C b w _ _) v = w #> v + b

randomNet :: (KnownNat i, SingI hs, KnownNat o, Init f) => f -> IO (Network i hs o)
randomNet = go sing
  where
    go :: (KnownNat h, KnownNat o, Init f) => Sing hs' -> f -> IO (Network h hs' o)
    go sing init =
      case sing of
        SNil            -> Output <$> genRandom init
        SNat `SCons` ss -> Layer <$> genRandom init <*> go ss init

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

data ModelActivations outA intA =
  ModelActivations outA intA

data Params =
  Params
    { bs :: Int
    , lr :: Double
    }

runEpochs ::
    (KnownNat i, KnownNat o, Activation outA, Activation intA)
 => Int
 -> ModelData i o
 -> ModelActivations outA intA
 -> Params
 -> IO (Network i hs o)
 -> IO (Network i hs o)
runEpochs numEpochs md ma ps net = foldl' (epoch md ma ps) net [1 .. numEpochs]

epoch ::
     (KnownNat i, KnownNat o, Activation outA, Activation intA)
  => ModelData i o
  -> ModelActivations outA intA
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

train ::
  (KnownNat i, KnownNat o, Activation outA, Activation intA)
  => DataBunch i o
  -> ModelActivations outA intA
  -> Params
  -> Network i hs o
  -> (Network i hs o, Double)
train db@(DataBunch !inps _) !ma !ps !net =
  let (_, net', costSum) = handleBatching db ma ps net 0
  in (net', -(costSum / (2 * fromIntegral (length inps))))
  where
    handleBatching ::
          (KnownNat i, KnownNat o, Activation outA, Activation intA)
      => DataBunch i o
      -> ModelActivations outA intA
      -> Params
      -> Network i hs o
      -> Double
      -> (DataBunch i o, Network i hs o, Double)
    handleBatching (DataBunch [] []) _ _ !net !cost =
      (DataBunch [] [], net, cost)
    handleBatching (DataBunch !inps !targets) ma ps@(Params bs lr) !net !cost =
      let (batchInps, inps') = splitAt bs inps
          (batchTargets, targets') = splitAt bs targets
          (net', cost') =
            backPropagate (DataBunch batchInps batchTargets) ma net
          optimisedNet = optimise ps net'
        in handleBatching
            (DataBunch inps' targets')
            ma
            ps
            optimisedNet
            (cost + cost')
    backPropagate ::
          (KnownNat i, KnownNat o, Activation outA, Activation intA)
      => DataBunch i o
      -> ModelActivations outA intA
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
    optimise ps (Output !c)   = Output (sgd ps c)
    optimise ps (c `Layer` n) = sgd ps c `Layer` optimise ps n

backPropOneInput ::
     (KnownNat i, KnownNat o, Activation outA, Activation intA)
  => R i
  -> R o
  -> ModelActivations outA intA
  -> Network i hs o
  -> (Network i hs o, Double)
backPropOneInput !inp !target !ma !net =
  let (net', _, cost) = go inp target ma net
   in (net', cost)
  where
    go ::
         (KnownNat i, KnownNat o, Activation outA, Activation intA)
      => R i
      -> R o
      -> ModelActivations outA intA
      -> Network i hs o
      -> (Network i hs o, R i, Double)
    go !inp !target (ModelActivations outAct _) (Output !c) =
      let z = weightedInput c inp
          a = calcActivations outAct z
          (c', dWs) = outputErrs inp target a z c
          cost = calcCost outAct target a
       in (Output c', dWs, cost)
    go inp target ma@(ModelActivations _ intAct) (c `Layer` n) =
      let z = weightedInput c inp
          a = calcActivations intAct z
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
         (KnownNat i, KnownNat o, KnownNat k, Activation outA, Activation intA)
      => R k
      -> R o
      -> Connections k i
      -> ModelActivations outA intA
      -> Network i hs o
      -> R i
      -> R i
      -> (Connections k i, Network i hs o, Double, R k)
    backProp inp target c@(C _ w _ _) ma@(ModelActivations _ intAct) net a z =
      let (net', dWs', cost) = go a target ma net
          errs = dWs' * calcGradients intAct z
          errWs = errs `outer` inp
          dWs = tr w #> errs
       in (updateGrads errs errWs c, net', cost, dWs)

getAccuracy ::
     (KnownNat i, KnownNat o, Activation outA, Activation intA)
  => DataBunch i o
  -> Double
  -> ModelActivations outA intA
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


