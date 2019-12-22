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

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

train ::
     Double -- Learning rate
  -> Vector Double --input vector
  -> Vector Double --target vector
  -> Network -- network to train
  -> (Network, Double)
train lr inp0 target net =
  let (trainedNet, _, cost) = go inp0 net
   in (trainedNet, cost)
  where
    go ::
         Vector Double -- input vector
      -> Network -- network to train
      -> (Network, Vector Double, Double)
    -- handle output layer
    go !inp (Output c) =
      let (z, a) = feedForward c inp
          (c', dWs) = outputErrs a z inp target c
          c'' = gradientDescent c' lr
          cost = 1 / 2 * (norm_2 (target - a) ** 2)
       in (Output c'', dWs, cost)
    -- handle the inner layers
    go !inp (c1@(C _ w _ _) `Layer` c2) =
      let (z, a) = feedForward c1 inp
          (c1', c2', cost, dWs) = backProp c1 c2 a z inp
          c1'' = gradientDescent c1' lr
       in (c1'' `Layer` c2', dWs, cost)
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
    gradientDescent :: Connections -> Double -> Connections
    gradientDescent (C b w bGrads wGrads) lr =
      let b' = b - scale lr bGrads
          w' = w - scale lr wGrads
       in C b' w' 0 0

netTest :: Double -> Int -> IO String
netTest rate n = do
  inps <-
    replicateM n $ do
      s <- randomIO :: IO Int
      return (randomVector s Uniform 2 * 2 - 1)
  let outs =
        flip map inps $ \v ->
          if v `inCircle` (fromRational 0.33, 0.33) ||
             v `inCircle` (fromRational (-0.33), 0.33)
            then fromRational 1
            else fromRational 0
  let net0 = randomNet 2 [16, 8] 1
  trained <- foldl' (trainEach rate) net0 (zip3 inps outs [0 ..])
  let outMat =
        [ [ render (norm_2 (runNet trained (vector [x / 25 - 1, y / 10 - 1])))
        | x <- [0 .. 50]
        ]
        | y <- [0 .. 20]
        ]
      render r
        | r <= 0.2 = ' '
        | r <= 0.4 = '.'
        | r <= 0.6 = '-'
        | r <= 0.8 = '='
        | otherwise = '#'
  return (unlines outMat)
  where
    inCircle :: Vector Double -> (Vector Double, Double) -> Bool
    v `inCircle` (o, r) = norm_2 (v - o) <= r

trainEach ::
     Double -> IO Network -> (Vector Double, Vector Double, Int) -> IO Network
trainEach rate ntIO (i, o, count) = do
  nt <- ntIO
  --putStrLn $ "Input " ++ show count
  let (net, cost) = train rate i o nt
  --putStrLn $ "Cost " ++ show cost
  return net

main :: IO ()
main = do
  args <- getArgs
  let n = readMaybe =<< (args !!? 0)
      lr = readMaybe =<< (args !!? 1)
  putStrLn "Training network..."
  res <- netTest (fromMaybe 0.25 lr) (fromMaybe 500000 n)
  putStrLn res

(!!?) :: [a] -> Int -> Maybe a
xs !!? i = listToMaybe (drop i xs)

readExams :: IO ([Vector Double], [Vector Double])
readExams = do
  inp <- readFile "./data/ex2data1.txt"
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

examsNet :: IO Network
examsNet = do
  (inps, outs) <- readExams
  let net0 = randomNet 2 [4] 1
  trained <-
    foldr'
      (\epoch net ->
         print epoch >> foldl' (trainEach 0.25) net (zip3 inps outs [0 ..]))
      net0
      [0 .. 50]
  print $ runNet trained (vector [34.6, 78])
  print $ runNet trained (vector [75.5, 90.4])
  return trained
