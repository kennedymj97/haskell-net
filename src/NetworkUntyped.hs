{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NetworkUntyped where

import           Control.Monad
import           Control.Monad.Random
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
    } -- m to n layer

data Network where
  Output :: !Connections -> Network
  Layer :: !Connections -> !Network -> Network

sigmoid :: Floating a => a -> a
sigmoid x = 1 / (1 + exp (-x))

sigmoid' :: Floating a => a -> a
sigmoid' x = sigmoid x * (1 - sigmoid x)

weightedInput :: Connections -> Vector Double -> Vector Double
weightedInput (C b w) v = w #> v + b

runNet :: Network -> Vector Double -> Vector Double
runNet (Output c) !inp = sigmoid (weightedInput c inp)
runNet (c `Layer` c') !inp =
  let inp' = sigmoid (weightedInput c inp)
   in runNet c' inp'

randomConnections :: MonadRandom m => Int -> Int -> m Connections
randomConnections i o = do
  seed1 :: Int <- getRandom
  seed2 :: Int <- getRandom
  let b = randomVector seed1 Uniform o * 2 - 1
      w = uniformSample seed2 o (replicate i (-1, 1))
  return (C b w)

randomConnections' :: Int -> Int -> IO Connections
randomConnections' i o = do
  seed1 <- randomIO :: IO Int
  seed2 <- randomIO :: IO Int
  let b = randomVector seed1 Uniform o * 2 - 1
      w = uniformSample seed2 o (replicate i (-1, 1))
  return (C b w)

randomNet :: MonadRandom m => Int -> [Int] -> Int -> m Network
randomNet i [] o     = Output <$> randomConnections i o
randomNet i (h:hs) o = Layer <$> randomConnections i h <*> randomNet h hs o

randomNet' :: Int -> [Int] -> Int -> IO Network
randomNet' i [] o     = Output <$> randomConnections i o
randomNet' i (h:hs) o = Layer <$> randomConnections i h <*> randomNet h hs o

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

train ::
     Double -- Learning rate
  -> Vector Double --input vector
  -> Vector Double --target vector
  -> Network -- network to train
  -> (Network, Double)
train lr inp0 target net = let (trainedNet, _, cost) = go inp0 net
                           in (trainedNet, cost)
  where
    go ::
         Vector Double -- input vector
      -> Network -- network to train
      -> (Network, Vector Double, Double)
    -- handle output layer
    go !inp (Output c@(C _ w)) =
      let z = weightedInput c inp
          a = sigmoid z
          errs = (a - target) * sigmoid' z
          c' = updateConnections c lr errs inp
          dWs = tr w #> errs
          cost = 1 / 2 * (norm_2 (target - a) ** 2)
       in (Output c', dWs, cost)
    -- handle the inner layers
    go !inp (c1@(C _ w) `Layer` c2) =
      let z = weightedInput c1 inp
          a = sigmoid z
          (c2', dWs', cost) = go a c2
          errs = sigmoid' z * dWs'
          c1' = updateConnections c1 lr errs inp
          dWs = tr w #> errs
       in (c1' `Layer` c2', dWs, cost)
    updateConnections ::
         Connections -> Double -> Vector Double -> Vector Double -> Connections
    updateConnections (C b w) lr errs inp =
      let b' = b - scale lr errs
          w' = w - scale lr (errs `outer` inp)
       in C b' w'

netTest :: MonadRandom m => Double -> Int -> m String
netTest rate n = do
  inps <-
    replicateM n $ do
      s <- getRandom
      return (randomVector s Uniform 2 * 2 - 1)
  let outs =
        flip map inps $ \v ->
          if v `inCircle` (fromRational 0.33, 0.33) ||
             v `inCircle` (fromRational (-0.33), 0.33)
            then fromRational 1
            else fromRational 0
  net0 <- randomNet 2 [16, 8] 1
  let trained = foldl' trainEach net0 (zip inps outs)
        where
          trainEach :: Network -> (Vector Double, Vector Double) -> Network
          trainEach nt (i, o) = fst $ train rate i o nt
      outMat =
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

netTest' :: Double -> Int -> IO String
netTest' rate n = do
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
  let net0 = randomNet' 2 [16, 8] 1
  trained <- foldl' trainEach net0 (zip3 inps outs [0 ..])
  
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
    trainEach :: IO Network -> (Vector Double, Vector Double, Int) -> IO Network
    trainEach ntIO (i, o, count) = do
      nt <- ntIO
      putStrLn $ "Input " ++ show count
      let (net, cost) = train rate i o nt
      putStrLn $ "Cost " ++ show cost
      return net

main :: IO ()
main = do
  args <- getArgs
  let n = readMaybe =<< (args !!? 0)
      rate = readMaybe =<< (args !!? 1)
  putStrLn "Training network..."
  putStrLn =<< evalRandIO (netTest (fromMaybe 0.25 rate) (fromMaybe 500000 n))

main' :: IO ()
main' = do
  args <- getArgs
  let n = readMaybe =<< (args !!? 0)
      lr = readMaybe =<< (args !!? 1)
  putStrLn "Training network..."
  res <- netTest' (fromMaybe 0.25 lr) (fromMaybe 500000 n)
  putStrLn res

(!!?) :: [a] -> Int -> Maybe a
xs !!? i = listToMaybe (drop i xs)
