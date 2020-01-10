module Learner.Init where

import           Data.Singletons.TypeLits
import           Learner.Connections
import           Numeric.LinearAlgebra.Static
import           System.Random

class Init a where
  genRandom :: (KnownNat i, KnownNat o) => a -> IO (Connections i o)

data RandomUniform =
  RandomUniform

instance Init RandomUniform where
  genRandom RandomUniform = randomConnections

randomConnections :: (KnownNat i, KnownNat o) => IO (Connections i o)
randomConnections = do
  seed1 <- randomIO :: IO Int
  seed2 <- randomIO :: IO Int
  let b = randomVector seed1 Uniform * 2 - 1
      w = uniformSample seed2 (-1) 1
  return (C b w 0 0)

data Kaiming =
  Kaiming

instance Init Kaiming where
  genRandom Kaiming = kaimingInit

kaimingInit :: (KnownNat i, KnownNat o) => IO (Connections i o)
kaimingInit = do
  let b = 0 * randomVector 0 Uniform
  w <- randn
  let w' = sqrt (2 / fromIntegral (snd $ size w)) * w
  return (C b w' 0 0)
