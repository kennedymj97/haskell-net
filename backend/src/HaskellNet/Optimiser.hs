module HaskellNet.Optimiser where

import           Data.Singletons.TypeLits
import           HaskellNet.Connections
import           HaskellNet.Params
import           Numeric.LinearAlgebra.Static

class Optimiser a where
    runOptimiser ::
      (KnownNat i, KnownNat o)
      => a
      -> Params
      -> Connections i o
      -> Connections i o

data GradientDescent =
    GradientDescent

instance Optimiser GradientDescent where
    runOptimiser GradientDescent = gradientDescent

gradientDescent :: (KnownNat i, KnownNat o) => Params -> Connections i o -> Connections i o
gradientDescent (Params bs lr) (C b w bGrads wGrads)
  | bs /= 1 =
    error
      "batch size must be one for gradient descent, use sgd if using a bs of more than 1"
  | otherwise = C (b - konst lr * bGrads) (w - konst lr * wGrads) 0 0

data Sgd =
  Sgd

instance Optimiser Sgd where
  runOptimiser Sgd = sgd

sgd :: (KnownNat i, KnownNat o) => Params -> Connections i o -> Connections i o
sgd (Params bs lr) (C b w bGrads wGrads) =
  C (b - (konst (lr / fromIntegral bs) * bGrads))
    (w - (konst (lr / fromIntegral bs) * wGrads))
    0
    0

