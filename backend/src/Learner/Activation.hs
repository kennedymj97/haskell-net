module Learner.Activation where

import           Data.Singletons.TypeLits
import qualified Numeric.LinearAlgebra                  as LA
import           Numeric.LinearAlgebra.Static
import           Numeric.LinearAlgebra.Static.Extension

class Activation a where
  calcActivations :: (KnownNat b) => a -> R b -> R b
  calcGradients :: (KnownNat b) => a -> R b -> R b
  calcCost :: (KnownNat b) => a -> R b -> R b -> Double

data Sigmoid =
  Sigmoid

instance Activation Sigmoid where
  calcActivations Sigmoid x = 1 / (1 + exp (-x))
  calcGradients Sigmoid x =
    let x' = calcActivations Sigmoid x
     in x' * (1 - x')
  calcCost Sigmoid target a =
    sumElements' $ (target * log a) + ((1 - target) * log (1 - a))

data Relu =
  Relu

instance Activation Relu where
  calcActivations Relu = dvmap (max 0)
  calcGradients Relu =
    dvmap
      (\ele ->
         if ele < 0
           then 0
           else 1)
  calcCost Relu = error "cannot use relu as an output activation"

data Softmax =
  Softmax

instance Activation Softmax where
  calcActivations Softmax x =
    let maxEle = LA.maxElement $ unwrap x
        shiftx = dvmap (\ele -> ele - maxEle) x
        expSum = sumElements' $ dvmap exp shiftx
     in dvmap (\ele -> exp ele / expSum) shiftx
  calcGradients Softmax = error "cannot use softmax for internal layers"
  calcCost Softmax target a = sumElements' $ target * log a
