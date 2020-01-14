module HaskellNet.Connections where

import           Data.Binary
import           Data.Singletons.TypeLits
import           GHC.Generics                 (Generic)
import           Numeric.LinearAlgebra.Static

data Connections i o =
  C
    { biases  :: !(R o)
    , weights :: !(L o i)
    , bGrads  :: !(R o)
    , wGrads  :: !(L o i)
    }
  deriving (Show, Generic)

instance (KnownNat i, KnownNat o) => Binary (Connections i o)

updateGrads ::
     (KnownNat i, KnownNat o)
  => R o
  -> L o i
  -> Connections i o
  -> Connections i o
updateGrads bGrads' wGrads' (C b w bGrads wGrads) =
  C b w (bGrads + bGrads') (wGrads + wGrads')

weightedInput :: (KnownNat i, KnownNat o) => Connections i o -> R i -> R o
weightedInput (C b w _ _) v = w #> v + b
