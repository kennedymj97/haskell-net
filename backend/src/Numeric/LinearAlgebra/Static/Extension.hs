module Numeric.LinearAlgebra.Static.Extension where

import           Data.Singletons.TypeLits
import qualified Numeric.LinearAlgebra        as LA
import           Numeric.LinearAlgebra.Static

sumElements' :: KnownNat n => R n -> Double
sumElements' = LA.sumElements . unwrap
