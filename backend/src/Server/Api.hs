module Server.Api where

import           Data.Aeson.Types
import           Data.Text
import           GHC.Generics                 (Generic)
import           HaskellNet.Activation
import           HaskellNet.Network
import           Network.HTTP.Types           as HTTP
import           Network.Wai.Handler.Warp     (run)
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.RequestLogger
import qualified Numeric.LinearAlgebra        as LA
import           Numeric.LinearAlgebra.Static
import           Servant
import           Servant.API

type MnistAPI = ReqBody '[ JSON] NumberString :> Post '[ JSON] Prediction

newtype NumberString =
  NumberString
    { img :: [Double]
    }
  deriving (Generic)

instance FromJSON NumberString

newtype Prediction =
  Prediction
    { probs :: [Double]
    }
  deriving (Generic)

instance ToJSON Prediction

getPrediction :: Network 784 '[ 100] 10 -> NumberString -> Prediction
getPrediction net (NumberString img) =
  Prediction
    ((LA.toList . unwrap) $
     runNet net (ModelActivations Sigmoid Relu) (convertToVec img))

convertToVec :: [Double] -> R 784
convertToVec img = (1 / 255) * vector img

-- convertToVec = vector
server :: Network 784 '[ 100] 10 -> Server MnistAPI
server = predictionHandler
  where
    predictionHandler ::
         Network 784 '[ 100] 10 -> NumberString -> Handler Prediction
    predictionHandler net imgStr = return $ getPrediction net imgStr

mnistAPI :: Proxy MnistAPI
mnistAPI = Proxy

application :: Network 784 '[ 100] 10 -> Application
application net = cors (const $ Just corsPolicy) $ logStdout $ serve mnistAPI (server net)

    --{ corsOrigins = Nothing
corsPolicy :: CorsResourcePolicy
corsPolicy =
  CorsResourcePolicy
    { corsOrigins = Nothing
    , corsMethods = allowedMethods
    , corsRequestHeaders = ["content-type"]
    , corsExposedHeaders = Nothing
    , corsMaxAge = Nothing
    , corsVaryOrigin = False
    , corsRequireOrigin = False
    , corsIgnoreFailures = False
    }

allowedMethods :: [HTTP.Method]
allowedMethods = ["GET", "POST", "HEAD", "OPTIONS"]

main :: IO ()
main = do
  net <- loadNet "./data/mnistNetSigmoid.txt" :: IO (Network 784 '[ 100] 10)
  putStrLn "Running API on port 8081"
  run 8081 (application net)
