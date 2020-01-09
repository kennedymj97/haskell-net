module Data.ReadMnist
  ( loadMnistImages
  , loadMnistLabels
  , main
  , Stage(Train, Test)
  ) where

import           Codec.Compression.GZip       (decompress)
import qualified Data.ByteString.Lazy         as BS
import           Data.Foldable
import           Numeric.LinearAlgebra.Static

main :: IO ()
main = do
  mnistTrainImgs <- loadMnistImages Train "./data/train-images-idx3-ubyte.gz"
  mnistTrainLbls <- loadMnistLabels Train "./data/train-labels-idx1-ubyte.gz"
  print $ length mnistTrainImgs
  print $ length mnistTrainLbls
  print $ head mnistTrainLbls

data Stage
  = Train
  | Test

loadMnistImages :: Stage -> String -> IO [R 784]
loadMnistImages stage fpath =
  case stage of
    Train -> loadMnistImages' 60000 fpath
    Test  -> loadMnistImages' 10000 fpath

loadMnistLabels :: Stage -> String -> IO [R 10]
loadMnistLabels stage fpath =
  case stage of
    Train -> loadMnistLabels' 60000 fpath
    Test  -> loadMnistLabels' 10000 fpath

loadMnistImages' :: Int -> String -> IO [R 784]
loadMnistImages' n fpath = bytesToImgVecs n . decompress <$> BS.readFile fpath
  where
    bytesToImgVecs :: Int -> BS.ByteString -> [R 784]
    bytesToImgVecs numImgs s =
      foldl' (\acc idx -> bytesToImgVec s idx : acc) [] [0 .. numImgs - 1]
    bytesToImgVec :: BS.ByteString -> Int -> R 784
    bytesToImgVec s n =
      1 / 255 *
      (vector
         (Prelude.map
            (\x ->
               fromIntegral $ BS.index s (fromIntegral (n * 28 ^ 2 + 16 + x)))
            [0 .. 783]) :: R 784)

loadMnistLabels' :: Int -> String -> IO [R 10]
loadMnistLabels' n fpath = bytesToLabels n . decompress <$> BS.readFile fpath
  where
    bytesToLabels :: Int -> BS.ByteString -> [R 10]
    bytesToLabels numLbls s =
      foldl' (\acc idx -> bytesToLblVec s idx : acc) [] [0 .. numLbls - 1]
    bytesToLblVec :: BS.ByteString -> Int -> R 10
    bytesToLblVec s n =
      vector $
      foldr'
        (\x acc ->
           if x == BS.index s (fromIntegral (n + 8))
             then 1 : acc
             else 0 : acc)
        []
        [0 .. 9]
