module ReadMnist
  ( loadMnistImages
  , loadMnistLabels
  , main
  ) where

import           Codec.Compression.GZip (decompress)
import qualified Data.ByteString.Lazy   as BS
import           Data.Foldable
import           Graphics.Image
import           Numeric.LinearAlgebra
import           System.Random

main :: IO ()
main = do
  mnistTrainImgs <- loadMnistImages 60000 "./data/train-images-idx3-ubyte.gz"
  mnistTrainLbls <- loadMnistLabels 60000 "./data/train-labels-idx1-ubyte.gz"
  print $ length mnistTrainImgs
  print $ length mnistTrainLbls
  print $ head mnistTrainLbls
  let img = vecToImg $ head mnistTrainImgs
  writeImage "./data/testImg.png" img

loadMnistImages :: Int -> String -> IO [Vector Double]
loadMnistImages n fpath = bytesToImgVecs n . decompress <$> BS.readFile fpath
  where
    bytesToImgVecs :: Int -> BS.ByteString -> [Vector Double]
    bytesToImgVecs numImgs s =
      foldl' (\acc idx -> bytesToImgVec s idx : acc) [] [0 .. numImgs - 1]
    bytesToImgVec :: BS.ByteString -> Int -> Vector Double
    bytesToImgVec s n =
      vector
        (Prelude.map
           (\x -> fromIntegral $ BS.index s (fromIntegral (n * 28 ^ 2 + 16 + x)))
           [0 .. 783])

loadMnistLabels :: Int -> String -> IO [Vector Double]
loadMnistLabels n fpath = bytesToLabels n . decompress <$> BS.readFile fpath
  where
    bytesToLabels :: Int -> BS.ByteString -> [Vector Double]
    bytesToLabels numLbls s =
      foldl' (\acc idx -> bytesToLblVec s idx : acc) [] [0 .. numLbls - 1]
    bytesToLblVec :: BS.ByteString -> Int -> Vector Double
    bytesToLblVec s n =
      vector $
      foldr'
        (\x acc ->
           if x == BS.index s (fromIntegral (n + 8))
             then 1 : acc
             else 0 : acc)
        []
        [0 .. 9]

vecToImg :: Vector Double -> Image RPU Y Double
vecToImg vec = makeImage (28, 28) (getPix vec)
  where
    getPix :: Vector Double -> (Int, Int) -> Pixel Y Double
    getPix vec (row, col) = PixelY (vec ! (row * 28 + col))
