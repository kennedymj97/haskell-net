{-# LANGUAGE DataKinds #-}

module ReadUntyped
  ( loadMnistImages
  , loadMnistLabels
  , loadExamData
  , main
  ) where

import           Codec.Compression.GZip (decompress)
import qualified Data.ByteString.Lazy   as BS
import           Data.Foldable
import           Graphics.Image
import           Numeric.LinearAlgebra  as LA
import           System.Random

main :: IO ()
main = do
  mnistTrainImgs <- loadMnistImages 60000 "./data/train-images-idx3-ubyte.gz"
  mnistTrainLbls <- loadMnistLabels 60000 "./data/train-labels-idx1-ubyte.gz"
  print $ length mnistTrainImgs
  print $ length mnistTrainLbls
  print $ head mnistTrainLbls
  --let img = vecToImg $ head mnistTrainImgs
  --writeImage "./data/testImg.png" img

loadMnistImages :: Int -> String -> IO [Vector Double]
loadMnistImages n fpath = bytesToImgVecs n . decompress <$> BS.readFile fpath
  where
    bytesToImgVecs :: Int -> BS.ByteString -> [Vector Double]
    bytesToImgVecs numImgs s =
      foldl' (\acc idx -> bytesToImgVec s idx : acc) [] [0 .. numImgs - 1]
    bytesToImgVec :: BS.ByteString -> Int -> Vector Double
    bytesToImgVec s n =
      LA.scale
        (1 / 255)
        (vector
           (Prelude.map
              (\x ->
                 fromIntegral $ BS.index s (fromIntegral (n * 28 ^ 2 + 16 + x)))
              [0 .. 783]))

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

{-
vecToImg :: Vector Double -> Image RPU Y Double
vecToImg vec = makeImage (28, 28) (getPix vec)
  where
    getPix :: Vector Double -> (Int, Int) -> Pixel Y Double
    getPix vec (row, col) = PixelY (vec ! (row * 28 + col))
-}
loadExamData :: String -> IO ([Vector Double], [Vector Double])
loadExamData fpath = do
  inp <- readFile fpath
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
