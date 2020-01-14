module HaskellNet.Train where

import HaskellNet.Network
import HaskellNet.Activation
import HaskellNet.Init
import HaskellNet.Optimiser
import HaskellNet.Params
import Data.ReadMnist
import Data.ReadExam
import Numeric.LinearAlgebra.Static

examsNet :: IO ()
examsNet = do
  (trainInps, trainOuts) <- loadExamData "./data/examScoresTrain.txt"
  (testInps, testOuts) <- loadExamData "./data/examScoresTest.txt"
  let trainInps' = map (dvmap (/ 100)) trainInps
      testInps' = map (dvmap (/ 100)) testInps
      examsData =
        ModelData
          (DataBunch trainInps' trainOuts)
          (DataBunch testInps' testOuts)
      net0 = initialiseNet Kaiming :: IO (Network 2 '[ 8, 8] 1)
      params = Params 64 0.1
      modelFuncs = ModelActivations Sigmoid Relu
  putStrLn "Training network..."
  trained <- runEpochs 10 examsData modelFuncs params Sgd net0
  return ()

mnistNet :: IO ()
mnistNet = do
  trainImgs <- loadMnistImages Train "./data/train-images-idx3-ubyte.gz"
  trainLbls <- loadMnistLabels Train "./data/train-labels-idx1-ubyte.gz"
  testImgs <- loadMnistImages Test "./data/t10k-images-idx3-ubyte.gz"
  testLbls <- loadMnistLabels Test "./data/t10k-labels-idx1-ubyte.gz"
  let mnistData =
        ModelData (DataBunch trainImgs trainLbls) (DataBunch testImgs testLbls)
      net0 = initialiseNet Kaiming :: IO (Network 784 '[ 100] 10)
      params = Params 64 0.25
      modelFuncs = ModelActivations Sigmoid Relu
  putStrLn "Training network..."
  trained <- runEpochs 20 mnistData modelFuncs params Sgd net0
  saveNet "./data/mnistNetSigmoid20.txt" trained
  return ()
