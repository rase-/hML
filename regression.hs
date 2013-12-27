module MachineLearning.Regression
( Parameters(..)
, Dataset(..)
, ParamFindStrategy(..)
, findParameters
, fromList
, run
) where

import qualified Numeric.LinearAlgebra as L

newtype Parameters = Parameters (L.Matrix Double) deriving (Show, Eq)

fromList :: [[Double]] -> L.Matrix Double
fromList xs = (samples L.>< features) $ concat xs
  where samples = length xs
        features = length $ head xs

-- For now only support for double values
-- For now implementation shall stay as a list, but all rows should be of equal
-- length
type X = L.Matrix Double
type Y = L.Matrix Double
type Results = [Double]
data Dataset = TrainingSet X Y | TestSet X

data ParamFindStrategy = NormalEquation | GradientDescent

findParameters :: Dataset -> ParamFindStrategy -> Parameters
findParameters (TestSet x) _ = error "You can't find parameters with label-less data"
findParameters (TrainingSet x y) NormalEquation = Parameters $ L.multiply (L.pinv $ L.multiply (L.ctrans x) x) (L.multiply (L.ctrans x) y)
findParameters d GradientDescent = Parameters $ fromList [[1,2,3]] -- TODO

-- Deprecated... find out how to map with row of matrix
run :: Dataset -> Parameters -> Results
run (TrainingSet x y) _ = error "You should not run experiments with training data" -- Actually maybe you should be able
run (TestSet ts) p = map (runIndividual p) rows
  where rows = L.toRows ts

-- Running the experiment for an individual datapoint
runIndividual :: Parameters -> L.Vector Double -> Double
runIndividual (Parameters p) row = getVal $ (L.ctrans p) L.<> row
  where getVal v = head $ L.toList v

-- Just testing stuff out
main = do putStrLn (show . Parameters $ fromList [[1, 2, 3, 4]])
          let params = findParameters (TrainingSet (fromList [[1,2,3],[1,2,3],[2,2,3]]) (fromList [[1],[2],[3]])) NormalEquation
          putStrLn $ show params
          let results = run (TestSet $ fromList [[1,2,3],[1,2,3],[2,2,3]]) params
          putStrLn $ show results
