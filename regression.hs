module MachineLearning.Regression
( Parameters(..)
, Dataset(..)
, ParamFindStrategy(..)
, findParameters
, run
) where

import Numeric.LinearAlgebra

-- TODO: sort of unnecessary to handle all the stuff as lists... why not just
-- have then as matrices to begin with?

newtype Parameters = Parameters [Double] deriving (Show, Eq)

-- For now only support for double values
-- For now implementation shall stay as a list, but all rows should be of equal
-- length
type X = [[Double]]
type Y = [Double]
type Results = [Double]
data Dataset = TrainingSet X Y | TestSet X

data ParamFindStrategy = NormalEquation | GradientDescent

findParameters :: Dataset -> ParamFindStrategy -> Parameters
findParameters (TestSet x) _ = error "You can't find parameters with label-less data"
findParameters (TrainingSet x y) NormalEquation = Parameters $ concat (toLists $ multiply (pinv $ multiply (ctrans xMat) xMat) (multiply (ctrans xMat) yMat))
  where xMat = (samples >< features) $ concat x
        yMat = (samples >< 1) y
        samples = length x
        features = length (head x)
findParameters d GradientDescent = Parameters [] -- TODO

run :: Dataset -> Parameters -> Results
run (TrainingSet x y) _ = error "You should not run experiments with training data" -- Actually maybe you should be able
run (TestSet ts) p = map (runIndividual p) ts

-- Running the experiment for an individual datapoint
runIndividual :: Parameters -> [Double] -> Double
runIndividual (Parameters p) row = sum $ zipWith (*) row p

-- Just testing stuff out
main = do putStrLn (show $ Parameters [1,2,3,4])
          putStrLn (show $ findParameters (TrainingSet [[1, 2, 3], [2, 2, 2], [2, 1, 2]] [1, 2, 2]) NormalEquation)
          putStrLn (show $ run (TestSet [[1, 2, 3], [1, 2, 3]]) (Parameters [1, 2, 3]))
