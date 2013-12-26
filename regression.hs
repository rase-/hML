module Regression
( Parameters(..)
, Dataset(..)
, ParamFindStrategy(..)
, findParameters
) where

import Numeric.LinearAlgebra

newtype Parameters = Parameters [Double] deriving (Show, Eq)

-- For now only support for double values
-- For now implementation shall stay as a list, but all rows should be of equal
-- length
type X = [[Double]]
type Y = [Double]
data Dataset = Dataset X Y

data ParamFindStrategy = NormalEquation | GradientDescent

findParameters :: Dataset -> ParamFindStrategy -> Parameters
findParameters (Dataset x y) NormalEquation = Parameters $ concat (toLists $ multiply (pinv $ multiply (ctrans xMat) xMat) (multiply (ctrans xMat) yMat))
  where xMat = (samples >< features) $ concat x
        yMat = (samples >< 1) y
        samples = length x
        features = length (head x)
findParameters d GradientDescent = Parameters [] -- TODO

main = do putStrLn (show $ Parameters [1,2,3,4])
          putStrLn (show $ findParameters (Dataset [[1, 2, 3], [2, 2, 2], [2, 1, 2]] [1, 2, 2]) NormalEquation)
