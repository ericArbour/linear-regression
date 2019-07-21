import GetData

type Point = (Double,Double)

linearEq :: Num a => a -> a -> (a -> a)
linearEq m b x = m * x + b

sumOfSquares :: [Point] -> Double -> Double -> Double
sumOfSquares pts m b = sum squares
  where squares = map squareResidual pts
        squareResidual (x,y) = (y - model x) ** 2
        model = linearEq m b

gradient :: Fractional a => (a -> a -> a) -> a -> a -> (a, a)
gradient f x y = ( ( (f (x + h) y) - ( f x y ) ) / h
             , ( (f x (y + h) ) - ( f x y ) ) / h )
  where h = 0.00001

gradientDescent :: (Double -> Double -> Double) -> Double -> Double -> Int -> (Double, Double)
gradientDescent costFn x y step =
  let (xSlope, ySlope) = gradient costFn x y
      xStepSize = xSlope * learningRate
      yStepSize = ySlope * learningRate
  in if (abs xStepSize <= minStepSize && abs yStepSize <= minStepSize) || step >= maxSteps
     then (x, y)
     else gradientDescent costFn (x - xStepSize) (y - yStepSize) (step + 1)
  where learningRate = 0.01
        minStepSize = 0.00001
        maxSteps = 1000

main :: IO ()
main = do
  points <- getData "test-data.csv"
  print $ gradientDescent (sumOfSquares points) 1 0 0
  return ()
