type Point = (Float,Float)

points :: [Point]
points = [(0.5,1.4),(2.3,1.9),(2.9,3.2)]

linearEq :: Num a => a -> a -> (a -> a)
linearEq m b x = m * x + b

sumOfSquares :: [Point] -> Float -> Float -> Float
sumOfSquares pts m b = sum squares
  where squares = map squareResidual pts
        squareResidual (x,y) = (y - model x) ** 2
        model = linearEq m b

gradient :: Fractional a => (a -> a -> a) -> a -> a -> (a, a)
gradient f x y = ( ( (f (x + h) y) - ( f x y ) ) / h
             , ( (f x (y + h) ) - ( f x y ) ) / h )
  where h = 0.00001

gradientDescent :: (Float -> Float -> Float) -> Float -> Float -> Int -> (Float, Float)
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
  print $ gradientDescent (sumOfSquares points) 1 0 0
  return ()
