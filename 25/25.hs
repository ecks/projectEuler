import Data.Number.CReal

main = do
  (putStrLn.show) (foldr (nextFibonacciAndLucas) (zip [] []) [0,1,2,3,6,12,24])
  return ()

nextFibonacciAndLucas :: (Integral a) => a -> [(a,a)] -> [(a,a)]
nextFibonacciAndLucas 0 flList                 = (0,2):flList
nextFibonacciAndLucas 1 flList                 = (1,1):flList
nextFibonacciAndLucas n flList@((f,l):flRest)  = (f*l,l^2-2*(-1)^(fromRational ((fromIntegral n)/2))):flList
