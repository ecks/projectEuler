
main = do
  (putStrLn.show) (head ans)
  return ()

ans = foldl (nextFibonacciAndLucas) ([]) ([0,1,2,3,6,12,24] ++ (mult 25) ++ [3201..4782])

mult = take 8 . iterate (*2)

nLength = length . show

--nextFibonacciAndLucas :: (RealFrac a) => [(Integer,Integer,a,Int)] -> a -> [(Integer,Integer,a,Int)]
nextFibonacciAndLucas flList 0                      = (0,2,0,1):flList
nextFibonacciAndLucas flList 1                      = (1,1,1,1):flList
nextFibonacciAndLucas flList@((f,l,nP,_):flRest) n  = if (n `div` 2) == nP then (f*l,l^2-2*(-1)^(n `div` 2),n,nLength (f*l)):flList 
								  else ((f+l) `div` 2,(5*f+l) `div` 2,n,nLength $ (f+l) `div` 2):flList
