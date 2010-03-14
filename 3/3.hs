
factorPrimes = filter (isFactor) (filter (isPrime) [0..10000000])

isFactor n = if 600851475143 `mod` n == 0 then True
                                  else False

isPrime n = if any (\a -> a == 0) (map (\m -> n `mod` m) [2..n-1]) then False
									     else True
