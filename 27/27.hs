import Data.Numbers.Primes
import Pr3.Pr3

main = do
   (putStrLn.show) [exhaustBPrimes a | a <- takeWhile (< 1000) primes]


--(map (\b -> lengthOfConsPrimes b) $ takeWhile (< 1000) primes)
exhaustBPrimes a = [ lengthOfConsPrimes a b |  b <- takeWhile (< 1000) primes]

lengthOfConsPrimes a b = (length . takeWhile (isPrime) . map (\n -> form n a b)) [0..b] 

form n a b = n^2 + (a*n) + b
