import Data.List

newtype StateTrans s a = ST( s -> (a,s) )

--(>>>=) :: StateTrans s a -> (a -> StateTrans s b) -> StateTrans s b
--p >>>= k = \s0 -> let (a,s1) = p s0
--                      (b,s2) = k a s1
--                   in (b,s2)

--unit :: a -> StateTrans s a
--unit a = \s -> (a,s)

instance Monad (StateTrans s)
  where
    (ST p) >>= k = ST( \s0 -> let (a,s1) = p s0
                                  (ST q) = k a
                              in q s1 )

    return a = ST( \s -> (a,s) )

applyST :: StateTrans s a -> s -> (a,s)
applyST (ST p) s = p s

main = let eval = do 
       {
		a <- ST (accumIfNotSumOfTwoAbundants 0 12); 
                b <- ST (accumIfNotSumOfTwoAbundants a 13); 
                c <- ST (accumIfNotSumOfTwoAbundants b 14); 
                return c
       }
       in putStrLn . show $ applyST eval []
 

--let eval   = ST(accumIfNotSumOfTwoAbundants 0 12) >>=
--             \a  -> ST(accumIfNotSumOfTwoAbundants a 13) >>=
--             \b  -> ST(accumIfNotSumOfTwoAbundants b 14) >>=
--             \c  -> return c
--        in putStrLn . show $ applyST eval []
       

accumIfNotSumOfTwoAbundants result num abundants
        | isAbundant num == True  && isSumOfTwoAbundants abundants num == True  = (result,num:abundants)
        | isAbundant num == False && isSumOfTwoAbundants abundants num == True  = (result,abundants)
        | isAbundant num == True  && isSumOfTwoAbundants abundants num == False = (num+result,num:abundants)
        | isAbundant num == False && isSumOfTwoAbundants abundants num == False = (num+result,abundants)

isSumOfTwoAbundants abundants n = if dropWhile (\a -> ((n - a) `elem` abundants) == False) abundants == [] then False else True

isAbundant n = (sigma n) - n > n
-- sum of divisors
sigma n = product $ map sigma' $ prime_factors_mult n
  where sigma' (p,n) = (p^(n+1) - 1) `div` (p-1)

-- calculating prime factors
primeFactors n = factor n primes
  where factor n (p:ps) | p*p > n = [n]
                        | n `mod` p /= 0 = factor n ps
                        | otherwise = p : factor (n `div` p) (p:ps)
        primes = 2 : filter ((==1) . length . primeFactors) [3,5..]

prime_factors_mult n = map swap $ encode $ primeFactors n
  where swap (x,y) = (y,x)

encode = map (\x -> (length x, head x)) . group
