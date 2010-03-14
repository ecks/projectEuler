import System.Process
import System.IO
import Data.List
import Data.Maybe
import Control.Monad

triangleNums = (reverse . scanr (\a b -> a + b) 55) (reverse [11..1000])

factors n = do
	(stdin, stdout, stderr, ph) <- runInteractiveCommand ("factor "++(show n))
        str <- hGetContents stdout
        return (words (drop (((fromJust . elemIndex ':') str)+2) str))

numOfDivisors n = do
	lst <- factors n
        return (((product . map (+1) . map length . group) lst), n)

ans = do
   divisors <- mapM (\a -> numOfDivisors a) triangleNums
   putStrLn ((show . maximum) divisors)
