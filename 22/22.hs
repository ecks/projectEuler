import List
import Data.List.Split
import Data.Maybe

main = do
  names <- readFile "names.txt"
  (putStrLn . show . foldr1 (+) . multWordAndPos . map trim . sort . endBy ",") names

alphaToNum char = lookup char $ zip ['A'..'Z'] [1..26]

wordToDigit = (foldr1 (+) . map (fromJust . alphaToNum))

multWordAndPos words = zipWith (*) (map wordToDigit words) [1..(length words)]

trim = f . f
  where f = reverse . dropWhile (== '\"')
