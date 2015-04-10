import Data.List(group)
import Data.Numbers.Primes
    
import qualified Data.HashSet as H
    
isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

sumProperDivisors :: Int -> Int
sumProperDivisors 1 = 0
sumProperDivisors n = (foldl (*) 1 multipliers) - n
    where factors = group $ primeFactors n
          powers prime = map (\x -> (prime!!0)^x) [0..(length prime)]
          allPowers = map powers factors
          multipliers = map (foldl (+) 0) allPowers

isAbundant :: Int -> Bool
isAbundant n = n < sumProperDivisors n
               
notASum :: Int -> [Int]
notASum limit = filter notSum [1..limit]
    where abundant = H.fromList $ filter isAbundant [1..limit]
          notSum n = let candidates =
                             H.map (\x -> (H.member x abundant)
                                        && (H.member (n-x) abundant))
                             abundant
                         result = H.filter (True==) candidates
                         in (H.size result) == 0
              
main :: IO ()
main = do
  let notSum = notASum 20161
      sumPairs = foldl (+) 0 notSum
  print sumPairs
