import Data.Numbers.Primes

maximumRatio :: Int -> Int
maximumRatio n = mr n 1 1
    where mr limit count partial
             | count > limit = partial
             | otherwise = if isPrime count
                           then
                               if count * partial > limit
                                  then partial
                                  else mr limit (count + 1) (partial * count)
                           else mr limit (count + 1) partial
                                        
main :: IO ()
main = do
  let answer = maximumRatio 1000000
  print answer
