lengthCollatz :: Int -> Int
lengthCollatz n = lc n 1
    where lc 1 count = count
          lc num count = if even num
                         then lc (quot num 2) (count + 1)
                         else lc (3*num + 1) (count + 1)

longestChain :: Int -> (Int, Int)
longestChain limit = lc 2 (1, 1)
    where lc n (oldN, oldMax)
             | n == limit = (oldN, oldMax)
             | otherwise = let chainLength = lengthCollatz n
                               newMax = max oldMax chainLength
                               newN = if newMax == oldMax
                                         then oldN
                                         else n
                           in lc (n + 1) (newN, newMax)
                           
main :: IO()
main = do
  let (n, maxChain) = longestChain 1000000
  print n
  print maxChain
                                     
