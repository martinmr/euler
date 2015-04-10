import Data.MemoTrie(memo)

d :: Int -> Int
d = memo dm
    where dm n = foldl (+) 0 $ properDivisors n

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

properDivisors :: Int -> [Int]
properDivisors n = 1 : divs ++ reverse (map (n `div`) divs)
    where divs = [x | x <- [2..(isqrt n)], n `rem` x == 0]

amicableNumbers :: Int -> [Int]
amicableNumbers limit = map snd $ filter isAmicable pairs
    where pairs = zip [1..limit] $ map d [1..limit]
          isAmicable (x, dx) = let index = min (limit - 1) $
                                           max 0 $ dx - 1
                                   (y, dy) = pairs !! index
                               in index == (dx - 1) &&
                                  x == dy &&
                                  y == dx &&
                                  x /= y
                                    
main :: IO ()
main = do
  let answer = foldl (+) 0 $ amicableNumbers 10000
  print answer
