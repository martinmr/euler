import Data.List.Split

lineToNumbers :: String -> [Int]
lineToNumbers line = map (\x -> read x :: Int) $
                     splitOn " " line

fileToLines :: String -> [[Int]]
fileToLines file = reverse $ map lineToNumbers $ lines file

maxPath :: [[Int]] -> Int
maxPath (x:[]) = x !! 0
maxPath (first:second:xs) = maxPath (third:xs)
    where localMax = (\i -> max ((first !! i) + second !! i)
                          ((first !! (i + 1)) + second !! i))
          end = (length second) - 1
          third = map localMax [0..end]
maxPath _ = 0

main :: IO()
main = do
  file <- readFile "67.txt"
  let numbers = fileToLines file
  let answer = maxPath numbers
  print answer
