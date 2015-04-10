import Data.List(group)
    
triangularNumber :: Int -> Int
triangularNumber n = quot (n*(n+1)) 2

findPrimeDivisor :: Int -> Int -> Int
findPrimeDivisor n current =
    if n `rem` current == 0
       then current
       else findPrimeDivisor n (current + 1)
            
primeDivisors :: Int -> [Int]
primeDivisors n = pd n []
    where pd num xs
             | num == 1 = xs
             | otherwise = let divisor = findPrimeDivisor num 2
                           in pd (quot num divisor) (divisor:xs)

countDivisors :: [Int] -> Int
countDivisors divisors =
    productDivisors groupedDivisors
    where groupedDivisors = group divisors
          productDivisors [] = 1
          productDivisors (x:xs) = ((length x) + 1) * productDivisors xs
                               
numberDivisors :: Int -> Int
numberDivisors n = countDivisors $ primeDivisors n

testAnswer :: Int -> Int -> Bool
testAnswer n divisors = numberDivisors n > divisors

answer :: Int -> Int
answer n =
    if testAnswer tn 100
       then tn
       else answer (n + 1)
    where tn = triangularNumber n

main :: IO ()
main = do
  let x = answer 1
  print x
