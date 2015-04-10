import Data.MemoTrie

first :: (a, a, a) -> a
first (x,_,_) = x
second :: (a, a, a) -> a
second (_,x,_) = x
third :: (a, a, a) -> a
third (_,_,x) = x

findCoefficient :: Int -> Int -> (Integer, Integer, Integer)
findCoefficient value n
                | n == 0 = (0, 1, a0)
                | otherwise = (nextM, nextD, nextA)
                where previous = memoFindCoefficient  value $ n - 1
                      a0 = floor $ sqrt $ fromIntegral value
                      m = first previous
                      d = second previous
                      a = third previous
                      nextM = d * a - m
                      nextD = quot ((fromIntegral value) - (nextM * nextM)) d
                      nextA = quot (a0 + nextM) nextD

memoFindCoefficient :: Int -> Int -> (Integer, Integer, Integer)
memoFindCoefficient = memo2 findCoefficient

findConvergent :: Int -> Int -> (Integer, Integer)
findConvergent value n
               | n == -2 = (0, 1)
               | n == -1 = (1, 0)
               | n ==  0 = (third $ memoFindCoefficient value 0, 1)
               | otherwise = (p, q)
               where
                an = third $ memoFindCoefficient value n
                (p1, q1) = memoFindConvergent value $ n - 1
                (p2, q2) = memoFindConvergent value $ n - 2
                p = an * p1 + p2
                q = an * q1 + q2
               
memoFindConvergent :: Int -> Int -> (Integer, Integer)
memoFindConvergent = memo2 findConvergent

isSolution :: Int -> Integer -> Integer -> Bool
isSolution d x y =
            if x^2 - ((fromIntegral d) * y^2) == 1
               then True
               else False

isPerfectSquare :: Int -> Bool           
isPerfectSquare n =
         sq * sq == n
         where sq = floor $ sqrt $ (fromIntegral n::Double)

solveEquation :: Int -> Int -> (Integer, Integer)
solveEquation d counter =
              if isPerfectSquare d
                 then (-1, -1)
              else
                if isSolution d num den
                   then (num, den)
                   else solveEquation d $ counter + 1
                where convergent = memoFindConvergent d counter 
                      num = fst convergent
                      den = snd convergent
              
maxXValue :: Int -> (Integer, Int)
maxXValue d
          | d == 1 = (fst $ solveEquation d 0, 1)
          | otherwise = (max oldX newX, newD)
          where
                previous = memoMaxXValue $ d - 1
                oldX = fst previous
                oldD = snd previous
                newX = max oldX $ fst $ solveEquation d 0
                newD = if newX > oldX
                     then d
                     else oldD
          
memoMaxXValue :: Int -> (Integer, Int)
memoMaxXValue = memo maxXValue
           
main :: IO ()                  
main = do
     n <- readLn :: IO Int
     let answer = snd $ memoMaxXValue n
     print answer
