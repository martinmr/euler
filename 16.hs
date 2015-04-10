sumDigits :: Integer -> Integer
sumDigits n = sd n 0
    where sd 0 count = count
          sd num count = let r = mod num 10
                             q = quot num 10
                         in sd q (count + r)
                                 
main :: IO()
main = do
  let x = quot (2^1000) 1
  print $ sumDigits x
