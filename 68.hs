import Control.Monad

magicRings :: [[Int]]
magicRings = do
  n00 <- [1..10]
  n01 <- [x | x <- [1..10], not $
                   elem x [n00]]
  n02 <- [x | x <- [1..10], not $
                   elem x [n00,n01]]
        
  n10 <- [x | x <- [1..10], not $
                   elem x [n00,n01,n02]]
  guard $ n10 > n00
  n11 <- [n02]
  n12 <- [x | x <- [1..10], not $
                   elem x [n00,n01,n02,n10]]

  guard $ (n00 + n01 + n02) == (n10 + n11 + n12)

  n20 <- [x | x <- [1..10], not $
                   elem x [n00,n01,n02,n10,n12]]
  guard $ n20 > n10
  n21 <- [n12]
  n22 <- [x | x <- [1..10], not $
                   elem x [n00,n01,n02,n10,n12,n20]]

  guard $ (n00 + n01 + n02) == (n20 + n21 + n22)

  n30 <- [x | x <- [1..10], not $
                   elem x [n00,n01,n02,n10,n12,n20,n22]]
  guard $ n30 > n20
  n31 <- [n22]
  n32 <- [x | x <- [1..10], not $
                   elem x [n00,n01,n02,n10,n12,n20,n22,n30]]

  guard $ (n00 + n01 + n02) == (n30 + n31 + n32)

  n40 <- [x | x <- [1..10], not $
                   elem x [n00,n01,n02,n10,n12,n20,n22,n30,32]]
  guard $ n40 > n30
  n41 <- [n32]
  n42 <- [n01]

  guard $ (n00 + n01 + n02) == (n40 + n41 + n42)
        
  return [n00, n01, n02, n10, n11, n12,
              n20, n21, n22,n30, n31, n32, n40, n41, n42]

canonicalRepresentation :: [Int] -> Int
canonicalRepresentation l =
    read (foldl1 (++) $ map show l) :: Int

maxMagicRing :: [[Int]] -> Int
maxMagicRing rings = maximum $
                     filter (\x -> x < 10^16) $
                            map canonicalRepresentation rings

main :: IO ()
main = do
  let rings = magicRings
  let maxRing = maxMagicRing rings
  print maxRing
