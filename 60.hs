{-# LANGUAGE GADTs, TypeFamilies, TypeOperators, ScopedTypeVariables, CPP #-}
{-# LANGUAGE StandaloneDeriving, FlexibleInstances #-}
{-# OPTIONS_GHC -fenable-rewrite-rules #-}

import Data.List
import Data.List.Split
import Data.Bits
import Data.Word
import Data.Int
import Control.Applicative
import Control.Arrow (first,(&&&))
import Data.Monoid
import Data.Function (on)

import Data.Void

infixr 0 :->:

-- | Mapping from all elements of @a@ to the results of some function
class HasTrie a where
    -- | Representation of trie with domain type @a@
    data (:->:) a :: * -> *
    -- | Create the trie for the entire domain of a function
    trie   :: (a  ->  b) -> (a :->: b)
    -- | Convert a trie to a function, i.e., access a field of the trie
    untrie :: (a :->: b) -> (a  ->  b)
    -- | List the trie elements.  Order of keys (@:: a@) is always the same.
    enumerate :: (a :->: b) -> [(a,b)]

-- | Domain elements of a trie
domain :: HasTrie a => [a]
domain = map fst (enumerate (trie (const oops)))
 where
   oops = error "Data.MemoTrie.domain: range element evaluated."

-- Hm: domain :: [Bool] doesn't produce any output.

instance (HasTrie a, Eq b) => Eq (a :->: b) where
  (==) = (==) `on` (map snd . enumerate)

instance (HasTrie a, Show a, Show b) => Show (a :->: b) where
  show t = "Trie: " ++ show (enumerate t)

memo :: HasTrie t => (t -> a) -> (t -> a)
memo = untrie . trie

-- | Memoize a binary function, on its first argument and then on its
-- second.  Take care to exploit any partial evaluation.
memo2 :: (HasTrie s,HasTrie t) => (s -> t -> a) -> (s -> t -> a)

-- | Memoize a ternary function on successive arguments.  Take care to
-- exploit any partial evaluation.
memo3 :: (HasTrie r,HasTrie s,HasTrie t) => (r -> s -> t -> a) -> (r -> s -> t -> a)

-- | Lift a memoizer to work with one more argument.
mup :: HasTrie t => (b -> c) -> (t -> b) -> (t -> c)
mup mem f = memo (mem . f)

memo2 = mup memo
memo3 = mup memo2

-- | Apply a unary function inside of a trie
inTrie :: (HasTrie a, HasTrie c) =>
          ((a  ->  b) -> (c  ->  d))
       -> ((a :->: b) -> (c :->: d))
inTrie = untrie ~> trie

-- | Apply a binary function inside of a trie
inTrie2 :: (HasTrie a, HasTrie c, HasTrie e) =>
           ((a  ->  b) -> (c  ->  d) -> (e  ->  f))
        -> ((a :->: b) -> (c :->: d) -> (e :->: f))
inTrie2 = untrie ~> inTrie

-- | Apply a ternary function inside of a trie
inTrie3 :: (HasTrie a, HasTrie c, HasTrie e, HasTrie g) =>
           ((a  ->  b) -> (c  ->  d) -> (e  ->  f) -> (g  ->  h))
        -> ((a :->: b) -> (c :->: d) -> (e :->: f) -> (g :->: h))
inTrie3 = untrie ~> inTrie2


instance HasTrie Void where
  -- As suggested by Audun Skaugen
  data Void :->: a = VoidTrie 
  trie _ = VoidTrie
  untrie VoidTrie = absurd
  enumerate VoidTrie = []

instance HasTrie () where
    newtype () :->: a = UnitTrie a
    trie f = UnitTrie (f ())
    untrie (UnitTrie a) = \ () -> a
    enumerate (UnitTrie a) = [((),a)]

instance HasTrie Bool where
    data Bool :->: x = BoolTrie x x
    trie f = BoolTrie (f False) (f True)
    untrie (BoolTrie f t) = if' f t
    enumerate (BoolTrie f t) = [(False,f),(True,t)]

if' :: x -> x -> Bool -> x
if' t _ False = t
if' _ e True  = e

instance (HasTrie a, HasTrie b) => HasTrie (Either a b) where
    data (Either a b) :->: x = EitherTrie (a :->: x) (b :->: x)
    trie f = EitherTrie (trie (f . Left)) (trie (f . Right))
    untrie (EitherTrie s t) = either (untrie s) (untrie t)
    enumerate (EitherTrie s t) = enum' Left s `weave` enum' Right t

enum' :: (HasTrie a) => (a -> a') -> (a :->: b) -> [(a', b)]
enum' f = (fmap.first) f . enumerate

weave :: [a] -> [a] -> [a]
[] `weave` as = as
as `weave` [] = as
(a:as) `weave` bs = a : (bs `weave` as)

instance (HasTrie a, HasTrie b) => HasTrie (a,b) where
    newtype (a,b) :->: x = PairTrie (a :->: (b :->: x))
    trie f = PairTrie (trie (trie . curry f))
    untrie (PairTrie t) = uncurry (untrie .  untrie t)
    enumerate (PairTrie tt) =
      [ ((a,b),x) | (a,t) <- enumerate tt , (b,x) <- enumerate t ]

instance (HasTrie a, HasTrie b, HasTrie c) => HasTrie (a,b,c) where
    newtype (a,b,c) :->: x = TripleTrie (((a,b),c) :->: x)
    trie f = TripleTrie (trie (f . trip))
    untrie (TripleTrie t) = untrie t . detrip
    enumerate (TripleTrie t) = enum' trip t

trip :: ((a,b),c) -> (a,b,c)
trip ((a,b),c) = (a,b,c)

detrip :: (a,b,c) -> ((a,b),c)
detrip (a,b,c) = ((a,b),c)


instance HasTrie x => HasTrie [x] where
    newtype [x] :->: a = ListTrie (Either () (x,[x]) :->: a)
    trie f = ListTrie (trie (f . list))
    untrie (ListTrie t) = untrie t . delist
    enumerate (ListTrie t) = enum' list t

list :: Either () (x,[x]) -> [x]
list = either (const []) (uncurry (:))

delist :: [x] -> Either () (x,[x])
delist []     = Left ()
delist (x:xs) = Right (x,xs)

#define WordInstance(Type,TrieType)\
instance HasTrie Type where \
    newtype Type :->: a = TrieType ([Bool] :->: a);\
    trie f = TrieType (trie (f . unbits));\
    untrie (TrieType t) = untrie t . bits;\
    enumerate (TrieType t) = enum' unbits t

WordInstance(Word,WordTrie)
WordInstance(Word8,Word8Trie)
WordInstance(Word16,Word16Trie)
WordInstance(Word32,Word32Trie)
WordInstance(Word64,Word64Trie)

-- instance HasTrie Word where
--     newtype Word :->: a = WordTrie ([Bool] :->: a)
--     trie f = WordTrie (trie (f . unbits))
--     untrie (WordTrie t) = untrie t . bits
--     enumerate (WordTrie t) = enum' unbits t


-- | Extract bits in little-endian order
bits :: (Num t, Bits t) => t -> [Bool]
bits 0 = []
bits x = testBit x 0 : bits (shiftR x 1)

-- | Convert boolean to 0 (False) or 1 (True)
unbit :: Num t => Bool -> t
unbit False = 0
unbit True  = 1

-- | Bit list to value
unbits :: (Num t, Bits t) => [Bool] -> t
unbits [] = 0
unbits (x:xs) = unbit x .|. shiftL (unbits xs) 1

instance HasTrie Char where
    newtype Char :->: a = CharTrie (Int :->: a)
    untrie (CharTrie t) n = untrie t (fromEnum n)
    trie f = CharTrie (trie (f . toEnum))
    enumerate (CharTrie t) = enum' toEnum t

-- Although Int is a Bits instance, we can't use bits directly for
-- memoizing, because the "bits" function gives an infinite result, since
-- shiftR (-1) 1 == -1.  Instead, convert between Int and Word, and use
-- a Word trie.  Any Integral type can be handled similarly.

#define IntInstance(IntType,WordType,TrieType) \
instance HasTrie IntType where \
    newtype IntType :->: a = TrieType (WordType :->: a); \
    untrie (TrieType t) n = untrie t (fromIntegral n); \
    trie f = TrieType (trie (f . fromIntegral)); \
    enumerate (TrieType t) = enum' fromIntegral t

IntInstance(Int,Word,IntTrie)
IntInstance(Int8,Word8,Int8Trie)
IntInstance(Int16,Word16,Int16Trie)
IntInstance(Int32,Word32,Int32Trie)
IntInstance(Int64,Word64,Int64Trie)

-- For unbounded integers, we don't have a corresponding Word type, so
-- extract the sign bit.

instance HasTrie Integer where
    newtype Integer :->: a = IntegerTrie ((Bool,[Bool]) :->: a)
    trie f = IntegerTrie (trie (f . unbitsZ))
    untrie (IntegerTrie t) = untrie t . bitsZ
    enumerate (IntegerTrie t) = enum' unbitsZ t


unbitsZ :: (Num n, Bits n) => (Bool,[Bool]) -> n
unbitsZ (positive,bs) = sig (unbits bs)
 where
   sig | positive  = id
       | otherwise = negate

bitsZ :: (Num n, Ord n, Bits n) => n -> (Bool,[Bool])
bitsZ = (>= 0) &&& (bits . abs)
instance (HasTrie a, Monoid b) => Monoid (a :->: b) where
  mempty  = trie mempty
  mappend = inTrie2 mappend

instance HasTrie a => Functor ((:->:) a) where
  fmap f = inTrie (fmap f)

instance HasTrie a => Applicative ((:->:) a) where
  pure b = trie (pure b)
  (<*>)  = inTrie2 (<*>)

instance HasTrie a => Monad ((:->:) a) where
  return a = trie (return a)
  u >>= k  = trie (untrie u >>= untrie . k)

-- | Identity trie
idTrie :: HasTrie a => a :->: a
idTrie = trie id

infixr 9 @.@
-- | Trie composition
(@.@) :: (HasTrie a, HasTrie b) =>
         (b :->: c) -> (a :->: b) -> (a :->: c)
(@.@) = inTrie2 (.)

(~>) :: (a' -> a) -> (b -> b') -> ((a -> b) -> (a' -> b'))
g ~> f = (f .) . (. g)

------------------------------------------------
isPrime :: Int -> Bool
isPrime = memo p
        where
        p 1 = False
        p 2 = True
        p n = ([] == [x | x <- [2..(floor . sqrt) (fromIntegral n)],
                    mod n x == 0])

allPrimes :: [Int] -> [Int]
allPrimes numbers = filter (\x -> isPrime x) numbers

primeSets :: Int -> Int -> [[Int]]
primeSets n k = sequence $ take k $ repeat primes
          where primes = allPrimes [2..n]

normalizePrimeSet :: [Int] -> [Int]
normalizePrimeSet xs = sort . nub $ xs
          
prod :: [Int] -> [(Int, Int)]
prod xs = [(x, y) | x<-xs, y<-xs, x < y]

isConcatPrime :: (Int, Int) -> Bool
isConcatPrime (x, y) = isPrime xy && isPrime yx
           where xy = read (show x ++ show y) :: Int
                 yx = read (show y ++ show x) :: Int

isSolution :: [(Int, Int)] -> Bool
isSolution [] = True
isSolution (x:xs) = if isConcatPrime $ x
                       then isSolution xs
                       else False

solveSet :: [Int] -> Int -> Maybe Int
solveSet xs n = if pairs == [] || length set /= length xs || 
                   maximum set > n
                 then Nothing
                 else if isSolution pairs
                    then Just $ sum xs
                    else Nothing
              where set = normalizePrimeSet xs
                    pairs = prod set
      
allAnswers :: Int -> Int -> [Maybe Int]
allAnswers n k = map (\x -> solveSet x n) $ primeSets n k
           
printMaybe :: Show a => Maybe a -> IO ()
printMaybe (Just x) = print x

printAnswer :: Maybe Int -> IO ()
printAnswer n =  if n /= Nothing
                    then printMaybe n
                    else return ()
              
main :: IO ()
main = do
     input <- getLine
     let numbers = splitOn " " input
         n = read (numbers !! 0) :: Int
         k = read (numbers !! 1) :: Int
     mapM_ printAnswer $ sort $ nub $ allAnswers n k
