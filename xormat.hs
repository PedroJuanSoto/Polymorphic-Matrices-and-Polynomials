{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

import Data.Bits

data Matrix a = Rows [[a]] | Cols [[a]] deriving (Show,Ord,Eq,Read)

data Poly a = Coef [a]  deriving (Show,Ord,Eq,Read)

class Numseq a where
  (+++) :: a -> a ->  a

class Mat a where
  transps ::  a -> a

instance Numseq (Matrix a) where
  (Rows x) +++ (Rows y) = (Rows (x++y))
  (Cols x) +++ (Cols y) = (Cols (x++y))

instance Mat (Matrix a) where
  transps (Rows []) = (Cols [])
  transps (Rows ([]:_)) = (Cols [])
  transps (Rows x)  = (Cols [(map head x)]) +++ (transps (Rows (map tail x)))
  transps (Cols []) = (Rows [])
  transps (Cols ([]:_)) = (Rows [])
  transps (Cols x)  = (Rows [(map head x)]) +++ (transps (Cols (map tail x)))

instance Num a => Num (Matrix a) where
  (Rows x) + (Rows []) = (Rows x)
  (Rows []) + (Rows x) = (Rows x)
  (Rows (x:xs)) + (Rows (y:ys)) = (Rows [zipWith (+) x y]) +++ the_rest
    where
      the_rest = ((Rows xs) + (Rows ys))
  (Rows x) * (Rows y) = (Rows x) * (transps (Rows y))
  (Rows x) * (Cols []) = (Rows [])
  (Rows []) * (Cols x) = (Rows [])
  (Rows (x:xs)) * (Cols y) = (Rows first_row) +++ ((Rows xs) * (Cols y))
    where
      first_row = [[(foldr (+) 0 (zipWith (*) x z)) | z<-y]]
  fromInteger i = (Rows [[fromInteger i]])
  abs (Rows []) = (Rows [[0]])
  abs (Rows (x:xs)) = (Rows [[abs (foldr (+) 0 (zipWith (*) x x))]]) + abs (Rows xs)
  signum (Rows []) = (Rows [[1]])
  signum (Rows (x:xs)) = (Rows [[signum (head x)]])
  negate (Rows []) = (Rows [])
  negate (Rows (x:xs)) = (Rows [map negate x]) +++ negate (Rows xs)


instance Numseq (Poly a) where
  (Coef x) +++ (Coef y) = (Coef (x++y))

instance (Num a, Eq a) => Num (Poly a) where
  (Coef x) + (Coef []) = (Coef x)
  (Coef []) + (Coef x) = (Coef x)
  (Coef (x:xs)) + (Coef (y:ys)) = (Coef [x + y]) +++ ((Coef xs) + (Coef ys))
  (Coef x) * (Coef []) = (Coef [0])
  (Coef []) * (Coef x) = (Coef [0])
  (Coef [g]) * (Coef [h]) = (Coef [g*h])
  (Coef x) * (Coef [0,1]) = (Coef (0:x))
  (Coef [0,1]) * (Coef x) = (Coef (0:x))
  (Coef (a:as)) * (Coef (b:bs)) = reduce (f + i + o + l)
    where
      f = (Coef [a*b])
      i = x * ((Coef [b]) * (Coef as))
      o = x * ((Coef [a]) * (Coef bs))
      l = x * (x * ((Coef as) * (Coef bs)))
      x = Coef [0,1]
  fromInteger i = (Coef [fromInteger i])
  abs (Coef x) = (Coef [abs (foldr (+) 0 (zipWith (*) x x))])
  signum (Coef x) = (Coef [signum (head x)])
  negate (Coef x) = Coef (map negate x)

reduce :: (Num a, Eq a) => Poly a -> Poly a
reduce (Coef []) = Coef []
reduce (Coef [x]) = Coef [x]
reduce (Coef (q:qs)) = if (head (reverse (q:qs))) == 0
  then reduce (Coef qs)
  else Coef (q:qs)

class Scalar a b | a -> b  where
  det :: a -> b

instance (Num a,Enum a,Eq a, Fractional a) => Scalar (Matrix a) a where
  det (Rows [[a,b],[c,d]]) = a*d-b*c
  det (Rows (x:xs)) = head x * det (transps (Cols (tail (unwr (transps (Rows rest))))))
    where
      unwr (Cols x) = x
      pivot y = map ( * (head y))
      redu = map (/ (head x)) x
      list = [pivot y redu | y <- xs]
      (!!) (u:us) (v:vs) = [(u-v)] ++ (us !! vs)
      (!!) [] [] = []
      rest = zipWith (!!) list xs


bool_to_comm :: Int -> (Int -> (Int -> Int)) -> Matrix Int
bool_to_comm n f = (Rows [map g [0..n-1] | g <- (map f [0..n-1]) ])

comm_to_bool :: Matrix Int -> (Int -> (Int -> Int))
comm_to_bool (Rows m) = \x -> (\y -> (m !! x) !! y)

xorify :: Int  -> (Int -> (Int -> Int)) -> Matrix Int
xorify n f = bool_to_comm m (\x -> (\y -> f (u x y) (v x y)))
  where
    m = 2^(2*n)
    u = (\x -> (\y ->(div (Data.Bits.xor x y) (2^n) )))
    v = (\x -> (\y ->(mod (Data.Bits.xor x y) (2^n) )))
