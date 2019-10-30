{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

import Data.Bits

data Matrix a = Rows [[a]] | Cols [[a]] deriving (Show,Ord,Eq,Read)

data Poly a = Coef [a]  deriving (Show,Ord,Eq,Read)

data Polymat a = Prows [[Poly a]] | Pcols [[Poly a]] deriving (Show,Ord,Eq,Read)

class Numseq a where
  (+++) :: a -> a ->  a
  (!+) ::  a -> a ->  a
  (!*) ::  a -> a ->  a

instance (Num a,Enum a,Eq a) => Numseq (Matrix a) where
  (Rows x) +++ (Rows y) = (Rows (x++y))
  (Cols x) +++ (Cols y) = (Cols (x++y))
  (Rows x) !+ (Rows []) = (Rows x)
  (Rows []) !+ (Rows x) = (Rows x)
  (Rows (x:xs)) !+ (Rows (y:ys)) = (Rows [zipWith (+) x y]) +++ the_rest
    where
      the_rest = ((Rows xs) !+ (Rows ys))
  (Rows x) !* (Rows y) = (Rows x) !* (transps (Rows y))
  (Rows x) !* (Cols []) = (Rows [])
  (Rows []) !* (Cols x) = (Rows [])
  (Rows (x:xs)) !* (Cols y) = (Rows first_row) +++ ((Rows xs) !* (Cols y))
    where
      first_row = [[(foldr (+) 0 (zipWith (*) x z)) | z<-y]]

instance (Num a,Enum a,Eq a) => Numseq (Poly a) where
  (Coef x) +++ (Coef y) = (Coef (x++y))
  (Coef x) !+ (Coef []) = (Coef x)
  (Coef []) !+ (Coef x) = (Coef x)
  (Coef (x:xs)) !+ (Coef (y:ys)) = (Coef [x + y]) +++ ((Coef xs) !+ (Coef ys))
  (Coef x) !* (Coef []) = (Coef [0])
  (Coef []) !* (Coef x) = (Coef [0])
  (Coef x) !* (Coef [0,1]) = (Coef (0:x))
  (Coef [0,1]) !* (Coef x) = (Coef (0:x))
  (Coef (a:as)) !* (Coef (b:bs)) = f !+ i !+ o !+ l
    where
      f = (Coef [a*b])
      i = x !* ((Coef [b]) !* (Coef as))
      o = x !* ((Coef [a]) !* (Coef bs))
      l = x !* (x !* ((Coef as) !* (Coef bs)))
      x = Coef [0,1]

instance (Num a,Enum a,Eq a) => Numseq (Polymat a) where
  (Prows x) +++ (Prows y) = (Prows (x++y))
  (Pcols x) +++ (Pcols y) = (Pcols (x++y))
  (Prows x) !+ (Prows []) = (Prows x)
  (Prows []) !+ (Prows x) = (Prows x)
  (Prows (x:xs)) !+ (Prows (y:ys)) = (Prows [zipWith (!+) x y]) +++ the_rest
    where
      the_rest = ((Prows xs) !+ (Prows ys))
  (Prows x) !* (Prows y) = (Prows x) !* (transps (Prows y))
  (Prows x) !* (Pcols []) = (Prows [])
  (Prows []) !* (Pcols x) = (Prows [])
  (Prows (x:xs)) !* (Pcols y) = (Prows first_row) +++ ((Prows xs) !* (Pcols y))
    where
      first_row = [[(foldr (!+) (Coef [0]) (zipWith (!*) x z)) | z<-y]]

class Mat a where
  transps ::  a -> a
  minor :: Int -> Int -> a -> a

instance (Num a,Enum a,Eq a) => Mat (Matrix a) where
  transps (Rows []) = (Cols [])
  transps (Rows ([]:_)) = (Cols [])
  transps (Rows x)  = (Cols [(map head x)]) +++ (transps (Rows (map tail x)))
  transps (Cols []) = (Rows [])
  transps (Cols ([]:_)) = (Rows [])
  transps (Cols x)  = (Rows [(map head x)]) +++ (transps (Cols (map tail x)))
  minor j i (Rows x) = transps c
    where
      a = Rows ((take j x) ++ (drop (j+1) x))
      b = transps a
      ta = \i -> (\(Cols b) -> (take i b))
      dr = \i -> (\(Cols b) -> (drop i b))
      c = Cols ((ta i b) ++ (dr (i+1) b))

instance (Num a,Enum a,Eq a) => Mat (Polymat a) where
  transps (Prows []) = (Pcols [])
  transps (Prows ([]:_)) = (Pcols [])
  transps (Prows x)  = (Pcols [(map head x)]) +++ (transps (Prows (map tail x)))
  minor j i (Prows x) = transps c
    where
      a = Prows ((take j x) ++ (drop (j+1) x))
      b = transps a
      ta = \i -> (\(Pcols b) -> (take i b))
      dr = \i -> (\(Pcols b) -> (drop i b))
      c = Pcols ((ta i b) ++ (dr (i+1) b))

class Num b => Scalar a b | a -> b  where
  det :: a -> b
  (!**) :: Num b => a -> a -> b
  (!***) :: Num b => b -> a -> a

instance (Num a,Enum a,Eq a) => Scalar (Matrix a) a where
  det (Rows [[a,b],[c,d]]) = a*d-b*c
  det (Rows x) = foldr (+) 0 (zipWith (*) (x !! 0) z)
    where
      u = [0..((length x)-1)]
      v = map (minor 0) u
      s = zipWith ($) v [(Rows x) |_<-u]
      z = map det s
  (Rows []) !** (Rows []) = 0
  (Rows (x:xs)) !** (Rows (y:ys)) = first_sum + ((Rows xs) !** (Rows ys))
    where
      first_sum = (foldr (+) 0 (zipWith (*) x y))
  x !*** (Rows rss) = (Rows (map (map (*x)) rss))

conv :: Num a => Poly a -> Poly a -> a
conv (Coef x) (Coef y) = (foldr (+) 0 (zipWith (*) x (reverse y)))

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
