module Random_matrix_code where

{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

data Matrix a = Rows [[a]] | Cols [[a]] deriving (Show,Ord,Eq,Read)

data Poly a = Coef [a]  deriving (Show,Ord,Eq,Read)

data GF2 = Zero | One deriving (Eq,Read)

data Field2q = Nums (Poly GF2) Int deriving Read

instance Show GF2 where
  show Zero = "0"
  show One = "1"

instance Show Field2q where
  show (Nums (Coef []) i) = ""
  show (Nums (Coef [x]) i) = show x
  show (Nums (Coef [0,1]) i) = "x"
  show (Nums (Coef [1,1]) i) = "1 + x"
  show (Nums (Coef x) i) = if (head (reverse x)) == 0
    then show a
    else if a == (Nums (Coef [0]) i) then "x^" ++ (show ((length x)-1))
    else (show a) ++ " + x^" ++ (show ((length x)-1))
      where
        a = field_reduce (Nums (Coef (reverse (tail (reverse x)))) i)

instance Eq Field2q where
  (Nums x i) == (Nums y j) = if (i == j || i == 1 || j == 1) && a == b
    then True else False
    where
      a = reduce (get_poly u)
      b = reduce (get_poly v)
      u = field_reduce (Nums x i)
      v = field_reduce (Nums y j)
      get_poly = \ (Nums z k) -> z

class Numseq a where
  (+++) :: a -> a ->  a

class Mat a where
  transps ::  a -> a

instance Num GF2 where
  Zero + One = One
  One + Zero = One
  Zero + Zero = Zero
  One + One = Zero
  Zero * One = Zero
  One * Zero = Zero
  Zero * Zero = Zero
  One * One = One
  fromInteger 0 = Zero
  fromInteger 1 = One
  fromInteger x = fromInteger (mod x 2)
  abs x = x
  negate x = x
  signum 0 = 0

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

instance (Eq a, Num a) => Num (Matrix a) where
  (Rows x) + (Rows [[0]]) = (Rows x)
  (Rows [[0]]) + (Rows x) = (Rows x)
  (Rows x) + (Rows []) = (Rows x)
  (Rows []) + (Rows x) = (Rows x)
  (Rows (x:xs)) + (Rows (y:ys)) = (Rows [zipWith (+) x y]) +++ the_rest
    where
      the_rest = ((Rows xs) + (Rows ys))
  (Rows x) * (Rows [[1]]) = (Rows x)
  (Rows [[1]]) * (Rows x) = (Rows x)
  (Rows x) * (Rows [[a]]) = (Rows ((map (map (* a))) x))
  (Rows [[a]]) * (Rows x) = (Rows ((map (map (* a))) x))
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
  (Coef (x:xs)) + (Coef (y:ys)) = reduce ((Coef [x + y]) +++ ((Coef xs) + (Coef ys)))
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
reduce (Coef p) = if (head (reverse p)) == 0
  then reduce (Coef (reverse (tail (reverse p))))
  else Coef p

instance Num Field2q where
  (Nums 0 1) * (Nums y j) = (Nums (Coef [0]) j)
  (Nums 1 1) * (Nums y j) = (Nums y j)
  (Nums x i) * (Nums 0 1) = (Nums (Coef [0]) i)
  (Nums x i) * (Nums 1 1) = (Nums x i)
  (Nums x i) * (Nums y j) = if i == j
    then field_reduce (Nums (x*y) i)
    else (Nums (Coef []) 0)
  (Nums 0 1) + (Nums y j) = (Nums y j)
  (Nums 1 1) + (Nums y j) = (Nums (Coef [1]) j) + (Nums y j)
  (Nums x i) + (Nums 0 1) = (Nums x i)
  (Nums x i) + (Nums 1 1) = (Nums x i) + (Nums (Coef [1]) i)
  (Nums x i) + (Nums y j) = if i == j then (Nums (x+y) i) else (Nums (Coef []) 0)
  fromInteger x = (Nums (Coef [fromInteger x]) 1)
  abs (Nums (Coef x) i) = (Nums (Coef [sum x]) 1)
  negate x = x
  signum x = abs x

field_reduce :: Field2q -> Field2q
field_reduce (Nums (Coef x) i) = if (length x) > i && i > 0
  then field_reduce (Nums (a + b) i)
  else (Nums (Coef x) i)
    where
      a = (Coef (reverse (tail (reverse x))))
      b = (Coef ((replicate ((length x) - i - 1) 0)) +++ (Coef [1,1]))
