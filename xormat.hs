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
  (Rows x) !* (Rows y) = (Rows x) !* (r_to_c (Rows y))
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
  (Prows x) !* (Prows y) = (Prows x) !* (r_to_c (Prows y))
  (Prows x) !* (Pcols []) = (Prows [])
  (Prows []) !* (Pcols x) = (Prows [])
  (Prows (x:xs)) !* (Pcols y) = (Prows first_row) +++ ((Prows xs) !* (Pcols y))
    where
      first_row = [[(foldr (!+) (Coef [0]) (zipWith (!*) x z)) | z<-y]]

class Mat a where
  r_to_c ::  a -> a

instance (Num a,Enum a,Eq a) => Mat (Matrix a) where
  r_to_c (Rows []) = (Cols [])
  r_to_c (Rows ([]:_)) = (Cols [])
  r_to_c (Rows x)  = (Cols [(map head x)]) +++ (r_to_c (Rows (map tail x)))

instance (Num a,Enum a,Eq a) => Mat (Polymat a) where
  r_to_c (Prows []) = (Pcols [])
  r_to_c (Prows ([]:_)) = (Pcols [])
  r_to_c (Prows x)  = (Pcols [(map head x)]) +++ (r_to_c (Prows (map tail x)))

(!**) :: Num a => Matrix a -> Matrix a -> a
(Rows []) !** (Rows []) = 0
(Rows (x:xs)) !** (Rows (y:ys)) = (foldr (+) 0 (zipWith (*) x y)) + ((Rows xs) !** (Rows ys))

(!***) :: Num a => a -> Matrix a -> Matrix a
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
