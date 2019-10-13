import Data.Bits

data Matrix a = Rows [[a]] | Cols [[a]] deriving (Show,Ord,Eq,Read)

(+++) :: Matrix a -> Matrix a -> Matrix a
(Rows x) +++ (Rows y) = (Rows (x++y))
(Cols x) +++ (Cols y) = (Cols (x++y))

(!+) :: Num a => Matrix a -> Matrix a -> Matrix a
(Rows x) !+ (Rows []) = (Rows x)
(Rows []) !+ (Rows x) = (Rows x)
(Rows (x:xs)) !+ (Rows (y:ys)) = (Rows [zipWith (+) x y]) +++ ((Rows xs) !+ (Rows ys))

(!**) :: Num a => Matrix a -> Matrix a -> a
(Rows []) !** (Rows []) = 0
(Rows (x:xs)) !** (Rows (y:ys)) = (foldr (+) 0 (zipWith (*) x y)) + ((Rows xs) !** (Rows ys))

(!***) :: Num a => a -> Matrix a -> Matrix a
x !*** (Rows rss) = (Rows (map (map (*x)) rss))

r_to_c :: Matrix a -> Matrix a
r_to_c (Rows []) = (Cols [])
r_to_c (Rows ([]:_)) = (Cols [])
r_to_c (Rows x)  = (Cols [(map head x)]) +++ (r_to_c (Rows (map tail x)))

(!*) :: Num a => Matrix a -> Matrix a -> Matrix a
(Rows x) !* (Rows y) = (Rows x) !* (r_to_c (Rows y))
(Rows x) !* (Cols []) = (Rows [])
(Rows []) !* (Cols x) = (Rows [])
(Rows (x:xs)) !* (Cols y) = (Rows [[(foldr (+) 0 (zipWith (*) x z)) | z<-y]]) +++ ((Rows xs) !* (Cols y))

xor :: Matrix a -> Matrix a
xor (Rows [[a,b],[c,d]]) = (Rows [[a,b,c,d],[c,d,a,b],[b,a,d,c],[d,c,b,a]])


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
