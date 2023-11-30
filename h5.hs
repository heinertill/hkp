import GHC.Int (neInt16)
import Data.Ratio (numerator, denominator)

data Color = Red | Blue | Green

data Tree a = Emp | Node a (Tree a) (Tree a)

instance Eq Color where
  (==) Red Red = True
  (==) Blue Blue = True
  (==) Green Green = True
  (==) _ _ = False

instance Show Color where
  show Red = "R"
  show Blue = "B"
  show Green = "G"

instance Eq a => Eq (Tree a) where
  (==) Emp Emp = True
  (==) (Node n1 l1 r1) (Node n2 l2 r2) = n1 == n2 && l1 == l2 && r1 == r2
  (==) _ _ = False

-- Aufgabe 2

data Bruch = Bruch {zaehler :: Integer, nenner :: Integer}

instance Show Bruch where
  show (Bruch z n) = show z ++ "/" ++ show n ++ " has the result " ++ show (fromIntegral z / fromIntegral n)

instance Eq Bruch where
  (==) (Bruch z1 n1) (Bruch z2 n2) = z1 * n2 == z2 * n1

instance Ord Bruch where
  (<=) (Bruch z1 n1) (Bruch z2 n2) = z1 * n2 <= z2 * n1

instance Num Bruch where
    fromInteger x = Bruch x 1
    (+) (Bruch z1 n1) (Bruch z2 n2) = Bruch (z1 * n2 + z2 * n1) (n1 * n2)
    (*) (Bruch z1 n1) (Bruch z2 n2) = Bruch (z1 * z2) (n1 * n2)
    negate (Bruch z n) = Bruch (negate z) n
    abs (Bruch z n) = Bruch (abs z) (abs n)
    signum (Bruch z n)
        | z == 0 = Bruch 0 1
        | z * n > 0 = Bruch 1 1
        | otherwise = Bruch (-1) 1

instance Fractional Bruch where
  fromRational r = Bruch (numerator r) (denominator r)
  (Bruch z1 n1) / (Bruch z2 n2) = Bruch (z1 * n2) (z2 * n1)
  recip (Bruch z n) = Bruch n z 

-- Aufgabe 3

class Eq a => Geo a where
  flaeche :: a -> Double
  umfang :: a -> Double

data Kreis = Kreis {
  radius :: Double
} deriving (Show)

data Rechteck = Rechteck {
  laenge :: Double,
  breite :: Double
} deriving (Show)

instance Eq Kreis where
  (==) (Kreis r1) (Kreis r2) = r1 == r2

instance Eq Rechteck where
  (==) (Rechteck l1 b1) (Rechteck l2 b2) = l1 == l2 && b1 == b2

instance Geo Kreis where
    umfang (Kreis r) = 2 * pi * r
    flaeche (Kreis r) = pi * r * r

instance Geo Rechteck where
    umfang (Rechteck l b) = 2 * (l + b)
    flaeche (Rechteck l b) = l * b