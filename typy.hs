module Typy where

data Kolor = Bialy | Czarny deriving (Eq, Show)
data Bierka = Pusta | Krol | Hetman | Goniec | Skoczek | Wieza | Pion deriving (Eq, Show)
data PoleGry = PoleGry Bierka Kolor
instance Eq PoleGry where
	(PoleGry b1 k1) == (PoleGry b2 k2) = (b1==b2) && (k1==k2)
data SzachownicaKol = SzachownicaKol PoleGry
instance Eq SzachownicaKol where
	(SzachownicaKol k1) == (SzachownicaKol k2) = (k1 == k2)
data SzachownicaRza = SzachownicaRza [SzachownicaKol]
instance Eq SzachownicaRza where
	(SzachownicaRza r1) == (SzachownicaRza r2) = (r1 == r2)
data Szachownica = Szachownica [SzachownicaRza]
instance Eq Szachownica where
	(Szachownica r) == (Szachownica r2) = (r == r2)

-- wartosci bierek
bierkaWartosc :: Bierka -> Int
bierkaWartosc Pion = 100
bierkaWartosc Hetman = 1000
bierkaWartosc Skoczek = 350
bierkaWartosc Goniec = 350
bierkaWartosc Wieza = 525
bierkaWartosc Krol = 10000
bierkaWartosc Pusta = 0

-- ACN
data ACN = ACN (Char,Char,Char,Char)

instance Show ACN where
  show (ACN (a,b,c,d)) = a:b:c:d:[]

instance Eq ACN where
    ACN (a,b,c,d) == ACN(a1,b1,c1,d1) = a == a1 && b == b1 && c == c1 && d == d1
