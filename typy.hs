module Typy where

data Kolor = Bialy | Czarny deriving (Eq)
data Bierka = Pusta | Krol | Hetman | Goniec | Skoczek | Wieza | Pion deriving (Eq)
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