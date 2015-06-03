module Typy where

data Kolor = Bialy | Czarny deriving (Eq)
data Bierka = Pusta | Krol | Hetman | Goniec | Skoczek | Wieza | Pion
data PoleGry = Empty | PoleGry Bierka Kolor
data SzachownicaKol = SzachownicaKol PoleGry
data SzachownicaRza = SzachownicaRza [SzachownicaKol]
data Szachownica = Szachownica [SzachownicaRza]