module Narzedzia where 
import Typy

-- zwracanie bierki (szachownicaKol) z pozycji (x,y)
zwrocBierke :: Szachownica -> (Int, Int) -> SzachownicaKol
zwrocBierke (Szachownica sz) (x,y) = pomocZwrocBierke (sz!!x) y
pomocZwrocBierke :: SzachownicaRza -> Int -> SzachownicaKol
pomocZwrocBierke (SzachownicaRza sr) y = sr!!y

-- pobranie koloru z PoleGry
pobierzKolor :: PoleGry -> Kolor
pobierzKolor (PoleGry _ k) = k 

-- sprawdzanie czy ruch jest mozliwy 
sprawdzCzyZajete :: SzachownicaKol -> Kolor -> Bool
sprawdzCzyZajete (SzachownicaKol p) k = ((p == PoleGry Krol Bialy) || (p == PoleGry Krol Czarny) || (pobierzKolor(p) == k)) && not (p == PoleGry Pusta Czarny)
sprawdzCzyZbil :: SzachownicaKol -> Kolor -> Bool
sprawdzCzyZbil (SzachownicaKol sk) k = (not (pobierzKolor(sk) == k)) && not (sk == PoleGry Pusta Czarny)
sprawdzCzyPlansza :: (Int, Int) -> Bool
sprawdzCzyPlansza (x, y) = (x < 8 && x > -1 && y < 8 && y > -1)

-- zlacz 2 listy
merge [] ys = ys
merge (x:xs) ys = x:merge ys xs

odfiltrujPuste lst = filter(\x -> x/=Szachownica []) lst