module Narzedzia where 
import Typy
import Text.Parsec.Char
import Text.ParserCombinators.Parsec

-- zwracanie bierki (szachownicaKol) z pozycji (x,y)
zwrocBierke :: Szachownica -> (Int, Int) -> SzachownicaKol
zwrocBierke (Szachownica sz) (x,y) = pomocZwrocBierke (sz!!x) y
pomocZwrocBierke :: SzachownicaRza -> Int -> SzachownicaKol
pomocZwrocBierke (SzachownicaRza sr) y = sr!!y

-- zwracanie PoleGry z SzachownicaKol
zwrocPoleGry :: SzachownicaKol -> PoleGry
zwrocPoleGry (SzachownicaKol sk) = sk

-- pobranie koloru z PoleGry
pobierzKolor :: PoleGry -> Kolor
pobierzKolor (PoleGry _ k) = k 

-- pobranie bierki z PoleGry
pobierzBierka :: PoleGry -> Bierka
pobierzBierka (PoleGry b _) = b

-- sprawdzanie czy ruch jest mozliwy 
sprawdzCzyZajete :: SzachownicaKol -> Kolor -> Bool
sprawdzCzyZajete (SzachownicaKol p) k = ((p == PoleGry Krol Bialy) || (p == PoleGry Krol Czarny) || (pobierzKolor(p) == k)) && not (p == PoleGry Pusta Czarny)
sprawdzCzyZbil :: SzachownicaKol -> Kolor -> Bool
sprawdzCzyZbil (SzachownicaKol sk) k = (not (pobierzKolor(sk) == k)) && not (sk == PoleGry Pusta Czarny) && not ((sk == PoleGry Krol Bialy) || (sk == PoleGry Krol Czarny))
sprawdzCzyPlansza :: (Int, Int) -> Bool
sprawdzCzyPlansza (x, y) = (x < 8 && x > -1 && y < 8 && y > -1)

-- zlacz 2 listy
merge [] ys = ys
merge (x:xs) ys = x:merge ys xs

-- odfiltruj puste Szachownica w liscie
odfiltrujPuste lst = filter(\x -> x/=Szachownica []) lst

-- ACN, parser
parseLitera :: Parser Char
parseLitera = oneOf "abcdefgh"

parseLiczba :: Parser Char
parseLiczba = oneOf "12345678"

parseACN :: Parser ACN
parseACN = do
          x1 <- parseLitera
          y1 <- parseLiczba
          x2 <- parseLitera
          y2 <- parseLiczba
          return $ ACN (x1,y1,x2,y2)

