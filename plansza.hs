module Plansza where

import Data.Char
import Data.List.Split
import Narzedzia 
import Typy

poleToString (PoleGry b k) 
	| k == Bialy = toUpper (bierkaToChar b)
	| k == Czarny = toLower (bierkaToChar b)

bierkaToChar Krol = 'K'
bierkaToChar Pion = 'P'
bierkaToChar Skoczek = 'N'
bierkaToChar Goniec = 'B'
bierkaToChar Hetman = 'Q'
bierkaToChar Wieza = 'R'
bierkaToChar Pusta = '.'

charToBierka 'k' = Krol
charToBierka 'p' = Pion
charToBierka 'n' = Skoczek
charToBierka 'b' = Goniec
charToBierka 'q' = Hetman
charToBierka 'r' = Wieza
charToBierka '.' = Pusta

wyswietlKol (SzachownicaKol k) = [poleToString k]

wyswietlRza (SzachownicaRza []) = "\n"
wyswietlRza (SzachownicaRza (r:rs)) = wyswietlKol r ++ (wyswietlRza (SzachownicaRza rs))

wyswietlSzachownica (Szachownica []) = "\n"
wyswietlSzachownica (Szachownica (s:ss)) =  wyswietlRza s ++ (wyswietlSzachownica (Szachownica ss))

instance Show Szachownica where
	show a = wyswietlSzachownica a

plansza = "rnbqkbnr\npppppppp\n........\n........\n........\n........\nPPPPPPPP\nRNBQKBNR"  
-- zrob split, zwroc liste stringow
zwrocRzedy (wejscie) = splitOn "\n" wejscie 

-- wez String (1 rzad) i zamien na PoleGry
przerobRzad :: String -> [PoleGry]
przerobRzad "" = []
przerobRzad (b:bb) 
	| isUpper b = PoleGry (charToBierka b2) Bialy
		:przerobRzad(bb)
	| otherwise = PoleGry (charToBierka b2) Czarny
		:przerobRzad (bb)
	where b2 = toLower b

-- zmapuj kol na rza
przerobNaRza lst = SzachownicaRza $map SzachownicaKol lst 

-- wez liste rzedow i zwroc pola gry
zwrocListeRzedow :: [String] -> [SzachownicaRza]
zwrocListeRzedow [] = []
zwrocListeRzedow (elem:lst) = przerobNaRza (przerobRzad elem)
	:zwrocListeRzedow(lst) 

-- zwroc szachownice ze stringa
zwrocSzachownicaString :: String -> Szachownica
zwrocSzachownicaString str = Szachownica(zwrocListeRzedow(zwrocRzedy(str)))

-- usuwanie bierki
usun :: Szachownica -> (Int, Int) -> Szachownica
usun sz (x, y) = Szachownica (szachownicaUsun sz (x, y))
-- pomocnicza funkcja do usuwania (rozklada szachownice)
szachownicaUsun :: Szachownica -> (Int, Int) -> [SzachownicaRza]
szachownicaUsun (Szachownica []) (_,_) = []
szachownicaUsun (Szachownica (sz:s)) (x, y)
	| (x == 0) = SzachownicaRza (rzadUsun sz y) : (szachownicaUsun (Szachownica s) ((x-1), y))
	| otherwise = sz : (szachownicaUsun (Szachownica s) ((x-1), y))
-- pomocnicza funkcja do usuwania (rozklada rzad)
rzadUsun :: SzachownicaRza -> Int -> [SzachownicaKol]
rzadUsun (SzachownicaRza []) _ = []
rzadUsun (SzachownicaRza (r:rs)) n 
	| (n == 0) = SzachownicaKol (PoleGry Pusta Czarny) : (rzadUsun (SzachownicaRza rs) (n-1))
	| otherwise = r : (rzadUsun (SzachownicaRza rs) (n-1))

-- przeniesienie bierki (xskad, yskad) (xgdzie, ygdzie)
przenies :: Szachownica -> (Int,Int) -> (Int,Int) -> Szachownica
przenies sz (xs, ys) (xg, yg) = usun (przeniesPrzedUsun sz (xs, ys) (xg,yg)) (xs, ys)
-- szachownica bez usunietej bierki
przeniesPrzedUsun :: Szachownica -> (Int, Int) -> (Int, Int) -> Szachownica
przeniesPrzedUsun sz (xs, ys) (xg, yg) = Szachownica (szachownicaPrzenies sz sz (xs, ys) (xg, yg))
-- pomocnicza funkcja do przenoszenia (rozklada szachownice), potrzebne 2 szachownice bo z drugiej bierze jaka bierke wstawic
szachownicaPrzenies :: Szachownica -> Szachownica -> (Int, Int) -> (Int, Int) -> [SzachownicaRza]
szachownicaPrzenies (Szachownica []) _ (_,_) (_,_) = []
szachownicaPrzenies (Szachownica (sz:s)) sz2 (xs, ys) (xg, yg)
	| (xg == 0) = SzachownicaRza (rzadPrzenies sz (sz2) (xs,ys) yg) : (szachownicaPrzenies (Szachownica s) sz2 (xs,ys) ((xg-1), yg))
	| otherwise = sz : (szachownicaPrzenies (Szachownica s) sz2 (xs,ys) ((xg-1), yg))
-- pomocnicza funkcja do przenoszenia (rozklada rzad)
rzadPrzenies :: SzachownicaRza -> Szachownica -> (Int, Int) -> Int -> [SzachownicaKol]
rzadPrzenies (SzachownicaRza []) _ (_,_) _ = []
rzadPrzenies (SzachownicaRza (r:rs)) sz (xs, ys) n 
	| (n == 0) = zwrocBierke sz (xs, ys) : (rzadPrzenies (SzachownicaRza rs) sz (xs,ys) (n-1))
	| otherwise = r : (rzadPrzenies (SzachownicaRza rs) sz (xs, ys) (n-1))

