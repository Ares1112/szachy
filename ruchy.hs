module Ruchy where
import Typy
import Narzedzia
import Plansza

-- ruch hetmana
ruszHetmana :: Szachownica -> Kolor -> (Int, Int) -> [Szachownica]
ruszHetmana sz k (x, y) = odfiltrujPuste(
	merge (merge (merge (ruszLewoRet sz k (x, y)) (ruszPrawoRet sz k (x, y))) (merge (ruszDolRet sz k (x, y)) (ruszGoraRet sz k (x, y)))) (ruszSkosRet sz k (x, y))
	)
	
-- ruch wiezy
ruszWieze :: Szachownica -> Kolor -> (Int, Int) -> [Szachownica]
ruszWieze sz k (x, y) = odfiltrujPuste(
	merge (merge (ruszLewoRet sz k (x, y)) (ruszPrawoRet sz k (x, y))) (merge (ruszDolRet sz k (x, y)) (ruszGoraRet sz k (x, y)))
	)
	
-- ruch gonca
ruszGonca :: Szachownica -> Kolor -> (Int, Int) -> [Szachownica]
ruszGonca sz k (x, y) = odfiltrujPuste(
	ruszSkosRet sz k (x,y)
	)
	
-- ruch skoczka (poprawic bo za duzo kodu)
ruszSkoczka :: Szachownica -> Kolor -> (Int, Int) -> [Szachownica]
ruszSkoczka sz k (x, y) = odfiltrujPuste(
	ruszKierunek sz k (x, y) (x+1, y-2) : ruszKierunek sz k (x, y) (x+2, y-1) : 
	ruszKierunek sz k (x, y) (x+1, y+2) : ruszKierunek sz k (x, y) (x+2, y+1) : 
	ruszKierunek sz k (x, y) (x-1, y-2) : ruszKierunek sz k (x, y) (x-2, y-1) : 
	ruszKierunek sz k (x, y) (x-1, y+2) : ruszKierunek sz k (x, y) (x-2, y+1) : []
	)
	
-- ruch piona (jeśli możliwe zbicie to zwraca tylko to) do zrobienia zamiana na hetmana
ruszPiona :: Szachownica -> Kolor -> (Int, Int) -> [Szachownica]
ruszPiona sz k (x, y)
	| k == Bialy = ruszPionaBialego sz (x, y)
	| k == Czarny = ruszPionaCzarnego sz (x, y)
	| otherwise = [Szachownica []]
ruszPionaBialego :: Szachownica -> (Int, Int) -> [Szachownica]
ruszPionaBialego sz (x, y)
	| not (sprawdzCzyPlansza (x-1, y)) = [Szachownica []]
	| sprawdzCzyZajete (zwrocBierke sz (x-1, y)) Bialy = [Szachownica []]
	| (y+1 < 7) && sprawdzCzyZbil (zwrocBierke sz (x-1, y+1)) Bialy = [(przenies sz (x, y) (x-1, y+1))]
	| (y-1 > 0) && sprawdzCzyZbil (zwrocBierke sz (x-1, y-1)) Bialy = [(przenies sz (x, y) (x-1, y-1))]
	| not (sprawdzCzyZajete (zwrocBierke sz (x-2, y)) Bialy) && (x == 6) = (przenies sz (x, y) (x-1, y)) : (przenies sz (x, y) (x-2, y)) : []
	| otherwise = [(przenies sz (x, y) (x-1, y))]
ruszPionaCzarnego :: Szachownica -> (Int, Int) -> [Szachownica]
ruszPionaCzarnego sz (x, y)
	| not (sprawdzCzyPlansza (x+1, y)) = [Szachownica []]
	| sprawdzCzyZajete (zwrocBierke sz (x+1, y)) Czarny = [Szachownica []]
	| sprawdzCzyZbil (zwrocBierke sz (x+1, y+1)) Czarny && y+1 < 7 = [(przenies sz (x, y) (x+1, y+1))]
	| sprawdzCzyZbil (zwrocBierke sz (x+1, y-1)) Czarny && y-1 > 0 = [(przenies sz (x, y) (x+1, y-1))]
	| not (sprawdzCzyZajete (zwrocBierke sz (x+2, y)) Czarny) && (x == 1) = (przenies sz (x, y) (x+1, y)) : (przenies sz (x, y) (x+2, y)) : []
	| otherwise = [(przenies sz (x, y) (x+1, y))]
	
-- ruch króla, dorobić obsługę szacha
ruszKrola :: Szachownica -> Kolor -> (Int, Int) -> [Szachownica]
ruszKrola sz k (x, y) = odfiltrujPuste(
	ruszKrolaKierunek sz k (x, y)(x+1, y) : ruszKrolaKierunek sz k (x, y) (x-1, y) : 
	ruszKrolaKierunek sz k (x, y) (x, y-1) : ruszKrolaKierunek sz k (x, y) (x, y+1) :
	ruszKrolaKierunek sz k (x, y) (x-1, y-1) : ruszKrolaKierunek sz k (x, y) (x+1, y-1) :
	ruszKrolaKierunek sz k (x, y) (x+1, y+1) : ruszKrolaKierunek sz k (x, y) (x-1, y+1) : []
	)
ruszKrolaKierunek :: Szachownica -> Kolor -> (Int, Int) -> (Int, Int) -> Szachownica
ruszKrolaKierunek sz k (xo, yo) (x, y)
	| not (sprawdzCzyPlansza (x, y)) = Szachownica []
	| sprawdzCzyZajete (zwrocBierke sz (x, y)) k = Szachownica []
	| sprawdzCzyZbil (zwrocBierke sz (x, y)) k = (przenies sz (xo, yo) (x, y))
	| (zwrocBierke sz (x, y) == SzachownicaKol (PoleGry Krol Czarny)) || (zwrocBierke sz (x, y) == SzachownicaKol(PoleGry Krol Bialy)) =  Szachownica []
	| otherwise = (przenies sz (xo, yo) (x, y))

-- ruch bierki w dol
ruszDolRet :: Szachownica -> Kolor -> (Int, Int) -> [Szachownica]
ruszDolRet _ _ (7, _) = []
ruszDolRet sz k (x, y)
	| sprawdzCzyZajete (zwrocBierke sz (x+1, y)) k = [Szachownica []]
	| sprawdzCzyZbil (zwrocBierke sz (x+1, y)) k = [(przenies sz (x, y) (x+1, y))]
	| otherwise = (przenies sz (x, y) (x+1, y)) : ruszDolRet (przenies sz (x, y) (x+1, y)) k (x+1, y)
	
-- ruch bierki w gore
ruszGoraRet :: Szachownica -> Kolor -> (Int, Int) -> [Szachownica]
ruszGoraRet _ _ (0, _) = []
ruszGoraRet sz k (x, y)
	| sprawdzCzyZajete (zwrocBierke sz (x-1, y)) k = [Szachownica []]
	| sprawdzCzyZbil (zwrocBierke sz (x-1, y)) k = [(przenies sz (x, y) (x-1, y))]
	| otherwise = (przenies sz (x, y) (x-1, y)) : ruszGoraRet (przenies sz (x, y) (x-1, y)) k (x-1, y)
	
-- ruch bierki w lewo
ruszLewoRet :: Szachownica -> Kolor -> (Int, Int) -> [Szachownica]
ruszLewoRet _ _ (_, 0) = []
ruszLewoRet sz k (x, y)
	| sprawdzCzyZajete (zwrocBierke sz (x, y-1)) k = [Szachownica []]
	| sprawdzCzyZbil (zwrocBierke sz (x, y-1)) k = [(przenies sz (x, y) (x, y-1))]
	| otherwise = (przenies sz (x, y) (x, y-1)) : ruszLewoRet (przenies sz (x, y) (x, y-1)) k (x, y-1)

-- ruch bierki w prawo
ruszPrawoRet :: Szachownica -> Kolor -> (Int, Int) -> [Szachownica]
ruszPrawoRet _ _ (_, 7) = []
ruszPrawoRet sz k (x, y)
	| sprawdzCzyZajete (zwrocBierke sz (x, y+1)) k = [Szachownica []]
	| sprawdzCzyZbil (zwrocBierke sz (x, y+1)) k = [(przenies sz (x, y) (x, y+1))]
	| otherwise = (przenies sz (x, y) (x, y+1)) : ruszPrawoRet (przenies sz (x, y) (x, y+1)) k (x, y+1)

-- ruch bierki w lewygorny skos
ruszLGSRet :: Szachownica -> Kolor -> (Int, Int) -> [Szachownica]
ruszLGSRet _ _ (0, 0) = []
ruszLGSRet sz k (x, y)
	| not (sprawdzCzyPlansza (x-1, y-1)) = [Szachownica []]
	| sprawdzCzyZajete (zwrocBierke sz (x-1, y-1)) k = [Szachownica []]
	| sprawdzCzyZbil (zwrocBierke sz (x-1, y-1)) k = [(przenies sz (x, y) (x-1, y-1))]
	| otherwise = (przenies sz (x, y) (x-1, y-1)) : ruszLGSRet (przenies sz (x, y) (x-1, y-1)) k (x-1, y-1)

-- ruch bierki w prawygorny skos
ruszPGSRet :: Szachownica -> Kolor -> (Int, Int) -> [Szachownica]
ruszPGSRet _ _ (0, 7) = []
ruszPGSRet sz k (x, y)
	| not (sprawdzCzyPlansza (x-1, y+1)) = [Szachownica []]
	| sprawdzCzyZajete (zwrocBierke sz (x-1, y+1)) k = [Szachownica []]
	| sprawdzCzyZbil (zwrocBierke sz (x-1, y+1)) k = [(przenies sz (x, y) (x-1, y+1))]
	| otherwise = (przenies sz (x, y) (x-1, y+1)) : ruszPGSRet (przenies sz (x, y) (x-1, y+1)) k (x-1, y+1)

-- ruch bierki w prawydolny skos
ruszPDSRet :: Szachownica -> Kolor -> (Int, Int) -> [Szachownica]
ruszPDSRet _ _ (7, 7) = []
ruszPDSRet sz k (x, y)
	| not (sprawdzCzyPlansza (x+1, y+1)) = [Szachownica []]
	| sprawdzCzyZajete (zwrocBierke sz (x+1, y+1)) k = [Szachownica []]
	| sprawdzCzyZbil (zwrocBierke sz (x+1, y+1)) k = [(przenies sz (x, y) (x+1, y+1))]
	| otherwise = (przenies sz (x, y) (x+1, y+1)) : ruszPDSRet (przenies sz (x, y) (x+1, y+1)) k (x+1, y+1)
	
-- ruch bierki w lewydolny skos
ruszLDSRet :: Szachownica -> Kolor -> (Int, Int) -> [Szachownica]
ruszLDSRet _ _ (7, 0) = []
ruszLDSRet sz k (x, y)
	| not (sprawdzCzyPlansza (x+1, y-1)) = [Szachownica []]
	| sprawdzCzyZajete (zwrocBierke sz (x+1, y-1)) k = [Szachownica []]
	| sprawdzCzyZbil (zwrocBierke sz (x+1, y-1)) k = [(przenies sz (x, y) (x+1, y-1))]
	| otherwise = (przenies sz (x, y) (x+1, y-1)) : ruszLDSRet (przenies sz (x, y) (x+1, y-1)) k (x+1, y-1)
	
-- ruch bierki w skosy
ruszSkosRet :: Szachownica -> Kolor -> (Int, Int) -> [Szachownica]
ruszSkosRet sz k (x, y) = merge (merge (ruszLDSRet sz k (x, y)) (ruszLGSRet sz k (x, y))) (merge (ruszPGSRet sz k (x, y)) (ruszPDSRet sz k (x, y)))

-- ruch bierki w konkretne miejsce
ruszKierunek :: Szachownica -> Kolor -> (Int, Int) -> (Int, Int) -> Szachownica
ruszKierunek sz k (xo, yo) (x, y)
	| not (sprawdzCzyPlansza (x, y)) = Szachownica []
	| sprawdzCzyZajete (zwrocBierke sz (x, y)) k = Szachownica []
	| sprawdzCzyZbil (zwrocBierke sz (x, y)) k = (przenies sz (xo, yo) (x, y))
	| (zwrocBierke sz (x, y) == SzachownicaKol (PoleGry Krol Czarny)) || (zwrocBierke sz (x, y) == SzachownicaKol(PoleGry Krol Bialy)) =  Szachownica []
	| otherwise = (przenies sz (xo, yo) (x, y))