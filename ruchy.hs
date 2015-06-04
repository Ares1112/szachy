module Ruchy where
import Typy
import Narzedzia
import Plansza

-- ruch hetmana
ruszHetmana :: Szachownica -> Kolor -> (Int, Int) -> [Szachownica]
ruszHetmana sz k (x, y) = odfiltrujPuste(
	merge (merge (merge (ruszLewoRet sz k (x, y)) (ruszPrawoRet sz k (x, y))) (merge (ruszDolRet sz k (x, y)) (ruszGoraRet sz k (x, y)))) (ruszSkosRet sz k (x, y))
	)

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