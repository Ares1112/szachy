import Data.Char
import Data.List.Split

data Kolor = Bialy | Czarny deriving (Eq)
data Bierka = Pusta | Krol | Hetman | Goniec | Skoczek | Wieza | Pion
data PoleGry = Empty | PoleGry Bierka Kolor
data SzachownicaKol = SzachownicaKol PoleGry
data SzachownicaRza = SzachownicaRza [SzachownicaKol]
data Szachownica = Szachownica [SzachownicaRza]

poleToString (Empty) = '.'

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

-- zmapuj rzazd na kol
przerobNaRza lst = SzachownicaRza $map SzachownicaKol lst  

-- wez liste rzedow i zwroc pola gry
zwrocListeRzedow :: [String] -> [SzachownicaRza]
zwrocListeRzedow [] = []
zwrocListeRzedow (elem:lst) = przerobNaRza (przerobRzad elem)
	:zwrocListeRzedow(lst) 

-- zwroc szachownice
zwrocSzach :: [SzachownicaRza] -> Szachownica
zwrocSzach lsta = Szachownica(lsta)

-- usuwanie bierki
usun :: Szachownica -> (Int, Int) -> Szachownica
usun (Szachownica sz) (x,y) = (SzachownicaRza (sz!!x))!!y

-- przeniesienie bierki
{-przenies :: Szachownica -> (Int,Int) -> Szachownica
przenies Szachownica sz (x,y) =  -}
