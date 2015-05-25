import Data.Char
import Data.List.Split

data Kolor = Bialy | Czarny deriving (Eq)
data Bierka = Krol | Hetman | Goniec | Skoczek | Wieza | Pion
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

charToBierka 'k' = Krol
charToBierka 'p' = Pion
charToBierka 'n' = Skoczek
charToBierka 'b' = Goniec
charToBierka 'q' = Hetman
charToBierka 'r' = Wieza

wyswietlKol (SzachownicaKol k) = [poleToString k]

wyswietlRza (SzachownicaRza []) = "\n"
wyswietlRza (SzachownicaRza (r:rs)) = wyswietlKol r ++ (wyswietlRza (SzachownicaRza rs))
wyswietlRzedy [] = "\n"
wyswietlRzedy(r:rs) = wyswietlRza r ++ wyswietlRzedy rs

wyswietlSzachownica (Szachownica []) = "\n koniec \n"
wyswietlSzachownica (Szachownica (s:ss)) =  wyswietlRza s ++ (wyswietlSzachownica (Szachownica ss))

plansza = "rnbqkbnr\npppppppp\n........\n........\n........\n........\nPPPPPPPP\nRNBQKBNR"  

zwrocRzedy (wejscie) = splitOn "\n" wejscie 
przerobRzad :: String -> [PoleGry]
przerobRzad "" = []
przerobRzad (b:bb) 
	| isUpper b = PoleGry (charToBierka b) Bialy
		:przerobRzad(bb)
	| isLower b = PoleGry (charToBierka b) Czarny
		:przerobRzad (bb)
przerobNaRza lst = SzachownicaRza $map SzachownicaKol lst  
zwrocListeRzedow :: [String] -> [SzachownicaRza]
zwrocListeRzedow [] = []
zwrocListeRzedow (elem:lst) = przerobNaRza (przerobRzad elem)
	:zwrocListeRzedow(lst) 
charToString :: Char -> String
charToString = (:[])
