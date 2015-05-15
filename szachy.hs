import Data.Char

data Kolor = Bialy | Czarny deriving (Eq)
data Bierka = Krol | Hetman | Goniec | Skoczek | Wieza | Pion
data PoleGry = Empty | PoleGry Bierka Kolor
data Szachownica = Szachownica [[PoleGry]]

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
