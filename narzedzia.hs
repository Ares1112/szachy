module Narzedzia where 
import Typy

-- zwracanie bierki z pozycji (x,y)
zwrocBierke :: Szachownica -> (Int, Int) -> SzachownicaKol
zwrocBierke (Szachownica sz) (x,y) = pomocZwrocBierke (sz!!x) y
pomocZwrocBierke :: SzachownicaRza -> Int -> SzachownicaKol
pomocZwrocBierke (SzachownicaRza sr) y = sr!!y