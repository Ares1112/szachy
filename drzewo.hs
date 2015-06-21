module Drzewo where

import Data.Tree
import Typy
import Ruchy
import Narzedzia
import Plansza

import System.IO
import System.IO.Unsafe
import System.Random

import Debug.Trace (traceShow)

data Info = Info {
    plansz :: Szachownica, poprzedniKolor :: Kolor
} deriving (Eq)

instance Ord Info where
    compare (Info a c) (Info b c2) = randomCompare (waliduj a c) (waliduj b c2)

instance Show Info where
        show (Info b c) = wyswietlSzachownica b
-- Info o (w) wezle
utworzInfo :: Kolor -> Szachownica -> Info
utworzInfo kolor ruch = Info ruch kolor 

-- zamien liste szachownic na info
ruchyDoInfo :: [Szachownica] -> Kolor -> [Info]
ruchyDoInfo ruchy kolor = map (utworzInfo kolor) ruchy

-- zamien info na szachownice
infoDoSzachownica :: Info -> Szachownica
infoDoSzachownica (Info s k) = s

-- drzewo rozwiazan
generujDrzewo :: Int -> Info -> Tree Info
generujDrzewo 0 pInfo =  Node pInfo []
generujDrzewo poz pInfo =  Node pInfo (map (generujDrzewo (poz-1)) infoRuchy)
    where k = zmienKolor (poprzedniKolor pInfo)
          infoRuchy = ruchyDoInfo (ruszWszystko (plansz pInfo) k) k 

zmienKolor kolor
	| kolor == Bialy = Czarny
	| otherwise = Bialy
 
 -- minimax
minimax :: Tree Info -> (Int, [Info])
minimax (Node info@(Info b c) []) = (waliduj b c, [info])
minimax (Node info@(Info _ Czarny) xs) = let (v, lst) = maximum (map minimax xs)
    in (v, info : lst)
minimax (Node info@(Info _ Bialy) xs) = let (v, lst) = minimum (map minimax xs)
    in (v, info : lst)

-- wygeneruj nastepny ruch
nextMove :: Info -> Info
nextMove info@(Info b c)
    | finalInfo info = info
    | otherwise = (snd (minimax (generujDrzewo 3 info)))!!1 -- 3 ruchy do przodu

-- czy zbito krola
finalInfo::Info->Bool
finalInfo (Info p k) = (waliduj p Bialy) > -9000 || (waliduj p Czarny) < 9000
		  
randomCompare :: Int -> Int -> Ordering
randomCompare a b
    | a < b = GT
    | a > b = LT
    | a==b = pick [LT,GT]
    | otherwise = EQ
-- wybierz randomowo
pick :: [a] ->  a
pick xs = unsafePerformIO(randomRIO (0, Prelude.length xs - 1) >>= return . (xs !!))
