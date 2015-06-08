module Drzewo where

import Data.Tree
import Typy

data Info = Info {
    plansza :: Szachownica, poprzedniKolor :: Kolor
} deriving (Eq,Show)

-- Info w wezle
utworzInfo :: Kolor -> Szachownica -> Info
utworzInfo kolor ruch = Info ruch kolor 

ruchyDoInfo :: [Szachownica] -> Kolor -> [Info]
ruchyDoInfo ruchy kolor = map (utworzInfo kolor) ruchy

-- drzewo
generujDrzewo :: Info -> Int -> Tree Info
generujDrzewo pInfo 1 =  Node pInfo [Node (k) [] | k <- ruchyInfo]
   where kolor = zmienKolor (poprzedniKolor pInfo)
         ruchyInfo = ruchyDoInfo (ruszWszystko (plansza pInfo) kolor) kolor
generujDrzewo pInfo poz =  Node pInfo [Node (k) [generujDrzewo k (poz-1)] | k <- ruchyInfo]
    where kolor = zmienKolor (poprzedniKolor pInfo)
          ruchyInfo = ruchyDoInfo (ruszWszystko (plansza pInfo) kolor) kolor

zmienKolor kolor
 | kolor == Bialy = Czarny
 | otherwise = Bialy
