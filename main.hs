import Drzewo
import Narzedzia
import Typy
import Ruchy
import Plansza
import Text.ParserCombinators.Parsec
import System.IO
import System.IO.Unsafe
import Data.Char

plansza = "rnbqkbnr\npppppppp\n........\n........\n........\n........\nPPPPPPPP\nRNBQKBNR"
pSzachownica = zwrocSzachownicaString plansza

-- aby zagraæ
graj = pobierzPoczatkowyRuch

pobierzPoczatkowyRuch = do
	putStrLn("Jakim kolorem chcesz grac?")
	kol <- getLine
	if kol == "w" 
		then graBialymi pSzachownica
		else graCzarnymiInit (ruchKomputera pSzachownica Czarny) -- komputer zaczyna
		
graCzarnymiInit s = do
	putStrLn("Ruch przeciwnika:")
	putStrLn(wyswietlSzachownica s)
	graCzarnymi s

graCzarnymi s = do
	sprawdzCzyPat s
	putStrLn("Podaj ruch w ACN")
	line <- getLine
	putStrLn("Ruch przeciwnika:")
	case parse parseACN "Blad" line of
		Left err -> fail("bledny acn")
		Right acn -> do 
			let ruchK = kompletnyRuch s Czarny acn -- najpierw ruch gracza, potem komp
			putStrLn(wyswietlSzachownica ruchK)
			graCzarnymi ruchK

graBialymi s = do
	sprawdzCzyPat s
	putStrLn("Podaj ruch w ACN")
	line <- getLine
	putStrLn("Ruch przeciwnika:")
	case parse parseACN "Blad" line of
		Left err -> fail("bledny acn")
		Right acn -> do 
			let ruchK = kompletnyRuch s Bialy acn -- najpierw ruch gracza, potem komp
			putStrLn(wyswietlSzachownica ruchK)
			graBialymi ruchK
				
-- kompletny ruch - gracz+komputer
kompletnyRuch p k acn = ruchKomputera (ruchGracza acn p k) k
ruchGracza (ACN(a,b,c,d)) p k = ruszKierunek p k ((ord b)-49, (ord a)-97) ((ord d)-49,(ord c)-97)
ruchKomputera p k = infoDoSzachownica(nextMove(Info p k))

sprawdzCzyPat s
	| sprawdzCzyPatRet s = fail("KONIEC GRY")
	| otherwise = return()

sprawdzCzyPatRet :: Szachownica -> Bool
sprawdzCzyPatRet s = (waliduj s Bialy) == -10000 && (waliduj s Czarny) == 10000
			  
	