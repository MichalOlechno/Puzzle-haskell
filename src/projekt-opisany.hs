import Data.Char
import Data.List.Split
import System.Console.ANSI
-- dane testowe

splitText tab=splitOn " " tab

readTable = do
		 content <- readFile "tabela2.txt"
		 return (splitText content)
readWords = do
		 content <- readFile "slowa2.txt"
		 return (splitText content)
--words1="ADORER":"ARROW":"BOUQUET":"BRIDE":"CARDS":"CARESS":"CHOCOLATE":"COUPLE":"CUPID":"DATE":"DATING":"DEVOTION":"EMBRACE":"FIRST KISS":"GROOM":"HEART":"HUGS":"ISEULT":"JULIET":"LOVE":"LUCK":"LYRE":"ODE":"POEM":"PRESENT":"QUEEN":"RENDEZVOUS":"RING":"ROMEO":"ROSES":"RYE":"SCENTS":"SENSE":"SONG":"SWEET":"TRISTAN":"WEDDING":[]
--table=["JULIETCARESS","WEDDINGROMEO","CHOCOLATERGF","BOUQUETTESRI","SWEETSLRUTOR","PEVETUOONEOS","BONNEDVNRAMT","LRESAZOYGRTK","RCIMEILSDRTI","SIEDTQGUAORS","CRNOEUUECWIS","AEVGHNHEEKSD","REEMBRACEETA","DATINGSRSNAT","SCOUPLEOYSNE","CUPIDPRESENT"]

main = do
 table <- readTable
 words1 <- readWords
 setTitle "WYKRESLANKA"
 setSGR[SetColor Foreground Vivid White]
 wypiszWTerminalu (wykresl (trimTable words1) table)
    

wypiszWTerminalu [] = return ()
wypiszWTerminalu (x:xs) = do
 piszWiersz x
 putStr "\n"
 wypiszWTerminalu xs
 
piszWiersz [] =return()
piszWiersz (x:xs) = if (ord x) < 96 then
 do
  setSGR[SetColor Foreground Vivid Red]
  putChar x
  piszWiersz xs
 else do
  setSGR[SetColor Foreground Vivid White]
  putChar (toUpper x)
  piszWiersz xs

trimTable []=[]
trimTable (x:xs) = trimRow x :trimTable xs

trimRow []=[]
trimRow (x:xs) = if x==' ' || x=='-' then trimRow xs
 else x:trimRow xs 










-- funkcja sprawdzająca czy litery są takie same bez względu na wielkość. toUpper i toLower są wbudowanymi funkcjami haskella ( nie nadpisywałem operatora '==' gdyż korzystam z niego takżee przy porównywaniu indeksów
areEqual a b = if a==b || (toUpper a) == b || a==(toUpper b) then True
 else False

-- to jest początek głównej funkcji (trzeba dodać jeszcze 2 warunki na skos i jeden warunek, gdy nie znajdzie danego słowa ( jakiś error, że nierozwiązywalne). 
-- Logikę zaprojektowałem w ten sposób, że używamy tych samych funckji do wykreślania (zawsze wykreślamy w poziomie), tzn. że dla przypadku pionowego: Transponuje każdą kolumnę -> przeliczam tak jak dla przypadku poziomego -> zamieniam litery na małe -> transponuje znowu do pierwotnej postaci ( z wykreślonymi już literami)

-- funkcja "wykreśl" przyjmuje 2 parametry: listę słów do wykreślenia i Macierz wszystkich liter, czyli to z czego wykreślamy.
-- pierwszy IF jest jasny : sprawdzamy czy można wykreślić poziomo funckją 'możnaWykreslic' a jeżeli tak to robimy rekurencje: wywołujemy jeszcze raz funckję wykreśl dla pozostałych elementów do wykreślenia, a jako drugi parametr podajemy ZMODYFIKOWANĄ macierz (z odpowiednimi literami zamienionymi na małe) - funcja "wykreslPoziomo"
-- drugi IF działa tak samo, też sprawdzamy czy można wykreślić PIONOWO, ale podajemy macierz TRANSPONOWANĄ za pomocą funckji "transponuj". funkcja "wykreslPionowoLitery" zwraca ZMODYFIKOWANĄ macierz ( w środku jest wywoływana funckcja "transponuj"
wykresl [] ys = ys
wykresl _ [] =[]
wykresl (x:xs) ys =if moznaWykreslic x ys then wykresl xs (wykreslPoziomo x ys)
 else if moznaWykreslic x (transponuj ys) then wykresl xs (wykreslPionowoLitery x ys)
 else if moznaWykreslic x (zamienNaSkosOdDolu ys) then wykresl xs (wykreslSkosDol x ys)
 else if moznaWykreslic x (zamienNaSkosOdGory ys) then wykresl xs (wykreslSkosGora x ys)
 else ["blad"]

 

--POMOCNICZE
-- sprawdza czy można wykreślić. Jeżeli funckja wykreslPioziomoWiersz	 zwróci poprawny indeks to można wykreślić, jeżli nie to Fasle -> w głównej funckji wywołanie przechodzi do następnej instrukcji IF.
moznaWykreslic _ []=False
moznaWykreslic [] ys = False
moznaWykreslic (x:xs) (y:ys)= if k>(-1) then True 
 else False || moznaWykreslic (x:xs) ys
 where k=(indeksWykreslanegoSlowa (x:xs) y 0 0)

-- POZIOMO
-- (x:xs) słowo "KOT", cała tablica table
-- Jeżeli poprzednia funkcja "moznaWykreslic" zwróciła True, to wywołaj tą funkcję do wykreślenia POZIOMO. Funkcja k=indeksWykreslanegoSlowa zwraca indeks oznaczający początek słowa (w danym wierszu), które ma być wykreślone -> następnie funkcja oznacz poziomo wiersz bierze jako parametry DŁUGOŚĆ SŁOWA, DANY WIERSZ i INDEKS od którego ma zacząć wykreślanie.
wykreslPoziomo _ [] = []
wykreslPoziomo [] ys = ys
wykreslPoziomo (x:xs) (y:ys)= if k>(-1) then (oznaczPoziomoWiersz (length (x:xs)) y k) :ys
 else y : (wykreslPoziomo (x:xs) ys)
 where k=(indeksWykreslanegoSlowa (x:xs) y 0 0)

-- x:xs - pojedyncze słowo "KOT", y:ys -- pojedynczy wiersz "KOTA"
-- sprawdzenie czy da się wykreślić słowo z wiersza. Funckja zwraca indeks od którego trzeba zacząć wykreślanie.
indeksWykreslanegoSlowa [] _ _ _ = -1
indeksWykreslanegoSlowa (xs) (ys) i j = if i==((length xs)) then j
 else if (i+j)==((length ys)) then -1
 else if  areEqual (xs!!i) (ys!!(i+j)) then indeksWykreslanegoSlowa xs ys (i+1) j
 else indeksWykreslanegoSlowa xs ys 0 (j+1) 
 
 -- funkcja faktycznie zamieniająca litery na małe. Przyjmuje długość słowa, dany wiersz i indeks
oznaczPoziomoWiersz _ [] _ =[]
oznaczPoziomoWiersz 0 ys 0 = ys
oznaczPoziomoWiersz i (y:ys) 0 = toLower y :oznaczPoziomoWiersz (i-1) ys 0 
oznaczPoziomoWiersz i (y:ys) j = y : oznaczPoziomoWiersz i ys (j-1)
--POZIOMO






--PIONOWO
-- funkcja zwraca macierz z wykreślonym słowem PIONOWO. jako parametry przyjmuje słowo i całą macierz. Używamy tutaj funkcji "transponuj", która transponuje macierz -> następnie wykonywane jest wykreślanie POZIOMO, przez funckję "wykreslPoziomo" opisaną wyżej -> następnie wykreśloną macierz transponujemy ponownie do pierwotnej postaci
wykreslPionowoLitery x ys = transponuj (wykreslPoziomo x (transponuj ys))

transponuj xs = transponuj' xs 0

transponuj' [] _=[] 
transponuj' xs i = if i<(length (xs!!1)) then (wezKolumneX xs i) : transponuj' xs (i+1) else [] 

wezKolumneX [] _ =[]
wezKolumneX (x:xs) i =x!!i :wezKolumneX xs (i)
--PIONOWO





-- to jakies notatki, mogą być pomocne.
--wykreslSkosGora x ys = odwrocSkosGora (wykreslPoziomo x (wezSkosGora ys 0 0))

--odwrocSkosGora ys=[]

--wezSkosGora [] _ _ =[]
--wezSkosGora ys i j = if i=length

--wezWartosc ys i j = (ys!!j)!!i

--funkcja zwracajaca wszystkie diagonale tablicy tablic (idace od lewego dolu do prawej gory)
zamienNaSkosOdDolu :: [[a]] -> [[a]]
zamienNaSkosOdDolu [] = []
zamienNaSkosOdDolu x = konwertujOdDolu x 0

--pomocnicza funkcja rekurencyjna przyjmuje tablice i index, od ktorego zaczynamy sczytywac diagonale
konwertujOdDolu :: [[a]] -> Int -> [[a]]
konwertujOdDolu x k | k == ((length x) + (length (x !! 0)) -1) = []
                    | otherwise = (zwrocDiagonalOdDolu x k 0):(konwertujOdDolu x (k+1))

--funkcja zwracajaca k-ty (liczac od gory) diagonal macierzy. x to tablica, i to kolumna od ktorej zaczynamy czytac
zwrocDiagonalOdDolu :: [[a]] -> Int -> Int -> [a]
zwrocDiagonalOdDolu _ (-1) _ = []
zwrocDiagonalOdDolu x k i | i == (length (x !! 0)) = []
                          | k >= (length x) = zwrocDiagonalOdDolu x (k-1) (i+1)
                          | otherwise = ((x !! k) !! i):(zwrocDiagonalOdDolu x (k-1) (i+1))


--funkcja zwracajaca wszystkie diagonale tablicy tablic (idace od lewej gory do prawego dolu)
--zamienNaSkosOdGory :: [[a]] -> [[a]]
zamienNaSkosOdGory x = zamienNaSkosOdDolu (reverse x)

--x to macierz diagonali
--k to wymiar docelowej macierzy w pionie
--i to wymiar docelowej macierzy w poziomie

odwrocOdDolu :: [[a]] -> Int -> Int -> [[a]]
odwrocOdDolu [] _ _ = []
odwrocOdDolu x k i = zrobWierszeOdDolu (map reverse x) k i

zrobWierszeOdDolu :: [[a]] -> Int -> Int -> [[a]]
zrobWierszeOdDolu [] _ _ = []
zrobWierszeOdDolu x k i | k == 0 = []
                        | otherwise = (zrobWierszOdDolu x i):(zrobWierszeOdDolu (usunNiepotrzebneOdDolu x (i)) (k-1) i) 

zrobWierszOdDolu :: [[a]] -> Int -> [a]
zrobWierszOdDolu [] _ = []
zrobWierszOdDolu (x:xs) k | k == 0 = []
                          | otherwise = (head x):(zrobWierszOdDolu xs (k-1))

usunNiepotrzebneOdDolu :: [[a]] -> Int -> [[a]]
usunNiepotrzebneOdDolu [] _ = []
usunNiepotrzebneOdDolu x k = tail ((map tail (fst (splitAt k x))) ++ (snd (splitAt k x)))

odwrocOdGory :: [[a]] -> Int -> Int -> [[a]]
odwrocOdGory [] _ _ = []
odwrocOdGory x k i = reverse (odwrocOdDolu x k i)


wykreslSkosDol a x = odwrocOdDolu (wykreslPoziomo a (zamienNaSkosOdDolu x)) (length x) (length  (x!!1))

wykreslSkosGora a x = odwrocOdGory (wykreslPoziomo a (zamienNaSkosOdGory x)) (length x) (length  (x!!1))








