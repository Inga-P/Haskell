module Dictionary where
import Data.List as List

-- Funkcijām argumenti var būt jebkura datu tipa, kuri pieder klasei 'Eq', 
-- tas nozīmē, ka šiem datu tipiem ir definēta salīdzināšana uz vienādību un nevienādību.

-- A

-- Funkcijai 'aa' tiek padots tulkojamais saraksts un vārdnīca, tā atgriež sarakstu ar iztulkotajām vērtībām
aa :: (Eq a) => [a] -> [(a, a)] -> [a]
aa [] (h:t) = []
aa (h:t) [] = []
aa [] [] = []

-- Tulkojamais saraksts tiek rekursīvi apstaigāts izmantojot funkciju 'aa',
-- kas tiek izsaukta saraksta astei līdz aste ir tukša.
-- Katram tulkojamam elementam tiek izsaukta funkcija 'translation', 
-- kura rekursīvi apstaigā vārdnīcu.
-- Funkciju atgrieztie saraksti tiek kontaktenēti izmantojot '++' operatoru,
-- kas pievieno otro masīvu pirmā masīva beigās.
-- No iegūtā saraksta tiek dzēsti dublikāti izmantojot funkciju 'dublicates'.
aa (h1:t1) (h2:t2) = dublicates(translation h1 (h2:t2) ++ aa t1 (h2:t2))

-- Funkcijai 'translation' tiek padots elements (galva) no tulkojamā saraksta un vārdnīca, 
-- tā atgriež sarakstu ar elementa tulkojumiem
translation :: (Eq a) => a -> [(a, a)] -> [a]
translation h1 [] = []

-- Vārdnīca tiek rekursīvi apstaigāta un 
-- padotais elements tiek salīdzināts ar katra vārdnīcas pāra pirmo elementu.
-- Ja tie ir vienādi, tad atgriežajamam sarakstam tiek pievienots pāra otrais elements,
-- un turpina pārējo tulkojumu meklēšanu, izsaucot šo pašu funkciju 'translation',
-- tai padodot elementu un vārdnīcas asti.
-- Ja tie nav vienādi, arī turpina tulkojumu meklēšanu.
translation h1 (h2:t2) = 
    if h1 == fst h2
        then [snd h2] ++ translation h1 t2
        else translation h1 t2

-- Funkcijai 'dublicates' tiek padots saraksts un tā atgriež sarakstu bez dublikātiem
dublicates :: (Eq a) => [a] -> [a]
dublicates [] = []

-- Funkcija 'dublicates' rekursīvi iet cauri sarakstam
-- un salīdzina vai saraksta galva nav vienāda ar kādu elementu no astes.
-- Ja galva nav vienāda ar kādu elementu no astes, tā tiek pievienota atgriežamajam sarakstam.
-- Abos gadījumos turpina dublikātu izslēgšanu izsaucot šo pašu funkciju saraksta astei.
dublicates (h:t) = 
    if any (== h) t 
        then dublicates t
        else [h] ++ dublicates t

-- Funkcijas 'aa' 1. testa piemērs.
-- Tiek padoti Integer tipa dati.
-- Tiek pārbaudīti šādi gadījumi:
-- Ja kādam elementam nav tulkojums (2 nav atbilstoša pāra vārdnīcā (2, x));
-- Ja kādam vārdnīcas pārim nav atbilstošs elements ((8, 80) - tulkojamā sarakstā nav 8);
-- Ja kādam elementam ir divi tulkojumi (3 atbilst (3, 30) un (3, 31));
-- Ja iztulkotajā sarakstā rodas dublikāti, tie tiek dzēsti ( 5 un 6 abi tulkojas uz 50 - (5, 50) un (6, 50)).
aa1 :: [Int]
aa1 = aa [1, 2, 3, 4, 5, 6, 7] [(1, 10), (3, 30), (4, 40), (5, 50), (6, 50), (7, 70), (8, 80), (3, 31)]

-- Funkcijas 'aa' 2. testa piemērs.
-- Tiek padoti String tipa dati.
-- Tiek pārbaudīti līdzīgi gadījumi kā 1. testa piemērā.
aa2 :: [String]
aa2 = aa ["Es", "tagad", "eju", "uz", "skolu"] [("Es", "I"), ("Es", "am"), ("tagadne", "present"), ("eju", "going"), ("uz", "to"), ("skolu", "school")]

-- B

-- Funkcijai 'bb' tiek padoti divi saraksti - vārdnīcas, tā atgriež sarakstu - abu vārdnīcu kompozīciju.
-- Funkcija 'bb' ir realizēta ļoti līdzīgi funkcijai 'aa', ar atšķirību, ka pirmais parametrs ir saraksts ar pāriem.
bb :: (Eq a) => [(a, a)] -> [(a, a)] -> [(a, a)]
bb [] (h2:t2) = []

-- Pirmā vārdnīca tiek rekursīvi apstaigāta izmantojot funkciju 'bb'.
-- Katram pirmās vārdnīcas elementam tiek izsaukta funkcija 'translation2', 
-- kura rekursīvi apstaigā otro vārdnīcu.
-- Funkciju atgrieztie saraksti tiek kontaktenēti izmantojot '++' operatoru.
-- No iegūtā saraksta tiek dzēsti dublikāti izmantojot funkciju 'dublicates2'.
bb (h1:t1) (h2:t2) = dublicates2 (translation2 h1 (h2:t2) ++ bb t1 (h2:t2))

-- Funkcijai 'translation2' tiek padots elements (galva) no pirmās vārdnīcas un otrā vārdnīca, 
-- tā atgriež elementa un otrās vārdnīcas kompozīciju.
translation2 :: (Eq a) => (a, a) -> [(a, a)] -> [(a, a)]
translation2 h1 [] = []

-- Otrā vārdnīca tiek rekursīvi apstaigāta un 
-- padotais elements tiek salīdzināts ar otrās vārdnīcas katru elementu.
-- Ja tie ir vienādi ((x, y), (y, z), y==y), tad atgriežajamam sarakstam tiek pievienots pāris ((x, y), (y, z) => (x, z))
translation2 h1 (h2:t2) =
    if snd h1 == fst h2
        then [(fst h1, snd h2)] ++ translation2 h1 t2
        else translation2 h1 t2

-- Funkcijai 'dublicates2' tiek padota vārnīca un tā atgriež vārdnīcu bez dublikātiem
dublicates2 :: (Eq a) => [(a, a)] -> [(a, a)]
dublicates2 [] = []

-- Funkcija 'dublicates2' rekursīvi iet cauri sarakstam
-- un salīdzina vai saraksta galva nav vienāda ar kādu elementu no astes.
-- Ja galva nav vienāda ar kādu elementu no astes, tā tiek pievienota atgriežamajam sarakstam.
dublicates2 (h:t) = 
    if any (== h) t 
        then dublicates2 t
        else [h] ++ dublicates2 t

-- Funkcijas 'bb' 1. testa piemērs.
-- Tiek padoti Integer tipa dati.
-- Tiek pārbaudīti šādi gadījumi:
-- Ja kādam 1. vārdnīcas pārim nav atbiltošs 2. vārdnīcas pāris ((2, 20) nav atbilstoša pāra 2. vārdnīcā (20, x));
-- Ja kādam 2. vārdnīcas pārim nav atbiltošs 1. vārdnīcas pāris ((80, 800) nav atbilstoša pāra 1. vārdnīcā);
-- Ja kādam 1. vārdnīcas pārim ir atbilstoši vairāki 2. vārdnīcas pāri ((3, 30) atbilst (30, 300) un (30, 310));
-- Ja kādam 2. vārdnīcas pārim ir atbilstoši vairāki 1. vārdnīcas pāri ((10, 100) atbilst (1, 10) un (8, 10));
-- Ja iztulkotajā sarakstā rodas dublikāti, tie tiek dzēsti ( 1. vārdnīcas (5, 50), (5, 60) un 2. vārdnīcas (50, 500), (60, 500), abos gadījumos sanāk (5, 500)).
bb1 :: [(Int, Int)]
bb1 = bb [(1, 10), (2, 20), (3, 30), (4, 40), (5, 50), (5, 60), (7, 70), (8, 10)] [(10, 100), (30, 300), (40, 400), (50, 500), (60, 500), (70, 700), (80, 800), (30, 310)]

-- Funkcijas 'bb' 2. testa piemērs.
-- Tiek padoti String tipa dati.
-- Tiek pārbaudīti līdzīgi gadījumi kā 1. testa piemērā.
bb2 :: [(String, String)]
bb2 = bb [("viens", "one"), ("vieninieks", "one"), ("divi", "two"), ("cetri", "four"), ("cetri", "fourr"), ("pieci", "five"), ("sesi", "six"), ("septini", "seven")] [("one", "uno"), ("two", "dos"), ("two", "due"), ("three", "tres"), ("four", "cuatro"), ("fourr", "cuatro"), ("six", "seis"), ("seven", "siete")]

-- C

-- Funkcijai 'kk' tiek padota vārdnīca, 
-- tā atgriež sarakstu ar pāriem (K, A<K>) visām K vērtībām no 1 līdz P(A), kas ir mazākais A pilnības skaitlis.
kk :: (Ord a) => [(a, a)] -> [(Int, [(a, a)])]
kk [] = []

-- Funkcija 'kk' izsauc nākamo funkciju - 'iterate1'.
kk a = iterate1 1 a a a

-- Funkcijai 'iterate1' tiek padots skaitītājs, saraksts, kuram iterē cauri, 
-- saraksts, kurš iegūts no iepriekšējās iterācijas
-- un saraksts kāds tas bija sākumā - bez izmaiņām.
iterate1 :: (Ord a) => Int -> [(a, a)] -> [(a, a)] -> [(a, a)] -> [(Int, [(a, a)])]
iterate1 a [] (h1:t1) (h2:t2)= []

-- Funkcija 'iterate1' veido sarakstu ar pāriem (K, A<K>),
-- kur K ir skaitītājs un A<K> ir saraksts bez dublikātiem,
-- kurš iegūts iepriekšējā iterācijā apvienojot A<K> un (A<K> o A<1>) (izmantojot funkciju 'bb').
-- Funkcija iterē no 1 līdz P(A), kur A ir mazākais pilnības skaitlis, jeb skaitlis,
-- kad K vērtības saraksts ir vienāds ar K+1 sarakstu.
-- Lai sarakstus varētu salīdzināt, vispirms tie tiek sakārtoti.
iterate1 a (h1:t1) (h2:t2) (h3:t3) = if (sort (h2:t2)) == (sort (dublicates2((h2:t2) ++ bb (h2:t2) (h3:t3))))
    then [(a, (h2:t2))]
    else [(a, (h2:t2))] ++ (iterate1 (a+1) t1 (dublicates2 ((h2:t2) ++ bb (h2:t2) (h3:t3))) (h3:t3))

-- Funkcijas 'kk' 1. testa piemērs.
-- Tiek padoti Integer tipa dati.
kk1 :: [(Int, [(Int, Int)])]
kk1 = kk [(1, 2), (2, 3), (3, 4), (4, 5), (5, 1)]

-- Funkcijas 'kk' 2. testa piemērs.
-- Tiek padoti String tipa dati.
kk2 :: [(Int, [(String, String)])]
kk2 = kk [("viens", "divi"), ("divi", "tris"), ("tris", "cetri"), ("cetri", "pieci"), ("pieci", "viens")]