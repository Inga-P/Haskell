module Tree where

-- Izveido jaunu datu tipu TTT ar tipa mainīgo 'aa' kokam, 
-- kuram katrai virsotnei ir 0, 1, 2 vai 3 bērni.
data TTT aa = Leaf0 [aa] | Leaf1 [aa] (TTT aa) | Leaf2 [aa] (TTT aa) (TTT aa) | Leaf3 [aa] (TTT aa) (TTT aa) (TTT aa)

-- Izveido jaunu instanci funkcijai 'Show', lai varētu parādīt koku.
instance (Show aa) => Show (TTT aa) where
   show x = prettyprint x ""

-- Palīgfunkcija funkcijai 'prettyprint', 
-- lai izveidotu nepieciešamo atkāpi no malas
padding :: String -> String
padding pad = "\n" ++ pad ++ "|\n" ++ pad ++ "+-"

-- Funkcija, kas rekursīvi apstaigā koku un to attēlo
prettyprint :: (Show aa) => (TTT aa) -> String -> String
prettyprint (Leaf0 val) pad = show val
prettyprint (Leaf1 val x) pad = show val ++ padding pad ++ prettyprint x (pad ++ "|  ")
prettyprint (Leaf2 val x y) pad = show val ++ padding pad ++ prettyprint x (pad ++ "|  ") ++ padding pad ++ prettyprint y (pad ++ "|  ")
prettyprint (Leaf3 val x y z) pad = show val ++ padding pad ++ prettyprint x (pad ++ "|  ") ++ padding pad ++ prettyprint y (pad ++ "|  ") ++ padding pad ++ prettyprint z (pad ++ "|  ")

-- Funkcija, kas kā pirmo argumentu saņem funkciju un otro TTT aa tipa koku,
-- tā atgriež TTT aa tipa koku, kas no sākotnēja koka iegūts pielietojot funkciju f
-- katras virsotnes saraksta elementam. Šī funkcija rekursīvi apstaigā koku, 
-- taču funkcija 'iterate1' rekursīvi apstaigā katras virsotnes sarakstu.
mm :: (aa -> aa) -> TTT aa -> TTT aa
mm f (Leaf0 val) = Leaf0 (iterate1 f val)
mm f (Leaf1 val x) = Leaf1 (iterate1 f val) (mm f x)
mm f (Leaf2 val x y) = Leaf2 (iterate1 f val) (mm f x) (mm f y)
mm f (Leaf3 val x y z) = Leaf3 (iterate1 f val) (mm f x) (mm f y) (mm f z)

-- Funkcija, kas kā pirmo argumentu saņem funkciju un otro sarakstu ar 'aa' tipa vērtībām,
-- tā atgriež sarakstu ar 'aa' tipa vērtībām, kas no sākotnējā saraksta iegūts pielietojot funkciju f
-- katram saraksta elementam.
iterate1 :: (aa -> aa) -> [aa] -> [aa]
iterate1 f [] = []
iterate1 f (h:t) = [f h] ++ iterate1 f t

-- Funkcija, kas kāpina argumentu kvadrātā.
a :: Int -> Int
a value = value * value

-- Funkcija, kas aprēķina argumenta atlikumu dalot ar 7.
b :: Int -> Int
b value = value `mod` 7

-- Funkcija, kas aprēķina argumenta moduli.
c :: Int -> Int
c value = if value < 0 
    then value * (-1)
    else value

-- Funkcija testa koka izveidošanai
testTree :: TTT Int
testTree = Leaf2 [0] (Leaf3 [-1, -2, -3] (Leaf0 [-4]) (Leaf0 [-5, -6]) (Leaf0 [-7, -8, -9])) (Leaf2 [1, 2, 3] (Leaf1 [4] (Leaf1 [10, 11, 12, 13] (Leaf0 [14, 15, 16, 17]))) (Leaf1 [5, 6] (Leaf0 [7, 8, 9])))

-- Funkcija, kas padotajam kokam izsauc funkciju mm, kā parametru padodot funkciju 'a' un koku
ff_a :: TTT Int -> TTT Int
ff_a x = mm a x

-- Funkcija, kas padotajam kokam izsauc funkciju mm, kā parametru padodot funkciju 'b' un koku
ff_b :: TTT Int -> TTT Int
ff_b x = mm b x

-- Funkcija, kas padotajam kokam izsauc funkciju mm, kā parametru padodot funkciju 'c' un koku
ff_c :: TTT Int -> TTT Int
ff_c x = mm c x