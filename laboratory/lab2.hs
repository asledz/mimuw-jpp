import BSTTrees
import Data.Semigroup
import Data.Monoid

import Prelude hiding(Either(..))

import Data.Char (isDigit, ord)
data Either a b = Left a | Right b

-- Solved in module BSTTrees:
-- Zadanie 1
-- Rozważmy typ drzew trochę inny niż na wykladzie
-- data Tree a = Empty 
--             | Node a (Tree a) (Tree a) 
-- a. stwórz własne instancje Eq, Show
-- b. Napisz funkcję
--     toList :: Tree a -> [a]
-- ktora zamieni drzewo w liste elementow drzewa (w porzadku infiksowym)
-- c. zaimplementuj drzewa BST z funkcjami
-- d. Stwórz moduł drzew BST i wykorzystaj go w innym module do sortowania [Int]

-- przy pomocy ghci
-- przy pomocy ghc

-- Zadanie 2
-- Niech typ

-- reprezentuje listy uporządkowane niemalejąco
newtype OrderedList a = OL [a] deriving (Eq, Show)

-- a. Uzupełnij instancje

-- instance Functor OrderedList where
-- instance  Ord a => Semigroup (OrderedList a) where
-- instance Ord a => Monoid (OrderedList a) where
-- tak, aby zachowywały niezmiennik uporządkowania.

instance Functor OrderedList where
    fmap f (OL l) = OL (map f l)

instance (Ord a) => Semigroup (OrderedList a) where
    (OL l1) <> (OL l2) = OL $ bstSort (l1 ++ l2)

instance (Ord a) => Monoid (OrderedList a) where 
    mempty = OL []
    mappend = (<>)

-- b. Napisz funkcję eliminującą duplikaty (analogicznie do nub)

nubOrdered :: (Ord a) => OrderedList a -> OrderedList a

nubOrdered (OL l) = OL (nubAcc Nothing l)


nubAcc :: (Eq a) => Maybe a -> [a] -> [a]
nubAcc Nothing [] = []
nubAcc Nothing (h:t) = nubAcc (Just h) t
nubAcc (Just a) [] = a : []
nubAcc (Just a) (h:t) 
    | a == h = nubAcc (Just a) t
    | a /= h = h:(nubAcc (Just h) t)


-- Zadanie 3
-- Rozważmy typ dla wyrażeń arytmetycznych z let:

data Exp 
  = EInt Int             -- stała całkowita       
  | EAdd Exp Exp         -- e1 + e2
  | ESub Exp Exp         -- e1 - e2
  | EMul Exp Exp         -- e1 * e2
  | EVar String          -- zmienna
  | ELet String Exp Exp  -- let var = e1 in e2 
    deriving Eq
-- a. Napisz instancje Eq oraz Show dla Exp

test = ELet "x" (EInt 2) (EAdd (EVar "x") (EInt 4))

instance Show Exp where
    show (EInt x) = show x
    show (EAdd e1 e2) = "(" ++ (show e1) ++ " + " ++ (show e2) ++ ")"
    show (ESub e1 e2) = "(" ++ (show e1) ++ " - " ++ (show e2) ++ ")"
    show (EMul e1 e2) = "(" ++ (show e1) ++ " * " ++ (show e2) ++ ")"
    show (EVar x) = x
    show (ELet x e1 e2) = "[let " ++ (show (EVar x)) ++ " = " ++ (show e1) ++ " in " ++ (show e2) ++ "]"


-- b. Napisz instancje Num dla Exp tak, żeby można było napisać
-- (metody abs i signum mogą mieć wartość undefined)
-- d. Zmodyfikuj instancję Num tak aby jej metody wykonywały (niektóre) uproszczenia z poprzedniego punktu (np 0 + x = x)

instance Num Exp where
    (+) e1 e2 = simpl $ EAdd e1 e2
    (-) e1 e2 = simpl $ ESub e1 e2
    (*) e1 e2 = simpl $ EMul e1 e2
    abs = undefined
    signum = undefined
    fromInteger n = EInt (fromIntegral n)

testExp2 :: Exp
testExp2 = (2 + 2) * 3
testExp3 :: Exp
testExp3 = ELet ("x") (1 + (EVar "x") * 7) ((EVar "x") * 2 + EVar "y")

-- c. Napisz funkcję simpl, przekształcającą wyrażenie na równoważne prostsze wyrażenie np.
-- 0x + 1y -> y
-- (nie ma tu precyzyjnej specyfikacji, należy użyć zdrowego rozsądku; uwaga na zapętlenie).

is0 :: Exp -> Bool 
is1 :: Exp -> Bool 

is0 (EInt 0) = True 
is0 (EInt _) = False 
is0 (EAdd e1 e2) = (is0 e1) && (is0 e2)
is0 (ESub e1 e2) = ((is0 e1) && (is0 e2)) || e1 == e2
is0 (EMul e1 e2) = (is0 e1) || (is0 e2)
is0 (EVar x) = False 
is0 (ELet x e1 e2) = is0 e2

is1 (EInt 1) = True
is1 (EInt _) = False
is1 (EAdd e1 e2) = ((is0 e1) && (is1 e2)) || ((is1 e1) && (is0 e2))
is1 (ESub e1 e2) = ((is1 e1) && (is0 e2))
is1 (EMul e1 e2) = (is1 e1) && (is1 e2)
is1 (EVar x) = False 
is1 (ELet x e1 e2) = is1 e2


simpl :: Exp -> Exp
simpl (EInt n) = EInt n

simpl (EAdd e1 e2)
    | is0 e1 = e2
    | is0 e2 = e1
    | otherwise = EAdd e1 e2

simpl (ESub e1 e2)
    | is0 e1 = ESub (EInt 0) e2
    | is0 e2 = e1
    | otherwise = ESub e1 e2

simpl (EMul e1 e2)
    | is0 e1 = EInt 0
    | is0 e2 = EInt 0
    | is1 e1 = e2
    | is1 e2 = e1
    | otherwise = EMul e1 e2

simpl (EVar x) = EVar x

simpl (ELet x e1 e2) 
    | is0 e2 = EInt 0
    | is1 e2 = EInt 1
    | otherwise = ELet x e1 e2


testSimpl = simpl (1 * EVar "x" + 0 * EVar "y")


-- Zadanie 4
-- Napisz funkcje

elimMaybe :: c -> (a -> c) -> Maybe a -> c

elimMabe x f Nothing = x
elimMaybe x f (Just a) = f a
 
fromMaybe :: a -> Maybe a -> a

fromMaybe a Nothing = a
fromMaybe _ (Just a) = a

mapMaybe :: (a -> b) -> Maybe a -> Maybe b

mapMaybe f Nothing = Nothing 
mapMaybe f (Just a) = Just (f a)

maybeHead :: [a] -> Maybe a

maybeHead [] = Nothing 
maybeHead (h:_) = Just h

elimEither :: (a  -> c) -> (b -> c) -> Either a b -> c

elimEither f h (Left a) = f a
elimEither f h (Right b) = h b

mapEither :: (a1 -> a2) -> (b1 -> b2) -> Either a1 b1 -> Either a2 b2

mapEither fa fb (Left a) = Left (fa a)
mapEither fa fb (Right b) = Right (fb b)

mapRight ::  (b1 -> b2) -> Either a b1 -> Either a b2

mapRight = mapEither id

fromEither :: Either a a -> a

fromEither = elimEither id id

reverseRight :: Either e [a] -> Either e [a]

reverseRight = mapRight reverse


-- Zadanie 5
-- a. Napisz funkcję
-- która odczyta z napisu występujące w nim liczby naturalne, np
-- *Main> readInts "1 23 456 7.8 abc 9"
-- [1,23,456,9]
-- *Main> readInts "foo"
-- []
-- użyj funkcji isDigit z modulu Data.Char oraz funkcji map, filter, all z Prelude

split input = split' input "" []

split' "" accC accL = 
    reverse( reverse accC : accL)

split' (c:res) accC accL = 
    if c /= ' ' then 
        split' res (c:accC) accL
    else 
        split' res "" (reverse accC:accL)

filterDigits :: [String] -> [String]
filterDigits input = filter (all isDigit) input

parseInt :: String -> Int
parseInt = foldl (\ r d -> 10 * r + ord(d) - ord('0')) 0


readInts :: String -> [Int]
-- readInts string = split string 
readInts input = map parseInt $ filterDigits $ split $ input


-- b. Napisz podobną funkcję readInts2 :: String -> Either String [Int] która da listę liczb, jeśli wszystkie słowa jej argumentu są liczbami a komunikat o błędzie w przeciwnym przypadku

-- Może się przydać funkcja reverseRight (albo mapRight)

--     *Main> readInts2  "1 23 456 foo 9"
--     Left "Not a number: foo"
--     *Main> readInts2  "1 23 456"     
--     Right [1,23,456]
-- c. Napisz funkcję

-- sumInts :: String -> String
-- jesli wszystkie slowa jej argumentu są liczbami da reprezentacje ich sumy
-- wpp komunikat o bledzie
-- stwórz program uruchamiający funkcję sumInts przy pomocy interact.

-- Zadanie 6
-- a.Uzupełnij instancje klasy Functor dla Either e:

-- import Prelude hiding(Either(..))
-- data Either a b = Left a | Right b

instance Functor (Either e) where
    fmap = mapRight

-- b. skonstruuj instancje klasy Functor dla Tree
-- Done in other task already


-- instance Functor Tree where...

-- *Tree> fmap (+1) $ Node 1 Empty Empty
-- Node 2 Empty Empty
-- c. napisz funkcję

-- reverseRight :: Either e [a] -> Either e [a]
-- z użyciem fmap