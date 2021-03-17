import BSTTrees
import Data.Semigroup
import Data.Monoid
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

-- data Exp 
--   = EInt Int             -- stała całkowita       
--   | EAdd Exp Exp         -- e1 + e2
--   | ESub Exp Exp         -- e1 - e2
--   | EMul Exp Exp         -- e1 * e2
--   | EVar String          -- zmienna
--   | ELet String Exp Exp  -- let var = e1 in e2 
-- a. Napisz instancje Eq oraz Show dla Exp

-- b. Napisz instancje Num dla Exp tak, żeby można było napisać

-- testExp2 :: Exp
-- testExp2 = (2 + 2) * 3
-- (metody abs i signum mogą mieć wartość undefined)

-- c. Napisz funkcję simpl, przekształcającą wyrażenie na równoważne prostsze wyrażenie np.

-- 0x + 1y -> y

-- (nie ma tu precyzyjnej specyfikacji, należy użyć zdrowego rozsądku; uwaga na zapętlenie).

-- d. Zmodyfikuj instancję Num tak aby jej metody wykonywały (niektóre) uproszczenia z poprzedniego punktu (np 0 + x = x)



-- Zadanie 4
-- Napisz funkcje

-- elimMaybe :: c -> (a -> c) -> Maybe a -> c
-- fromMaybe :: a -> Maybe a -> a
-- mapMaybe :: (a -> b) -> Maybe a -> Maybe b
-- maybeHead :: [a] -> Maybe a
-- elimEither :: (a  -> c) -> (b -> c) -> Either a b -> c
-- mapEither :: (a1 -> a2) -> (b1 -> b2) -> Either a1 b1 -> Either a2 b2
-- mapRight ::  (b1 -> b2) -> Either a b1 -> Either a b2
-- fromEither :: Either a a -> a
-- oraz

--     reverseRight :: Either e [a] -> Either e [a]



-- Zadanie 5
-- a. Napisz funkcję

-- readInts :: String -> [Int]
-- która odczyta z napisu występujące w nim liczby naturalne, np

-- *Main> readInts "1 23 456 7.8 abc 9"
-- [1,23,456,9]
-- *Main> readInts "foo"
-- []
-- użyj funkcji isDigit z modulu Data.Char oraz funkcji map, filter, all z Prelude

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

-- instance Functor (Either e) where
--   -- fmap :: (a -> b) -> Either e a -> Either e b
-- b. skonstruuj instancje klasy Functor dla Tree

-- -- class  Functor f  where
-- --    fmap :: (a -> b) -> f a -> f b

-- instance Functor Tree where...

-- *Tree> fmap (+1) $ Node 1 Empty Empty
-- Node 2 Empty Empty
-- c. napisz funkcję

-- reverseRight :: Either e [a] -> Either e [a]
-- z użyciem fmap