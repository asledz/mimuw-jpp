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