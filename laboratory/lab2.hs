import BSTTrees

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

newtype OrderedList a = OL [a] deriving (Eq, Show)

instance Functor OrderedList where
    fmap f (OL l) = OL (map f l)


-- reprezentuje listy uporządkowane niemalejąco

-- a. Uzupełnij instancje

-- instance Functor OrderedList where
-- instance  Ord a => Semigroup (OrderedList a) where
-- instance Ord a => Monoid (OrderedList a) where
-- tak, aby zachowywały niezmiennik uporządkowania.

-- b. Napisz funkcję eliminującą duplikaty (analogicznie do nub)

-- nubOrdered :: Ord a => OrderedList a -> OrderedList a