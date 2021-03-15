-- Zadanie 1
-- Rozważmy typ drzew trochę inny niż na wykladzie

-- data Tree a = Empty 
--             | Node a (Tree a) (Tree a) 
-- a. stwórz własne instancje Eq, Show

-- instance Show a => Show (Tree a) where
--    show t = ...

-- instance Eq a => Eq (Tree a) where
--    t1 == t2 = ...
-- b. Napisz funkcję

--     toList :: Tree a -> [a]
-- ktora zamieni drzewo w liste elementow drzewa (w porzadku infiksowym)

-- c. zaimplementuj drzewa BST z funkcjami

--     insert :: (Ord a) => a -> Tree a -> Tree a
--     member :: (Ord a) -> a -> Tree a -> Bool
--     fromList :: (Ord a) => [a] -> Tree a
-- d. Stwórz moduł drzew BST i wykorzystaj go w innym module do sortowania [Int]

-- przy pomocy ghci
-- przy pomocy ghc

data Tree a = Empty 
    | Node a (Tree a) (Tree a) 

instance Show a => Show (Tree a) where
   show t = 