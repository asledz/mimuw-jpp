module BSTTrees (
    Tree (Empty, Node),
    toList,
    insert,
    member,
    fromList,
    bstSort
) where

data Tree a = Empty 
    | Node a (Tree a) (Tree a) -- deriving (Eq, Show)

instance Functor Tree where 
    fmap f Empty = Empty
    fmap f (Node a tl tr) = Node (f a) (fmap f tl) (fmap f tr)


instance Show a => Show (Tree a) where
   show Empty = "[.]"
   show (Node a t1 t2) = "[" ++ (show t1) ++ "]" ++ (show a) ++ "[" ++ (show t2) ++ "]"
-- TODO implement it better way

instance Eq a => Eq (Tree a) where
   Empty == Empty = True 
   Empty == _ = False
   _ == Empty = False
   (Node a t1 t2) == (Node b t3 t4) = a == b && t1 == t3 && t2 == t4

toList :: Tree a -> [a]

prependTree Empty l = l
prependTree (Node a tl tr) l = 
    let l1 = prependTree tr l in
    let l2 = a : l1 in
    let l3 = prependTree tl l2 in
    l3

toList t = prependTree t []

insert :: (Ord a) => a -> Tree a -> Tree a

insert a Empty = Node a Empty Empty
insert a (Node b tl tr) = 
    if (a <= b) then
        let tl1 = insert a tl in
            (Node b tl1 tr)
    else
        let tr1 = insert a tr in
            (Node b tl tr1)


-- member :: Ord a -> a -> Tree a -> Bool

member a Empty = False 
member a (Node b tl tr)
    | a < b = member a tl
    | a == b = True
    | a > b = member a tr


fromList l = foldl (\ t a -> insert a t) Empty l


bstSort l = toList $ fromList l
