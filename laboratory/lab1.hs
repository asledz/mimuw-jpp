-- TASK 1.

-- a. Napisz przy pomocy map funkcje
-- incAll :: [[Int]] -> [[Int]]
-- która zwiększy o 1 każdy element każdego elementu swojego argumentu, np

-- > incAll $ inits [1..3]
-- [[],[2],[2,3],[2,3,4]]

incAllnaive listOfList =
    map (\ list -> map (+1) list) listOfList

incAll listOfList =
    map (map (+1)) listOfList

incAll' = map (map (+1))

-- b. Napisz przy pomocy foldr
-- silnię/sumę
silnia_long n =  foldr (\ a b -> a * b ) 1 [1..n] 
silnia n =  foldr (*) 1 [1..n] 

-- concat :: [[a]] -> [a]

concat_long listOfLists = foldr (\ a b -> a ++ b ) [] listOfLists
concat' listOfLists = foldr (++) [] listOfLists
concat_foldleft listOfLists = foldl (++) [] listOfLists

-- c. Napisz nub (funkcję eliminującą duplikaty z listy) przy pomocy filter

nub' [] = []
nub' (x:xs) = x:(nub' (filter (\ y -> y /= x) xs))

nub2 [] = []
nub2 (x:xs) = x:(nub2 (filter (/= x) xs))

-- d. Napisz funkcję obliczającą iloczyn skalarny dwóch list liczb; użyj zipWith

scalarP' xs ys = foldl (+) 0 (zipWith (*) xs ys)

scalarP'' xs ys = foldl (+) 0 $ zipWith (*) xs ys

-- TASK 2

-- Napisz funkcję triples :: Int -> [(Int,Int,Int)],
-- która dla argumentu n da listę wszystkich trójek liczb laturalnych o elementach z [1..n]

triples n = [ (x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n] ]

triples_iter n x y z acc = 
    if (x == 0) then
        acc
    else if (y == 0) then
        triples_iter n (x-1) n n acc
    else if (z == 0) then
        triples_iter n x (y-1) n acc
    else 
        triples_iter n x y (z-1) ((x, y, z):acc)

triples2 n = triples_iter n n n n []


-- TASK 3

-- Napisz funkcję triads :: Int -> [(Int,Int,Int)], ktora da liste trojek pitagorejskich 
-- W pierwszym podejsciu dostaniemy np.

-- *Main> triads 20
-- [(3,4,5),(4,3,5),(5,12,13),(6,8,10),(8,6,10),(8,15,17),
--   (9,12,15),(12,5,13),(12,9,15),(12,16,20),(15,8,17),(16,12,20)]

-- Teraz można dodać wymaganie, aby byly one nietrywialne, tzn

-- skoro (3,4,5) juz jest na liscie to (4,3,5) jest "trywialne"
-- skoro (3,4,5) juz jest na liscie to (6,8,10) i (12,16,20) sa "trywialne"
-- *Main> triads 20
-- [(3,4,5),(5,12,13),(8,15,17)]

triads_simple n = filter (\ (x, y, z) -> x*x + y*y == z*z) (triples2 n)

-- TODO: triads 

-- TASK 4

-- a. Napisz funkcję incMaybe :: Maybe Int -> Maybe Int
-- > incMaybe (Just 41)
-- Just 42

-- > incMaybe Nothing
-- Nothing

incMaybe Nothing = Nothing
incMaybe (Just x) = Just (x+1)

-- b. Napisz funkcję addMaybe :: Maybe Int -> Maybe Int -> Maybe Int

-- > addMaybe (Just 2) (Just 3)
-- Just 5
-- > addMaybe Nothing (Just 3)
-- Nothing

addMaybe (Just x) (Just y) = Just (x + y)
addMaybe  _ _ = Nothing


-- Napisz funkcję

-- indexOf :: Char -> String -> Maybe Int
-- taką, że jeśli c wystepuje w s na pozycji n, to (indexOf c s) == Just n, wpp Nothing, np

-- *Main> indexOf 'a' "Ala"
-- Just 2
-- *Main> indexOf 'b' "Ala"
-- Nothing

indexOf' x [] n = Nothing
indexOf' x (y:xs) n = 
    if x == y 
        then (Just n)
    else 
        indexOf' x xs (n+1)

indexOf x xs = indexOf' x xs 0 

-- Napisz funkcje

-- positions :: Char -> String -> [Int]
-- taką, że (positions c s) daje listę wszystkich pozycji, na których c wystepuje w s, np.

-- *Main> positions 'a' "Ala ma kota"
-- [2,5,10]
-- *Main> positions 'b' "Ala ma kota"
-- []


-- positions :: (Eq x) => x -> [x] -> [Int]
positions x xs = positions' x xs 0 []

positions' x [] _ res = reverse res

positions' x (y:xs) n res = 
    if x == y then
        positions' x xs (n+1) (n:res)
    else 
        positions' x xs (n+1) res



-- Fibonacci

fib n = 
    if (n<2) then
        (1,1)
    else
        let (a, b) = fib (n-1) in
            (b, a+b)


fib1 a b n = 
    if (n <= 1) then b
    else fib1 a (a+b) (n-1)

-- PRIME NUMBERS

nats' n = n:(nats' (n+1))
nats = nats' 0

isPrime' k n = 
    if (k >= n) then True
    else
        if (mod n k == 0) then False
        else isPrime' (k+1) n


isPrime n = isPrime' 2 n

primes = filter isPrime nats