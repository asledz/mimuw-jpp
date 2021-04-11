-- Zadanie 1
-- W poprzednim tygodniu pisaliśmy funkcje

-- readInts2 :: String -> Either String [Int]
-- sumInts :: String -> String
-- Zamiast case ... of {Left -> ...; Right -> ...} użyj w nich operacji monadycznych lub do. Można też użyć readMaybe lub readEither.

-- Prelude> import Text.Read
-- Prelude Text.Read> :t readMaybe
-- readMaybe :: Read a => String -> Maybe a
-- Prelude Text.Read> :t readEither
-- readEither :: Read a => String -> Either String a
-- (Uwaga: w tym zadaniu raczej nadal tworzymy funkcje String -> String i korzystamy z interact niż bezpośrednio z IO, chyba że ktoś bardzo chce)

-- *Main> readInts2 "1 23 456 abc 9"
-- Left "Not a number: abc"
-- *Main> sumInts "1 23 456 abc 9"
-- "Not a number: abc"
-- *Main> sumInts "1 2 3"
-- "6"



-- Zadanie 2
-- a. Zaimplementuj i uruchom przykład z wykładu

-- data Exp = Val Int | Div Exp Exp
-- safediv :: Int -> Int -> Maybe Int
-- safediv _ 0 = Nothing
-- safediv x y = Just (div x y)

-- eval :: Exp -> Maybe Int
-- uzupełnij go o inne operacje arytmetyczne.

-- b. Napisz funkcje obliczające wartość listy wyrażeń:

-- evalList' :: [Exp] -> [Maybe Int]
-- evalList :: [Exp] -> Maybe [Int]
-- c. Zmodyfikuj kod z (a) i (b) używając Either zamiast Maybe:

-- safediv :: Int -> Int -> Either String Int
-- safediv _ 0 = Left "Division by zero"
-- safediv x y = Right (div x y)

-- eval :: Exp -> Either String Int
-- d. Rozszerz wyrażenia o

-- data Exp = ... Var String
-- Napisz funkcję

-- eval :: Env -> Either String Int
-- W przypadku użycia w wyrazeniu nieznanej zmiennej zgłoś odpowiedni błąd.

-- Można przyjąć Env = [(String, Int)] lub Data.Map.Map String Int.

-- Zadanie 3
-- Uzupełnić przykład z wykładu:

-- import Control.Monad.Error.Class
-- data ParseError = Err {location::Int, reason::String}

-- type ParseMonad = Either ParseError
-- parseHexDigit :: Char -> Int -> ParseMonad Integer
-- parseHex :: String -> ParseMonad Integer
-- toString :: Integer -> ParseMonad String

-- -- convert zamienia napis z liczba szesnastkowa 
-- --   na napis z liczba dziesietna
-- convert :: String -> String
-- convert s = str where
--  (Right str) = tryParse s `catchError` printError
--  tryParse s = do {n <- parseHex s; toString n}
--  printError e = ...
-- Wskazówka: to zadanie, chociaż wygląda groźnie nie jest bardzo odległe od sumInts w jednym z poprzednich zadań.

-- Zadanie 4 - IO
-- a. Napisz program który wypisze swoje argumenty, każdy w osobnej linii (wskazówka: System.Environment.getArgs)

-- b. Napisz program, który będzie pytał użytkownika o ulubiony język programowania, tak długo aż odpowiedzią będzie 'Haskell' ;)

-- c. Napisz uproszczoną wersję programu wc (wypisującą ilość linii, słów i znaków w pliku o nazwie podanej jako argument, bądź stdin jeśli bez argumentu)