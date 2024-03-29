module Guia0 where

-- Ej1

{-
     null :: [a] -> Bool
     Devuelve si una lista esta vacia o no
     
     head :: [a] -> a
     Devuelve el la cabeza de una lista

     tail :: [a] -> [a]
     Devuelve la cola de una lista

     last :: [a] -> a
     Devuelve el ultimo elemento de una lista

     take :: Int -> [a] -> [a]
     Toma los primeros n elementos de una lista

     drop :: Int -> [a] -> [a]
     Toma todos los elementos a partir de n

     (++) :: [a] -> [a] -> [a]
     Pega dos listas

     concat :: [[a]] -> [a]
     Genera una unica lista con las listas internas

     (!!) :: [a] -> Int -> a
     Devuelve el elemento de la posicion n

     elem :: a -> [a] -> Bool
     Devuelve si el elemento pertenece a la lista
-}

-- Ej2

valorAbsoluto :: Float -> Float
valorAbsoluto a | a < 0 = -a
                | otherwise = a

bisiesto :: Int -> Bool
bisiesto anio | anio `mod` 4 /= 0 = False
bisiesto anio | anio `mod` 100 /= 0 = True
bisiesto anio | anio `mod` 400 == 0 = True
              | otherwise = False

factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x - 1)

esPrimo' :: Int -> [Int] -> Bool
esPrimo' e [] = True
esPrimo' e (x:xs) | x /= 1 && x /= e = (e `mod` x /= 0) && esPrimo' e xs
                  | otherwise = esPrimo' e xs

esPrimo :: Int -> Bool
esPrimo e = esPrimo' e [1..e]

cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos x = length [ e | e <- [1..x], esPrimo e && x `mod` e == 0 ]

-- Ej3

--data Maybe a = Nothing | Just a
-- data Either a b = Left a | Right b

inverso :: Float -> Maybe Float
inverso x | x /= 0 = Just (1/x)
          | otherwise = Nothing

aEntero :: Either Int Bool -> Int
aEntero (Left a) = a
aEntero (Right b) | b = 1
                  | otherwise = 0

-- Ej4

eliminarChar :: Char -> [Char] -> [Char]
eliminarChar c [] = []
eliminarChar c (x:xs) | x /= c = x : eliminarChar c xs
                      | otherwise = eliminarChar c xs

limpiar :: [Char] -> [Char] -> [Char]
-- limpiar charsABorrar listaFinal
limpiar [] listaFinal = listaFinal
limpiar (xCharsABorrar:xsCharsABorrar) listaFinal = limpiar xsCharsABorrar (eliminarChar xCharsABorrar listaFinal)

difPromedio' :: [Float] -> Float -> [Float]
difPromedio' [] _ = []
difPromedio' (x:xs) promedio = (x - promedio) : difPromedio' xs promedio

difPromedio :: [Float] -> [Float]
difPromedio lista = difPromedio' lista (sum lista / (fromIntegral (length lista) :: Float))

todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales [x] = True
todosIguales (x:y:ys) = x == y && todosIguales (y:ys)

-- Ej5

data AB a = Nil | Bin (AB a) a (AB a) deriving (Show, Eq)

vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB _ = False

negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin i r d) = Bin (negacionAB i) (not r) (negacionAB d)

productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin i r d) = r * productoAB i * productoAB d