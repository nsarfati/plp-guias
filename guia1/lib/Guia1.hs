module Guia1 where
import Data.Foldable (Foldable(fold))

-- Ej1.1

max2(x, y) | x >= y = x
           | otherwise = y


{-
     max2 :: Float -> Float -> Float
     normaVectorial :: (Float -> Float) -> Float
     substract :: Float -> Float -> Float
     predecesor :: Float -> Float
     
     evaluarEnCero = \f -> f 0
     evaluarEnCero :: (Float -> Float) -> Float

     dosVeces = \f -> f . f
     dosVeces :: (Float -> Float) -> Float -> Float

     flipAll = map flip
     
          map :: (a -> b) -> [a] -> [b]
          flip :: (a -> b -> c) -> b -> a -> c

     [a -> b -> c] -> [b -> a -> c]

     flipRaro = flip flip

     flip :: (a -> b -> c) -> (b -> a -> c)

     (b -> a -> c) ->
     (a -> b -> c) -> b -> a -> c -> (a -> b -> c)


-}

-- Ej1.2

     {-
          max2 :: NO esta currificada
          max2Curri x y | x >= y = x
                        | otherwise = y

          normaVectorial :: NO esta currificada
               normaVectorialCurri x y = sqrt(x^2 + y^2)

          subtract esta currificada
          predecesor esta currificada
          evaluarEnCero esta currificada
          dosVeces esta currificada pues recibe una funcion
          flipAll esta currificada
          flipRaro esta currificada

     -}

-- Ej2

curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f x y = f (x, y)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f(x, y) = f x y

curry2 :: ((a, b) -> c) -> (a -> b -> c)
curry2 f x y = f (x, y)
curry3 :: ((a, b, c) -> d) -> (a -> b -> c -> d)
curry3 f x y z = f (x, y, z)
curry4 :: ((a, b, c, d) -> e) -> (a -> b -> c -> d -> e)
curry4 f x y z w = f (x, y, z, w)

{-
     Uno se podria imaginar que podria dar una funcion, que recibe n
     funciones parciales y las aplica todas juntas, pero el problema,
     es que necesito la tupla con los parametros de entrada. Y dado
     que los tipos de haskell son explicitos, no puedo definir una funcion
     que reciba una tupla de 2 elementos o una tripla o una tupla con n elementos sin
     explicitarlo. Por lo que hacer curryN es imposible.
-}

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x : xs) = f x (foldr' f z xs)

-- Ej3.1

sum' :: [Float] -> Float
sum' = foldr (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
-- elem' e = foldr ((||) . (==e)) False
elem' e = foldr (\y rec -> (e == y) || rec) False

(+++) :: [a] -> [a] -> [a]
(+++) = foldr(\x rec -> x : rec)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x rec -> if p x then x:rec else rec) []

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x rec -> (f x):rec) []

-- Ej3.2
mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\x y -> if f x y then x else y)

maximum' :: Ord a => [a] -> a
maximum' = mejorSegun (>)

-- Ej3.3
sumasParciales :: Num a => [a] -> [a]
-- sumasParciales [] = []
-- sumasParciales [x] = [x]
-- sumasParciales (x:y:xs) = x:sumasParciales ((x+y):xs)
-- sumasParciales = foldl(\acc x -> if (null acc) then [x] else acc++[((last acc) + x)]) [] # mas ilegible
sumasParciales = foldl(\acc x -> acc ++ [x + (if null acc then 0 else last acc)]) []

-- Ej3.4
sumaAlt :: Num a => [a] -> a
-- sumaAlt = foldr(\x rec -> x-rec) 0
sumaAlt = foldr (-) 0
-- sumaAlt [1,2,3] -> 1 - [2, 3] -> 1 - [2 - [3]] -> 1 - [2 - 3 - 0] -> 1 - (2 - (3 - 0))


-- Ej3.5
reverse' :: [a] -> [a]
reverse' = foldr(\x rec -> rec ++ [x]) []

sumaAltRev :: Num a => [a] -> a
sumaAltRev = sumaAlt . reverse'

sumaAltRev' :: Num a => [a] -> a
sumaAltRev' = foldl(\acc x -> x - acc) 0

-- Ej4.1
intercalar :: a -> [a] -> [[a]]
intercalar e [] = [[e]]
intercalar e (x:xs) = (e:x:xs) : [(x:ys) | ys <- intercalar e xs]

quitarElemento :: Eq a => a -> [a] -> [a]
quitarElemento _ [] = []
quitarElemento x (y:ys)
    | x == y = ys
    | otherwise = y : quitarElemento x ys

permutaciones :: Eq a => [a] -> [[a]]
permutaciones [] = [[]]
permutaciones lista = concatMap(\x -> map (x:) (permutaciones (quitarElemento x lista))) lista

-- Ej4.2
partes :: [a] -> [[a]]
-- partes [] = [[]]
-- partes (x:xs) = partes xs ++ map (x:) (partes xs)
partes = foldr(\x rec -> rec ++ map(x:) rec) [[]]

-- Ej4.3
prefijos :: [a] -> [[a]]
prefijos l = [ take n l | n <- [0 .. (length l)]]

-- Ej4.4
sinRepetidos :: (Eq a) => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) | x `elem` xs = sinRepetidos xs
                    | otherwise = x:sinRepetidos xs

sublistas :: (Eq a) => [a] -> [[a]]
sublistas [] = [[]]
sublistas (x:xs) = sinRepetidos (prefijos (x:xs) ++ sublistas xs)
-- sublistas lista = foldr(\x rec -> [tail lista] ++ prefijos(head rec)) [[]] lista

-- sublistas l = sinRepetidos (
--      [ take n l | n <- [0 .. (length l)]] ++
--      [ take n l | n <- [0 .. (length l)]] ++
--      [ drop n l | n <- [0 .. (length l)]]
--      )

-- Ej5
elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) = if null xs then [x] else x:elementosEnPosicionesPares(tail xs)

-- Esto segun chatgpt???
elementosEnPosicionesPares' :: [a] -> [a]
elementosEnPosicionesPares' xs = snd $ foldr (\x (isEven, acc) -> (not isEven, if isEven then x : acc else acc)) (False, []) xs

elementosEnPosicionesPares'' :: [a] -> [a]
elementosEnPosicionesPares'' = recr(\x xs rec -> if null xs then [x] else x:rec ++ tail xs) []

{-
     Es recursion estructural?
     * Devuelve un valor fijo en el caso base. SI
     * El caso recursivo se escribe usando (cero, una o muchas veces) x. SI
     * El caso recursivo se escribe usando (cero, una o muchas veces) (g xs),
     pero sin usar el valor de xs ni otros llamados recursivos. NO
          Pues se usa el valor de xs en el llamado recursivo preguntando if null xs y al hacer tail xs

     >> NO es recursiones estructural, por lo tanto no se puede escribir con foldr
-}

entralazar :: [a] -> [a] -> [a]
entralazar [] = id
entralazar (x:xs) = \ys ->
     if null ys then x:entralazar xs []
     else x:head ys : entralazar xs (tail ys)

{-
     Es recursion estructural?
     * Devuelve un valor fijo en el caso base. SI
     * El caso recursivo se escribe usando (cero, una o muchas veces) x. SI
     * El caso recursivo se escribe usando (cero, una o muchas veces) (g xs),
     pero sin usar el valor de xs ni otros llamados recursivos. NO
          Pues se usa el valor de ys en el llamado recursivo preguntando if null ys y al hacer head + tail xs

     >> NO es recursiones estructural, por lo tanto no se puede escribir con foldr
-}

-- entralazar' :: [a] -> [a] -> [a]
-- entralazar' l1 l2 = foldr(\x rec -> if null ys then x:rec else (x : head l2 : rec)) l2

-- Ej 6
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b

recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

-- Ej 6.a
sacarUna' :: Eq a => a -> [a] -> [a]
sacarUna' _ [] = []
sacarUna' e (x:xs) | e == x = xs
                   | otherwise = x:sacarUna' e xs

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna e = recr(\x xs rec -> if x == e then xs else x:rec) []

trim = recr (\ x xs rec -> if x == ' ' then rec else x : xs) []

-- Ej 6.b
{-
     foldr no es adecuado para implementar sacarUna, pues si bien sacarUna devuelve un valor
     fijo en el caso base y se utiliza una sola vez la x en el caso recursivo; se utiliza
     el valor de xs en el llamado recursivo y eso es una indicacion que no se trata de una
     recursion estructural.
-}

-- Ej 6.c
insertarOrdenado' :: Ord a => a -> [a] -> [a]
insertarOrdenado' e [] = [e]
insertarOrdenado' e (x:xs) | e <= x = e:x:xs
                           | otherwise = x:insertarOrdenado' e xs

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado e = recr(\x xs rec -> if e <= x then e:x:xs else x:rec) [e]


-- Ej 7.1
genLista :: a -> (a -> a) -> Int -> [a]
genLista inicial f 0 = []
genLista inicial f cant = inicial : genLista (f inicial) f (cant - 1)

-- Ej 7.2
desdeHasta :: Int -> Int -> [Int]
desdeHasta a b | a > b = error "El segundo debe ser mayor al primero"
desdeHasta a b = genLista a (+1) (b - a)

-- Ej8.1
mapPares :: (a -> b -> c) -> [(a, b)] -> [c]
mapPares f = map (uncurry f)

-- Ej8.2
armarPares :: [a] -> [b] -> [(a, b)]
armarPares [] _ = []
armarPares _ [] = []
armarPares (x:xs) (y:ys) = (x, y):armarPares xs ys

build :: a -> ([b] -> [(a, b)]) -> [b] -> [(a, b)]
build x rec [] = []
build x rec (y:ys) = (x, y) : rec ys

armarPares' :: [a] -> [b] -> [(a, b)]
armarPares' = foldr build (const [])

-- Ej8.3
mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f l1 l2 = mapPares f (armarPares l1 l2)

-- Ej9.1

-- ChatGPT como se recorren dos listas en simultaneo usando foldr
-- zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
-- zipWith' f xs ys = foldr combine (const []) xs ys
--   where
--     combine x acc [] = []
--     combine x acc (y:ys) = f x y : acc ys


sumarMs :: [Int] -> ([[Int]] -> [[Int]]) -> [[Int]] -> [[Int]]
sumarMs x_m1 rec [] = []
sumarMs x_m1 rec (y_m2:m2) = zipWith (+) x_m1 y_m2 : rec m2

sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
-- sumaMat [] [] = []
-- sumaMat (x:xs) (y:ys) = zipWith (+) x y : sumaMat xs ys
sumaMat = foldr sumarMs (const [])

-- Ej9.2
{-
[
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9]
]

[
    [1, 4, 7],
    [2, 5, 8],
    [3, 6, 9]
]
-}

trMatriz :: [[Int]] -> ([[Int]] -> [[Int]]) -> [[Int]]
trMatriz [] rec = []
trMatriz matriz rec = map head matriz : rec (map tail matriz)


trasponer :: [[Int]] -> [[Int]]
-- trasponer [] = []
-- trasponer ([]:_) = []
-- trasponer matriz = map head matriz : trasponer (map tail matriz)
trasponer = foldr (\fila rec -> zipWith (:) fila rec) (repeat [])

-- Ej 10.1

stop' :: [Int] -> Bool
stop' [] = False
stop' (x:xs) | x > 100 = True
             | otherwise = stop' xs

next' :: [Int] -> Int
next' l = length l + 1

generate :: ([a] -> Bool) -> ([a] -> a) -> [a]
generate stop next = generateFrom stop next []

-- generateFrom :: ([a] -> Bool) -> ([a] -> a) -> [a] -> [a]
-- generateFrom stop next xs | stop xs = init xs
--                           | otherwise = generateFrom stop next (xs ++ [next xs])

generateFrom' :: ([a] -> Bool) -> (a -> a) -> [a] -> [a]
generateFrom' stop next xs | stop xs = init xs
                           | otherwise = generateFrom' stop next (xs ++ [next (last xs)])


generateBase :: ([a] -> Bool) -> a -> (a -> a) -> [a]
generateBase stop casoBase next  = generateFrom' stop next [casoBase]

-- Ej 10.2
factorial :: Int -> Int
factorial 1 = 1
factorial n = n * factorial (n - 1)

factoriales :: Int -> [Int]
factoriales n = generate (\x -> length x > n) (\x -> factorial(length x + 1))

-- Ej 10.3

iterateN :: Int -> (a -> a) -> a -> [a]
-- iterateN n f x = take n (iterate f x)
iterateN n f casoBase = generateBase (\x -> length x > n) casoBase f

-- Ej 10.4
generateFrom :: ([a] -> Bool) -> ([a] -> a) -> [a] -> [a]

-- generateFrom stop next xs = init (takeWhile stop (iterate next (xs ++ [next xs])))

generateFrom stop next xs | stop xs = init xs
                          | otherwise = generateFrom stop next (xs ++ [next xs])

