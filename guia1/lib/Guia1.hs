module Guia1 where

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

-- Ej3

sum' :: [Float] -> Float
sum' = foldr (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
-- elem' e = foldr ((||) . (==e)) False
elem' e = foldr (\y rec -> (e == y) || rec) False

(+++) :: [Float] -> [Float] -> [Float]
-- (+++) = foldr((:))

(+++) = foldr(\x rec -> x : rec)