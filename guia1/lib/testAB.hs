data AB a = Nil | Bin (AB a) a (AB a) deriving(Show)

foldAB ::
    b                       -- Nil
    -> (b -> a -> b -> b)   -- Bin
    -> AB a
    -> b

foldAB z f x = case x of
    Nil -> z
    (Bin l v r) -> f (rec l) v (rec r)
    where rec = foldAB z f

esNil :: AB a -> Bool
esNil x = case x of
    Nil -> True
    _ -> False

-- mismaEstructura :: AB a -> AB a -> Bool
mismaEstructura :: AB a -> AB b -> Bool
mismaEstructura Nil = esNil
mismaEstructura (Bin i _ d) = 
    \a -> not (esNil a) && mismaEstructura i (ladoIzq a) && mismaEstructura d (ladoDer a)

mismaEstructura2 :: AB a -> AB b -> Bool
mismaEstructura2 = foldAB (const True) (\i _ d a2 ->
        (not (esNil a2)) && (i (ladoIzq a2)) && (d (ladoDer a2))
    )
-- mismaEstructura2 = foldAB esNil (\ri _ rd a -> not (esNil a) && ri (ladoIzq a) && rd (ladoDer a))



ladoIzq :: AB a -> AB a
ladoIzq (Bin i _ _) = i

ladoDer :: AB a -> AB a
ladoDer (Bin _ _ d) = d

