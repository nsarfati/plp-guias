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

-- foldAB z f Nil = z
-- foldAB z f (Bin l v r) = f (foldAB z f l) v (foldAB z f r)

recAB ::
    b                                       -- Nil
    -> (AB a -> a -> AB a -> b -> b -> b)   -- Bin
    -> AB a
    -> b

recAB cBase cRec x = case x of
    Nil -> cBase
    (Bin l v r) -> cRec l v r (rec l) (rec r)
    where rec = recAB cBase cRec

esNil :: AB a -> Bool
esNil x = case x of
    Nil -> True
    _ -> False


mejorSegún :: (a -> a -> Bool) -> AB a -> a
mejorSegún f (Bin l v r) = foldAB v (\rl v rr -> (rl `g` v) `g` rr) (Bin l v r)
    where g x y = if f x y then x else y

mejorSegun2 :: (a -> a -> Bool) -> AB a -> a
mejorSegun2 f (Bin l v r) = foldAB v (\rl v rr -> g (g rl v) rr
    ) (Bin l v r)
    where g x y = if f x y then x else y
    
cantNodos :: AB a -> Int
cantNodos = foldAB 0 (\ri _ rd -> 1 + ri + rd)

esABB :: Ord a => AB a -> Bool
esABB = recAB True (\subIzq raiz subDer ri rd ->
        (esNil subIzq && esNil subDer) ||
            (raiz >= valor subIzq) && (raiz < valor subDer) && ri && rd
    )

valor :: AB a -> a
valor (Bin l v r) = v

-- esABB :: Ord a => AB a -> Bool
-- esABB = recAB True f
--     where
--         f l v r rl rr
--             | esNil l && esNil r = True
--             | esNil r = rl && raíz l <= v
--             | esNil l = rr && v < raíz r
--             | otherwise = rl && rr && raíz l <= v && v < raíz r

-- raíz :: AB a -> a
-- raíz (Bin l v r) = v

cantHojas :: AB a -> Int
cantHojas = recAB 0 (\subIzq raiz subDer ri rd ->
        (if (esHoja subIzq subDer) then 1 else 0) + ri + rd
    )
    where esHoja subIzq subDer = esNil subIzq && esNil subDer

espejo :: AB a -> AB a
espejo = foldAB Nil (\ri r rd -> Bin rd r ri)

mismaEstructura :: AB a -> AB b -> Bool
-- mismaEstructura = foldAB (const True) (\i1 r1 d1 -> \a2 -> 
--     if (esHoja a2) then
--         True
--     else
--         i1 && r1
--     )

mismaEstructura = recAB (const True) (\subIzq1 raiz1 subDer1 ri1 rd1 -> \a2 ->
    
    if esHoja a2 then
        esNil subIzq1 && esNil subDer1
    else
        ri1 (izq a2) && rd1 (der a2)
    )

esHoja :: AB a -> Bool
esHoja Nil = False
esHoja (Bin i r d) = esNil i && esNil d

izq :: AB a -> AB a
izq Nil = Nil
izq (Bin i r d) = i

der :: AB a -> AB a
der Nil = Nil
der (Bin i r d) = d