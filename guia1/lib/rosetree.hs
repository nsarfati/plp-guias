data RoseTree a = Rose a [RoseTree a]


tamaño :: RoseTree a -> Int
-- tamaño (Rose x hijos) = 1 + sum (map tamaño hijos)
tamaño = foldRT (\_ recs -> 1 + sum recs)


foldRT :: (a -> [b] -> b) -> RoseTree a -> b
foldRT f (Rose x hijos) = f x (map (foldRT f) hijos)
