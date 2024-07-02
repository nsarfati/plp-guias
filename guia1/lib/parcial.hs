data AT a = NilT | Tri a (AT a) (AT a) (AT a) deriving Show

at1 = Tri 1 ( Tri 2 NilT NilT NilT) (Tri 3 (Tri 4 NilT NilT NilT) NilT NilT) (Tri 5 NilT NilT NilT)

foldAT ::
    b                       -- Nil
    -> (a -> b -> b -> b -> b)   -- Bin
    -> AT a
    -> b

foldAT cBase cTri arbol = case arbol of
    NilT -> cBase
    (Tri r i m d) -> cTri r (rec i) (rec m) (rec d)
    where rec = foldAT cBase cTri

preorder :: AT a -> [a]
preorder = foldAT [] (\r i m d -> [r] ++ i ++ m ++ d)

mapAT :: (a -> b) -> AT a -> AT b
mapAT f = foldAT NilT (\r i m d -> Tri (f r) i m d)
-- mapAT f = foldAT NilT (\r i m d -> Tri (f r) i m d)
-- mapAT f = foldAT NilT (\r i m d -> Tri r i m d)

-- data Matriz a = NuevaMatriz a | Agregar a Int Int (Matriz a) deriving(Show)

-- foldMatriz :: (a -> b) -> (a -> Int -> Int -> b -> b) -> Matriz a -> b
-- foldMatriz cBase cRec matriz = case matriz of
--     NuevaMatriz e -> cBase e
--     Agregar e x y m -> cRec e x y (foldMatriz cBase cRec m)

-- tt = foldMatriz (const []) (\e x y rec -> (x,y):rec)

-- ver :: Int -> Int -> Matriz a -> Matriz a
-- ver x y = foldMatriz (id) (\e x' y' rec -> rec)