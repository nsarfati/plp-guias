data Matriz a = NuevaMatriz a | Agregar a Int Int (Matriz a) deriving(Show)

foldMatriz :: (a -> b) -> (a -> Int -> Int -> b -> b) -> Matriz a -> b
foldMatriz cBase cRec matriz = case matriz of
    NuevaMatriz e -> cBase e
    Agregar e x y m -> cRec e x y (foldMatriz cBase cRec m)

tt = foldMatriz (const []) (\e x y rec -> (x,y):rec)

ver :: Int -> Int -> Matriz a -> a
ver x y = foldMatriz id (\e x' y' rec ->
    if (x' == x && y' == y) then
        e
    else    
        rec
    )

-- suma :: Num a => Matriz a -> Matriz a -> Matriz a
-- suma = foldMatriz (const id) (\e1 x1 y1 rec -> m2 -> rec m2
    
    -- )
        -- mapMatriz (Agregar ((ver x1 y1 m2) + e1) x1 y1) (rec m2)
        
        -- (rec m2)


