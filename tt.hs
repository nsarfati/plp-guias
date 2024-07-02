data Polinomio a = X
    | Cte a
    | Suma (Polinomio a) (Polinomio a)
    | Prod (Polinomio a) (Polinomio a)
    deriving (Eq, Show)

foldPoli :: (a -> b) -> b -> (b -> b -> b) -> (b -> b -> b) -> Polinomio a -> b
-- foldPoli :: (t1 -> t2) -> (Polinomio a -> t2) -> (t2 -> t2 -> t2) -> (t2 -> t2 -> t2) -> Polinomio t1 -> t2
foldPoli fCte fX fSuma fProd pol = 
    case pol of
        Cte c -> fCte c
        X -> fX
        Suma pa pb -> fSuma (recu pa) (recu pb)
        Prod pa pb -> fProd (recu pa) (recu pb)
    where recu = foldPoli fCte fX fSuma fProd

evaluar :: Num a => a -> Polinomio a -> a
-- evaluar n = foldPoli id n (\pa pb -> pa + pb) (\pa pb -> pa * pb)
evaluar n = foldPoli id n (+) (*)

sinConstantesNegativas :: (Num a, Ord a) => Polinomio a -> Bool
sinConstantesNegativas = foldPoli (>= 0) True (&&) (&&)