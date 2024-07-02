-- armarPares :: [a] -> [b] -> [(a, b)]
-- armarPares (x:xs) (y:ys) = (x,y) : armarPares xs ys
-- armarPares _ _ = []

-- armarPares2 :: [a] -> [b] -> [(a, b)]
-- armarPares2 l1 l2 = foldr(\x rec1 -> (foldr (\y rec2 -> (x,y) : rec1) [] l2) [] l1)

-- build :: a -> ([b] -> [(a, b)]) -> [b] -> [(a, b)]
-- build x rec [] = []
-- build x rec (y:ys) = (x, y) : rec ys

-- armarPares' :: [a] -> [b] -> [(a, b)]
armarPares' l1 l2 = foldr (\x rec -> foldr (\y rec' -> (x, y) : rec') rec l2) [] l1

-- armarPares' :: [a] -> [b] -> [(a, b)]
-- armarPares' l1 l2 = foldr (\x rec -> x:rec)

-- build x rec = foldr (\y _ -> (x, y):rec) []

-- armarPares' = foldr build (const [])

---------

-- armarPares666 l1 l2 = foldr (\x rec -> (ff x l2) : rec) [] l1

-- ff x [] = []
-- ff x (y:ys) = [(x,y)]

fff x [] rec = []
fff x (y:ys) rec = (x, y) : rec ys

-- armarPares88 xs = foldr(\x rec -> \ys -> (x, head ys) : (rec (tail ys))) [] xs
-- armarPares88 xs = foldr(\x rec -> \ys -> (fff x ys rec)) (const []) xs
-- armarPares88 xs = foldr(\x rec -> \ys -> (fff x ys rec)) (const []) xs

armarPares88 :: [a] -> [b] -> [(a, b)]
armarPares88 = foldr(\x rec -> \ys -> 
    
    if (length ys > 0) then
        (x, head ys) : rec (tail ys)
    else
        []
    ) (const [])

armarPares99 :: [a] -> [b] -> [(a, b)]
armarPares99 = foldr(\x rec -> \ys -> 
    
    if (length ys > 0) then
        (x, head ys) : rec (tail ys)
    else
        []
    ) (const [])
    

-- armarPares99 xs = foldr(\x rec -> \ys -> (x, head ys) : (rec (tail ys))) (const []) xs
