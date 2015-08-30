-- |
-- Module      : Conjunto
-- Description : TAD de los conjuntos.
-- License     : Creative Commons
-- Maintainer  : José A. Alonso
-- 
-- TAD (tipo abstracto de datos) de los conjuntos.
--
-- Este módulo contiene el código del TAD de los conjuntos
-- estudiado en el <http://bit.ly/1WYZzmW tema 17> del curso.

module I1M.Conjunto
    (Conj,
     vacio,     -- Conj a                       
     esVacio,   -- Conj a -> Bool               
     pertenece, -- Ord a => a -> Conj a -> Bool  
     inserta,   -- Ord a => a -> Conj a -> Conj a
     elimina    -- Ord a => a -> Conj a -> Conj a
    ) where

-- | Tipo de dato de los conjuntos.
newtype Conj a = Cj [a]
    deriving Eq

-- Procedimiento de escritura de los conjuntos.
instance (Show a) => Show (Conj a) where
    showsPrec _ (Cj s) cad = showConj s cad

showConj []     cad = showString "{}" cad
showConj (x:xs) cad = showChar '{' (shows x (showl xs cad))
     where showl []     cad = showChar '}' cad
           showl (x:xs) cad = showChar ',' (shows x (showl xs cad))

-- En los ejemplos se usará el siguiente conjunto.
-- 
-- > ghci> c1
-- > {0,1,2,3,5,7,9}
c1 :: Conj Int
c1 = foldr inserta vacio [2,5,1,3,7,5,3,2,1,9,0]

-- | vacio es el conjunto vacío. Por ejemplo,
-- 
-- > ghci> vacio
-- > {}
vacio :: Conj a                         
vacio = Cj []

-- | (esVacio c) se verifica si c es el conjunto vacío. Por ejemplo, 
-- 
-- > λ> esVacio (foldr inserta vacio [2,5])
-- > False
-- > λ> esVacio vacio
-- > True
esVacio :: Conj a -> Bool                
esVacio (Cj xs) = null xs

-- | (pertenece x c) se verifica si x pertenece al conjunto c. Por ejemplo, 
-- 
-- > λ> let c1 = foldr inserta vacio [2,5,3,2]
-- > λ> pertenece 3 c1
-- > True
-- > λ> pertenece 4 c1
-- > False
pertenece :: Ord a => a -> Conj a -> Bool 
pertenece x (Cj s) = elem x (takeWhile (<= x) s)

-- | (inserta x c) es el conjunto obtenido añadiendo el elemento x al
-- conjunto c. Por ejemplo,
-- 
-- > λ> let c1 = foldr inserta vacio [2,5,3,2]
-- > λ> c1
-- > {2,3,5}
-- > λ> inserta 3 c1
-- > {2,3,5}
-- > λ> inserta 4 c1
-- > {2,3,4,5}
inserta :: Ord a => a -> Conj a -> Conj a
inserta x (Cj s) = Cj (agrega x s)
    where agrega x []                    = [x]                
          agrega x s@(y:ys) | x > y      = y : (agrega x ys)
                            | x < y      = x : s
                            | otherwise  = s

-- | (elimina x c) es el conjunto obtenido eliminando el elemento x
-- del conjunto c. Por ejemplo,
-- 
-- > λ> let c1 = foldr inserta vacio [2,5,3,2]
-- > λ> c1
-- > {2,3,5}
-- > λ> elimina 3 c1
-- > {2,5}
-- > λ> elimina 7 c1
-- > {2,3,5}
elimina :: Ord a => a -> Conj a -> Conj a
elimina x (Cj s) = Cj (elimina x s)
    where elimina x []                   = []
          elimina x s@(y:ys) | x > y     = y : (elimina x ys)
                             | x < y     = s
                             | otherwise = ys
