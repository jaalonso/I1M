-- |
-- Module      : Tabla
-- Description : TAD de las tablas.
-- License     : Creative Commons
-- Maintainer  : José A. Alonso
-- 
-- TAD (tipo abstracto de datos) de las tablas.
--
-- Este módulo contiene el código del TAD de las tablas 
-- estudiado en el <http://bit.ly/1F5RSjM tema 18> del curso.
-- 
-- En los ejemplos se usarán las siguientes tablas:
-- 
-- > t1 = tabla [(i,f i) | i <- [1..6] ] 
-- >      where f x | x < 3     = x
-- >                | otherwise = 3-x
-- > t2 = tabla [(4,89), (1,90), (2,67)]

module I1M.Tabla
    (Tabla,
     tabla,   -- Eq i => [(i,v)] -> Tabla i v           
     valor,   -- Eq i => Tabla i v -> i -> v            
     modifica -- Eq i => (i,v) -> Tabla i v -> Tabla i v
    ) where

-- | El tipo de las tablas.
newtype Tabla i v = Tbl [(i,v)]
    deriving Show

-- Ejemplos de tabla:
--    ghci> t1
--    Tbl [(1,1),(2,2),(3,0),(4,-1),(5,-2),(6,-3)]
--    ghci> t2
--    Tbl [(4,89),(1,90),(2,67)]
t1 = tabla [(i,f i) | i <- [1..6] ] 
     where f x | x < 3     = x
               | otherwise = 3-x
t2 = tabla [(4,89), (1,90), (2,67)]
    
-- | (tabla ivs) es la tabla correspondiente a la lista de asociación
-- ivs (que es una lista de pares formados por los índices y los
-- valores). Por ejemplo,
-- 
-- > tabla [(4,89), (1,90), (2,67)]  ==  Tbl [(4,89),(1,90),(2,67)]
tabla :: Eq i => [(i,v)] -> Tabla i v
tabla ivs = Tbl ivs

-- | (valor t i) es el valor del índice i en la tabla t. Por ejemplo, 
-- 
-- > valor t1 6  ==  -3
-- > valor t2 2  ==  67
-- > valor t2 5  ==  *** Exception: fuera de rango
valor :: Eq i => Tabla i v -> i -> v
valor (Tbl []) i = error "fuera de rango"
valor (Tbl ((j,v):r)) i
     | i == j    = v
     | otherwise = valor (Tbl r) i 

-- | (modifica (i,x) t) es la tabla obtenida modificando en la tabla t el
-- valor de i por x. Por ejemplo, 
-- 
-- > valor t1 6                   ==  -3
-- > valor (modifica (6,9) t1) 6  ==  9
modifica :: Eq i => (i,v) -> Tabla i v -> Tabla i v
modifica p (Tbl []) = (Tbl [p])
modifica p'@(i,_) (Tbl (p@(j,_):r))
     | i == j     = Tbl (p':r)
     | otherwise  = Tbl (p:r')
     where Tbl r' = modifica p' (Tbl r)

-- Nota sobre la eficiencia: 
-- * modifica y valor son de O(n) pasos en el peor caso,
--   donde n es el número de entradas en la tabla. 
-- * modifica requiere O(n) celdas en el peor caso.

-- ---------------------------------------------------------------------
-- Igualdad                                                           --
-- ---------------------------------------------------------------------

--- Las tablas son comparables por igualdad.
instance (Eq i, Eq v) => Eq (Tabla i v) where
    (Tbl [])        == (Tbl []) = True
    (Tbl ((i,v):t)) == Tbl t'   = elem (i,v) t' && 
                                  Tbl t == Tbl [p | p <- t', p /= (i,v)]
