-- |
-- Module      : ArbolBin
-- Description : TAD de los árboles binarios de búsqueda.
-- License     : Creative Commons
-- Maintainer  : José A. Alonso
--
-- == TAD (tipo abstracto de datos) de los árboles binarios de búsqueda.
--
-- Este módulo contiene el código del TAD de los árboles binarios
-- estudiado en el <http://bit.ly/1F5RFgF tema 19> del curso.
--
-- Un árbol binario de búsqueda (ABB) es un árbol binario tal que el
-- valor de cada nodo es mayor que los valores de su subárbol izquierdo
-- y es menor que los valores de su subárbol derecho y, además, ambos
-- subárboles son árboles binarios de búsqueda. Por ejemplo, al
-- almacenar los valores de [2,3,4,5,6,8,9] en un ABB se puede obtener
-- los siguientes ABB:
--
-- >       5                     5
-- >      / \                   / \
-- >     /   \                 /   \
-- >    2     6               3     8
-- >     \     \             / \   / \
-- >      4     8           2   4 6   9
-- >     /       \
-- >    3         9
--
-- El objetivo principal de los ABB es reducir el tiempo de acceso a los
-- valores.
--
-- En los ejemplos se usarán los siguientes ABB:
--
-- > abb1, abb2 :: ABB Int
-- > abb1 = crea (reverse [5,2,6,4,8,3,9])
-- > abb2 = foldr inserta vacio (reverse [5,2,4,3,8,6,7,10,9,11])

{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module I1M.ArbolBin
  (ABB,
   vacio,     -- ABB
   inserta,   -- (Ord a,Show a) => a -> ABB a -> ABB a
   elimina,   -- (Ord a,Show a) => a -> ABB a -> ABB a
   crea,      -- (Ord a,Show a) => [a] -> ABB a
   crea',     -- (Ord a,Show a) => [a] -> ABB a
   menor,     -- Ord a => ABB a -> a
   elementos, -- (Ord a,Show a) => ABB a -> [a]
   pertenece, -- (Ord a,Show a) => a -> ABB a -> Bool
   valido     -- (Ord a,Show a) => ABB a -> Bool
  ) where

-- | El tipo de dato de los ABB,
data ABB a = Vacio
           | Nodo a (ABB a) (ABB a)
  deriving Eq

-- Procedimiento de escritura de árboles binarios de búsqueda.
instance (Show a, Ord a) => Show (ABB a) where
  show Vacio        = " -"
  show (Nodo x i d) = " (" ++ show x ++ show i ++ show d ++ ")"

-- Ejemplos de ABB
--    ghci> abb1
--     (5 (2 - (4 (3 - -) -)) (6 - (8 - (9 - -))))
--    ghci> abb2
--     (5 (2 - (4 (3 - -) -)) (8 (6 - (7 - -)) (10 (9 - -) (11 - -))))
-- abb1, abb2 :: ABB Int
-- abb1 = crea (reverse [5,2,6,4,8,3,9])
-- abb2 = foldr inserta vacio (reverse [5,2,4,3,8,6,7,10,9,11])

-- | vacio es el ABB vacío. Por ejemplo,
--
-- > ghci> vacio
-- >  -
vacio :: ABB a
vacio = Vacio

-- | (pertenece v' a) se verifica si v' es el valor de algún nodo del ABB
-- a. Por ejemplo,
--
-- > pertenece 3 abb1  ==  True
-- > pertenece 7 abb1  ==  False
pertenece :: (Ord a,Show a) => a -> ABB a -> Bool
pertenece _  Vacio = False
pertenece v' (Nodo v i d)
  | v == v'   = True
  | v' < v    = pertenece v' i
  | otherwise = pertenece v' d

-- pertenece requiere O(n) paso en el peor caso O(n) y O(log n) en el mejor,
-- donde n es el número de nodos del ABB.

-- | (inserta v a) es el árbol obtenido añadiendo el valor v al ABB a, si
-- no es uno de sus valores. Por ejemplo,
--
-- > ghci> inserta 7 abb1
-- >  (5 (2 - (4 (3 - -) -)) (6 - (8 (7 - -) (9 - -))))
inserta :: (Ord a,Show a) => a -> ABB a -> ABB a
inserta v' Vacio = Nodo v' Vacio Vacio
inserta v' (Nodo v i d)
  | v' == v   = Nodo v i d
  | v' < v    = Nodo v (inserta v' i) d
  | otherwise = Nodo v i (inserta v' d)

-- inserta requiere O(n) pasos en el peor caso y O(log n) en el mejor.

-- | (crea vs) es el ABB cuyos valores son vs. Por ejemplo,
--
-- > ghci> crea [3,7,2]
-- >  (2 - (7 (3 - -) -))
crea :: (Ord a,Show a) => [a] -> ABB a
crea = foldr inserta Vacio

-- | (crea' vs) es el ABB de menor profundidad cuyos valores son los de
-- la lista ordenada vs. Por ejemplo,
--
-- > ghci> crea' [2,3,7]
-- >  (3 (2 - -) (7 - -))
crea' :: (Ord a,Show a) => [a] -> ABB a
crea' [] = Vacio
crea' vs = Nodo x (crea' l1) (crea' l2)
  where n      = length vs `div` 2
        l1     = take n vs
        (x:l2) = drop n vs

-- | (elementos a) es la lista de los valores de los nodos del ABB en el
-- recorrido inorden. Por ejemplo,
--
-- > elementos abb1  ==  [2,3,4,5,6,8,9]
-- > elementos abb2  ==  [2,3,4,5,6,7,8,9,10,11]
elementos :: (Ord a,Show a) => ABB a -> [a]
elementos Vacio        = []
elementos (Nodo v i d) = elementos i ++ [v] ++ elementos d

-- | (elimina v a) es el ABB obtenido borrando el valor v del ABB a. Por
-- ejemplo,
--
-- > ghci> abb1
-- >  (5 (2 - (4 (3 - -) -)) (6 - (8 - (9 - -))))
-- > ghci> elimina 3 abb1
-- >  (5 (2 - (4 - -)) (6 - (8 - (9 - -))))
-- > ghci> elimina 2 abb1
-- >  (5 (4 (3 - -) -) (6 - (8 - (9 - -))))
-- > ghci> elimina 5 abb1
-- >  (6 (2 - (4 (3 - -) -)) (8 - (9 - -)))
-- > ghci> elimina 7 abb1
-- >  (5 (2 - (4 (3 - -) -)) (6 - (8 - (9 - -))))
elimina  :: (Ord a,Show a) => a -> ABB a -> ABB a
elimina _ Vacio = Vacio
elimina v' (Nodo v i Vacio) | v'==v = i
elimina v' (Nodo v Vacio d) | v'==v = d
elimina v' (Nodo v i d)
  | v' < v    = Nodo v (elimina v' i) d
  | v' > v    = Nodo v i (elimina v' d)
  | otherwise = Nodo k i (elimina k d)
  where k = menor d

-- | (menor a) es el mínimo valor del ABB a. Por ejemplo,
--
-- > menor abb1  ==  2
menor :: Ord a => ABB a -> a
menor (Nodo v Vacio _) = v
menor (Nodo _ i _)     = menor i
menor Vacio            = error "No tiene"

-- | (menorTodos v a) se verifica si v es menor que todos los elementos
-- del ABB a.
menorTodos :: (Ord a, Show a) => a -> ABB a -> Bool
menorTodos _ Vacio = True
menorTodos v a        = v < minimum (elementos a)

-- | (mayorTodos v a) se verifica si v es mayor que todos los elementos
-- del ABB a.
mayorTodos :: (Ord a, Show a) => a -> ABB a -> Bool
mayorTodos _ Vacio = True
mayorTodos v a = v > maximum (elementos a)

-- | (valido a) se verifica si a es un ABB correcto. Por ejemplo,
--
-- > valido abb1 == True
valido :: (Ord a, Show a) => ABB a -> Bool
valido Vacio = True
valido (Nodo v a b) =
  mayorTodos v a &&
  menorTodos v b &&
  valido a &&
  valido b
