-- |
-- Module      : ColaDePrioridad
-- Description : TAD de las colas de prioridad.
-- License     : Creative Commons
-- Maintainer  : José A. Alonso
-- 
-- TAD (tipo abstracto de datos) de las colas de prioridad.
--
-- Este módulo contiene el código del TAD de las colas de prioridad
-- estudiado en el <http://bit.ly/1WYZsrz tema 16> del curso.

module I1M.ColaDePrioridad
    (CPrioridad,
     vacia,   -- Ord a => CPrioridad a 
     inserta, -- Ord a => a -> CPrioridad a -> CPrioridad a 
     primero, -- Ord a => CPrioridad a -> a
     resto,   -- Ord a => CPrioridad a -> CPrioridad a
     esVacia, -- Ord a => CPrioridad a -> Bool 
     valida   -- Ord a => CPrioridad a -> Bool
    ) where

import qualified I1M.Monticulo as M

-- | Tipo de datos de las colas de prioridad.
newtype CPrioridad a = CP (M.Monticulo a)
    deriving (Eq, Show)

-- Ejemplo de cola de prioridad
--    ghci> foldr inserta vacia [3,1,7,2,9]
--    CP (M 1 2 
--          (M 2 2 
--             (M 9 1 VacioM VacioM) 
--             (M 7 1 VacioM VacioM)) 
--          (M 3 1 VacioM VacioM))
cp1 :: CPrioridad Int
cp1 = foldr inserta vacia [3,1,7,2,9]

-- | vacia es la cola de prioridad vacía. Por ejemplo,
-- 
-- > vacia  ==  CP Vacio
vacia :: Ord a => CPrioridad a 
vacia = CP M.vacio

-- | (inserta x c) añade el elemento x a la cola de prioridad c. Por ejemplo, 
-- 
-- > ghci> foldr inserta vacia [3,1,7,2,9]
-- > CP (M 1 2 
-- >       (M 2 2 
-- >          (M 9 1 VacioM VacioM) 
-- >          (M 7 1 VacioM VacioM)) 
-- >       (M 3 1 VacioM VacioM))
-- > ghci> inserta 5 (foldr inserta vacia [3,1,7,2,9])
-- > CP (M 1 2 
-- >       (M 2 2 
-- >          (M 9 1 VacioM VacioM) 
-- >          (M 7 1 VacioM VacioM)) 
-- >       (M 3 1 
-- >          (M 5 1 VacioM VacioM) VacioM))
inserta :: Ord a => a -> CPrioridad a -> CPrioridad a 
inserta v (CP c) = CP (M.inserta v c)

-- | (primero c) es la cabeza de la cola de prioridad c. Por ejemplo,
-- 
-- > primero (foldr inserta vacia [3,1,7,2,9])  ==  1
primero :: Ord a => CPrioridad a -> a
primero (CP c) = M.menor c

-- | (resto c) elimina la cabeza de la cola de prioridad c. Por ejemplo, 
--  
-- > ghci> (foldr inserta vacia [3,1,7,2,9])
-- > CP (M 1 2 
-- >       (M 2 2 
-- >          (M 9 1 VacioM VacioM) 
-- >          (M 7 1 VacioM VacioM)) 
-- >       (M 3 1 VacioM VacioM))
-- > ghci> resto (foldr inserta vacia [3,1,7,2,9])
-- > CP (M 2 2 
-- >       (M 9 1 VacioM VacioM) 
-- >       (M 3 1 
-- >          (M 7 1 VacioM VacioM) VacioM))
resto :: Ord a => CPrioridad a -> CPrioridad a
resto (CP c) = CP (M.resto c)

-- | (esVacia c) se verifica si la cola de prioridad c es vacía. Por
-- ejemplo,   
-- 
-- > esVacia (foldr inserta vacia [3,1,7,2,9])  ==  False
-- > esVacia vacia                              ==  True
esVacia :: Ord a => CPrioridad a -> Bool 
esVacia (CP c) = M.esVacio c

-- | (valida c) se verifica si c es una cola de prioridad válida. En la
-- representación mediante montículo todas las colas de prioridad son
-- válidas. 
valida :: Ord a => CPrioridad a -> Bool
valida _ = True

-- Nota:
-- * inserta usa O(log n) pasos.
-- * resto usa O(log n) pasos.
