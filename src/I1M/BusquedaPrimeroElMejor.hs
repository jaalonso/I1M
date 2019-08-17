-- |
-- Module      : BusquedaPrimeroElMejor
-- Description : El patrón de búsqueda por primero el mejor.
-- License     : Creative Commons
-- Maintainer  : José A. Alonso
-- 
-- = El patrón de búsqueda por primero el mejor
-- 
-- Este módulo contiene la definición del patrón de búsqueda por primero
-- el mejor estudiado en el <http://bit.ly/1IstIjL tema 15> del
-- curso. 
--
-- Además, en el tema se incluye como de aplicación del patrón el 
-- <http://bit.ly/1IstOI7 problema del 8-puzzle>.

module I1M.BusquedaPrimeroElMejor (buscaPM)  where

-- ---------------------------------------------------------------------
-- Importaciones                                                      --
-- ---------------------------------------------------------------------

import I1M.ColaDePrioridad

-- ---------------------------------------------------------------------
-- Búsqueda por primero el mejor                                      --
-- ---------------------------------------------------------------------

-- | (buscaPM s o e) es la lista de soluciones del problema de espacio de
-- estado definido por la función sucesores (s), el objetivo (o) y el
-- estado inicial (e), obtenidas buscando por primero el mejor.
buscaPM :: (Ord nodo) => 
           (nodo -> [nodo])   -- sucesores
           -> (nodo -> Bool)  -- esFinal
           -> nodo            -- nodo actual
           -> [nodo]          -- solución
buscaPM sucesores esFinal x = busca' (inserta x vacia)
 where
   busca' c 
     | esVacia c = []
     | esFinal (primero c)  = primero c : busca' (resto c)
     | otherwise = busca' (foldr inserta (resto c) (sucesores (primero c)))
