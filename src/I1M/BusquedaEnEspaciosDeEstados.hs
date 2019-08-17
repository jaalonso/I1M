-- |
-- Module      : BusquedaEnEspaciosDeEstados
-- Description : El patrón de búsqueda en espacios de estados.
-- License     : Creative Commons
-- Maintainer  : José A. Alonso
-- 
-- = El patrón de búsqueda en espacios de estados
-- 
-- Las características de los problemas de espacios de estados son:
-- 
-- * un conjunto de las posibles situaciones o nodos que constituye
--   el espacio de estados; estos son las potenciales soluciones que se
--   necesitan explorar;
-- * un conjunto de movimientos de un nodo a otros nodos, llamados los
--   sucesores del nodo; 
-- * un nodo inicial;
-- * un nodo objetivo, que es la solución.
--
-- Este módulo contiene la definición del patrón de búsqueda en espacios
-- de estados estudiado en el <http://bit.ly/1LIvQeO tema 15> del
-- curso. Además, en el tema se incluye dos casos de aplicación del patrón:  
--
-- * <http://bit.ly/1LIvV1R el problema de las n reinas> y
-- * <http://bit.ly/1LIvXqE el problema de la mochila>.

module I1M.BusquedaEnEspaciosDeEstados (buscaEE) where

-- ---------------------------------------------------------------------
-- Importaciones                                                      --
-- ---------------------------------------------------------------------

import I1M.Pila

-- ---------------------------------------------------------------------
-- Descripción de los problemas de espacios de estados                --
-- ---------------------------------------------------------------------

-- Las características de los problemas de espacios de estados son:
-- * un conjunto de las posibles situaciones o nodos que constituye
--   el espacio de estados; estos son las potenciales soluciones que se
--   necesitan explorar;
-- * un conjunto de movimientos de un nodo a otros nodos, llamados los
--   sucesores del nodo; 
-- * un nodo inicial;
-- * un nodo objetivo, que es la solución.

-- ---------------------------------------------------------------------
-- El patrón de búsqueda en espacios de estados                       --
-- ---------------------------------------------------------------------

-- Nota: Se supone que el grafo implícito de espacios de estados es
-- acíclico. 

-- | (buscaEE s o e) es la lista de soluciones del problema de espacio de
-- estado definido por la función sucesores (s), el objetivo (o) y el
-- estado inicial (e).
buscaEE:: (Eq nodo) => (nodo -> [nodo])    -- sucesores
                       -> (nodo -> Bool)   -- esFinal
                       -> nodo             -- nodo actual
                       -> [nodo]           -- soluciones
buscaEE sucesores esFinal x = busca' (apila x vacia) 
 where
   busca' p  
     | esVacia p        = [] 
     | esFinal (cima p) = cima p : busca' (desapila p)
     | otherwise        = busca' (foldr apila (desapila p) (sucesores (cima p)))
