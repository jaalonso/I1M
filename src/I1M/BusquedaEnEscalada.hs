-- |
-- Module      : BusquedaEnEscalada
-- Description : El patrón de búsqueda en escalada.
-- License     : Creative Commons
-- Maintainer  : José A. Alonso
-- 
-- = El patrón de búsqueda en escalada
-- 
-- Este módulo contiene la definición del patrón de búsqueda en escalada
-- estudiado en el <http://bit.ly/1LIx3CJ tema 15> del curso. 
--
-- Además, en el tema se incluye como de casos de aplicación del patrón 
--
-- * <http://bit.ly/1LIx4GQ el problema del cambio de monedas> y
-- * <http://bit.ly/1LIx5ui el algoritmo de Prim del mínimo árbol de
-- expansión>. 

module I1M.BusquedaEnEscalada (buscaEscalada) where

-- ---------------------------------------------------------------------
-- Importaciones                                                      --
-- ---------------------------------------------------------------------

import I1M.ColaDePrioridad

-- ---------------------------------------------------------------------
-- El patrón de búsqueda en escalada                                  --
-- ---------------------------------------------------------------------

-- | (buscaEscalada s o e) es la lista de soluciones del problema de espacio de
-- estado definido por la función sucesores (s), el objetivo (o) y el
-- estado inicial (e), obtenidas buscando por escalada.
buscaEscalada :: Ord nodo => 
                 (nodo -> [nodo])   -- sucesores
                 -> (nodo -> Bool)  -- es final
                 -> nodo            -- nodo actual
                 -> [nodo]          -- soluciones
buscaEscalada sucesores esFinal x = busca' (inserta x vacia) 
    where
      busca' c  
        | esVacia c           = [] 
        | esFinal (primero c) = [primero c]
        | otherwise           = 
            busca' (foldr inserta vacia (sucesores (primero c)))
