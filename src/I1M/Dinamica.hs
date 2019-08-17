-- |
-- Module      : Dinamica
-- Description : El patrón de programación dinámica.
-- License     : Creative Commons
-- Maintainer  : José A. Alonso
-- 
-- = El patrón de programación dinámica
-- 
-- Este módulo contiene la definición del patrón de programación dinámica
-- estudiado en el <http://bit.ly/1LIxi0u tema 15> del curso. 
--
-- Además, en el tema se incluye como de casos de aplicación del patrón 
--
-- * <http://bit.ly/1LIxkWi la sucesión de Fibonacci>,
-- * <http://bit.ly/1LIxlth el producto de cadenas de matrices>,
-- * <http://bit.ly/1IsuUn1 los árboles binarios de búsqueda optimales>,
-- * <http://bit.ly/1IsuYDe los caminos mínimos entre todos los pares de
--   nodos de un grafo> y 
-- * < http://bit.ly/1Isv0 el problema del viajante>.

module I1M.Dinamica (module I1M.Tabla, dinamica)  where

import I1M.Tabla
import Data.Array

-- | (dinamica f r) wa la tabla de cálculo dinámica de la función f en
-- el rango r.
dinamica :: Ix i => (Tabla i v -> i -> v) -> (i,i) -> Tabla i v
dinamica calcula cotas = t
  where t = tabla [(i,calcula t i) | i <- range cotas]
