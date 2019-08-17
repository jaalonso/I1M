-- |
-- Module      : DivideVenceras
-- Description : El patrón "divide y vencerás".
-- License     : Creative Commons
-- Maintainer  : José A. Alonso
-- 
-- = El patrón "divide y vencerás"
-- 
-- La técnica "divide y vencerás" consta de los siguientes pasos:
-- 
-- 1. Dividir el problema en subproblemas menores.
-- 2. Resolver por separado cada uno de los subproblemas; si los
--    subproblemas son complejos, usar la misma técnica recursivamente;
--    si son simples, resolverlos directamente.
-- 3. Combinar todas las soluciones de los subproblemas en una solución
--    simple. 
-- 
-- Este módulo contiene la definición del patrón "divide y vencerás"
-- estudiado en el <http://bit.ly/1IstbhD tema 15> del curso. Además, en
-- el tema se incluye dos casos de aplicación del patrón:   
--
-- * <http://bit.ly/1Istd99 la ordenación por mezcla> y
-- * <http://bit.ly/1Istid7 la ordenación rápida>.

module I1M.DivideVenceras (divideVenceras) where

-- ---------------------------------------------------------------------
-- El patrón de diseño "divide y vencerás"                            --
-- ---------------------------------------------------------------------

-- La técnica "divide y vencerás" consta de los siguientes pasos:
-- 1. Dividir el problema en subproblemas menores.
-- 2. Resolver por separado cada uno de los subproblemas; si los
--    subproblemas son complejos, usar la misma técnica recursivamente;
--    si son simples, resolverlos directamente.
-- 3. Combinar todas las soluciones de los subproblemas en una solución
--    simple. 

-- | (divideVenceras ind resuelve divide combina pbInicial) resuelve el
-- problema pbInicial mediante la técnica de divide y vencerás, donde
-- 
-- * (ind pb) se verifica si el problema pb es indivisible 
-- * (resuelve pb) es la solución del problema indivisible pb
-- * (divide pb) es la lista de subproblemas de pb
-- * (combina pb ss) es la combinación de las soluciones ss de los
--      subproblemas del problema pb.
-- * pbInicial es el problema inicial
divideVenceras ::    
    (p -> Bool)     
    -> (p -> s)        
    -> (p -> [p])      
    -> (p -> [s] -> s) 
    -> p               
    -> s
divideVenceras ind resuelve divide combina = dv' where
  dv' pb
    | ind pb    = resuelve pb
    | otherwise = combina pb [dv' sp | sp <- divide pb]
