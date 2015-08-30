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
divideVenceras ind resuelve divide combina pbInicial 
    = dv' pbInicial
    where dv' pb
              | ind pb    = resuelve pb
              | otherwise = combina pb [dv' sp | sp <- divide pb]

-- ---------------------------------------------------------------------
-- Ordenación por mezcla                                              --
-- ---------------------------------------------------------------------

-- (ordenaPorMezcla xs) es la lista obtenida ordenando xs por el
-- procedimiento de ordenación por mezcla. Por ejemplo,

--    ghci> ordenaPorMezcla [3,1,4,1,5,9,2,8]
--    [1,1,2,3,4,5,8,9]
ordenaPorMezcla :: Ord a => [a] -> [a]
ordenaPorMezcla xs = 
    divideVenceras ind id divide combina xs
    where 
      ind xs            = length xs <= 1
      divide xs         = [take n xs, drop n xs]
                          where n = length xs `div` 2
      combina _ [l1,l2] = mezcla l1 l2

-- (mezcla xs ys) es la lista obtenida mezclando xs e ys. Por ejemplo,
--    mezcla [1,3] [2,4,6]  ==  [1,2,3,4,6]
mezcla :: Ord a => [a] -> [a] -> [a]
mezcla [] b = b
mezcla a [] = a
mezcla a@(x:xs) b@(y:ys) 
    | x <= y    = x : (mezcla xs b)
    | otherwise = y : (mezcla a ys)

-- ---------------------------------------------------------------------
-- Ordenación rápida                                                  --
-- ---------------------------------------------------------------------

-- (ordenaRapida xs) es la lista obtenida ordenando xs por el
-- procedimiento de ordenación rápida. Por ejemplo,
--    ghci> ordenaRapida [3,1,4,1,5,9,2,8]
--    [1,1,2,3,4,5,8,9]
ordenaRapida :: Ord a => [a] -> [a]
ordenaRapida xs = 
    divideVenceras ind id divide combina xs
    where 
      ind xs                = length xs <= 1
      divide (x:xs)         = [[ y | y<-xs, y<=x],
                               [ y | y<-xs, y>x] ]
      combina (x:_) [l1,l2] = l1 ++ [x] ++ l2

