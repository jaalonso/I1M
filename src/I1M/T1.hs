-- |
-- Module      : T1
-- Description : Tema 1: Introducción a la programación funcional.
-- License     : Creative Commons
-- Maintainer  : José A. Alonso

-- Este módulo contiene el código del <http://bit.ly/1L87d5x Tema 1>:
-- Introducción a la programación funcional.

module I1M.T1
    ( doble
    , prop_doble
    , prop_prod_suma
    , prop_prod_suma'
    , suma
    , sumaLista
    , ordena
    )
    where

import Test.QuickCheck

-- | (doble x) es el doble de x. Por ejemplo,
-- 
-- > doble 3         == 6
-- > doble (doble 3) == 12
doble x = x + x

-- | prop_doble se verifica si para todo par de números x e y, el doble de x más
-- y es el doble de x más el doble de y. Por ejemplo, 
-- 
-- > ghci> quickCheck prop_doble
-- > +++ OK, passed 100 tests.
prop_doble x y = doble (x+y) == (doble x) + (doble y)  

-- | prop_prod_suma se verifica si para todo par de números, su producto es
-- distinto su suma. Por ejemplo,
-- 
-- > ghci> quickCheck prop_prod_suma
-- > *** Failed! Falsifiable (after 1 test):
-- > 0
-- > 0
prop_prod_suma x y = x*y /= x+y

-- | prop_prod_suma' se verifica si el producto de dos números no nulos
-- cualesquiera es distinto de su suma. Por ejemplo,
-- > ghci> quickCheck prop_prod_suma'
-- > +++ OK, passed 100 tests.
-- > ghci> quickCheck prop_prod_suma'
-- > *** Failed! Falsifiable (after 5 tests):  
-- > 2
-- > 2
prop_prod_suma' x y = 
    x /= 0 && y /= 0 ==> x*y /= x+y

-- | (suma n) es la suma de los n primeros números. Por ejemplo, 
-- 
-- > suma 4 == 10
suma n = sum [1..n]

-- | (sumaLista xs) es la suma de los elementos de xs. Por ejemplo,
-- 
-- sumaLista [2,3,7] == 12
sumaLista []     = 0
sumaLista (x:xs) = x + sumaLista xs

-- | (ordena xs) es la lista obtenida ordenando xs mediante el algoritmo
-- de ordenación rápida. Por ejemplo,
-- 
-- > ordena [4,6,2,5,3] == [2,3,4,5,6]
-- > ordena "deacb"     == "abcde"
ordena [] = []
ordena (x:xs) = 
    (ordena menores) ++ [x] ++ (ordena mayores)
    where menores = [a | a <- xs, a <= x]
          mayores = [b | b <- xs, b > x]

