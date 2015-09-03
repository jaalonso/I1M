-- |
-- Module      : T2
-- Description : Tema 2: Introducción a la programación con Haskell.
-- License     : Creative Commons
-- Maintainer  : José A. Alonso

-- Este módulo contiene el código del <http://bit.ly/1hVU8p8 Tema 2>:
-- Introducción a la programación con Haskell.  

module I1M.T2
    ( doble
    , cuadruple
    , factorial
    , media
    )
    where

-- | (doble x) es el doble de x. Por ejemplo,
--
-- > doble 3 == 6
doble x     = x+x

-- | (cuadruple x) es el cuadruple de x. Por ejemplo,
-- 
-- > cuadruple 3 == 12
cuadruple x = doble (doble x)

-- | (factorial x) es el factorial de x. Por ejemplo,
-- 
-- > factorial 4 == 24
factorial n = product [1..n]

-- | (media ns) es la media aritmética de la lista de números ns. Por
-- ejemplo,
-- 
-- > media [1,5,3]  ==  3
media ns    = sum ns `div` length ns
