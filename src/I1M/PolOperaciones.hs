-- |
-- Module      : PolOperaciones
-- Description : Operaciones con el TAD de los polinomios.
-- License     : Creative Commons
-- Maintainer  : José A. Alonso
-- 
-- En este módulo se definen operaciones con el TAD (tipo abstracto de
-- datos) de los polinomios estudiado en el 
-- <http://bit.ly/1WYZJuC tema 21> del curso. 
-- 
-- En los ejemplos se usarán los siguientes polinomios:
--
-- > ejPol1, ejPol2, ejPol3, ejTerm:: Polinomio Int
-- > ejPol1 = consPol 4 3 (consPol 2 (-5) (consPol 0 3 polCero))
-- > ejPol2 = consPol 5 1 (consPol 2 5 (consPol 1 4 polCero))
-- > ejPol3 = consPol 4 6 (consPol 1 2 polCero)
-- > ejTerm = consPol 1 4 polCero

module I1M.PolOperaciones
    ( module I1M.Pol
    -- * Funciones sobre términos
    , creaTermino
    , termLider
    -- * Suma de polinomios
    , sumaPol
    -- * Producto de polinomios
    , multPorTerm 
    , multPol 
    , polUnidad
    -- * Valor de un polinomio en un punto
    , valor
    -- * Verificación de raices de polinomios
    , esRaiz
    -- * Derivación de polinomios
    , derivada 
    -- * Resta de polinomios
    , restaPol) 
    where

-- ---------------------------------------------------------------------
-- Importación de librerías                                           --
-- ---------------------------------------------------------------------

import I1M.Pol

-- ---------------------------------------------------------------------
-- Ejemplos                                                           --
-- ---------------------------------------------------------------------

-- Ejemplos de polinomios con coeficientes enteros:
ejPol1, ejPol2, ejPol3, ejTerm:: Polinomio Int
ejPol1 = consPol 4 3 (consPol 2 (-5) (consPol 0 3 polCero))
ejPol2 = consPol 5 1 (consPol 2 5 (consPol 1 4 polCero))
ejPol3 = consPol 4 6 (consPol 1 2 polCero)
ejTerm = consPol 1 4 polCero

-- ---------------------------------------------------------------------
-- Funciones sobre términos
-- ---------------------------------------------------------------------

-- | (creaTermino n a) es el término a*x^n. Por ejemplo, 
-- 
-- > creaTermino 2 5  ==  5*x^2
creaTermino:: (Num t, Eq t) => Int -> t -> Polinomio t
creaTermino n a = consPol n a polCero

-- | (termLider p) es el término líder del polinomio p. Por ejemplo,
-- 
-- > ejPol2            ==  x^5 + 5*x^2 + 4*x
-- > termLider ejPol2  ==  x^5
termLider:: (Num t, Eq t) => Polinomio t -> Polinomio t
termLider p = creaTermino (grado p) (coefLider p)

-- ---------------------------------------------------------------------
-- Suma de polinomios                                                 --
-- ---------------------------------------------------------------------

-- | (sumaPol p q) es la suma de los polinomios p y q. Por ejemplo, 
-- 
-- > ejPol1                 ==  3*x^4 + -5*x^2 + 3
-- > ejPol2                 ==  x^5 + 5*x^2 + 4*x
-- > sumaPol ejPol1 ejPol2  ==  x^5 + 3*x^4 + 4*x + 3
sumaPol:: (Num a, Eq a) => Polinomio a -> Polinomio a -> Polinomio a
sumaPol p q 
    | esPolCero p = q
    | esPolCero q = p
    | n1 > n2      = consPol n1 a1 (sumaPol r1 q)
    | n1 < n2      = consPol n2 a2 (sumaPol p r2)
    | otherwise    = consPol n1 (a1+a2) (sumaPol r1 r2)
    where n1 = grado p
          a1 = coefLider p
          r1 = restoPol p
          n2 = grado q
          a2 = coefLider q
          r2 = restoPol q

-- ---------------------------------------------------------------------
-- Producto de polinomios                                             --
-- ---------------------------------------------------------------------

-- | (multPorTerm t p) es el producto del término t por el polinomio
-- p. Por ejemplo,
-- 
-- > ejTerm                     ==  4*x
-- > ejPol2                     ==  x^5 + 5*x^2 + 4*x
-- > multPorTerm ejTerm ejPol2  ==  4*x^6 + 20*x^3 + 16*x^2
multPorTerm :: (Num t, Eq t) => Polinomio t -> Polinomio t -> Polinomio t
multPorTerm term pol 
    | esPolCero pol = polCero
    | otherwise     = consPol (n+m) (a*b) (multPorTerm term r)
    where n = grado term
          a = coefLider term
          m = grado pol
          b = coefLider pol
          r = restoPol pol    

-- | (multPol p q) es el producto de los polinomios p y q. Por
-- ejemplo,
-- 
-- > ghci> ejPol1
-- > 3*x^4 + -5*x^2 + 3
-- > ghci> ejPol2
-- > x^5 + 5*x^2 + 4*x
-- > ghci> multPol ejPol1 ejPol2
-- > 3*x^9 + -5*x^7 + 15*x^6 + 15*x^5 + -25*x^4 + -20*x^3 + 15*x^2 + 12*x
multPol :: (Num a, Eq a) => Polinomio a -> Polinomio a -> Polinomio a
multPol p q
    | esPolCero p = polCero
    | otherwise    = sumaPol (multPorTerm (termLider p) q)
                             (multPol (restoPol p) q)

-- | polUnidad es el polinomio unidad. Por ejemplo, 
-- 
-- > ghci> polUnidad
-- > 1
polUnidad:: (Num t, Eq t) => Polinomio t
polUnidad = consPol 0 1 polCero

-- ---------------------------------------------------------------------
-- Valor de un polinomio en un punto                                  --
-- ---------------------------------------------------------------------

-- | (valor p c) es el valor del polinomio p al sustituir su variable por
-- c. Por ejemplo, 
-- 
-- > ejPol1             ==  3*x^4 + -5*x^2 + 3
-- > valor ejPol1 0     ==  3
-- > valor ejPol1 1     ==  1
-- > valor ejPol1 (-2)  ==  31
valor:: (Num a, Eq a) => Polinomio a -> a -> a
valor p c 
    | esPolCero p = 0
    | otherwise   =  b*c^n + valor r c
    where n = grado p
          b = coefLider p
          r = restoPol p

-- ---------------------------------------------------------------------
-- Verificación de raices de polinomios                               --
-- ---------------------------------------------------------------------

-- | (esRaiz c p) se verifica si c es una raiz del polinomio p. Por
-- ejemplo, 
-- 
-- > ejPol3           ==  6*x^4 + 2*x
-- > esRaiz 1 ejPol3  ==  False
-- > esRaiz 0 ejPol3  ==  True
esRaiz:: (Num a, Eq a) => a -> Polinomio a -> Bool
esRaiz c p = valor p c == 0

-- ---------------------------------------------------------------------
-- Derivación de polinomios                                           --
-- ---------------------------------------------------------------------

-- | (derivada p) es la derivada del polinomio p. Por ejemplo, 
-- 
-- > ejPol2           ==  x^5 + 5*x^2 + 4*x
-- > derivada ejPol2  ==  5*x^4 + 10*x + 4
derivada :: (Eq a, Num a) => Polinomio a -> Polinomio a
derivada p 
    | n == 0     = polCero
    | otherwise  = consPol (n-1) (b * fromIntegral n) (derivada r)
    where n = grado p
          b = coefLider p
          r = restoPol p

-- ---------------------------------------------------------------------
-- Resta de polinomios                                                --
-- ---------------------------------------------------------------------

-- | (resta p q) es el polinomio obtenido restándole a p el q. Por
-- ejemplo, 
-- 
-- > ejPol1                  ==  3*x^4 + -5*x^2 + 3
-- > ejPol2                  ==  x^5 + 5*x^2 + 4*x
-- > restaPol ejPol1 ejPol2  ==  -1*x^5 + 3*x^4 + -10*x^2 + -4*x + 3
restaPol :: (Num a, Eq a) => Polinomio a -> Polinomio a -> Polinomio a
restaPol p q  = 
    sumaPol p (multPorTerm (creaTermino 0 (-1)) q)
