-- |
-- Module      : Pol
-- Description : TAD de los polinomios.
-- License     : Creative Commons
-- Maintainer  : José A. Alonso
-- 
-- = TAD (tipo abstracto de datos) de los polinomios
--
-- Este módulo contiene el código del TAD de los polinomios
-- estudiado en el <http://bit.ly/1WYZJuC tema 21> del curso.
-- 
-- En los ejemplos se usarán los siguientes polinomios:
--
-- > ejPol1, ejPol2, ejPol3:: Polinomio Int
-- > ejPol1 = consPol 4 3 (consPol 2 (-5) (consPol 0 3 polCero))
-- > ejPol2 = consPol 5 1 (consPol 2 5 (consPol 1 4 polCero))
-- > ejPol3 = consPol 4 6 (consPol 1 2 polCero)
-- > ejPol5, ejPol6, ejPol7:: Polinomio Float
-- > ejPol5 = consPol 4 3 (consPol 2 (-5) (consPol 0 3 polCero))
-- > ejPol6 = consPol 5 1 (consPol 2 5 (consPol 1 4 polCero))
-- > ejPol7 = consPol 1 2 (consPol 4 6 polCero)
--
-- Su escritura es
-- 
-- > ejPol1  ==  3*x^4 + -5*x^2 + 3
-- > ejPol2  ==  x^5 + 5*x^2 + 4*x
-- > ejPol3  ==  6*x^4 + 2*x
-- > ejPol5  ==  3.0*x^4 + -5.0*x^2 + 3.0
-- > ejPol6  ==  x^5 + 5.0*x^2 + 4.0*x
-- > ejPol7  ==  6.0*x^4 + 2.0*x

module I1M.Pol
  ( Polinomio,
    polCero,   -- Polinomio a                                         
    esPolCero, -- Polinomio a -> Bool                       
    consPol,   -- (Num a, Eq a)) => Int -> a -> Polinomio a -> Polinomio a   
    grado,     -- Polinomio a -> Int                                  
    coefLider, -- Num t => Polinomio t -> t                           
    restoPol   -- Polinomio t -> Polinomio t                          
  ) where

-- ---------------------------------------------------------------------
-- TAD de los polinomios mediante un tipo de dato algebraico.         --
-- ---------------------------------------------------------------------

-- Representamos un polinomio mediante los constructores ConsPol y
-- PolCero. Por ejemplo, el polinomio 
--    6x^4 -5x^2 + 4x -7 
-- se representa por 
--    ConsPol 4 6 (ConsPol 2 (-5) (ConsPol 1 4 (ConsPol 0 (-7) PolCero)))

-- | Tipo de dato de polinomios.
data Polinomio a = PolCero 
                 | ConsPol Int a (Polinomio a)
                 deriving Eq
             
-- ---------------------------------------------------------------------
-- Escritura de los polinomios                                        --
-- ---------------------------------------------------------------------

instance (Num a, Show a, Eq a) => Show (Polinomio a) where
    show PolCero               = "0"
    show (ConsPol 0 b PolCero) = show b
    show (ConsPol 0 b p)       = concat [show b, " + ", show p] 
    show (ConsPol 1 b PolCero) = concat [show b, "*x"]
    show (ConsPol 1 b p)       = concat [show b, "*x + ", show p] 
    show (ConsPol n 1 PolCero) = concat ["x^", show n] 
    show (ConsPol n b PolCero) = concat [show b, "*x^", show n] 
    show (ConsPol n 1 p)       = concat ["x^", show n, " + ", show p] 
    show (ConsPol n b p)       = concat [show b, "*x^", show n, " + ", show p] 

-- ---------------------------------------------------------------------
-- Ejemplos de polinomios                                             --
-- ---------------------------------------------------------------------

-- Ejemplos de polinomios con coeficientes enteros:
ejPol1, ejPol2, ejPol3:: Polinomio Int
ejPol1 = consPol 4 3 (consPol 2 (-5) (consPol 0 3 polCero))
ejPol2 = consPol 5 1 (consPol 2 5 (consPol 1 4 polCero))
ejPol3 = consPol 4 6 (consPol 1 2 polCero)

-- Comprobación de escritura:
--    > ejPol1
--    3*x^4 + -5*x^2 + 3
--    > ejPol2
--    x^5 + 5*x^2 + 4*x
--    > ejPol3
--    6*x^4 + 2*x

-- Ejemplos de polinomios con coeficientes reales:
ejPol5, ejPol6, ejPol7:: Polinomio Float
ejPol5 = consPol 4 3 (consPol 2 (-5) (consPol 0 3 polCero))
ejPol6 = consPol 5 1 (consPol 2 5 (consPol 1 4 polCero))
ejPol7 = consPol 1 2 (consPol 4 6 polCero)

-- Comprobación de escritura:
--    > ejPol5
--    3.0*x^4 + -5.0*x^2 + 3.0
--    > ejPol6
--    x^5 + 5.0*x^2 + 4.0*x
--    > ejPol7
--    6.0*x^4 + 2.0*x

-- ---------------------------------------------------------------------
-- Implementación de la especificación                                --
-- ---------------------------------------------------------------------

-- | polCero es el polinomio cero. Por ejemplo,
-- 
-- > > polCero
-- > 0
polCero :: Polinomio a
polCero = PolCero

-- | (esPolCero p) se verifica si p es el polinomio cero. Por ejemplo,
-- 
-- > esPolCero polCero  ==  True
-- > esPolCero ejPol1   ==  False
esPolCero :: Polinomio a -> Bool
esPolCero PolCero = True
esPolCero _       = False

-- | (consPol n b p) es el polinomio bx^n+p. Por ejemplo,
-- 
-- > ejPol2               ==  x^5 + 5*x^2 + 4*x
-- > consPol 3 0 ejPol2   ==  x^5 + 5*x^2 + 4*x
-- > consPol 3 2 polCero  ==  2*x^3
-- > consPol 6 7 ejPol2   ==  7*x^6 + x^5 + 5*x^2 + 4*x
-- > consPol 4 7 ejPol2   ==  x^5 + 7*x^4 + 5*x^2 + 4*x
-- > consPol 5 7 ejPol2   ==  8*x^5 + 5*x^2 + 4*x
consPol :: (Num a, Eq a) => Int -> a -> Polinomio a -> Polinomio a  
consPol _ 0 p = p
consPol n b PolCero = ConsPol n b PolCero
consPol n b (ConsPol m c p) 
    | n > m      = ConsPol n b (ConsPol m c p)
    | n < m      = ConsPol m c (consPol n b p)
    | b+c == 0   = p
    | otherwise  = ConsPol n (b+c) p

-- | (grado p) es el grado del polinomio p. Por ejemplo,
-- 
-- > ejPol3        ==  6*x^4 + 2*x
-- > grado ejPol3  ==  4
grado:: Polinomio a -> Int
grado PolCero         = 0
grado (ConsPol n _ _) = n

-- | (coefLider p) es el coeficiente líder del polinomio p. Por ejemplo,
-- 
-- > ejPol3            ==  6*x^4 + 2*x
-- > coefLider ejPol3  ==  6
coefLider:: Num t => Polinomio t -> t
coefLider PolCero         = 0
coefLider (ConsPol _ b _) = b

-- | (restoPol p) es el resto del polinomio p. Por ejemplo,
-- 
-- > ejPol3           ==  6*x^4 + 2*x
-- > restoPol ejPol3  ==  2*x
-- > ejPol2           ==  x^5 + 5*x^2 + 4*x
-- > restoPol ejPol2  ==  5*x^2 + 4*x
restoPol :: Polinomio t -> Polinomio t
restoPol PolCero         = PolCero
restoPol (ConsPol _ _ p) = p

