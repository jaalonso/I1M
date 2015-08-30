-- PolPropiedades.hs
-- Propiedades del TAD de los polinomios.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 8 de Enero de 2011
-- ---------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}

module PolPropiedades (tests) where

import I1M.Pol
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

-- ---------------------------------------------------------------------
-- Generador de polinomios                                            --
-- ---------------------------------------------------------------------

-- (genPol n) es un generador de polinomios. Por ejemplo,
--    ghci> sample (genPol 1)
--    7*x^9 + 9*x^8 + 10*x^7 + -14*x^5 + -15*x^2 + -10
--    -4*x^8 + 2*x
--    -8*x^9 + 4*x^8 + 2*x^6 + 4*x^5 + -6*x^4 + 5*x^2 + -8*x
--    -9*x^9 + x^5 + -7
--    8*x^10 + -9*x^7 + 7*x^6 + 9*x^5 + 10*x^3 + -1*x^2
--    7*x^10 + 5*x^9 + -5
--    -8*x^10 + -7
--    -5*x
--    5*x^10 + 4*x^4 + -3
--    3*x^3 + -4
--    10*x
genPol :: Int -> Gen (Polinomio Int)
genPol 0 = return polCero
genPol n = do n <- choose (0,10)
              b <- choose (-10,10)
              p <- genPol (div n 2)
              return (consPol n b p) 

instance Arbitrary (Polinomio Int) where
    arbitrary = sized genPol

-- ---------------------------------------------------------------------
-- Propiedades                                                        --
-- ---------------------------------------------------------------------

-- Propiedad. polCero es el polinomio cero.
prop_polCero_es_cero :: Bool
prop_polCero_es_cero =
    esPolCero polCero

-- Comprobación.
--    ghci> quickCheck prop_polCero_es_cero
--    +++ OK, passed 100 tests.

-- Propiedad. Si n es mayor que el grado de p y b no es cero, entonces 
-- (consPol n b p) es un polinomio distinto del cero.
prop_consPol_no_cero :: Int -> Int -> Polinomio Int -> Property
prop_consPol_no_cero n b p =
    n > grado p && b /= 0  ==>  
      not (esPolCero (consPol n b p))

-- Comprobación.
--    ghci> quickCheck prop_consPol_no_cero
--    +++ OK, passed 100 tests.

-- Propiedad. (consPol (grado p) (coefLider p) (restoPol p)) es igual a p.
prop_consPol :: Polinomio Int -> Bool
prop_consPol p =
    consPol (grado p) (coefLider p) (restoPol p) == p

-- Comprobación
--    > quickCheck prop_consPol
--    +++ OK, passed 100 tests.

-- Propiedad. Si n es mayor que el grado de p y b no es cero, entonces
-- el grado de (consPol n b p) es n.
prop_grado :: Int -> Int -> Polinomio Int -> Property
prop_grado n b p = 
    n > grado p && b /= 0 ==> 
      grado (consPol n b p) == n 
    
-- Comprobación.
--    ghci> quickCheck prop_grado
--    +++ OK, passed 100 tests.

-- Propiedad. Si n es mayor que el grado de p y b no es cero, entonces
-- el coeficiente líder de (consPol n b p) es b.
prop_coefLider :: Int -> Int -> Polinomio Int -> Property
prop_coefLider n b p = 
    n > grado p && b /= 0 ==> 
      coefLider (consPol n b p) == b 
    
-- Comprobación.
--    ghci> quickCheck prop_coefLider
--    +++ OK, passed 100 tests.

-- Propiedad. Si n es mayor que el grado de p y b no es cero, entonces
-- el resto de (consPol n b p) es p.
prop_restoPol :: Int -> Int -> Polinomio Int -> Property
prop_restoPol n b p = 
    n > grado p && b /= 0 ==> 
      restoPol (consPol n b p) == p 
    
-- Comprobación.
--    ghci> quickCheck prop_restoPol
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Comprobaciones                                                     --
-- ---------------------------------------------------------------------

ejPol1, ejPol2, ejPol3:: Polinomio Int
ejPol1 = consPol 4 3 (consPol 2 (-5) (consPol 0 3 polCero))
ejPol2 = consPol 5 1 (consPol 2 5 (consPol 1 4 polCero))
ejPol3 = consPol 4 6 (consPol 1 2 polCero)

ejPol5, ejPol6, ejPol7:: Polinomio Float
ejPol5 = consPol 4 3 (consPol 2 (-5) (consPol 0 3 polCero))
ejPol6 = consPol 5 1 (consPol 2 5 (consPol 1 4 polCero))
ejPol7 = consPol 1 2 (consPol 4 6 polCero)

tests :: TestTree
tests = 
    testGroup "Propiedades del TAD polinomio"
      [ testGroup "Ejemplos"
         [ testCase "EsPolCero1" $ esPolCero polCero  @?=  True
         , testCase "EsPolCero1" $ esPolCero ejPol1   @?=  False
         , testCase "grado"      $ grado ejPol3       @?=  4
         , testCase "coefLider"  $ coefLider ejPol3   @?=  6
         ]
      , testGroup "Propiedades"
          [testProperty "P1" prop_polCero_es_cero,
           testProperty "P2" prop_consPol_no_cero,
           testProperty "P3" prop_consPol,
           testProperty "P4" prop_grado,
           testProperty "P5" prop_coefLider,
           testProperty "P6" prop_restoPol]]
