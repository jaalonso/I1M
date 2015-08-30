-- |
-- Module      : PolOperacionesPropiedades
-- Description : Propiedades de las Operaciones con el TAD de los polinomios.
-- License     : Creative Commons
-- Maintainer  : José A. Alonso

{-# LANGUAGE FlexibleInstances #-}

module PolOperacionesPropiedades (tests) where

import I1M.PolOperaciones
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

-- ---------------------------------------------------------------------
-- Suma de polinomios                                             --
-- ---------------------------------------------------------------------

-- Propiedad. El polinomio cero es el elemento neutro de la suma.
prop_neutroSumaPol :: Polinomio Int -> Bool
prop_neutroSumaPol p = 
    sumaPol polCero p == p

-- Comprobación con QuickCheck.
--    ghci> quickCheck prop_neutroSumaPol
--    OK, passed 100 tests.

-- Propiedad. La suma es conmutativa.
prop_conmutativaSuma :: Polinomio Int -> Polinomio Int -> Bool
prop_conmutativaSuma p q = 
    sumaPol p q == sumaPol q p

-- Comprobación:
--    ghci> quickCheck prop_conmutativaSuma
--    OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Producto de polinomios                                             --
-- ---------------------------------------------------------------------

-- Propiedad. El producto de polinomios es conmutativo.
prop_conmutativaProducto :: Polinomio Int -> Polinomio Int -> Bool
prop_conmutativaProducto p q = 
    multPol p q == multPol q p

-- La comprobación es
--    ghci> quickCheck prop_conmutativaProducto
--    OK, passed 100 tests.

-- El producto es distributivo respecto de la suma.
prop_distributivaProductoSuma :: Polinomio Int -> Polinomio Int 
                                 -> Polinomio Int -> Bool
prop_distributivaProductoSuma p q r =
    multPol p (sumaPol q r) == sumaPol (multPol p q) (multPol p r)

-- Comprobación:
--    ghci> quickCheck prop_distributivaProductoSuma
--    OK, passed 100 tests.

-- Propiedad. El polinomio unidad es el elemento neutro del producto.
prop_polUnidad :: Polinomio Int -> Bool
prop_polUnidad p = 
    multPol p polUnidad == p

-- Comprobación:
--    ghci> quickCheck prop_polUnidad
--    OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Derivación de polinomios                                           --
-- ---------------------------------------------------------------------

-- Propiedad. La derivada de la suma es la suma de las derivadas.
prop_derivada :: Polinomio Int -> Polinomio Int -> Bool
prop_derivada p q =
    derivada (sumaPol p q) == sumaPol (derivada p) (derivada q)

-- Comprobación
--    ghci> quickCheck prop_derivada
--    OK, passed 100 tests. 

-- ---------------------------------------------------------------------
-- Comprobaciones                                                     --
-- ---------------------------------------------------------------------

ejPol1, ejPol2, ejPol3, ejTerm:: Polinomio Int
ejPol1 = consPol 4 3 (consPol 2 (-5) (consPol 0 3 polCero))
ejPol2 = consPol 5 1 (consPol 2 5 (consPol 1 4 polCero))
ejPol3 = consPol 4 6 (consPol 1 2 polCero)
ejTerm = consPol 1 4 polCero

tests :: TestTree
tests = 
    testGroup "Propiedades de las operaciones con polinomios"
      [ testGroup "Ejemplos"
         [ testCase "termLider" $ 
             termLider ejPol2  @?= creaTermino 5 1
         , testCase "valor1" $
             valor ejPol1 0     @?=  3
         , testCase "valor2" $
             valor ejPol1 1     @?=  1
         , testCase "valor3" $
             valor ejPol1 (-2)  @?=  31
         , testCase "esRaiz1" $
             esRaiz 1 ejPol3  @?=  False
         , testCase "esRaiz2" $
             esRaiz 0 ejPol3  @?=  True
         ]
      , testGroup "Propiedades"
        [testProperty "P1" prop_neutroSumaPol,
         testProperty "P2" prop_conmutativaSuma,
         testProperty "P3" prop_conmutativaProducto,
         testProperty "P4" prop_distributivaProductoSuma,
         testProperty "P5" prop_polUnidad,
         testProperty "P6" prop_derivada]]
