-- MonticuloPropiedades.hs
-- Propiedades del TAD montículos.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 30 de Diciembre de 2010
-- ---------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}

module MonticuloPropiedades (tests) where

import I1M.Monticulo
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

-- ---------------------------------------------------------------------
-- Generador de montículos                                            --
-- ---------------------------------------------------------------------

-- (creaMonticulo xs) es el montículo correspondiente a la lista xs. Por
-- ejemplo,
--    ghci> creaMonticulo [6,1,4,8]
--    M 1 2 (M 4 1 (M 8 1 Vacio Vacio) Vacio) (M 6 1 Vacio Vacio)
creaMonticulo :: [Int] -> Monticulo Int
creaMonticulo = foldr inserta vacio

-- genMonticulo es un generador de montículos. Por ejemplo,
--    ghci> sample genMonticulo
--    VacioM
--    M (-1) 1 (M 1 1 VacioM VacioM) VacioM
--    ...
genMonticulo :: Gen (Monticulo Int)
genMonticulo = do xs <- listOf arbitrary
                  return (creaMonticulo xs)

-- Montículo es una instancia de la clase arbitraria.
instance Arbitrary (Monticulo Int) where
    arbitrary = genMonticulo

-- genMonticulo genera montículos válidos.
prop_genMonticulo :: Monticulo Int -> Bool
prop_genMonticulo m = valido m

-- Comprobación.
--    ghci> quickCheck prop_genMonticulo
--    +++ OK, passed 100 tests.

-- monticuloNV es un generador de montículos no vacío. Por ejemplo,
--    ghci> sample monticuloNV
--    M 0 1 VacioM VacioM
--    M 1 1 (M 1 1 (M 1 1 VacioM VacioM) VacioM) VacioM
--    M 0 2 (M 1 1 VacioM VacioM) (M 2 1 VacioM VacioM)
--    M (-4) 2 (M (-3) 1 VacioM VacioM) (M 1 1 VacioM VacioM)
--    M 3 1 VacioM VacioM
--    M (-8) 1 (M (-5) 1 VacioM VacioM) VacioM
monticuloNV :: Gen (Monticulo Int)
monticuloNV = do xs <- listOf arbitrary
                 x <- arbitrary
                 return (creaMonticulo (x:xs))

-- Prop. monticuloNV genera montículos no vacío.
prop_monticuloNV :: Monticulo Int -> Property
prop_monticuloNV m =
    forAll monticuloNV (\m -> (valido m) && not (esVacio m))

-- Comprobación.
--    *Main> quickCheck prop_monticuloNV
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Propiedades
-- ---------------------------------------------------------------------

-- Propiedades de vacio   
-- --------------------

-- Propiedad. vacio es un montículo.
prop_vacio_es_monticulo :: Bool
prop_vacio_es_monticulo = 
    esVacio (vacio :: Monticulo Int)

-- Comprobación.
--    ghci> prop_vacio_es_monticulo
--    True

-- Propiedades de inserta  
-- ----------------------

-- Propiedad. inserta produce montículos válidos.
prop_inserta_es_valida :: Int -> Monticulo Int -> Bool
prop_inserta_es_valida x m =
    valido (inserta x m)

-- Comprobación.
--    ghci> quickCheck prop_inserta_es_valida
--    +++ OK, passed 100 tests.

-- Propiedad. Los montículos creados con inserta son no vacío.
prop_inserta_no_vacio :: Int -> Monticulo Int -> Bool
prop_inserta_no_vacio x m =
    not (esVacio (inserta x m))

-- Comprobación.
--    ghci> quickCheck prop_inserta_no_vacio
--    +++ OK, passed 100 tests.

-- Propiedades de resto   
-- --------------------

-- Propiedad. Al borrar el menor elemento de un montículo no vacío se
-- obtiene un montículo válido.
prop_resto_es_valida :: Monticulo Int -> Property
prop_resto_es_valida m =
    forAll monticuloNV  (\m -> valido (resto m))

-- Comprobación.
--    ghci> quickCheck prop_resto_es_valida
--    +++ OK, passed 100 tests.

-- Propiedad. El resto de (inserta x m) es m si m es el montículo vacío
-- o x es menor o igual que el menor elemento de m o (inserta x (resto m)), 
-- en caso contrario. 
prop_resto_inserta :: Int -> Monticulo Int -> Bool
prop_resto_inserta x m =
    resto (inserta x m)
    == if esVacio m || x <= menor m
       then m
       else inserta x (resto m)

-- Comprobación.
--    ghci> quickCheck prop_resto_inserta
--    +++ OK, passed 100 tests.

-- Propiedades de menor  
-- --------------------

-- Propiedad. (menor m) es el menor elemento del montículo m.
prop_menor_es_minimo :: Monticulo Int -> Bool
prop_menor_es_minimo m =
    esVacio m ||
    esVacio (resto m) ||
    menor m <= menor (resto m)

-- Comprobación.
--    ghci> quickCheck prop_menor_es_minimo
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Comprobaciones                                                     --
-- ---------------------------------------------------------------------

tests :: TestTree
tests = 
    testGroup "Propiedades del TAD monticulo"
      [ testGroup "Ejemplos"
         [ testCase "menor1" $
             menor (foldr inserta vacio [6,1,4,8])  @?=  1
         , testCase "menor2" $
             menor (foldr inserta vacio [7,5])      @?=  5
         , testCase "igualdad" $
             (foldr inserta vacio [6,1,4]) == (foldr inserta vacio [6,4,1])
               @?= True
         ]
      , testGroup "Propiedades"
         [testProperty "P1" prop_genMonticulo,
          testProperty "P2" prop_monticuloNV,
          testProperty "P3" prop_vacio_es_monticulo,
          testProperty "P4" prop_inserta_es_valida,
          testProperty "P5" prop_inserta_no_vacio,
          testProperty "P6" prop_resto_es_valida,
          testProperty "P7" prop_resto_inserta,
          testProperty "P8" prop_menor_es_minimo]]
