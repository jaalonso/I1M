-- ConjuntoPropiedades.hs
-- Propiedades del TAD conjuntos.
-- José A. Alonso Jiménez <jalonso@us.es>
-- ---------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}

module ConjuntoPropiedades (tests) where

import I1M.Conjunto
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

-- ---------------------------------------------------------------------
-- Generador de conjuntos                                          --
-- ---------------------------------------------------------------------

-- genConjunto es un generador de conjuntos. Por ejemplo,
--    ghci> sample genConjunto
--    {}
--    {}
--    {}
--    {3,-2,-2,-3,-2,4}
--    {-8,0,4,6,-5,-2}
--    {12,-2,-1,-10,-2,2,15,15}
--    {2}
--    {}
--    {-42,55,55,-11,23,23,-11,27,-17,-48,16,-15,-7,5,41,43}
--    {-124,-66,-5,-47,58,-88,-32,-125}
--    {49,-38,-231,-117,-32,-3,45,227,-41,54,169,-160,19}
genConjunto :: Gen (Conj Int)
genConjunto = do xs <- listOf arbitrary
                 return (foldr inserta vacio xs)

-- Los conjuntos son concreciones de los arbitrarios.
instance Arbitrary (Conj Int) where
    arbitrary = genConjunto

-- ---------------------------------------------------------------------
-- Propiedades                                          --
-- ---------------------------------------------------------------------

-- Propiedades de inserta
-- ----------------------

-- Propiedad. El número de veces que se añada un elemento a un conjunto
-- no importa.
prop_independencia_repeticiones :: Int -> Conj Int -> Bool
prop_independencia_repeticiones x c =
    inserta x (inserta x c) == inserta x c

-- Comprobación.
--    ghci> quickCheck prop_independencia_repeticiones
--    +++ OK, passed 100 tests.

-- Propiedad. El orden en que se añadan los elementos a un conjunto no
-- importa. 
prop_independencia_del_orden :: Int -> Int -> Conj Int -> Bool
prop_independencia_del_orden x y c =
    inserta x (inserta y c) == inserta y (inserta x c)

-- Comprobación.
--    ghci> quickCheck prop_independencia_del_orden
--    +++ OK, passed 100 tests.

-- Propiedades de pertenece
-- ------------------------

-- Propiedad. El conjunto vacío no tiene elementos.
prop_vacio_no_elementos:: Int -> Bool
prop_vacio_no_elementos x = 
    not (pertenece x vacio)

-- Comprobación.
--    ghci> quickCheck prop_vacio_no_elementos
--    +++ OK, passed 100 tests.

-- Propiedad. Un elemento pertenece al conjunto obtenido añadiendo x al
-- conjunto c syss es igual a x o pertenece a c.
prop_pertenece_inserta :: Int -> Int -> Conj Int -> Bool
prop_pertenece_inserta x y c =
    pertenece y (inserta x c) == (x==y) || pertenece y c

-- Comprobación.
--    ghci> quickCheck prop_pertenece_inserta
--    +++ OK, passed 100 tests.

-- Propiedades de elimina
-- ----------------------

-- Propiedad. Al eliminar cualquier elemento del conjunto vacío se
-- obtiene el conjunto vacío.
prop_elimina_vacio :: Int -> Bool
prop_elimina_vacio x =
    elimina x vacio == vacio

-- Comprobación.
--    ghci> quickCheck prop_elimina_vacio
--    +++ OK, passed 100 tests.

-- Propiedad. El resultado de eliminar x en el conjunto obtenido
-- añadiéndole x al conjunto c es c menos x, si x e y son iguales y es el
-- conjunto obtenido añadiéndole y a c menos x, en caso contrario.
prop_elimina_inserta :: Int -> Int -> Conj Int -> Bool
prop_elimina_inserta x y c =
    elimina x (inserta y c) 
    == if x == y 
       then elimina x c
       else inserta y (elimina x c)

-- Comprobación
--    ghci> quickCheck prop_elimina_inserta
--    +++ OK, passed 100 tests.

-- Propiedades de esVacio
-- ----------------------

-- Propiedad. vacio es vacío.
prop_vacio_es_vacio :: Bool
prop_vacio_es_vacio = 
    esVacio (vacio :: Conj Int)

-- Comprobación.
--    ghci> quickCheck prop_vacio_esvacio
--    +++ OK, passed 100 tests.

-- Propiedad. Los conjuntos construidos con inserta no son vacío.
prop_inserta_es_no_vacio :: Int -> Conj Int -> Bool
prop_inserta_es_no_vacio x c =
    not (esVacio (inserta x c))

-- Comprobación
--    ghci> quickCheck prop_inserta_es_no_vacio
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Comprobaciones                                                     --
-- ---------------------------------------------------------------------

c1, c2, c3 :: Conj Int
c1 = foldr inserta vacio [2,5,3,2]
c2 = foldr inserta vacio [2,3,4,5]
c3 = foldr inserta vacio [2,5]

tests :: TestTree
tests = 
    testGroup "Propiedades del TAD de conjuntos"
      [ testGroup "Ejemplos"
         [ testCase "esVacio1"   $ esVacio c1     @?= False
         , testCase "esVacio2"   $ esVacio vacio  @?= True
         , testCase "pertenece1" $ pertenece 3 c1 @?= True
         , testCase "pertenece2" $ pertenece 3 c1 @?= True
         , testCase "inserta1"   $ inserta 3 c1   @?= c1
         , testCase "inserta2"   $ inserta 4 c1   @?= c2
         , testCase "elimina1"   $ elimina 3 c1   @?= c3
         , testCase "elimina2"   $ elimina 4 c1   @?= c1
         ]
      , testGroup "Propiedades"
          [testProperty "P1" prop_vacio_es_vacio,
           testProperty "P2" prop_inserta_es_no_vacio,
           testProperty "P3" prop_independencia_repeticiones,
           testProperty "P4" prop_independencia_del_orden,
           testProperty "P5" prop_vacio_no_elementos,
           testProperty "P6" prop_pertenece_inserta,
           testProperty "P7" prop_elimina_vacio,
           testProperty "P8" prop_elimina_inserta]]
