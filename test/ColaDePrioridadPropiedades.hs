-- ColaDePrioridadPropiedades.hs
-- Propiedades del TAD de colas de prioridad.
-- José A. Alonso Jiménez <jalonso@us.es>
-- ---------------------------------------------------------------------

module ColaDePrioridadPropiedades (tests) where

import I1M.ColaDePrioridad
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

-- ---------------------------------------------------------------------
-- Generador de colas de prioridad
-- ---------------------------------------------------------------------

-- genCPrioridad es un generador de colas de prioridad. Por ejemplo,
--    ghci> sample genCPrioridad
--    CP []
--    CP []
--    CP [-4]
--    CP [-2,-1,-1,2,5]
--    CP [-8,-5,4,6,8]
--    CP [-4,-1,3,3,6,7,10,10,10,10]
--    CP [-12,-10,-9,-7,2,4,6,7,7,9]
--    CP [-14,-12,-12,-7,-7,-4,4,9,14]
--    CP [-10,-9,-5,14]
--    CP [18]
--    CP [-19,-17,-16,-15,-13,-13,-13,-12,-6,-5,-3,0,2,3,4,5,8,18]
genCPrioridad :: (Arbitrary a, Num a, Ord a) =>  Gen (CPrioridad a)
genCPrioridad = do xs <- listOf arbitrary
                   return (foldr inserta vacia xs)

-- La colas de prioridad son una concreción de la clase arbitraria. 
instance (Arbitrary a, Num a, Ord a) => Arbitrary (CPrioridad a) where
    arbitrary = genCPrioridad

-- Prop.: Las colas de prioridad producidas por genCPrioridad son
-- válidas. 
prop_genCPrioridad_correcto ::  CPrioridad Int -> Bool
prop_genCPrioridad_correcto c = valida c

-- Comprobación.
--    ghci> quickCheck prop_genCPrioridad_correcto
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Propiedades
-- ---------------------------------------------------------------------

-- Propiedad. Si se añade dos elementos a una cola de prioridad se
-- obtiene la misma cola de prioridad idependientemente del orden en
-- que se añadan los elementos.
prop_inserta_conmuta :: Int -> Int -> CPrioridad Int -> Bool
prop_inserta_conmuta x y c =
    inserta x (inserta y c) == inserta y (inserta x c)

-- Comprobación.
--    ghci> quickCheck prop_inserta_conmuta
--    +++ OK, passed 100 tests.

-- Propiedad. La cabeza de la cola de prioridad obtenida anadiendo un
-- elemento x a la cola de prioridad vacía es x.
prop_primero_inserta_vacia :: Int -> CPrioridad Int -> Bool
prop_primero_inserta_vacia x c =
    primero (inserta x vacia) == x

-- Comprobación.
--    ghci> quickCheck prop_primero_inserta_vacia
--    +++ OK, passed 100 tests.
 
-- Propiedad. El primer elemento de una cola de prioridad c no cambia
-- cuando se le añade un elemento mayor o igual que algún elemento de c. 
prop_primero_inserta :: Int -> Int -> CPrioridad Int -> Property
prop_primero_inserta x y c =
    x <= y ==> 
    primero (inserta y c') == primero c'
    where c' = inserta x c

-- Comprobación.
--    ghci> quickCheck prop_primero_inserta
--    +++ OK, passed 100 tests.

-- Propiedad. El resto de añadir un elemento a la cola de prioridad
-- vacía es la cola vacía. 
prop_resto_inserta_vacia :: Int -> Bool
prop_resto_inserta_vacia x =
    resto (inserta x vacia) == vacia

-- Comprobación.
--    ghci> quickCheck prop_resto_inserta_vacia
--    +++ OK, passed 100 tests.

-- Propiedad. El resto de la cola de prioridad obtenida añadiendo un
-- elemento y a una cola c' (que tiene algún elemento menor o igual que
-- y) es la cola que se obtiene añadiendo y al resto de c'.
prop_resto_inserta :: Int -> Int -> CPrioridad Int -> Property
prop_resto_inserta x y c =
    x <= y ==> 
    resto (inserta y c') == inserta y (resto c')
    where c' = inserta x c

-- Comprobación:
--    ghci> quickCheck prop_resto_inserta
--    +++ OK, passed 100 tests.

-- Propiedad. vacia es una cola vacía.
prop_vacia_es_vacia :: Bool
prop_vacia_es_vacia = esVacia (vacia :: CPrioridad Int)

-- Comprobación.
--    ghci> quickCheck prop_vacia_es_vacia
--    +++ OK, passed 100 tests.

-- Propiedad. Si se añade un elemento a una cola de prioridad se obtiene
-- una cola no vacía.
prop_inserta_no_es_vacia :: Int -> CPrioridad Int -> Bool
prop_inserta_no_es_vacia x c =
    not (esVacia (inserta x c))

-- Comprobación.
--    ghci> quickCheck prop_inserta_no_es_vacia
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Comprobaciones                                                     --
-- ---------------------------------------------------------------------

tests :: TestTree
tests = 
    testGroup "Propiedades del TAD de colas de prioridad"
      [ testGroup "Ejemplos"
         [ testCase "primero" $
             primero (foldr inserta vacia [3,1,7,2,9])  
               @?=  1
         , testCase "resto" $
             resto (foldr inserta vacia [3,1,7,2,9])
               @?= foldr inserta vacia [3,7,2,9]
         , testCase "esVacia1" $
             esVacia (foldr inserta vacia [3,1,7,2,9])  
               @?= False
         , testCase "esVacia2" $
             esVacia (vacia :: CPrioridad Int)
               @?= True
         ]
      , testGroup "Propiedades"
          [testProperty "P1" prop_inserta_conmuta,
           testProperty "P2" prop_primero_inserta_vacia,
           testProperty "P3" prop_primero_inserta,
           testProperty "P4" prop_resto_inserta_vacia,
           testProperty "P5" prop_resto_inserta,
           testProperty "P6" prop_vacia_es_vacia,
           testProperty "P7" prop_inserta_no_es_vacia
          ]]

