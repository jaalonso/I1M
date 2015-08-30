-- ArbolBinPropiedades.hs
-- Generador de árboles binarios de búsqueda y propiedades del TAD.
-- José A. Alonso Jiménez <jalonso@us.es>
-- ---------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}

module ArbolBinPropiedades (tests) where

import I1M.ArbolBin
import Data.List 
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

-- ---------------------------------------------------------------------
-- Generador de ABB                                                   --
-- ---------------------------------------------------------------------

-- genABB es un generador de árboles binarios de búsqueda. Por ejemplo,
--    ghci> sample genABB
--     -
--     (1 (-1 - -) -)
--     (1 - -)
--     (-1 (-3 - -) (1 - (4 - -)))
--     -
--     (10 (-7 - -) -)
--     (1 (-9 - -) (7 (5 - -) (10 - -)))
--     ...
genABB :: Gen (ABB Int)
genABB = do xs <- listOf arbitrary
            return (foldr inserta vacio xs)

-- Los árboles binarios de búsqueda son concreciones de la clase
-- arbitraria. 
instance Arbitrary (ABB Int) where
    arbitrary = genABB

-- Propiedad. Todo los elementos generados por genABB son árboles binarios
-- de búsqueda.
prop_genABB_correcto :: ABB Int -> Bool
prop_genABB_correcto = valido 

-- Comprobación.
--    ghci> quickCheck prop_genABB_correcto
--    +++ OK, passed 100 tests.

-- listaOrdenada es un generador de listas ordenadas de números
-- enteros. Por ejemplo,
--    ghci> sample listaOrdenada
--    [1]
--    [1]
--    [-2,-1,0]
--    [-1,0,1]
--    [-8,-5,-4,-3,3,4,8]
--    [-6,-3,8]
--    [-14,-13]
--    [-31,-23,-16,-13,-11,-5,1,4,11,14,15,21,26,29]
--    []
--    []
--    []
listaOrdenada :: Gen [Int]
listaOrdenada = 
    frequency [(1,return []),
               (4,do xs <- orderedList
                     n <- arbitrary
                     return (nub ((case xs of
                                     []  -> n
                                     x:_ -> n `min` x)
                                  :xs)))]

-- (ordenada xs) se verifica si xs es una lista ordenada creciente. Por
-- ejemplo, 
--    ordenada [3,5,9]  ==  True
--    ordenada [3,9,5]  ==  False
ordenada :: [Int] -> Bool
ordenada xs = and [x<y | (x,y) <- zip xs (tail xs)]

-- Propiedad. El generador listaOrdenada produce listas ordenadas. 
prop_listaOrdenada_correcta :: [Int] -> Property
prop_listaOrdenada_correcta xs = 
    forAll listaOrdenada ordenada

-- Comprobación:
--    ghci> quickCheck prop_listaOrdenada_correcta
--    +++ OK, passed 100 tests.

-- Propiedad. Al eliminar las repeticiones en las listas producidas por el
-- generador orderedList se obtienen listas ordenadas.  
prop_orderedList_correcta :: [Int] -> Property
prop_orderedList_correcta xs = 
    forAll orderedList (ordenada . nub)

-- Comprobación:
--    ghci> quickCheck prop_orderedList_correcta
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Propiedades                                                        --
-- ---------------------------------------------------------------------

-- Propiedades de vacio
-- --------------------

-- Prop. vacio es un ABB.
prop_vacio_es_ABB :: Bool
prop_vacio_es_ABB =
    valido (vacio :: ABB Int)

-- Comprobación:
--    ghci> quickCheck prop_vacio_es_ABB
--    +++ OK, passed 100 tests.

-- Propiedades de inserta
-- ----------------------

-- Propiedad. Si a es un ABB, entonces (inserta v a) también lo es.
prop_inserta_es_valida :: Int -> ABB Int -> Bool
prop_inserta_es_valida v a =
    valido (inserta v a)

-- Comprobación:
--    ghci> quickCheck prop_inserta_es_valida
--    +++ OK, passed 100 tests.

-- Propiedad. El árbol que resulta de añadir un elemento a un ABB es no
-- vacío. 
prop_inserta_es_no_vacio :: Int -> ABB Int -> Bool
prop_inserta_es_no_vacio x a =
    inserta x a /= vacio

-- Comprobación.
--    ghci> quickCheck prop_inserta_es_no_vacio
--    +++ OK, passed 100 tests.

-- Propiedad. Para todo x y a, x es un elemento de (inserta x a). 
prop_elemento_de_inserta :: Int -> ABB Int -> Bool
prop_elemento_de_inserta x a =
    pertenece x (inserta x a)

-- Comprobación:
--    ghci> quickCheck prop_elemento_de_inserta
--    +++ OK, passed 100 tests.

-- Propiedades de pertenece
-- ------------------------

-- Propiedad. En en árbol vacio no hay ningún elemento.
prop_vacio_sin_elementos :: Int -> Bool
prop_vacio_sin_elementos x =
    not (pertenece x vacio)

-- Comprobación:
--    ghci> quickCheck prop_vacio_sin_elementos
--    +++ OK, passed 100 tests.

-- Propiedad. Los elementos de (inserta x a) son x y los elementos de
-- a. 
prop_elementos_de_inserta :: Int -> Int -> ABB Int -> Bool
prop_elementos_de_inserta x y a =
    pertenece y (inserta x a) == (x == y) || pertenece y a

-- Comprobación.
--    ghci> quickCheck prop_elementos_de_inserta
--    +++ OK, passed 100 tests.

-- Propiedades de elimina
-- ----------------------

-- Propiedad. Si a es un ABB, entonces (elimina v a) también lo es.
prop_elimina_es_valida :: Int -> ABB Int -> Bool
prop_elimina_es_valida v a = 
    valido (elimina v a)

-- Comprobación:
--    ghci> quickCheck prop_elimina_es_valida
--    +++ OK, passed 100 tests.

-- Prop. El resultado de eliminar el elemento x en (inserta x a) es
-- (elimina x a). 
prop_elimina_agrega :: Int -> ABB Int -> Bool
prop_elimina_agrega x a =
    elimina x (inserta x a) == elimina x a

-- Comprobación
--    ghci> quickCheck prop_elimina_agrega
--    +++ OK, passed 100 tests.

-- Propiedades de crea
-- -------------------

-- Propiedad. (crea xs) es un ABB.
prop_crea_es_valida :: [Int] -> Bool
prop_crea_es_valida xs =
    valido (crea xs)

-- Comprobación:
--    ghci> quickCheck prop_crea_es_valida
--    +++ OK, passed 100 tests.

-- Propiedades de crea'
-- --------------------

-- Propiedad. Para todas las listas ordenadas xs, se tiene que (crea' xs)
-- es un ABB.
prop_crea'_es_valida :: [Int] -> Property
prop_crea'_es_valida xs =
    forAll listaOrdenada (valido . crea')

-- Comprobación:
--    ghci> quickCheck prop_crea'_es_valida
--    +++ OK, passed 100 tests.

-- Propiedades de elementos
-- ------------------------

-- Propiedad. (elementos (crea xs)) es igual a la lista xs ordenada y
-- sin repeticiones. 
prop_elementos_crea :: [Int] -> Bool
prop_elementos_crea xs =
    elementos (crea xs) == sort (nub xs)

-- Comprobación
--    ghci> quickCheck prop_elementos_crea
--    +++ OK, passed 100 tests.

-- Propiedad. Si ys es una lista ordenada sin repeticiones, entonces
-- (elementos (crea' ys)) es igual ys. 
prop_elementos_crea' :: [Int] -> Bool
prop_elementos_crea' xs =
    elementos (crea' ys) == ys
    where ys = sort (nub xs)

-- Comprobación
--    ghci> quickCheck prop_elementos_crea'
--    +++ OK, passed 100 tests.

-- Propiedad. Un elemento pertenece a (elementos a) syss es un valor de a.
prop_en_elementos :: Int -> ABB Int -> Bool
prop_en_elementos v a =
    pertenece v a == elem v (elementos a)

-- Comprobación:
--    ghci> quickCheck prop_enElementos''
--    +++ OK, passed 100 tests.

-- Propiedades de menor
-- --------------------

-- Propiedad. (menor a) es menor o igual que todos los elementos de ABB
-- a. 
prop_menoresMinimo ::Int -> ABB Int -> Bool
prop_menoresMinimo v a =
    and [menor a <= v | v <- elementos a]

-- Comprobación.
--    ghci> quickCheck prop_menoresMinimo
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Comprobaciones                                                     --
-- ---------------------------------------------------------------------

abb1, abb2 :: ABB Int
abb1 = crea (reverse [5,2,6,4,8,3,9])
abb2 = foldr inserta vacio (reverse [5,2,4,3,8,6,7,10,9,11])

tests :: TestTree
tests = 
    testGroup "Propiedades del TAD árboles binarios"
      [ testGroup "Ejemplos"
         [ testCase "pertenece1" $ pertenece 3 abb1  @?= True
         , testCase "pertenece2" $ pertenece 7 abb1  @?= False
         , testCase "elementos1" $ elementos abb1    @?= [2,3,4,5,6,8,9]
         , testCase "elementos2" $ elementos abb2    @?= [2,3,4,5,6,7,8,9,10,11]
         , testCase "menor"      $ menor abb1        @?= 2
         , testCase "valido"     $ valido abb1       @?= True
         ]
      , testGroup "Propiedades"
          [testProperty "P1"  prop_listaOrdenada_correcta,
           testProperty "P2"  prop_orderedList_correcta,
           testProperty "P3"  prop_vacio_es_ABB,
           testProperty "P4"  prop_inserta_es_valida,
           testProperty "P5"  prop_inserta_es_no_vacio,
           testProperty "P6"  prop_elemento_de_inserta,
           testProperty "P7"  prop_vacio_sin_elementos,
           testProperty "P8"  prop_elementos_de_inserta,
           testProperty "P9"  prop_elimina_es_valida,
           testProperty "P10" prop_elimina_agrega,
           testProperty "P11" prop_crea_es_valida,
           testProperty "P12" prop_crea'_es_valida,
           testProperty "P13" prop_elementos_crea,
           testProperty "P14" prop_elementos_crea',
           testProperty "P15" prop_en_elementos,
           testProperty "P16" prop_menoresMinimo,
           testProperty "P17" prop_genABB_correcto]]
