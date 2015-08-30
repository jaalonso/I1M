-- TablaPropiedades.hs
-- Propiedades del TAD tablas.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 29 de Diciembre de 2010
-- ---------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}

module TablaPropiedades (tests) where

import I1M.Tabla
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

-- ---------------------------------------------------------------------
-- Generadores de tablas                                              --
-- ---------------------------------------------------------------------

-- genTabla es un generador de tablas. Por ejemplo,
--    ghci> sample genTabla
--    Tbl [(1,0)]
--    Tbl [(1,-1)]
--    Tbl [(1,0),(2,-1),(3,1),(4,1),(5,0)]
--    Tbl [(1,1),(2,-1),(3,-1),(4,3)]
--    Tbl [(1,3),(2,-5),(3,-7),(4,-2),(5,-8)]
--    Tbl [(1,16),(2,-6),(3,-13),(4,-7),(5,2),(6,11)]
--    Tbl [(1,-4),(2,-1),(3,3),(4,5)]
--    Tbl [(1,-8),(2,16),(3,32)]
genTabla :: Gen (Tabla Int Int)
genTabla = 
    do x <- arbitrary
       xs <- listOf arbitrary
       return (tabla (zip [1..] (x:xs)))

-- Las tablas son concreciones de los arbitrarios.
instance Arbitrary (Tabla Int Int) where
    arbitrary = genTabla

-- ---------------------------------------------------------------------
-- Propiedades                                                        --
-- ---------------------------------------------------------------------

-- Propiedades de modifica
-- -----------------------

-- Propiedad. Al modificar una tabla dos veces con la misma clave se
-- obtiene el mismos resultado que modificarla una vez con el último
-- valor. 
prop_modifica_modifica_1 :: Int -> Int -> Int -> Tabla Int Int -> Bool
prop_modifica_modifica_1 i v v' t =
    modifica (i,v') (modifica (i,v) t) 
    == modifica (i,v') t 

-- Comprobación.
--    ghci> quickCheck prop_modifica_modifica_1
--    +++ OK, passed 100 tests.

-- Propiedad. Al modificar una tabla con dos pares con claves distintas
-- no importa el orden en que se añadan los pares. 
prop_modifica_modifica_2 :: Int -> Int -> Int -> Int -> Tabla Int Int 
                              -> Property
prop_modifica_modifica_2 i i' v v' t =
    i /= i' ==>
    modifica (i',v') (modifica (i,v) t) 
    == modifica (i,v) (modifica (i',v') t) 

-- Comprobación.
--    ghci> quickCheck prop_modifica_modifica_2
--    +++ OK, passed 100 tests.

-- Propiedades de valor
-- --------------------

-- Propiedad. El valor de la clave i en la tabla obtenida añadiéndole el
-- par (i,v) a la tabla t es v.
prop_valor_modifica_1 :: Int -> Int -> Tabla Int Int -> Bool
prop_valor_modifica_1 i v t =
    valor (modifica (i,v) t) i == v

-- Comprobación.
--    ghci> quickCheck prop_valor_modifica_1
--    +++ OK, passed 100 tests.

-- Propiedad. Sean i y i' dos claves distintas. El valor de la clave i'
-- en la tabla obtenida añadiéndole el par (i,v) a la tabla t' (que
-- contiene la clave i') es el valor de i' en t'. 
prop_valor_modifica_2 :: Int -> Int -> Int -> Int -> Tabla Int Int 
                            -> Property
prop_valor_modifica_2 i v i' v' t =
    i /= i' ==>
    valor (modifica (i,v) t') i' == valor t' i'
    where t' = modifica (i',v') t

-- Comprobación.
--    ghci> quickCheck prop_valor_modifica_2
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Comprobaciones                                                     --
-- ---------------------------------------------------------------------

t1 = tabla [(i,f i) | i <- [1..6] ] 
     where f x | x < 3     = x
               | otherwise = 3-x
t2 = tabla [(4,89), (1,90), (2,67)]

tests :: TestTree
tests = 
    testGroup "Propiedades del TAD tablas"
      [ testGroup "Ejemplos"
         [ testCase "valo1" $ valor t1 6  @?=  -3
         , testCase "valo2" $ valor t2 2  @?=  67
         , testCase "modifica" $
             valor (modifica (6,9) t1) 6  @?=  9
         ]
      , testGroup "Propiedades"
          [testProperty "P1" prop_modifica_modifica_1,
           testProperty "P2" prop_modifica_modifica_2,
           testProperty "P3" prop_valor_modifica_1,
           testProperty "P4" prop_valor_modifica_2]]
