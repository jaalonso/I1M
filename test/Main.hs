-- Main.hs
-- Propiedades de los TAD,
-- José A. Alonso Jiménez <jalonso@us.es> y
-- Sevilla, 15 de Noviembre de 2010
-- ---------------------------------------------------------------------

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import qualified PilaPropiedades (tests)
import qualified ColaPropiedades (tests)
import qualified ColaDePrioridadPropiedades (tests)
import qualified ConjuntoPropiedades (tests)
import qualified TablaPropiedades (tests)
import qualified ArbolBinPropiedades (tests)
import qualified MonticuloPropiedades (tests)
import qualified PolPropiedades (tests)
import qualified PolOperacionesPropiedades (tests)

main :: IO ()
main = defaultMain tests

tests =
    testGroup "Comprobaciones"
    [ PilaPropiedades.tests
    , ColaPropiedades.tests  
    , ColaDePrioridadPropiedades.tests
    , ConjuntoPropiedades.tests
    , TablaPropiedades.tests
    , ArbolBinPropiedades.tests
    , MonticuloPropiedades.tests
    , PolPropiedades.tests
    , PolOperacionesPropiedades.tests
    ]
