-- |
-- Module      : RecorridoEnAnchura
-- Description : Recorrido de grafos en anchura.
-- License     : Creative Commons
-- Maintainer  : José A. Alonso
-- 
-- = Recorrido de grafos en anchura
-- 
-- En los ejemplos se usará el siguiente grafo
-- 
-- >   +---> 2 <---+
-- >   |           |
-- >   |           |
-- >   1 --> 3 --> 6 --> 5
-- >   |                 |
-- >   |                 |
-- >   +---> 4 <---------+
-- definido por
-- > g = creaGrafo D (1,6) 
-- >               [(1,2,0),(1,3,0),(1,4,0),(3,6,0),(5,4,0),(6,2,0),(6,5,0)]

module I1M.RecorridoEnAnchura (recorridoEnAnchura) where

-- ---------------------------------------------------------------------
-- Librerías auxiliares                                               --
-- ---------------------------------------------------------------------

import Data.Ix
import I1M.Grafo

-- ---------------------------------------------------------------------
-- Ejemplo de grafo                                                   --
-- ---------------------------------------------------------------------

-- g es el grafo
--    +---> 2 <---+
--    |           |
--    |           |
--    1 --> 3 --> 6 --> 5
--    |                 |
--    |                 |
--    +---> 4 <---------+
-- g = creaGrafo D (1,6) 
--               [(1,2,0),(1,3,0),(1,4,0),(3,6,0),(5,4,0),(6,2,0),(6,5,0)]

-- ---------------------------------------------------------------------
-- Recorrido en anchura con colas                                      --
-- ---------------------------------------------------------------------

-- | (recorridoEnAnchura i g) es el recorrido en anchura del grafo g
-- desde el vértice i, usando colas. Por ejemplo, 
-- 
-- > recorridoEnAnchura 1 g  ==  [1,4,3,2,6,5]
recorridoEnAnchura :: (Num p, Ix v) => v -> Grafo v p -> [v]
recorridoEnAnchura i g = reverse (ra [i] []) where 
  ra [] vis    = vis
  ra (c:cs) vis 
    | c `elem` vis = ra cs vis
    | otherwise    = ra (cs ++ adyacentes g c) (c:vis)

-- Traza del cálculo de (recorridoEnAnchura 1 g)
--    RecorridoEnAnchura 1 g
--    = ra [1]     []
--    = ra [2,3,4] [1]
--    = ra [3,4]   [2,1]
--    = ra [4,6]   [3,2,1]
--    = ra [6]     [4,3,2,1]
--    = ra [2,5]   [6,4,3,2,1]
--    = ra [5]     [6,4,3,2,1]
--    = ra [4]     [5,6,4,3,2,1]
--    = ra []      [5,6,4,3,2,1]
--    = [1,2,3,4,6,5]
