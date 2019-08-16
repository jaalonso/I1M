-- |
-- Module      : Analizador
-- Description : Analizadores sintácticos de expresiones aritméticas.
-- License     : Creative Commons
-- Maintainer  : José A. Alonso
-- 
-- = Analizadores sintácticos de expresiones aritméticas.
-- 
-- Este módulo contiene la definición de los analizadores sintácticos de
-- expresiones aritméticas estudiados en el 
-- <http://bit.ly/1IswyVV tema 12> del curso.  

module I1M.Analizador
    ( -- * El tipo de los analizadores
      Analizador
    , analiza 
    -- * Analizadores básicos
    , resultado 
    , fallo 
    , elemento 
    -- * Secuenciación
    , (>*>) 
    -- * Elección
    , (+++) 
    -- * Primitivas derivadas
    , sat 
    , digito 
    , minuscula 
    , mayuscula 
    , letra 
    , alfanumerico 
    , caracter 
    , cadena 
    , varios 
    , varios1 
    , ident 
    , nat 
    , espacio 
    -- * Tratamiento de espacios
    , unidad 
    , identificador 
    , natural 
    , simbolo 
    , listaNat 
    -- * Expresiones aritméticas
    , expr 
    , valor)
    where

import Data.Char

-- ---------------------------------------------------------------------
-- § El tipo de los analizadores                                      --
-- ---------------------------------------------------------------------

-- | El tipo de los analizadores.
type Analizador a = String -> [(a,String)]

-- | (analiza a cs) es el resultado de aplicar el analizador a a la
-- cadena cs.
analiza :: Analizador a -> String -> [(a,String)]
analiza a cs = a cs

-- ---------------------------------------------------------------------
-- § Analizadores básicos                                             --
-- ---------------------------------------------------------------------

-- | El analizador (resultado v`) siempre tiene éxito, devuelve v y no
-- consume nada. Por ejemplo,
-- 
-- > analiza (resultado 3) "Hola"  ==  [(3,"Hola")]
resultado :: a -> Analizador a
resultado v =  \ent -> [(v,ent)]

-- | El analizador @fallo@ siempre falla. Por ejemplo,
-- 
-- > analiza fallo "Hola"  ==  []
fallo :: Analizador a
fallo = \_ -> []

-- | El analizador @elemento@ falla si la cadena es vacía y consume el primer
-- elemento en caso contrario. Por ejemplo,
-- 
-- > analiza elemento "Hola"  ==  [('H',"ola")]
-- > analiza elemento ""      ==  []
elemento :: Analizador Char
elemento = \xs -> case xs of
                    [] -> []
                    (y:ys) -> [(y,ys)]

-- ---------------------------------------------------------------------
-- § Secuenciación                                                    --
-- ---------------------------------------------------------------------

-- | ((p >*> f) e) falla si el análisis de e por p falla, en caso
-- contrario, se obtiene un valor (v) y una salida (s), se aplica la
-- función f al valor v obteniéndose un nuevo analizador con el que se
-- analiza la salida s.
infixr 5 >*>

(>*>) :: Analizador a -> (a -> Analizador b) -> Analizador b
p >*> f = \ent -> case analiza p ent of
                       []        -> []
                       [(v,sal)] -> analiza (f v) sal
                       _         -> error "Imposible"

-- ---------------------------------------------------------------------
-- § Elección                                                         --
-- ---------------------------------------------------------------------

-- | ((p +++ q) e) analiza e con p y si falla analiza e con q. Por
-- ejemplo,
-- 
-- > analiza (elemento +++ resultado 'd') "abc"  ==  [('a',"bc")]
-- > analiza (fallo +++ resultado 'd') "abc"     ==  [('d',"abc")]
-- > analiza (fallo +++ fallo) "abc"             ==  []
(+++) :: Analizador a -> Analizador a -> Analizador a
p +++ q = \ent -> case analiza p ent of
                    []        -> analiza q ent
                    [(v,sal)] -> [(v,sal)]
                    _         -> error "Imposible"
                      
-- ---------------------------------------------------------------------
-- Primitivas derivadas                                               --
-- ---------------------------------------------------------------------

-- | (sat p) es el analizador que consume un elemento si dicho elemento
-- cumple la propiedad p y falla en caso contrario. Por ejemplo,
-- 
-- > analiza (sat isLower) "hola"  ==  [('h',"ola")]
-- > analiza (sat isLower) "Hola"  ==  []
sat :: (Char -> Bool) -> Analizador Char
sat p = elemento >*> \x ->
        if p x then resultado x else fallo

-- | @digito@ analiza si el primer carácter es un dígito. Por ejemplo,
--
-- > analiza digito "123"  ==  [('1',"23")]
-- > analiza digito "uno"  ==  []
digito :: Analizador Char
digito = sat isDigit

-- | @`minuscula@ analiza si el primer carácter es una letra
-- minúscula. Por ejemplo,
-- 
-- > analiza minuscula "eva"  ==  [('e',"va")]
-- > analiza minuscula "Eva"  ==  []
minuscula :: Analizador Char
minuscula = sat isLower

-- | @mayuscula@ analiza si el primer carácter es una letra
-- mayúscula. Por ejemplo, 
-- 
-- > analiza mayuscula "Eva"  ==  [('E',"va")]
-- > analiza mayuscula "eva"  ==  []
mayuscula :: Analizador Char
mayuscula = sat isUpper

-- | @letra@ analiza si el primer carácter es una letra. Por ejemplo,
-- 
-- > analiza letra "Eva"  ==  [('E',"va")]
-- > analiza letra "eva"  ==  [('e',"va")]
-- > analiza letra "123"  ==  []
letra :: Analizador Char
letra = sat isAlpha

-- | @alfanumerico@ analiza si el primer carácter es una letra o un
-- número. Por ejemplo,
-- 
-- > analiza alfanumerico "Eva"   ==  [('E',"va")]
-- > analiza alfanumerico "eva"   ==  [('e',"va")]
-- > analiza alfanumerico "123"   ==  [('1',"23")]
-- > analiza alfanumerico " 123"  ==  []
alfanumerico :: Analizador Char
alfanumerico = sat isAlphaNum

-- | @(caracter x)@ analiza si el primer carácter es igual al carácter
-- x. Por ejemplo, 
-- 
-- > analiza (caracter 'E') "Eva"  ==  [('E',"va")]
-- > analiza (caracter 'E') "eva"  ==  []
caracter :: Char -> Analizador Char
caracter x = sat (== x)

-- | @(cadena c)@ analiza si empieza con la cadena c. Por ejemplo,
-- 
-- > analiza (cadena "abc") "abcdef"  ==  [("abc","def")]
-- > analiza (cadena "abc") "abdcef"  ==  []
cadena :: String -> Analizador String
cadena []     = resultado []
cadena (x:xs) = caracter x >*> \y  ->
                cadena xs  >*> \ys ->
                resultado (y:ys)

-- | (varios p) aplica el analizador p cero o más veces. Por ejemplo,
-- 
-- > analiza (varios digito) "235abc"  ==  [("235","abc")]
-- > analiza (varios digito) "abc235"  ==  [("","abc235")]
varios :: Analizador a -> Analizador [a]
varios p  = varios1 p +++ resultado []

-- | (varios1 p) aplica el analizador p una o más veces. Por ejemplo,
-- 
-- > analiza (varios1 digito) "235abc"  ==  [("235","abc")]
-- > analiza (varios1 digito) "abc235"  ==  []
varios1 :: Analizador a -> Analizador [a]
varios1 p = p        >*> \v  ->
            varios p >*> \vs ->
            resultado (v:vs)

-- | @ident@ analiza si comienza con un identificador (i.e. una cadena que 
-- comienza con una letra minúscula seguida por caracteres
-- alfanuméricos). Por ejemplo,
-- 
-- > analiza ident "lunes12 de Ene"  ==  [("lunes12"," de Ene")]
-- > analiza ident "Lunes12 de Ene"  ==  []
ident :: Analizador String
ident =  minuscula           >*> \x  ->
         varios alfanumerico >*> \xs ->
         resultado (x:xs)

-- | @nat@ analiza si comienza con un número natural. Por ejemplo,
-- 
-- > analiza nat "14DeAbril"   ==  [(14,"DeAbril")]
-- > analiza nat " 14DeAbril"  ==  []
nat :: Analizador Int
nat = varios1 digito >*> \xs ->
      resultado (read xs)

-- | @espacio@ analiza si comienza con espacios en blanco. Por ejemplo,
-- 
-- > analiza espacio "    a b c"  ==  [((),"a b c")]
espacio :: Analizador ()
espacio = varios (sat isSpace) >*> \_ ->
          resultado ()

-- ---------------------------------------------------------------------
-- § Tratamiento de espacios                                          --
-- ---------------------------------------------------------------------

-- | (unidad p) ignora los espacios en blanco y aplica el analizador
-- `p`. Por ejemplo,
-- 
-- > analiza (unidad nat) " 14DeAbril"     ==  [(14,"DeAbril")]
-- > analiza (unidad nat) " 14   DeAbril"  ==  [(14,"DeAbril")]
unidad :: Analizador a -> Analizador a
unidad p = espacio >*> \_ ->
           p       >*> \v ->
           espacio >*> \_ ->
           resultado v

-- | @identificador@ analiza un identificador ignorando los espacios
-- delante y detrás. Por ejemplo,
-- 
-- > analiza identificador "  lunes12  de Ene"  ==  [("lunes12","de Ene")]
identificador :: Analizador String
identificador = unidad ident

-- | @natural@ analiza un número natural ignorando los espacios delante
-- y detrás. Por ejemplo, 
-- 
-- > analiza natural "  14DeAbril"  ==  [(14,"DeAbril")]
natural :: Analizador Int
natural =  unidad nat

-- | (simbolo xs) analiza la cadena `xs` ignorando los espacios delante
-- y detrás. Por ejemplo, 
-- 
-- > analiza (simbolo "abc") "  abcdef"  ==  [("abc","def")]
simbolo :: String -> Analizador String
simbolo xs =  unidad (cadena xs)

-- | @listaNat@ analiza una lista de naturales ignorando los
-- espacios. Por ejemplo,
-- 
-- > analiza listaNat " [  2,  3, 5   ]"  ==  [([2,3,5],"")]
-- > analiza listaNat " [  2,  3,]"       ==  []
listaNat :: Analizador [Int]
listaNat = simbolo "["          >*> \_ ->
           natural              >*> \n ->
           varios (simbolo ","  >*> \_ ->
                   natural)     >*> \ns ->
           simbolo "]"          >*> \_ ->
           resultado (n:ns)

-- ---------------------------------------------------------------------
-- § Expresiones aritméticas                                          --
-- ---------------------------------------------------------------------

-- | @expr@ analiza una expresión aritmética devolviendo su valor. Por
-- ejemplo, 
-- 
-- > analiza expr "2*3+5"     ==  [(11,"")]
-- > analiza expr "2*(3+5)"   ==  [(16,"")]
-- > analiza expr "2+3*5"     ==  [(17,"")]
-- > analiza expr "2*3+5abc"  ==  [(11,"abc")]
expr :: Analizador Int
expr = term                  >*> \t ->
       (simbolo "+"          >*> \_ ->
        expr                 >*> \e ->
        resultado (t+e))
       +++ (simbolo "-"      >*> \_ ->
            expr             >*> \e ->
            resultado (t-e))
       +++ resultado t

-- analiza term "2*3+5"  ==  [(6,"+5")]
term :: Analizador Int
term = factor >*> \f ->
       (simbolo "*"                >*> \_ ->
        term                       >*> \t ->
        resultado (f*t))
       +++ (simbolo "/"            >*> \_ ->
            term                   >*> \t ->
            resultado (f `div` t))
       +++ resultado f

-- analiza factor "2*3+5"      ==  [(2,"*3+5")]
-- analiza factor "(2+3)*5"    ==  [(5,"*5")]
-- analiza factor "(2+3*7)*5"  ==  [(23,"*5")]
factor :: Analizador Int
factor =  (simbolo "(" >*> \_ ->
           expr        >*> \e ->
           simbolo ")" >*> \_ ->
           resultado e)
          +++ natural

-- | (valor cs) analiza la cadena cs devolviendo su valor si es una
-- expresión aritmética y un mensaje de error en caso contrario. Por
-- ejemplo, 
-- 
-- > valor "2*3+5"      ==  11
-- > valor "2*(3+5)"    ==  16
-- > valor "2 * 3 + 5"  ==  11
-- > valor "2*3x+5y"    ==  *** Exception: entrada sin usar x+5y
-- > valor "-1"         ==  *** Exception: entrada no valida
valor :: String -> Int
valor xs = case (analiza expr xs) of
             [(n,[])]  -> n
             [(_,sal)] -> error ("entrada sin usar " ++ sal)
             []        -> error "entrada no valida"
             _         -> error "Imposible"
