En este repositorio se encuentra el código en Haskell de las librerías desarrolladas y
usadas en el curso de 
[Informática de 1º del Grado en Matemáticas](http://bit.ly/1WYZ1O9) 
de la Universidad de Sevilla.

Los pasos para instalar las librerías son:

+ Descargar el paquete [I1M](https://github.com/jaalonso/I1M/archive/master.zip)
+ Descomprimirlo (y se crea el directorio I1M-master.zip).
+ Cambiar al directorio I1M-master.
+ Ejecutar `cabal install I1M.cabal`

La documentación se puede consultar [aquí](http://jaalonso.github.io/I1M/).

Las librerías incluidas son:

+ Tipos abstractos de datos:
    + [TAD de las pilas](src/I1M/Pila.hs)   
    + [TAD de las colas](src/I1M/Cola.hs) 
    + [TAD de las colas de prioridad](src/I1M/ColaDePrioridad.hs) 
    + [TAD de los conjuntos](src/I1M/Conjunto.hs) 
    + [TAD de las tablas](src/I1M/Tabla.hs) 
    + [TAD de los árboles binarios de búaqueda](src/I1M/ArbolBin.hs) 
    + [TAD de los montículos](src/I1M/Monticulo.hs) 
    + [TAD de los polinomios](src/I1M/Pol.hs) 
    + [Operaciones con el TAD de los polinomios](src/I1M/PolOperaciones.hs) 
    + [TAD de los grafos](src/I1M/Grafo.hs) 
    + [Recorrido en profundidad](src/I1M/RecorridoEnProfundidad.hs) 
    + [Recorrido en anchura](src/I1M/RecorridoEnAnchura.hs)
+ Patrones:
    + [Divide y vencerás](src/I1M/DivideVenceras.hs) 
    + [Búsqueda en espacios de estados](src/I1M/BusquedaEnEspaciosDeEstados.hs) 
    + [Búsqueda por primero el mejor](src/I1M/BusquedaPrimeroElMejor.hs) 
    + [Búsqueda en escalada](src/I1M/BusquedaEnEscalada.hs) 
    + [Programación dinámica](src/I1M/Dinamica.hs) 
+ Analizadores:
    + [Analizadores funcionales](src/I1M/Analizador.hs) 



