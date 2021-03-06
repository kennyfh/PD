-- PD-Práctica 8.5
-- Tipos: definición y uso de tipos (exámenes antiguos)
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Ejercicios 1 y 2. Realice los dos primeros ejercicios del examen cuyo
-- enunciado se encuentra en la url:
-- http://www.cs.us.es/cursos/pd-2011/examenes/Febrero_control_3/examen-P1-P5.pdf
-- ---------------------------------------------------------------------
{-
Ejercicio 1
Las Torres de Hanoi es un juego matemático. Consiste en tres varillas verticales y
un número indeterminado de discos que determinarán la complejidad de la solución.
No hay dos discos iguales, están colocados de mayor a menor en la primera varilla
ascendentemente, y no se puede colocar ningún disco mayor sobre uno menor a él
en ningún momento. El juego consiste en pasar todos los discos a la tercera varilla
colocados de mayor a menor ascendentemente.

En este ejercicio representamos las varillas del juego con las cadenas "I", "C" y
"D" y los tamaños de los n discos del juego con los números enteros de 1 a n.
Definir el tipo de datos PilaDeDiscos como un sinónimo de una lista de números
de tipo Int y el tipo de datos Varilla como un sinónimo de cadena.
Definir un nuevo tipo de datos TorreDeHanoi que tenga un único constructor
con tres argumentos que sean del tipo PilaDeDiscos. Siempre asumiremos que al
construir un valor de este tipo de datos las pilas de discos proporcionadas son
correctas.

Definir la función moverDisco que reciba una TorreDeHanoi y dos Varillas y
devuelva la TorreDeHanoi resultante de mover un disco de la primera a la segunda
Varilla proporcionadas. Siempre asumiremos que todos los argumentos recibidos
son correctos y que el movimiento se puede realizar.
-}

type PilaDeDiscos = [Int]
type Varilla = String 
data TorreDeHanoi =  Torre PilaDeDiscos PilaDeDiscos PilaDeDiscos
                    deriving Show

x:: TorreDeHanoi
x = Torre [1,2,3] [4,5] [6,7]

moverDisco :: TorreDeHanoi -> Varilla -> Varilla ->TorreDeHanoi

moverDisco (Torre a b c) v1 v2 
    | v1 == "A" && v2 =="B" = (Torre (init a) (b++[last a]) (c))
    | v1 == "A" && v2 == "C" = (Torre (init a) b (c++[last a]))
    | v1 == "B" && v2 == "A" = (Torre (a ++[last  b]) (init b) c)
    | v1 == "B" && v2 == "C" = (Torre a  (init b) (c ++[last  b]))
    | v1 == "C" && v2 == "A" = (Torre (a++[last  c]) b (init c) )
    | v1 == "C" && v2 == "B" = (Torre a (b ++[last  c]) (init c) )

{-
Ejercicio 2
Consideremos la siguiente definición de un nuevo tipo de dato que representa de
manera recursiva los polinomios con coeficientes enteros:
type Termino a = ( a , a )
data Polinomio a = PolCero | Pol ( Termino a ) ( Polinomio a )
deriv ing Show
Es decir, un polinomio con coeficientes enteros es el polinomio cero (0x
0
) o un
polinomio obtenido añadiendo un nuevo término con un cierto coeficiente entero y
un cierto grado a un polinomio ya existente. Siempre asumiremos que no añadimos
un término de grado igual a uno del polinomio ya existente, con la única excepción
de que el término sea de grado 0, que se podrá añadir si el único término de grado
0 del polinomio ya existente es 0x0

Por otra parte, los términos no tienen por qué
añadirse en orden creciente de grado.
Definir la función grado que reciba un Polinomio y devuelva el grado de ese
polinomio. Siempre asumiremos que el polinomio recibido está construido de manera
correcta.
-}

type Termino a = ( a , a )
data Polinomio a = PolCero | Pol ( Termino a ) ( Polinomio a )
    deriving Show

-- type PolinomioS  = Polinomio Int

ejemplo :: Polinomio Int
ejemplo = Pol (2,3) (Pol (1,5) (Pol (2,12) PolCero))

grado :: Polinomio Int -> Int
grado (PolCero) = 0
grado (Pol a b ) = maximum [snd a, grado b]
-- ---------------------------------------------------------------------
-- Ejercicios 3 y 4. Realice los dos primeros ejercicios del examen cuyo
-- enunciado se encuentra en la url:
-- http://www.cs.us.es/cursos/pd-2011/examenes/Febrero_control_3/examen-P2-P6.pdf
-- ---------------------------------------------------------------------
{-
Ejercicio 1
En la orilla izquierda de un río se encuentran un cierto número de misioneros y
de caníbales que pretenden cruzar a la orilla derecha. Para ello disponen de una
barca con una capacidad limitada. El problema consiste en trasladar a todos los
misioneros y caníbales teniendo en cuenta que nunca pueden quedar en una orilla
un número de misioneros en menor cantidad que de caníbales.
Definir el tipo de datos Orilla como un sinónimo de una tupla de dos números de
tipo Int y el tipo de datos Barca como un sinónimo de una cadena.
Definir un nuevo tipo de datos MisionerosYCanibales que tenga un único constructor
con dos argumentos que sean del tipo Orilla y un argumento que sea del tipo Barca.
Este tipo de datos representa un posible estado del problema, de tal forma que
los dos primeros argumentos representan el número de misioneros y de caníbales
(en ese orden) que hay en las orillas izquierda y derecha, respectivamente, del río
y el tercer argumento representa la orilla ("Izquierda" o "Derecha") en la que se
encuentra la barca. Siempre asumiremos que al construir un valor de este tipo de
datos los argumentos proporcionados son correctos.
Definir la función atravesarRio que reciba un estado de MisionerosYCanibales y
dos enteros de tipo Int (el número de misioneros y de caníbales que atraviesan
el río) y devuelva el estado de MisionerosYCanibales resultante de hacer que los
misioneros y los caníbales indicados atraviesen el río. Siempre asumiremos que
todos los argumentos recibidos son correctos y que el movimiento se puede realizar.
-}

type Orilla = (Int,Int)
type Barca =  String
data MisionerosYCanibales = MYC (Orilla,Orilla,Barca) --  (izq,der,pos barca)
    deriving Show

atravesarRio ::  MisionerosYCanibales -> Int -> Int -> MisionerosYCanibales
atravesarRio (MYC ((m1,c1),(m2,c2) , barca)) i1 i2
    | barca == "Izquierda" = (MYC ((m1-i1,c1-i2),(m2+i1,c2+i2),"Derecha"))
    | barca == "Derecha" = (MYC ((m1+i1,c1+i2),(m2-i1,c2-i2),"Izquierda"))

ej3 :: MisionerosYCanibales
ej3 = MYC ((40,20),(10,10),"Izquierda")

{-
Ejercicio 2

Ejercicio 2
Consideremos la siguiente definición de un nuevo tipo de dato que representa de
manera recursiva los polinomios con coeficientes enteros:
type Termino a = ( a , a )
data Polinomio a = PolCero | Pol ( Termino a ) ( Polinomio a )
deriving Show
Es decir, un polinomio con coeficientes enteros es el polinomio cero (0x
0
) o un
polinomio obtenido añadiendo un nuevo término con un cierto coeficiente entero y
un cierto grado a un polinomio ya existente. Siempre asumiremos que no añadimos
un término de grado igual a uno del polinomio ya existente, con la única excepción
de que el término sea de grado 0, que se podrá añadir si el único término de grado
0 del polinomio ya existente es 0x0

Por otra parte, los términos no tienen por qué
añadirse en orden creciente de grado.
Definir la función coeficiente que reciba un Polinomio y un número entero y
devuelva el coeficiente del término del polinomio cuyo grado es el especificado. Si el
polinomio no tiene ningún término con ese grado, entonces la función debe devolver
el número 0. Siempre asumiremos que el polinomio recibido está construido de
manera correcta.
-}

coeficiente :: Polinomio Int -> Int -> Int
coeficiente (Pol (t1,pol) t2) a
        | pol == a = t1
        | otherwise = coeficiente t2 a
coeficiente _ a = 0
-- ---------------------------------------------------------------------
-- Ejercicios 5 y 6. Realice los dos primeros ejercicios del examen cuyo
-- enunciado se encuentra en la url:
-- http://www.cs.us.es/cursos/pd-2011/examenes/Febrero_control_3/examen-P3-P7.pdf
-- ---------------------------------------------------------------------
{-
Ejercicio 1
Se dispone de dos jarras, sin marcas de medición, pero de las que se sabe sus
capacidades máximas. Hay un grifo que permite llenar las jarras completamente de
agua. También se puede verter el contenido de una jarra en la otra, bien quedando
vacía la jarra desde la que se vierte el agua, bien quedando llena la jarra en la que
se vierte el agua. El problema consiste en obtener una cantidad prefijada de agua
en una de las jarras.

Definir el tipo de datos Jarra como un sinónimo de una tupla de dos números
de tipo Int (representando el contenido y la capacidad máxima de una jarra) y
el tipo de datos Vertido como un sinónimo de una cadena ("1 a 2" o "2 a 1",
representando el vertido del contenido de una jarra en la otra).

Definir un nuevo tipo de datos ProblemaDeLasJarras que tenga un único constructor
con dos argumentos que sean del tipo Jarra. Este tipo de datos representa un posible
estado del problema. Siempre asumiremos que al construir un valor de este tipo de
datos los argumentos proporcionados son correctos.

Definir la función verter que reciba un estado del ProblemaDeLasJarras y un
Vertido a realizar y devuelva el estado del ProblemaDeLasJarras resultante de
realizar el vertido indicado. Siempre asumiremos que todos los argumentos recibidos
son correctos. Para simplificar los cálculos, también admitiremos como definición
válida una en la que la jarra que vierte su contenido siempre quede vacía (es decir,
el contenido sobrante del vertido se desecha).
-}
type Jarra = (Int,Int) -- (Contenido, Capacidad maxima de la jarra)
type Vertido = String
data ProblemaDeLasJarras = ProbJarras (Jarra,Jarra)
    deriving Show

verter :: ProblemaDeLasJarras -> Vertido -> ProblemaDeLasJarras
verter (ProbJarras ((c1,cap1),(c2,cap2))) v  
    | v == "1 a 2" = ProbJarras ((0,cap1),(min (c1+c2) cap2,cap2))
    | v == "2 a 1" =  ProbJarras ((min (c1+c2) cap1,cap1),(0,cap2))

--capmax cont cap = if (cont >= cap) then cap else cont -- se puede cambiar por un min


ej5 :: ProblemaDeLasJarras
ej5 = ProbJarras ((2,5),(1,3))

{-
EJERCICIO 6

Consideremos la siguiente definición de un nuevo tipo de dato que representa de
manera recursiva los polinomios con coeficientes enteros:
type Termino a = ( a , a )
data Polinomio a = PolCero | Pol ( Termino a ) ( Polinomio a )
deriv ing Show
Es decir, un polinomio con coeficientes enteros es el polinomio cero (0x
0
) o un
polinomio obtenido añadiendo un nuevo término con un cierto coeficiente entero y
un cierto grado a un polinomio ya existente. Siempre asumiremos que no añadimos
un término de grado igual a uno del polinomio ya existente, con la única excepción
de que el término sea de grado 0, que se podrá añadir si el único término de grado
0 del polinomio ya existente es 0x
0
. Por otra parte, los términos no tienen por qué
añadirse en orden creciente de grado.

Definir la función valor que reciba un Polinomio y un número real y devuelva el
valor que toma el polinomio en ese número (es decir, el valor que se obtendría al
sustituir en el polinomio la variable x por el número real especificado). 
Siempre asumiremos que el polinomio recibido está construido de manera correcta. Recuérdese
la existencia de la función fromIntegral de transformación de tipos numéricos.
-}

valor :: Polinomio Int-> Float -> Float
valor (PolCero) _ = 0
valor (Pol (a,c) a2) b = (fromIntegral a)*(b ** fromIntegral c) + (valor a2 b)

ej6 :: Polinomio Int
ej6 = Pol (3,3) (Pol (2,2) PolCero)

-- ---------------------------------------------------------------------
-- Ejercicios 7 y 8. Realice los dos primeros ejercicios del examen cuyo
-- enunciado se encuentra en la url:
-- http://www.cs.us.es/cursos/pd-2011/examenes/Febrero_control_3/examen-P4-P8.pdf
-- ---------------------------------------------------------------------
{-
EJERCICIO 7
Los operadores móviles de un mundo imaginario han llegado a un acuerdo para
aplicar a sus abonados únicamente dos tipos de tarifas: con la tarifa 1, el coste por
minuto de una llamada es siempre de 0.12 €; con la tarifa 2, el coste por minuto
de una llamada es de 0.10 € si se llama a un número del mismo operador y de
0.15 € si se llama a un número de otro operador.

Definir el tipo de datos Movil como un sinónimo de una tupla de una cadena y
un número de tipo Int (representando el operador móvil y el número de móvil)
y el tipo de datos Tarifa como un sinónimo de un número de tipo Int (1 ó 2,
representando una tarifa que se puede contratar).

Definir un nuevo tipo de datos ContratoMovil que tenga un único constructor con
un argumento del tipo Movil y un argumento del tipo Tarifa. Siempre asumiremos
que al construir un valor de este tipo de datos los argumentos proporcionados son
correctos.

Definir la función costeLlamada que reciba un ContratoMovil, un Movil y un
número de tipo Float (representando la duración de la llamada) y devuelva el
coste final de la llamada. Siempre asumiremos que todos los argumentos recibidos
son correctos.
-}

type Movil =  (String,Int) -- (Operador movil, numero del movil)
type Tarifa = Int
data ContratoMovil = CM (Movil, Tarifa)
    deriving Show

ej7 :: ContratoMovil
ej7 = CM (("op1",661762235),2)

costeLlamada :: ContratoMovil -> Movil -> Float -> Float
costeLlamada (CM ((op1,m1),t1)) (op2,m2) num
    | t1 == 1 = 0.12 * num
    | t1 == 2 && op1 == op2 = 0.10 * num
    | otherwise = 0.15*num


{-
Ejercicio 8
Consideremos la siguiente definición de un nuevo tipo de dato que representa de
manera recursiva los polinomios con coeficientes enteros:
type Termino a = ( a , a )
data Polinomio a = PolCero | Pol ( Termino a ) ( Polinomio a )
deriving Show
Es decir, un polinomio con coeficientes enteros es el polinomio cero (0x
0
) o un
polinomio obtenido añadiendo un nuevo término con un cierto coeficiente entero y
un cierto grado a un polinomio ya existente. Siempre asumiremos que no añadimos
un término de grado igual a uno del polinomio ya existente, con la única excepción
de que el término sea de grado 0, que se podrá añadir si el único término de grado
0 del polinomio ya existente es 0x
0
. Por otra parte, los términos no tienen por qué
añadirse en orden creciente de grado.
Definir la función multiplicarPorTermino que reciba un Polinomio y un Termino
y devuelva el polinomio resultante de multiplicar el polinomio especificado por el
término especificado. Siempre asumiremos que el polinomio recibido está construido
de manera correcta.
-}

multiplicarPorTermino :: Polinomio Int -> Termino Int-> Polinomio Int
multiplicarPorTermino (PolCero) term =  PolCero
multiplicarPorTermino (Pol (a1,b1) b) (at,bt) = (Pol (a1*at,b1+bt) (multiplicarPorTermino b (at,bt)))


ej8 :: Polinomio Int
ej8 = (Pol (2,2) (Pol (1,1) PolCero))
--multiplicarPorTermino ej8 (2,3)

-- ---------------------------------------------------------------------
-- Ejercicios 9 y 10. Realice los ejercicios 6 y 7 del examen cuyo
-- enunciado se encuentra en la url:
-- http://www.cs.us.es/cursos/pd-2011/examenes/Febrero_final/examen.pdf
-- ---------------------------------------------------------------------
{-
EJERCICIO 9

El puzle de la serpiente consta de una cuadrícula n × n (donde las casillas tienen
coordenadas (x, y) según la representación habitual de los ejes de coordenadas).
Sobre esta cuadrícula se desplaza una serpiente de m segmentos que ocupan casillas
adyacentes desde la cabeza hasta la cola. La cabeza de la serpiente puede desplazarse
a cualquier casilla adyacente en horizontal o vertical, arrastrando consigo el resto
de segmentos y con las únicas restricciones de no poder atravesar los bordes de la
cuadrícula y no poder superponerse a otros segmentos de la serpiente.


Definir el tipo de datos Serpiente como un sinónimo de una lista de tuplas de
dos números de tipo Int. Este tipo de datos representa las casillas ocupadas por la
serpiente, desde la cabeza hasta la cola. Definir el tipo de datos Movimiento como
un sinónimo de cadena.


Definir un nuevo tipo de datos PuzleSerpiente que tenga un único constructor
con dos argumentos, uno de tipo Serpiente y otro de tipo Int. Este tipo de datos
representa la configuración actual del puzle, es decir, las casillas ocupadas por
la serpiente y el número de movimientos realizados. Siempre asumiremos que al
construir un valor de este tipo de datos la lista de casillas ocupadas por la serpiente
y el número de movimientos son correctos.

Definir la función moverSerpiente que reciba un puzleSerpiente y un Movimiento
y devuelva el puzleSerpiente resultante de mover la serpiente en la dirección especificada. Siempre asumiremos que la configuración actual del puzle recibida es
correcta, que el movimiento recibido es una de las cadenas "Arriba", "Abajo",
"Izquierda" o "Derecha", y que el movimiento se puede realizar.
-}

type Serpiente = [(Int,Int)]--Este tipo de datos representa las casillas ocupadas por la
--serpiente, desde la cabeza hasta la cola.
type Movimiento = String
data PuzleSerpiente = Serp (Serpiente,Int)
    deriving Show

moverSerpiente :: PuzleSerpiente -> Movimiento -> PuzleSerpiente
moverSerpiente (Serp (xs,n)) mov
    | mov == "Arriba" = Serp ([(i,j+1)] ++ (init xs),n+1)
    | mov == "Abajo" = Serp ([(i,j-1)] ++ (init xs),n+1)
    | mov == "Izquierda" = Serp ([(i-1,j)] ++ (init xs),n+1)
    | mov == "Derecha" = Serp ([(i+1,j)] ++ (init xs),n+1)
    where (i,j) = head xs

quitarCola :: Serpiente -> Serpiente
quitarCola (x:[]) = []
quitarCola (x:xs) = [x] ++ quitarCola xs

ej9 :: PuzleSerpiente
ej9 = Serp ([(1,1),(1,2),(1,3)],4)

{-
Ejercicio 10
Consideremos la siguiente definición de un nuevo tipo de datos que representa de
manera recursiva los polinomios con coeficientes enteros:
type Termino a = ( a , a )
data Polinomio a = PolCero | Pol ( Termino a ) ( Polinomio a )
deriv ing Show
Es decir, un polinomio con coeficientes enteros es el polinomio cero (0x
0
) o un
polinomio obtenido añadiendo un nuevo término con un cierto coeficiente entero y
un cierto grado a un polinomio ya existente. Siempre asumiremos que no añadimos
un término de grado igual a uno del polinomio ya existente, con la única excepción
de que el término sea de grado 0, que se podrá añadir si el único término de grado
0 del polinomio ya existente es 0x
0
. Por otra parte, los términos no tienen por qué
añadirse en orden creciente de grado.
Definir la función derivada que reciba un Polinomio y devuelva la derivada de
ese polinomio. Siempre asumiremos que el polinomio recibido está construido de
manera correcta.-}

derivada :: Polinomio Int -> Polinomio Int
derivada (PolCero) =  PolCero
derivada (Pol (a1,b1) b)
    | b1 == 0 = (Pol (a1*b1,b1) (derivada b))
    | otherwise = (Pol (a1*b1,b1-1) (derivada b))


ej10 :: Polinomio Int
ej10 = (Pol (2,3) (Pol (1,32) PolCero))
-- ---------------------------------------------------------------------
-- Ejercicio 11 y 12. Realice los ejercicios 6 y 7 del examen cuyo
-- enunciado se encuentra en la url:
-- http://www.cs.us.es/cursos/pd-2011/examenes/Septiembre/examen.pdf
-- ---------------------------------------------------------------------
{-
Ejercicio 11

Definir el tipo de datos Ficha como un sinónimo de un valor lógico. Definir el
tipo de datos Columna como un sinónimo de una lista de fichas. Este tipo de
datos representa las fichas colocadas en una de las columnas del tablero.
Definir un nuevo tipo de datos CuatroEnRaya que tenga un único constructor
con un argumento, una lista de columnas. Este tipo de datos representa la
configuración actual del tablero. Siempre asumiremos que al construir un valor
de este tipo de datos la lista de columnas proporcionada es correcta.

Definir la función colocarFicha que recibe una Ficha, un Int (la columna) y
un tablero CuatroEnRaya y devuelve el tablero CuatroEnRaya resultante de
colocar la ficha en la columna especificada. Siempre asumiremos que la ficha, la
columna y la configuración actual del tablero recibidas son correctas.

Por ejemplo, dada la configuración inicial del tablero, [[ ],[ ],[ ],[ ],[ ],[ ],[ ]], si el jugador
que inicia la partida juega con fichas True y decide colocar en la tercera columna,
el tablero quedaría [[ ],[ ],[True],[ ],[ ],[ ],[ ]]. Si tras algunas jugadas el tablero se
encuentra en una configuración [[False,False,False],[ ],[True],[ ],[True],[True],[ ]]
y el jugador que juega con True decide poner en la primera columna, el tablero
debería quedar con [[True,False,False,False],[ ],[True],[ ],[True],[True],[ ]].

Consideremos la siguiente definición de un nuevo tipo de datos que representa de
manera recursiva las colas con prioridad de números enteros:

type P ri o ri d a d = Int
data Cola a = ColaVacia | Encola r a P ri o ri d a d ( Cola a )
deriving Show
Es decir, una cola con prioridad de números enteros es la cola vacía o la cola
obtenida añadiendo un número entero con una cierta prioridad (siempre de tipo
Int) a una cola ya existente.
-}

type Ficha = Bool
type Columna =  [Ficha]
data CuatroEnRaya =  Raya [Columna]
    deriving Show

colocarFicha ::  Ficha -> Int -> CuatroEnRaya -> CuatroEnRaya
colocarFicha f n xs = undefined
        

type Prioridad = Int
data Cola a = ColaVacia | Encolar a Prioridad ( Cola a )
    deriving Show



-- Definir la función colocarFicha que recibe una Ficha, un Int (la columna) y
-- un tablero CuatroEnRaya y devuelve el tablero CuatroEnRaya resultante de
-- colocar la ficha en la columna especificada. Siempre asumiremos que la ficha, la
-- columna y la configuración actual del tablero recibidas son correctas.
{-
Ejercicio 12    
Definir la función primerElemento que recibe una Cola y devuelve un par con
el primer elemento de la cola (es decir, de todos los elementos de la cola con la
máxima prioridad, aquel que se introdujo primero) y su prioridad. La función
no estará definida para la cola vacía.
Así, por ejemplo, el primer elemento de la cola resultante de encolar el elemento 4
con prioridad 5 en la cola vacía, devolvería el par compuesto por el elemento 4 y
la prioridad 5. En el caso de que tuviéramos como entrada la cola resultante de
encolar 3 con prioridad 10 en la cola resultante de encolar 6 con prioridad 8 en
3
la cola resultante de encolar 9 con prioridad 10 en la cola vacía, devolvería el par
compuesto por el elemento 9 y la prioridad 10.
Según la pseudociencia de la numerología, para calcular el número de nacimiento
de una persona basta sumar los números que componen su fecha de nacimiento,
volver a hacerlo con el resultado obtenido y así reiteradamente hasta obtener un
solo guarismo entre el uno y el nueve.-}

-- -------------------------------------------------------------------
-- Ejercicio 13. (parcial 2 del curso 2018/19)
--
-- 1. Defina, con sintaxis de registro, un nuevo tipo que contenga la
--    información sobre planetas que aparecen en las películas
--    de star wars:
--    * name, diameter, population, de tipo String
--    * residents, de tipo lista de String
--
-- 2. Haga que el tipo anterior disponga de un valor por defecto,
-- de modo que podamos posteriormente crear elementos del tipo
-- sin necesidad de proporcionar todos los datos solicitados
--
-- 3. Defina un tipo sinónimo de una lista de planetas
--
-- ------------------------------------------------------------------
-- 1) 
data Planeta = 
    Pla {
        name :: String, diameter :: String, population :: String, residents :: [String]
    }
    deriving Show

    -- 2) 

planetaPorDefecto :: Planeta
planetaPorDefecto = Pla { name = "none", diameter= "none", population= "none", residents = [] }

--3 ) 
type Planetas = [Planeta]