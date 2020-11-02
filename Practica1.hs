
-- PD-Practica 1
-- Definiciones de funciones, tipos y clases.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================


-- A continuación se importa el módulo QuickCheck. Necesita ser instalado
-- previamente con Cabal o Stack
import   Test.QuickCheck


-- ---------------------------------------------------------------------
-- Ejercicio 1. Evalúa las siguientes líneas para entender cómo funciona
-- el sistema de tipos que proporciona Haskell.
-- ---------------------------------------------------------------------
-- :type True
-- :t True
-- :t 1
-- :t 1.1
-- :t 'a'
-- :t "a"
-- :t [1,2]
-- :t [1,2.1]
-- :t [1,'a']
-- :t (1,'s')
-- :t [[1],[1,2]] 
-- :t not
-- :t sum
-- :t (+)
-- :t []
-- :t ()
-- :t (3+)
-- :t length
-- :t zip
-- :t take


-- ---------------------------------------------------------------------
-- Ejercicio 2. Sin evaluar las expresiones en GHC, decide qué tipo es  
-- el adecuado para cada una de ellas. Intenta dar el tipo más general.
-- ---------------------------------------------------------------------


-- i1:: Integer  -- El primero va de regalo
-- i1 = 45


-- i2 = "123"
-- i3 = 45 <= i1
-- i4 = 'c'
-- i5 = ["abc","ok"]
-- i6 = head i5
-- i7 = tail "abc"
-- i8 = (True,4.5)
-- i9 = [i1,34]
-- i10 = sum
-- i11 x = length [1..x]


-- ---------------------------------------------------------------------
-- Ejercicio 3. Para cada una de las siguientes expresiones, reemplaza
-- undefined por una expresión válida para el tipo que la declara.
-- ---------------------------------------------------------------------


j1 :: ( String , Integer )
j1 = ("Ejemplo", 5)


j2 :: [ Integer ]
j2 = [12]


j3 ::  Char
j3 = 'k'

j4 ::  Double
j4 = 5.5

j5 :: ( Integer , String , Integer , Char )
j5 = (5, "Hola mundo", 3, 'K')

j6 :: ([ Char ],( Bool , String ))
j6 = (['M'], (True, "Hola"))


j7 :: [[ Bool ]]
j7 = [[False]]


j8 :: [( String , Bool )]
j8 = [("Adios", False)]


j9 ::  Integer  ->  Integer
j9 x = x + 3


j10 ::  Float  -> [ Bool ] ->  Bool
j10 x ys = x > 0 && head ys


j11 :: [ Char ] -> [[ Int ]]
j11 xs
    | length xs > 1 = [length xs-1]:j11 (tail xs)
    | otherwise    = [[0]]

--Andrés Carrasco Garzón ======================================

--Una función que dada una lista y una posición meta un elemento en la posición indicada en la lista indicada.
j12 :: Num a => a -> Int -> [a] -> [a]
j12 e n ns = (take n ns) ++ (e:(drop n ns))

-- Sólo funciona con números ==================================

{--
Problema 1:


  Conocemos el cambio actual del euro a dólares estadounidenses: 1
  Euro son 1.17507 dólares


  * Definir la constante tipoCambio con dicho valor.


  * Calcular el cambio a dólares de distintas cantidades de euros y viceversa


  * Definir dos funciones, aEuros y aDolares, que dada una cantidad de
    dólares (resp. euros) permita obtener la cantidad de euros (resp.
    dólares) equivalente. 


    Nota: No es necesario redondear el resultado.


  * Volver a calcular los cambios anteriores utilizando las funciones
    definidas.


  * Escribir la siguiente propiedad: dada cualquier cantidad de euros,
    si la cambiamos a dólares y los dólares obtenidos los volvemos a
    cambiar a euros, obtenemos la cantidad de euros original.


  * Si la propiedad anterior ha fallado analiza el posible problema y
    busca Escribir la siguiente propiedad: dada cualquier cantidad de
    euros, una solución al mismo.


  * Utilizar :browse para conocer los tipos de las definiciones
    anteriores y añadírselos a cada una.
--}


{--
Indicación:
Para cada una de las constantes y funciones que se definan a
continuación usar :t para averiguar el tipo que infiere haskell y
añadirlo a la definición (rectificándolo cuando sea conveniente).
--}

-- Kenny Jesús Flores Huamán ==========================================
tipoCambio :: Float
tipoCambio = 1.17507 -- 1 euro son 1.17507 dolares

aEuros :: Float -> Float
aEuros d = d / tipoCambio

aDolares :: Float -> Float
aDolares e = e*tipoCambio

-- Una propiedad es una funcion que devuelva un booleano
prop_dolar :: Float -> Bool
prop_dolar d = aDolares (aEuros d) == d

prop_euro :: Float -> Bool
prop_euro e = aEuros (aDolares e) == e
-- ===============================================================


{--
Problema 2:


  Conocemos que 0ºC se corresponden con 32ªF y que un incremento de
  5ºC suponen un incremento de 9ºF.


  * Definir una función que permita pasar de ºC a ºF (y otra para el
    cambio contrario).


  * Si para mañana está prevista un mínimo de 19ºC y un máximo de
    34ºC, ¿cuál sería el rango expresado en ºF?
--}

-- Kenny Jesús Flores Huamán ===============================================
aFahrenheit :: Fractional a => a -> a
aFahrenheit g = (g / 5) * 9 + 32

aGrados :: Fractional a => a -> a
aGrados f =  ( ( (f - 32) * 5) / 9)

-- =====================================================================


{--
Problema 3:


  Una tienda vende las mallas de 2kg de patatas a 2.70 euros. Para 
  favorecer la venta de cantidades mayores ofrece un precio reducido
  de 2.20 euros a partir de la quinta malla. Es decir, si un cliente
  compra 18 mallas, las cinco primeras las cobra a 2.70 y las 13
  restantes a 2.20.


  * Definir una función que, dada la cantidad de mallas calcule el
    precio sin tener en cuenta la promoción. Calcular el precio del
    ejemplo proporcionado.


  * Definir una función que, dada la cantidad de mallas, calcular el
    precio correspondiente según la promoción. Usar dicha función
    para calcular, de nuevo, el precio del ejemplo.


  La oferta ha tenido tanto éxito que el vendedor decide ampliarla
  reduciendo el precio a 2 euros a partir de la décima malla.


  * Definir una función para la nueva promoción y volver al calcular
    el precio del ejemplo.
--}


cMalla = 2.7
cMallaEspecial = 2.2
cMallaEspecial2 = 2
costeMallaSinPromocion m = m * cMalla
costeMallaConPromocion m = if (m > 4)
  then ((5*cMalla) + ((m-5)*cMallaEspecial))
  else costeMallaSinPromocion m

costeMallaConPromocion2 m = if (m>10)
  then ((costeMallaConPromocion 10) + (m-10)*cMallaEspecial2)
  else costeMallaSinPromocion m


{--
Problema 4:


  Consideremos el siguiente juego: Dado un número mayor que 1, si es
  par divídelo entre 2 y si es impar multiplícalo por 3 y súmale 1.
  Si el resultado es 1 ya has terminado, en caso contrario repite el
  procedimiento sobre el resultado.


  Pregunta: Dado un número inicial cualquiera, cuántas veces tendrás
  que aplicar el procedimiento.


  Ejemplos:


  Si empezamos por 10 => dividimos por 2 y obtenemos 5 =>
  multiplicamos por 3 y sumamos 1, obteniendo 16 => toca volver a
  dividir y obtenemos 8 => repetimos y obtenemos 4 => seguimos y
  obtenemos 2 => alcanzamos el 1.


  los valores han sido 5, 16, 8, 4, 2, 1: lo hemos aplicado 6 veces


  Si empezamos por 7 los valores serán 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5,
  16, 8, 4, 2, 1: lo hemos aplicado 16 veces.


  * Definir una función que aplique una vez el procedimiento
    anterior. Utilizarla sucesivamente para verificar que los
    resultados proporcionados a partir de 10 y de 7 son correctos.


    Nota: Pueden ser de utilidad las funciones even y div


  * Definir una función que dado un número natural mayor que uno
    calcule el número de veces que se repite el resultado.


  * Definir una función que devuelva la lista de resultados hasta
    llegar  a 1.
--}

juego :: [Integer] -> [Integer]
juego x
   | last x > 1 && even (last x) = juego (x++[(last x `div` 2)])
   | last x > 1 && not (even (last x)) = juego (x++[((last x)*3)+1])
   | otherwise = x

juegoA n = length (tail (juego [n]))

juegoB n = tail (juego [n])
