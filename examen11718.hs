-- ProgramaciÃ³n Declarativa
-- Grado de IngenierÃ­a InformÃ¡tica - TecnologÃ­as InformÃ¡ticas
-- 1 Parcial                                       8 de Noviembre 2017
-- -------------------------------------------------------------------
-- Apellidos:
-- Nombre:
-- -------------------------------------------------------------------
-- AVISOS IMPORTANTES
-- Â· Antes de continuar, cambie el nombre de este archivo por:
--                   <uvus>Ejercicio20171108.hs
--   donde <uvus> debe ser su usuario virtual.
-- Â· Escriba la soluciÃ³n de cada ejercicio en el hueco reservado para
--   ello.
-- Â· AsegÃºrese de utilizar exactamente el nombre y el tipo indicado
--   para cada funciÃ³n solicitada. Puede aÃ±adir tantas funciones
--   auxiliares (incluyendo el tipo que considere mÃ¡s adecuado) como
--   necesite describiendo claramente su objetivo.
-- -------------------------------------------------------------------

import Test.QuickCheck
import Data.Char

-- En caso de no resolver adecuadamente el ejercicio 1 descomentar la
-- siguiente lÃ­nea. DispondrÃ¡ de las funciones letraNumero y
-- numeroLetra del apartado 1.5 (Ãºtiles para los ejercicios 2 y 3).

-- import Codigo
-- -------------------------------------------------------------------

-- -------------------------------------------------------------------
-- Ejercicio 1.
-- Llamaremos lista asociativa a una lista de tuplas binarias. A los
-- primeros elementos de dichas tuplas los denominaremos claves, y a
-- los segundos valores. Por ejemplo:

listaEjemplo :: [(Int, String)]
listaEjemplo = [(1, "A"), (2, "B"), (2, "C")]

-- es una lista asociativa de enteros de precisiÃ³n fija y cadenas de
-- caracteres. Las claves serÃ­an los nÃºmeros 1 y 2. Los valores las
-- cadenas "A", "B" y "C".

-- -------------------------------------------------------------------
-- (1.1) Definir, utilizando recursiÃ³n, la funciÃ³n

esClave :: (Eq a) => a -> [(a, b)] -> Bool
esClave c xs = (length [t | t <- xs, (fst t) == c]) > 0

-- que, dado un elemento c y una lista asociativa l, determine si c
-- es una clave de l. Por ejemlo:
--   esClave 1 listaEjemplo ==> True
--   esClave 3 listaEjemplo ==> False
-- -------------------------------------------------------------------

-- -------------------------------------------------------------------
-- (1.2) Definir la funciÃ³n
-- listaEjemplo = [(1, "A"), (2, "B"), (2, "C")] (c,v)
asoc1 :: (Eq a) => a -> [(a, b)] -> b
asoc1 c l = head (asoc1_aux c l [])

asoc1_aux c [] acc = acc
asoc1_aux c (l:lss) acc
    | (c == fst l) = [snd l] ++ asoc1_aux c lss acc
    | otherwise = asoc1_aux c lss acc

-- que, dados un elemento c y una lista asociativa l, devuelva un
-- elemento v tal que el par (c, v) pertenece a l. AnÃ¡logamente,
-- definir la funciÃ³n

asoc2 :: (Eq b) => b -> [(a, b)] -> a
asoc2 v l = head (asoc2_aux v l [])

asoc2_aux v [] acc = acc
asoc2_aux v (l:lss) acc
    | (v == snd l) = [fst l] ++ asoc2_aux v lss acc
    | otherwise = asoc2_aux v lss acc

-- que , dados un elemento v y una lista asociativa l, devuelva un
-- elemento c tal que el par (c, v) pertenece a l. Por ejemplo:
--   asoc1 "butcher" [("butcher", "231 e22nd St."),
--                    ("baker", "515 w23rd St."),
--                    ("hardware", "988 Lexington Ave.")]
--   ==> "231 e22nd St."
--   asoc1 2 listaEjemplo ==> "B" (tambiÃ©n serÃ­a
--         vÃ¡lida "C" como respuesta)
--   asoc1 [1] [([1,2,3], 3), ([3,3,3,4,5], 5), ([], 0)]
--   ==> *** Exception: asoc1: Clave no vÃ¡lida.
--   asoc2 3 [([1,2,3], 3), ([3,3,3,4,5], 5), ([3, 2, 1], 3)]
--   ==> [1,2,3] (tambiÃ©n serÃ­a vÃ¡lida [3, 2, 1] como respuesta)
--   asoc2 "C" [(1, "A"), (2, "B"), (3, "C")] ==> 3
--   asoc2 "GonzÃ¡lez JimÃ©nez" [("Carlos", "Parrilla GonzÃ¡lez"),
--                             ("Nuria", "GarcÃ­a JimÃ©nez"),
--                             ("Alicia", "GarcÃ­a Granados")]  
--   ==> *** Exception: asoc2: Valor no vÃ¡lido.
-- -------------------------------------------------------------------

-- -------------------------------------------------------------------
-- (1.3) Definir

codigo :: [(Char, Int)]
listaletras = ['a'..'n'] ++ [chr 241] ++ ['o'..'z']
codigo = zip listaletras [0..]

-- como una lista asociativa que represente la siguiente
-- correspondencia entre las 27 letras del alfabeto espaÃ±ol (en
-- minÃºsculas) y los 27 primeros nÃºmeros naturales.
-- -------------------------------------------------------------------
-- Letra   a b c d e f g h i j  k  l  m  n  ñ  o  p  q  r  s  t  u  v
-- NÃºmero  0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22
-- -------------------------------------------------------------------
-- Letra   w  x  y  z
-- NÃºmero 23 24 25 26
-- -------------------------------------------------------------------

-- Se dice que una lista asociativa es un cÃ³digo si las claves no
-- aparecen repetidas y los valores tampoco.

-- -------------------------------------------------------------------
-- (1.4) Definir

-- esCodigo :: (Eq a, Eq b) => [(a, b)] -> Bool
-- esCodigo xs = foldl (\x acc -> if ()   ) True
--     where k = [ fst x | x <- codigo xs]
--           v = [ snd x | x <- codigo  xs]
 
-- que, dada una lista asociativa, determine si es un codigo. Por
-- ejemplo:
--   esCodigo codigo ==> True
--   esCodigo listaEjemplo ==> False
--   esCodigo [(1, "C"), (2, "B"), (3, "C")] ==> False
-- -------------------------------------------------------------------
-- (1.5) Definir

letraNumero :: Char -> Int
letraNumero l = head [n | (c,n) <- codigo, c == l] 
                
-- que reciba una letra y devuelva la correspondencia numÃ©rica de dicha
-- letra segÃºn el cÃ³digo definido. De forma anÃ¡loga, definir
                                                    
numeroLetra :: Int -> Char
numeroLetra num
    | num > 27 = error "no puede ser mayor que 27"
    | otherwise =  head [c | (c,n) <- codigo, num == n]

-- que reciba un nÃºmero natural menor que 27 y devuelva la letra que
-- corresponde a dicho nÃºmero. Por ejemplo:
--   letraNumero 'g' ==> 6
--   letraNumero 'n' ==> 3
--   letraNumero 'A' ==>
--   *** Exception: letraNumero: sÃ³lo disponible para el alfabeto
--   espaÃ±ol (minÃºsculas).
--   numeroLetra 6   ==> 'g'
--   numeroLetra 13  ==> 'n'
--   numeroLetra 30  ==>
--   *** Exception: numeroLetra: sÃ³lo hay 27 letras disponibles
--   (numeradas de 0 a 26). 

-- NOTA IMPORTANTE: Ambas definiciones deben estar basadas en la
-- correspondencia entre letras y nÃºmeros definida. Es decir, si se
-- cambia la definiciÃ³n de codigo por otra correspondencia del mismo
-- tipo, las funciones deben funcionar de acuerdo a la nueva
-- definiciÃ³n.
-- -------------------------------------------------------------------

-- Representaremos un mensaje como una lista cuyos elementos son los
-- caracteres alfabÃ©ticos que lo componen; es decir, descartamos los
-- espacios, los signos de puntuaciÃ³n y demÃ¡s.
-- Por ejemplo, los siguientes mensajes:
--    Probando, probando.
--    Este mensaje se autodestruirÃ¡ en cinco segundos.
--    La contraseÃ±a es: esos tipos con bigote, que parecen hotentotes.
-- vendrÃ¡n representados por las siguientes listas:

type Mensaje = [Char]
mensaje1 :: Mensaje
mensaje1 = "probandoprobando"
mensaje2 :: Mensaje           
mensaje2 = "estemensajeseautodestruiraencincosegundos"
mensaje3 :: Mensaje
mensaje3 = "laclaveesesostiposconbigotesqueparecenotentotes"

-- Para codificar los mensajes utilizaremos la correspondencia entre
-- las letras del alfabeto y los primeros 27 nÃºmeros naturales dada
-- en el ejercicio anterior a travÃ©s de las funciones letraNumero y
-- numeroLetra.

-- -------------------------------------------------------------------
-- Ejercicio 2. Cifrado por desplazamiento.
-- Dado un nÃºmero natural k, cada letra del mensaje se reemplaza por
-- la letra que se encuentra k lugares a la derecha en el cÃ³digo. Por
-- ejemplo, para k = 3 la letra 'a' se reemplazarÃ¡ por 'd', la letra
-- 'b' por 'e', y asÃ­ hasta la letra 'z' que se reemplaza por 'c'.
-- Nota: puede ser de utilidad la funciÃ³n mod.
-- -------------------------------------------------------------------
-- (2.1) Definir

desplaza :: Int -> Char -> Char
desplaza = undefined

-- que dados k y una letra devuelva la letra que se encuentra k
-- posiciones a la derecha en el cÃ³digo. Por ejemplo
--   desplaza 3 'a' ==> 'd'
--   desplaza 3 'z' ==> 'c'

-- -------------------------------------------------------------------
-- (2.2) Definir, sin utilizar recursiÃ³n, 

cifradoDesplazamiento :: Int -> Mensaje -> Mensaje
cifradoDesplazamiento = undefined

-- que dados k y un mensaje devuelva el mensaje cifrado. Por ejemplo:
--   cifradoDesplazamiento 3 mensaje1 ==> "suredpgrsuredpgr"
--   cifradoDesplazamiento 0 mensaje3
--   ==> "laclaveesesostiposconbigotesqueparecenotentotes"
--   cifradoDesplazamiento 5 mensaje2
--   ==> "jxyjqjrxfÃ±jxjfzytijxywznwfjrhnrhtxjlzritx"
-- -------------------------------------------------------------------

-- -------------------------------------------------------------------
-- Ejercicio 3. Cifrado VigenÃ¨re
-- Utiliza una palabra clave p. Si dicha palabra tiene m letras, se
-- divide el mensaje en bloques de m letras. Y a cada bloque se le
-- suma la clave.
-- Por ejemplo, supongamos que usamos prueba como palabra clave y
-- queremos cifrar el mensaje 'Probando, probando'.
--    proban doprob ando
--  + prueba prueba prue
--  --------------------
--    fjjfbn sgkvpb pexs
-- -------------------------------------------------------------------

-- -------------------------------------------------------------------
-- (3.1) Definir

sumaLetras :: Char -> Char -> Char
sumaLetras = undefined

-- que dadas dos letras calcule el resultado de 'sumarlas'. Por
-- ejemplo:
--   sumaLetras 'b' 'e' ==> 'f' ('b' es 1, 'e' es 4 y 'f' es 5).
--   sumaLetras 'd' 'p' ==> 's' ('d' es 3, 'p' es 16 y 's' es 19).
-- -------------------------------------------------------------------
-- (3.2) Definir

vigenere :: [Char] -> Mensaje -> Mensaje
vigenere = undefined

-- que dada una palabra clave (la lista de caracteres que la
-- componen) y un mensaje, devuelva el mensaje cifrado. Por ejemplo:
--   vigenere "mision" mensaje2
--   ==> "pammaqyasqsfpinbdppamzjudiwuquykhassguvwh"
--   vigenere "tebeo" mensaje3
--   ==> "eedoooifwsmstxwjstgdgfjkdnitujxtbvsviÃ±sixqusixw"
-- -------------------------------------------------------------------

-- -------------------------------------------------------------------
-- Ejercicio 4
-- El cifrado de un mensaje no cambia la longitud del mismo.
-- Comprobar utilizando quickCheck, definiendo las propiedades
-- adecuadas, que las funciones cifradoDesplazamiento y vigenere lo
-- verifican.



-- -------------------------------------------------------------------

-- -------------------------------------------------------------------
-- Ejercicio 5
-- (5.1) Definir, utilizando recursiÃ³n

menoresR :: Int -> [[a]] -> [a]
menoresR = undefined

-- que, dado entero positivo k y una lista de listas lls, devuelva los
-- elementos de las listas ls de lls que tienen menos de k elementos.
-- Por ejemplo:
--   menoresR 3 [[], [1], [4, 4, 4, 4], [2, 2], [5, 5, 5, 5, 5]]
--   ==> [1, 2, 2]
--   menoresR 6 ["casa", "caminante", "ostracismo", "merienda", "socio"]
--   ==> "casasocio"

-- -------------------------------------------------------------------
-- (5.2) Definir, sin utilizar recursiÃ³n

menoresA :: Int -> [[a]] -> [a]
menoresA = undefined

-- anÃ¡loga a la anterior.
-- Por ejemplo:
--   menoresA 3 [[], [1], [4, 4, 4, 4], [2, 2], [5, 5, 5, 5, 5]]
--   ==> [1, 2, 2]
--   menoresA 6 ["casa", "caminante", "ostracismo", "merienda", "socio"]
--   ==> "casasocio"

-- -------------------------------------------------------------------
-- Ejercicio 6
-- (6.1) Definir, proporcionando un tipo adecuado,

longitud = undefined

-- que calcule la longitud del segmento de la recta real determinado
-- por los dos elementos del par. Por ejemplo:
--   longitud (3.5, (-2.7)) ==> 6.2
--   longitud ((-2.7), 3.5) ==> 6.2
-- -------------------------------------------------------------------

-- -------------------------------------------------------------------
-- (6.2) Definir, proporcionando un tipo adecuado y con la
-- menor cantidad de ecuaciones posibles (sin utilizar guardas), 

cantidadVerdaderos = undefined

-- que, dada una tupla con tres valores lÃ³gicos, calcule cuÃ¡ntos son
-- verdaderos. Por ejemplo:
--  cantidadVerdaderos (5 < 0, 'a' == 'A', not False) ==> 1
--  cantidadVerdaderos (2 == 1 + 1,2 * 2 == 2 + 2,3 `elem` [1, 2, 3])
--  ==> 3
-- -------------------------------------------------------------------