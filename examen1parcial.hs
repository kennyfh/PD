-- ProgramaciÃ³n Declarativa 2019/20
-- Grado de IngenierÃ­a InformÃ¡tica - TecnologÃ­as InformÃ¡ticas
-- Parcial 1                                       24 de Octubre de 2019
-- ----------------------------------------------------------------------
-- Apellidos:
-- Nombre:
-- UVUS:
-- ----------------------------------------------------------------------
-- INSTRUCCIONES
-- Â· Antes de continuar, cambie el nombre de este archivo por:
--                   Parcial1_<codigo>_<uvus>.hs
--   donde "<uvus>" es su usuario virtual de la Universidad de Sevilla, y
--   "<codigo>" es el cÃ³digo alfanumÃ©rico de arriba.
-- Â· Escriba la soluciÃ³n de cada ejercicio en el hueco reservado para
--   ello.
-- Â· AsegÃºrese de utilizar correctamente el nombre y el tipo indicado
--   para cada funciÃ³n solicitada. Puede aÃ±adir tantas funciones
--   auxiliares (incluyendo el tipo adecuadamente) como necesite,
--   describiendo su objetivo.
-- Â· Una vez finalizado el examen, comprima el fichero .hs en un .tar.gz
--   con el siguiente nombre:
--                  ENTREGA-<uvus>.tar.gz
--   dejÃ¡ndolo en el escritorio. Reiniciar el equipo y en el menÃº de
--   selecciÃ³n del sistema elija "Enviar examen".
-- Â· DespuÃ©s, ponga sus apellidos, nombre y UVUS en el enunciado en papel,
--   y entrÃ©guelo al profesor.
-- ----------------------------------------------------------------------

import Test.QuickCheck
import Data.List

-- ----------------------------------------------------------------------
-- Ejercicio 1.1 (2 puntos)
-- Definir el operador infijo (*&) tal que reciba y devuelva números
-- reales de doble precisiÃ³n. El cÃ¡lculo realizado es la multiplicaciÃ³n
-- de los dos argumentos redondeado al siguiente entero. El operador debe
-- tener la misma precedencia que la multiplicaciÃ³n (*), y tener la
-- asociatividad necesaria (derecha o izquierda) para que los siguientes
-- ejemplos sean correctos:
-- Î»> 2.3 *& 2.1 == 5.0 
-- Î»> 5.0 *& 0.5 *& 0.5 == 2.0
-- Î»> 5.0 *& (0.5 *& 0.5) == 5.0
-- Nota: la funciÃ³n 'ceiling' puede ser de ayuda.

-- ----------------------------------------------------------------------
(*&) :: Double -> Double -> Double
x *& y = fromIntegral (ceiling (x * y))
-- ----------------------------------------------------------------------
-- Ejercicio 2.1 (2 puntos)
-- Definir la funciÃ³n (intervalos a b xs), donde xs es una lista de posiciones
-- en una recta (nÃºmeros reales) que puede venir desordenada, a y b son dos
-- valores reales que definen un intervalo en dicha recta. Necesitamos que la
-- funciÃ³n devuelva una terna donde la primera lista sean los elementos de xs
-- por debajo de a, la segunda lista sean los elementos de xs que estÃ©n entre a
-- y b (incluÃ­dos), y la tercera lista los mayores que b. Las tres listas
-- resultantes deben estar ordenadas. Visualmente, queremos (zs,ys,ks) tal que:
--
--  -x-x--a-x-xx---b---x-x
--        |        |    
--    zs  |   ys   |  ks  
--
-- Por ejemplo:
-- Î»> intervalos 3 5 [1..10] == ([1,2],[3,4,5],[6,7,8,9,10])
-- Î»> intervalos 3 5 [10,9..1] ==  ([1,2],[3,4,5],[6,7,8,9,10])
-- Î»> intervalos 0.0 10.0 [9.5,10.0,-3,0.3,5.0,-10.56,114.5] ==
-- ([-10.56,-3.0],[0.3,5.0,9.5,10.0],[114.5])

intervalos a b xs = (zs,ys,ks)
    where   os = sort xs
            zs = [x | x <- os, x < a]
            ys = [x | x <- os, x>=a && x<=b]
            ks = [x | x <- os, x > b]

-- Ejercicio 2.2. (1 punto)
-- Comprobar con quickCheck si se cumple lo siguiente: xs es igual al intervalo
-- comprendido entre el mÃ­nimo y el mÃ¡ximo de xs calculado mediante la funciÃ³n
-- intervalos.

-- ----------------------------------------------------------------------
prop_ej22 xs = (xs == ys)
    where (_,ys,_) = intervalos (minimum xs) (maximum xs) xs

-- ----------------------------------------------------------------------
-- Ejercicio 3. (2,5 puntos)
-- La funciÃ³n (zigzag xs) sobre una lista cambia el orden de los elementos de
-- xs tal que el primero de xs va al comienzo, el segundo de xs al final, el
-- tercero en la segunda posiciÃ³n, el cuarto en la penÃºltima posiciÃ³n, etc. Es
-- decir,
-- Î»> zigzag [1..5] == [1,3,5,4,2]
-- Î»> zigzag [1..10] == [1,3,5,7,9,10,8,6,4,2]
-- Î»> zigzag "abcdefghi" == "acegihfdb"
-- Se pide definir dicha funciÃ³n de tres formas distintas, usando un tipado
-- polimÃ³rfico:
-- 1) Usando recursiÃ³n (zigzagR xs)
zigzagR xs = zigzagR_aux [] [] xs 0

zigzagR_aux ls lz [] _ = ls ++ (reverse lz)
zigzagR_aux ls lz (x:xs) acc
    | (mod acc 2) == 0 = zigzagR_aux (ls ++ [x]) lz xs (acc+1)
    | otherwise = zigzagR_aux (ls) (lz ++ [x]) xs (acc+1)
    
-- 2) Usando comprensiÃ³n (zigzagC xs)
zigzagC xs = ls ++ lz
    where tuplas = zip xs [0..length xs]
          ls = [x | (x,y) <- tuplas, (mod y 2) == 0]
          lz = reverse [x | (x,y) <- tuplas, (mod y 2) == 1]

-- 3) Usando orden superior (zigzagO xs)

-- ----------------------------------------------------------------------

-- ----------------------------------------------------------------------  
-- Ejercicio 4.1. (1,5 puntos)
-- Definir la funciÃ³n (maxListas xss) tal que reciba una lista de listas xss,
-- y devuelva una lista de pares (i,j), donde i es el Ã­ndice de la sublista no
-- vacÃ­a i-Ã©sima dentro de xss y j es la posiciÃ³n donde sucede el mÃ¡ximo de la
-- sublista i-Ã©sima, por ejemplo, 
-- Î»> maxListas [[4,2],[7],[2,8,1],[10,20]] == [(1,1),(2,1),(3,2),(4,2)]
-- Î»> maxListas [[1,2,3],[],[0,4,1],[10]] == [(1,3),(3,2),(4,1)]

maxListas :: Ord a => [[a]] -> [(Int,Int)]
maxListas xss = [ (i,j) | (xs,i) <- zip xss [1..], (x,j) <-zip xs [1..] , x == (maximum xs) ]

-- Ejercicio 4.2. (1 punto)
-- Definir la funciÃ³n (listaIncrementales xs) que reciba una lista de pares
-- como la calculada en el ejercicio anterior. Dado que cada par (i,j) indica
-- el Ã­ndice de la lista, i, y la posiciÃ³n de su mÃ¡ximo, j, la funciÃ³n debe
-- devolver True si no falta ninguna lista i (porque era vacÃ­a), y la posiciÃ³n
-- donde sucede el mÃ¡ximo en la lista i es siempre mayor o igual que la
-- posiciÃ³n donde sucede en la lista i-1. Por ejemplo,
-- Î»> listasIncrementales [(1,1),(2,3),(3,3),(4,5)] == True
-- Î»> listasIncrementales [(1,1),(2,3),(4,3),(5,5)] == False
-- Î»> listasIncrementales [(1,1),(2,3),(3,3),(4,2)] == False

listaIncrementales  

-- ----------------------------------------------------------------------