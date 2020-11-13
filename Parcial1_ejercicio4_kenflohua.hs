-- Nombre: KENNY JESÚS
-- Apellidos: FLORES HUAMÁN
-- UVUS: kenflohua

import Data.Char
-- ---------------------------------------------------------------------
-- Ejercicio 4. (2,5 puntos)
-- Define la función (digitosConDecimales n), tal que reciba un número
-- real (de simple precisión) y devuelva un par donde ambas componentes
-- son una lista de números enteros. La función devuelve la lista de los
-- digitos del número n, poniendo los dígitos de la parte entera en la 
-- primera componente del par, y los dígitos de los decimales en la segunda
-- componente. 
-- Nota: Se conseguirá 2,5 puntos si se desarrolla usando solo funciones 
-- de orden superior cuando haya que recorrer listas. Si no, si se emplea
-- recursión o comprensión, la nota máxima será un 1,5.
-- Pista: si n=10, y n es un Float, entonces show n == "10.0"
-- Por ejemplo:
--  > digitosConDecimales 3.1415
--  ([3],[1,4,1,5])
--  > digitosConDecimales 10 
--  ([1,0],[0])
--  > digitosConDecimales (-10)       -- descartamos el signo para los negativos
--  ([1,0],[0])
-- ---------------------------------------------------------------------

digitosConDecimales n =  (foldl (\acc x -> if("." == x) then [x]:acc else acc) []) $ show n

{-
En primero lugar voy a recorrer toda la lista, todo lo que encuentre después del punto
no me lo va a devolver, y voy a crear otra ilsta que haga lo conrario,
hubiera sido buena idea usar dropwhile  y su hermana.

-}
