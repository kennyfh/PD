-- Programación Declarativa 2020/21
-- Grado de Ingeniería Informática - Tecnologías Informáticas
-- Parcial 2 (parte a)                            22 de Diciembre de 2020
-- ----------------------------------------------------------------------
-- Apellidos: Flores Huamán
-- Nombre: Kenny Jesús
-- UVUS: Kenflohua
-- ----------------------------------------------------------------------

import Data.List
import Test.QuickCheck

-- ----------------------------------------------------------------------
-- Ejercicio 1. (2 puntos)
-- Define la función (parte ns xs), tal que dadas una lista de enteros 'ns' 
-- y una lista de elementos 'xs' (de cualquier tipo), devuelva una lista de
-- listas con los segmentos de 'xs' de tamaños según la lista 'ns'. Esto se ve
-- mejor con un ejemplo, si ns = [2,3] y xs = [1,2,3,4,5], entonces
-- devuelve una lista con los 2 primeros elementos de xs y después los 3
-- siguientes, es decir, parte ns xs = [[1,2],[3,4,5]]. Más ejemplos:
-- > parte [3,3,4] [1..10]
-- [[1,2,3],[4,5,6],[7,8,9,10]]
-- > parte [3,3] [1..10]  
-- [[1,2,3],[4,5,6]]
-- > parte [3,3] [1..5] 
-- [[1,2,3],[4,5]]

-- Ejercicio 1.a. (0,75 puntos) 
-- Define la función (parte n xs) con recursión

parte [] xs = [] -- cuando se haya vaciado la lista de numeros, nuestro caso base va a ser una lista vacia
parte (n:ns) xs= [a] ++ (parte ns b) 
    where (a,b) = splitAt n xs {- Para cada elemento de la lista de enteros, vamos a hacer un splitAt de
    la lista de numeros, y nos vamos a quedar con el primer componente de la tupla, y vamos a seguir recursivamente
    haciendo splitAt de la lista con el resto de ns que nos quedan, usando el restante de la lista
    xs que nos queda,  hasta que ya no haya más numeros por dividir
    -}

-- Ejercicio 1.b. (0,75 puntos) 
-- Define la función (parte n xs) haciéndote valer del plegado por la
-- izquierda (foldl)

-- parte' xs = foldl (\acc x ->) []


-- Ejercicio 1.c. (0,5 puntos)
-- Comprueba con QuickCheck que la longitud del resultado de evaluar
-- (parte ns xs) es igual a la longitud de ns, siempre y cuando se cumpla que:
--   * la suma de los elementos de ns es igual a la longitud de xs.
--   * y todos los elementos de ns son mayores estrictos que 0

prop_parte ns xs = (sum ns == length xs) && (all (>0) ns)
-- si la suma de los elementos de ns es igual a la longitud de ns
-- y todos los elentos de ns son mayores que 0, debería dar True

-- La comprobación es quickCheck prop_parte
-- *** Gave up! Passed only 9 tests; 1000 discarded tests.

-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 2. (1,5 puntos)
-- Define la función
--   agrupa :: (Ord a) => [a] -> [a] -> ( [[a]] , [[a]] )
-- tal que (agrupa xs ys) reciba dos listas de elementos 'xs' e 'ys'
-- y devuelva un par cuyas dos componentes tendrán el resultado de aplicar
-- la siguiente operación a 'xs' e 'ys', respectivamente. Para ilustrar
-- la operación con un ejemplo, vamos a usar xs = [1,1,2,2,1], e 
-- ys = [3,2,2,1,3]. La operación a realizar es:
--  * Cambia de orden los elementos de xs y de ys, ordenándolos primero por
--    xs, y si hay igualdad, después por ys. Por ejemplo, el resultado
--    sería para xs, xs' = [1,1,1,2,2], y para ys, ys' = [2,2,3,1,2]. 
--    Observa que los elementos de ys cambian de lugar junto con los de xs.
--  * Calcula para xs' (el resultado de aplicar lo anterior a xs) los 
--    segmentos de elementos que son iguales de forma consecutiva. Por 
--    ejemplo, si xs' = [1,1,1,2,2], entonces [[1,1,1],[2,2]]
--  * Realiza en ys' los mismos segmentos (de igual tamaño y posiciones)
--    que los realizados para xs' en el punto anterior. Por ejemplo,
--    si tenemos ys' = [2,2,3,1,2] entonces obtenemos [[2,2,3],[2,1]].
--  * Devuelve en el par: primero la lista de listas obtenida en el
--    segundo punto, y segundo la list de listas del tercer punto.
-- Por simplicidad, asume que las longitudes de xs e ys son iguales.
-- Algunos ejemplos:
-- > agrupa [1,1,2,2,1] [3,2,2,1,2]
-- ([[1,1,1],[2,2]], [[2,2,3],[1,2]])
-- > agrupa [3,3,3,4] [1,2,3,4]
-- ([[3,3,3],[4]], [[1,2,3],[4]])
-- > agrupa ["S","S","N","L"] ["A","A","A","S"]  
-- ([["L"],["N"],["S","S"]], [["S"],["A"],["A","A"]])


-- [3,1,2]   [10,11,12] --> [1,2,3] [11,12,10]
agrupa :: (Ord a) => [a] -> [a] -> ([[a]],[[a]])
agrupa xs ys= (parte ls xs', parte ls ys') -- y dividimos esos segmentos son iguales en listas 
    where xs' = sort xs -- ordenamos las listas
          ys' = sort ys -- ordenamos las listas
          ls = listaparte xs -- dividimos los segmentos segun xs

-- lista que nos devuelve el numero de elementos que debemos dividir la lista 
listaparte xs = [contarrepetidos z xs| z <- ls]
    where ls = deleteDuplicate xs

-- contar numero de elementos de una lista 
contarrepetidos x1 [] = 0
contarrepetidos x1 (x:xs)
    | x1 == x = 1 + contarrepetidos x1 xs
    | otherwise = contarrepetidos x1 xs

-- funcion para eliminar elementos repetidos de una lista
deleteDuplicate :: (Eq a) => [a] -> [a]
deleteDuplicate [] = []
deleteDuplicate (x:xs) = x : deleteDuplicate (filter (/= x) xs)

-- ---------------------------------------------------------------------

