-- Programación Declarativa 2020/21
-- Grado de Ingeniería Informática - Tecnologías Informáticas
-- Parcial 2 (parte b)                            22 de Diciembre de 2020
-- ----------------------------------------------------------------------
-- Apellidos: Flores Huamán
-- Nombre: Kenny Jesús
-- UVUS: Kenflohua
-- ----------------------------------------------------------------------

import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 3. (2 puntos)

-- Ejercicio 3.a. (0,5 puntos)
-- Declara el tipo Tabla con un solo constructor, T, tal que sirva
-- para representar una tabla de datos cuyos valores son de tipo 'a'
-- (polimórfico). El constructor T debe tener asociados los siguientes 
-- parámetros:
--  * un String, que será el nombre de la tabla.
--  * una lista de strings, que codificará el nombre de las columnas de 
--    la tabla.
--  * una lista de listas de cualquier tipo 'a', que almacenará los valores
--    de cada columna como listas.
--  * un Int, que corresponderá con el número de filas de la tabla.
-- El tipo se debe poder imprimir por pantalla. 

-- Tabla = ...

-- 1) 

data Tabla a = T String [String] [[a]] Int deriving Show



-- Ejercicio 3.b. (1,5 puntos)
-- Para este ejercicio vamos a usar la función agrupa que debiste definir
-- en el ejercicio 2. Si no la pudiste definir, puedes hacer uso de la 
-- definición siguiente de juguete (aunque no haga lo que debería, la 
-- puedes usar para este ejercicio sin afectar a la nota)

--agrupa :: (Ord a) => [a] -> [a] -> ( [[a]] , [[a]] )
--agrupa xs ys = ([xs], [ys])

-- Define la función 
--    (agrupaPor t colref coldest fagr),
-- tal que reciba una tabla 't' (de tipo Tabla y que contiene valores de 
-- tipo Ordenables), dos argumentos de tipo String, 'colref' y 'coldest', y 
-- una función de agregación 'fagr' (es decir, de tipo '[a] -> a', por ejemplo,
-- podrá ser sum, maximum, product, etc.).
-- La función toma de 't' la columna cuyo nombre es 'colref', la columna con nombre
-- 'coldest' y las agrupa con la función 'agrupa' (ver ejercicio 2). Sea (xss,yss)
-- la salida de 'agrupa', entonces vamos a resumir cada sublista de xss e yss de 
-- la siguiente forma: 
--   * para cada xs de xss obtenemos un valor x que es el primero de la lista
--   * para cada ys de yss obtenemos un valor y que es el resultado de aplicar la
--     función de agregación, fagr, a ys. Por ejemplo,
--     si el resultado de 'agrupa' es ([[1,1,1],[2,2]],[[1,2,3],[4,5]]), y 'fagr' es
--     la función sum, entonces obtenemos [1,2] y [5,9], respectivamente.
-- Finalmente, el resultado de 'agrupaPor' es otra tabla cuyas columnas son 'colref'
-- y 'coldest', y los valores son los resultados de aplicar la operación descrita
-- anteriormente. Veamos algunos ejemplos para entenderlo mejor:

-- Supongamos la siguiente tabla (descoméntala una vez hayas hecho el 3.a)
tablaEj :: Tabla Int 
tablaEj = T "origen" ["Edad","Altura","Salario"] [[25,25,24,25],[150,165,150,175],[1234,1435,1102,1421]] 4 

-- > agrupaPor tablaEj "Edad" "Salario" sum
-- T "agrupado" ["Edad","Salario"] [[24,25],[1102,4090]] 2
-- > agrupaPor tablaEj "Altura" "Salario" maximum
-- T "agrupado" ["Altura","Salario"] [[150,165,175],[1234,1435,1421]] 3

-- Si has usado la definición de agrupa de jueguete de arriba, los ejemplos son:
-- > agrupaPor tablaEj "Edad" "Salario" sum
-- T "agrupado" ["Edad","Salario"] [[25],[5192]] 1
-- > agrupaPor tablaEj "Altura" "Salario" maximum
-- T "agrupado" ["Altura","Salario"] [[150],[1435]] 1

agrupaPor = undefined
          
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 4. (1,5 puntos)
-- Los árboles binarios con valores distintos en nodos y en hojas se 
-- pueden representar mediante el tipo Arbol definido por 

data Arbol a b = H b 
                | N a (Arbol a b) (Arbol a b)
            deriving Show

-- Por ejemplo, un árbol con pares de enteros en los nodos y enteros en las
-- hojas:
--        (4,8)
--         / \ 
--        /   \
--       /     \
--    (4,2)   (8,5)
--     / \     / \
--    4   2   8   5 
--
-- se puede definir por
--    N (4,8) (N (4,2) (H 4) (H 2)) (N (8,5) (H 8) (H 5)) 

-- Un árbol como el anterior lo vamos a llamar Árbol de Máximos, ya que para
-- cada nodo interno hay un par que indica con la primera componente cuál es
-- el valor máximo de las hojas por el subárbol izquierdo, y en la segunda
-- componente cual es el máximo de las hojas del subárbol derecho. Define 
-- la función
--    maximosHojas :: Arbol Int Int -> Arbol (Int,Int) Int
-- tal que (maximosHojas a) calcule el árbol de máximos, tal y como se ha 
-- ilustrado arriba, para el árbol a. Por ejemplo,

ej1,ej2,ej3 :: Arbol Int Int 
ej1 = (N 1 (N 1 (H 4) (H 2)) (N 1 (H 8) (H 5)))
ej2 = (N 9 (N 4 (H 8) (H 4)) (N 8 (H 4) (H 9)))
ej3 = (N 29 (H 1) (N 524 (H 1) (H 1)))

-- data Arbol a b = H b 
--                 | N a (Arbol a b) (Arbol a b)
--             deriving Show
-- > maximosHojas ej1
-- N (4,8) (N (4,2) (H 4) (H 2)) (N (8,5) (H 8) (H 5))
-- > maximosHojas ej2
-- N (8,9) (N (8,4) (H 8) (H 4)) (N (4,9) (H 4) (H 9))
-- > maximosHojas ej3
-- N (1,1) (H 1) (N (1,1) (H 1) (H 1))
-- ---------------------------------------------------------------------

maximosHojas :: Arbol Int Int -> Arbol (Int,Int) Int
maximosHojas (H b) = (H b) -- si es una hoja, devuelve su propia hoja
maximosHojas (N a izq der)= N (maximolista $ maximo izq, maximolista $ maximo der) (maximosHojas izq) (maximosHojas der)
{-
Si es un arbol, el nodo padre va a estar formado por una tupla de:
    (valor maximo de la hoja por la izq, valor máximo de la hoja por la derecha)
    y se va a ir recorriendo recursivamente hasta obtener el nuevo arbol

-}
-- devolvemos el valor maximo de las hojas en un árbol concreto
maximolista :: [Int] -> Int
maximolista xs =  maximum xs

-- calculamos una lista de valores que tienen las hojas
maximo :: Arbol Int Int -> [Int]
maximo (H b) = [b]
maximo (N a izq der)= (maximo izq) ++ (maximo der) 