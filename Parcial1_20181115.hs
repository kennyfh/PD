-- Programación Declarativa
-- Grado de Ingeniería Informática - Tecnologías Informáticas
-- Parcial 1                                   15 de Noviembre de 2018
-- -------------------------------------------------------------------
-- Apellidos:
-- Nombre:
-- -------------------------------------------------------------------
-- AVISOS IMPORTANTES
-- · Antes de continuar, cambie el nombre de este archivo por:
--                   Parcial1_<uvus>.hs
--   donde <uvus> debe ser su usuario virtual.
-- · Escriba la solución de cada ejercicio en el hueco reservado para
--   ello.
-- · Asegúrese de utilizar correctamente el nombre y el tipo indicado
--   para cada función solicitada. Puede añadir tantas funciones
--   auxiliares (incluyendo el tipo adecuadamente) como necesite,
--   describiendo su objetivo.
-- -------------------------------------------------------------------
import Data.List.Split
import Data.List --(sortBy)
import Data.Function --(on)
-- ---------------------------------------------------------------------
-- Ejercicio 1. [0.75 ptos]
-- En geometría, la fórmula de Brahmagupta, dice que:
-- el área de un cuadrilátero cuyo lados miden a, b, c y d es la raíz
-- cuadrada de (s-a)(s-b)(s-c)(s-d), donde s es el semiperímetro 
--    s = (a+b+c+d)/2
-- 
-- Definir adecuadamente, evitando cálculos redundantes, la función 
--    area :: Double -> Double -> Double -> Double -> Double
-- tal que (area a b c d) es el área del cuadrilátero de lados a,b,c,d.
-- Por ejemplo:
--    area 6 9 7 2 ==> 30.
-- ---------------------------------------------------------------------
area :: Double -> Double -> Double -> Double -> Double
area a b c d = sqrt ((s-a)*(s-b)*(s-c)*(s-d))
        where s = (a+b+c+d) / 2        

-- -------------------------------------------------------------------
-- Ejercicio 2. [0.75 ptos]
-- Defina una función comparaDistintos, explicitando su tipo,
-- tal que reciba dos argumentos y devuelva un resultado, de modo que:
-- a. El tipo de los argumentos sea polimórfico (no concreto):
--  - El primero debe admitir el operador de igualdad y ser entero.
--  - El segundo ha de ser ordenable y racional.
--  - El resultado debe ser del mismo tipo que el segundo argumento.
-- b. La función haga lo siguiente:
--  - Si el primer parámetro es mayor que el segundo, devolver su
--  diferencia.
--  - Si el segundo es mayor, devolver el doble del primero.
--  - Si en esencia valen lo mismo, devolver su valor.
--
-- Por ejemplo:
--   comparaDistintos 3 4.5 ==> 9.0
--   comparaDistintos 4 3.5 ==> 0.5
--   comparaDistintos 4 4.0 ==> 4.0
-- -------------------------------------------------------------------
comparaDistintos :: (Integral a, Eq a, Ord b, Fractional b) => a -> b -> b
comparaDistintos x y
    | m < y = 2*y
    | m > y = m-y
    | otherwise = m
    where m = fromIntegral x
-- ---------------------------------------------------------------------
-- Ejercicio 3. [1 pto]
-- Definir la función casi_extremos tal que (casi_extremos n xs) es
-- la lista formada por los n primeros elementos de xs (salvo el primero)
-- y los n elementos finales de xs (salvo el ultimo).
-- Nota: si la lista no tien elementos debe saltar un error controlado.
--
-- Por ejemplo:
--    casi_extremos 2 [] ==> "No es posible obtener los casi extremos"
--    casi_extremos 3 [1..10] ==> [2,3,4,7,8,9]
--    casi_extremos 2 [2,6,7,1,2,4,5,8,9,2,3]  ==>  [6,7,9,2]
--    casi_extremos 3 [2,6,7,1,2,4]  ==>  [6,7,1,8,9,2]
-- ---------------------------------------------------------------------
casi_extremos n [] = error "No es posible obtener los casi extremos"
casi_extremos n xs = (take n (tail xs)) ++ (reverse $ take n rv)
    where rv = tail $ reverse xs
-- ---------------------------------------------------------------------
-- Ejercicio 4. [0.5 ptos]
-- Definir una propiedad prop_casiext_reverse (y probarla con QuickCheck)
-- que indique que invertir la lista de los casi extremos n xs
-- es equivalente a
-- calcular los casi extremos n de la lista inversa de xs
-- Indique cómo debemos realizar la llamada a QuickCheck para probar
-- la propiedad
-- ---------------------------------------------------------------------

-- -------------------------------------------------------------------
-- Ejercicio 5. [1 pto]
-- Defina una función cumpleUnoDeTres,
-- que reciba como argumentos un predicado y una lista de elementos,
-- e indique si uno y solo uno de cada grupo de 3 elementos de la
-- lista, tomados de izquierda a derecha, cumple el predicado.
-- Nota: los grupos de menos de 3 elementos deben responder
--       afirmativamente al ejercicio.
--
-- Por ejemplo:
--   cumpleUnoDeTres even [1..100] ==> False
--   cumpleUnoDeTres (\x -> mod x 3 == 0) [1..100] ==> True
--   cumpleUnoDeTres (elem 'a') ["no","hay","prob","bro"] ==> True
--   cumpleUnoDeTres (elem 'a') ["no","prob","bro"] ==> False
--   cumpleUnoDeTres (elem 'a') ["no","hay","prab","bro"] ==> False
--   cumpleUnoDeTres (elem 'a') ["no","hay","prob","e","bro"] ==> True
--   cumpleUnoDeTres (elem 'a') ["a","prob","e","b","r","o"] ==> False
-- -------------------------------------------------------------------
cumpleUnoDeTres _ [] = True
cumpleUnoDeTres _ [x] = True
cumpleUnoDeTres _ [x,y] = True
cumpleUnoDeTres p (x:y:z:xs) = length (filter p [x,y,z]) == 1 && cumpleUnoDeTres p xs
-- -------------------------------------------------------------------
-- Ejercicio 6. [1.5 ptos]
-- Desarrolle una función principal, main, con una animación usando
-- CodeWorld, de modo que la escena incluya un fondo estático (basta
-- con un cuadrado negro que ocupe la mayor parte de la pantalla),
-- y una parte móvil (círculo o cuadrado) que se vaya desplazando a
-- izquierda y derecha o bien hacia arriba y hacia abajo.
-- -------------------------------------------------------------------

-- -------------------------------------------------------------------
-- Ejercicio 7. [1 pto]
-- Dada la siguiente información acerca de personas, incluyendo su
-- nombre, ámbito en que destacaron, y rango en que vivieron dado por
-- (inicio, fin):

personas :: [(String,String,(Int,Int))]
personas = [("Cervantes","Literatura",(1547,1616)),
            ("Velazquez","Pintura",(1599,1660)),
            ("Picasso","Pintura",(1881,1973)),
            ("Beethoven","Musica",(1770,1823)),
            ("Poincare","Ciencia",(1854,1912)),
            ("Quevedo","Literatura",(1580,1654)),
            ("Goya","Pintura",(1746,1828)),
            ("Einstein","Ciencia",(1879,1955)),
            ("Mozart","Musica",(1756,1791)),
            ("Botticelli","Pintura",(1445,1510)),
            ("Borromini","Arquitectura",(1599,1667)),
            ("Bach","Musica",(1685,1750))]

-- Definir, mediante listas por comprensión, la función
-- primero_destacado, tal que primero_destacado x bd devuelva el
-- nombre de la primera persona nacida, destacada en el ámbito x
-- en la base de datos bd, en orden cronológico.
--
-- Por ejemplo:
--    primero_destacado "Musica" personas ==> "Bach"
--    primero_destacado "Ciencia" personas ==> "Poincare"
--
-- Ayuda: no tenga reparo en ir definiendo cuantas funciones
-- auxiliares necesite haciendo uso de cuantas funciones, es mejor
-- descomponer un problema en partes para facilitar su resolución.
-- 
-- -------------------------------------------------------------------
primero_destacado x bd = head [nom | (nom,a,t) <- ord_anyo]
    where ls = [(n,a,fst t) | (n,a,t) <- bd, a==x]
          ord_anyo = (sortBy (compare `on` (\(a,b,c) -> c)) ls)


-- ord_anyo xs =  reverse (sortBy (compare `on` (\(a,b,c) -> c)) xs)
-- lista_tuplas_ambito x bd = [(n,a,fst t) | (n,a,t) <- bd, a==x]

-- ---------------------------------------------------------------------
-- Ejercicio 8. [2 ptos]
-- Se considera la función procesaNoValidos
-- :: (Num a, Ord b) => (a -> b) -> (a -> b) -> (a -> Bool) -> [a] -> [b]
-- tal que (procesaNoValidos f g p xs) es la lista obtenida aplicándole a
-- los elementos de xs que NO cumplen el predicado p, el máximo de
-- los resultados de la aplicación de la función f y la función g.
-- Por ejemplo:
--    procesaNoValidos (4+) (2*) (<3) [1..7]  =>  [7,8,10,12,14]
-- Se pide, definir la función
-- 1. usando map y filter,
-- 2. por recursión,
-- 3. por recursión con acumulador,
-- 4. por plegado (a izquierda o derecha).
-- ---------------------------------------------------------------------
procesaNoValidosOrd f g p xs = map (\x -> max (f x) (g x)) $ filter (not.p) xs

procesaNoValidosRe f g p [] = []
procesaNoValidosRe f g p (x:xs)
    | not (p x) = [max (f x) (g x)] ++ procesaNoValidosRe f g p xs
    | otherwise = procesaNoValidosRe f g p xs

procesaNoValidosRee f g p xs = procesaNoValidosRee_aux f g p xs []

procesaNoValidosRee_aux f g p [] acum = acum
procesaNoValidosRee_aux f g p (x:xs) acum
    | not (p x) = procesaNoValidosRee_aux f g p xs (acum ++ [max (f x) (g x)])
    | otherwise =  procesaNoValidosRee_aux f g p xs acum

procesaNoValidosplegado f g p = foldl (\acc x -> if (not (p x)) then acc ++ [max (f x) (g x)] else acc) []

-- ---------------------------------------------------------------------
-- Ejercicio 9. [1.5 ptos]
-- Escriba un programa de Entrada y Salida que haga lo siguiente:
-- 1. Imprima un mensaje por pantalla solicitando un número natural al
--    usuario
-- 2. Reciba el número del usuario por teclado
-- 3. Calcule el cuadrado del número
-- 4. Muestre por pantalla que el cuadrado del número x es y, o similar.
-- 5. Almacene esa misma frase emitida a un fichero de texto.
-- ---------------------------------------------------------------------
