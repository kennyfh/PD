-- PD-PrÃ¡ctica 3 
-- Definiciones por comprensiÃ³n
-- Departamento de Ciencias de la ComputaciÃ³n e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- IntroducciÃ³n                                                       --
-- ---------------------------------------------------------------------

-- En esta relaciÃ³n se presentan ejercicios con definiciones de
-- funciones por comprensiÃ³n. Estos ejercicios se corresponden con el
-- tema 5 

-- ---------------------------------------------------------------------
-- ImportaciÃ³n de librerÃ­as auxiliares                                  
-- ---------------------------------------------------------------------
 
import Test.QuickCheck
import Data.Char
 
-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir, por comprensiÃ³n, la funciÃ³n
--    sumaDeCuadrados :: Integer -> Integer
-- tal que (sumaDeCuadrados n) es la suma de los cuadrados de los
-- primeros n nÃºmeros; es decir, 1^2 + 2^2 + ... + 100^2. Por ejemplo,
--    sumaDeCuadrados 3    ==  14
--    sumaDeCuadrados 100  ==  338350
-- ---------------------------------------------------------------------

sumaDeCuadrados n = sum [x^2 | x <- [1..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Un entero positivo es perfecto si es igual a la suma de
-- sus factores, excluyendo el propio nÃºmero. Usando una lista por
-- comprensiÃ³n y la funciÃ³n factores (del tema), definir la funciÃ³n 
--    perfectos :: Int -> [Int]
-- tal que (perfectos n) es la lista de todos los nÃºmeros perfectos
-- menores que n. Por ejemplo: 
--    *Main> perfectos 500
--    [6,28,496]
-- ---------------------------------------------------------------------
 
factores :: Int -> [Int]

factores n = [x | x <- [1..n-1], mod n x == 0]

perfectos :: Int -> [Int]

perfectos n = [x | x <- [1..n], sum (factores x)==x ]
 
-- ---------------------------------------------------------------------
-- Ejercicio 3. El producto escalar de dos listas de enteros xs y ys de
-- longitud n viene dado por la suma de los productos de los elementos
-- correspondientes. Definir por comprensiÃ³n la funciÃ³n 
--    productoEscalar :: [Int] -> [Int] -> Int
-- tal que (productoEscalar xs ys) es el producto escalar de las listas
-- xs e ys. Por ejemplo,
--    productoEscalar [1,2,3] [4,5,6]  =>  32
--
-- Usar QuickCheck para comprobar la propiedad conmutativa del producto
-- escalar.  
-- ---------------------------------------------------------------------
 
-- La definiciÃ³n es
productoEscalar :: [Int] -> [Int] -> Int
productoEscalar xs ys = sum [ x * y | (x,y) <- zip xs ys ]

-- La propiedad conmutativa es
prop_conmutativa_productoEscalar xs ys = undefined

-- La comprobaciÃ³n es 

-- ---------------------------------------------------------------------
-- Ejercicio 4 (Problema 1 del proyecto Euler) Definir la funciÃ³n
--    euler1 :: Integer -> Integer
-- (euler1 n) es la suma de todos los mÃºltiplos de 3 Ã³ 5 menores que
-- n. Por ejemplo,
--    euler1 10  ==  23
-- 
-- Calcular la suma de todos los mÃºltiplos de 3 Ã³ 5 menores que 1000.
-- ---------------------------------------------------------------------

euler1 :: Integer -> Integer
euler1 n = sum[  x  | x <- [1..n-1], mod x 3 == 0 || mod x 5 == 0]

-- El cÃ¡lculo es

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir por comprensiÃ³n la funciÃ³n
--    replica :: Int -> a -> [a]
-- tal que (replica n x) es la lista formada por n copias del elemento
-- x. Por ejemplo,
--    *Main> replica 3 True
--    [True, True, True]
-- Se corresponde con la funciÃ³n replicate.
-- ---------------------------------------------------------------------
 
replica :: Int -> a -> [a]
replica n x = [x | _ <-[1..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 6. Un nÃºmero natural n se denomina abundante si es menor
-- que la suma de sus divisores propios. Por ejemplo, 12 y 30 son
-- abundantes pero 5 y 28 no lo son.
-- ---------------------------------------------------------------------
-- Ejercicio 6.1. Definir la funciÃ³n numeroAbundante tal que
-- (numeroAbundante n) se verifica si n es un nÃºmero abundante. Por
-- ejemplo, 
--    numeroAbundante 5  == False
--    numeroAbundante 12 == True
--    numeroAbundante 28 == False
--    numeroAbundante 30 == True
-- ---------------------------------------------------------------------

numeroAbundante n = n < sum (factores n)

-- ---------------------------------------------------------------------
-- Ejercicio 6.2. Definir la funciÃ³n numerosAbundantesMenores tal que
-- (numerosAbundantesMenores n) es la lista de nÃºmeros abundantes
-- menores o iguales que n. Por ejemplo,
--    numerosAbundantesMenores 50  ==  [12,18,20,24,30,36,40,42,48]
-- ---------------------------------------------------------------------

numerosAbundantesMenores n = [x | x <-[1..n], (numeroAbundante x) ]

-- ---------------------------------------------------------------------
-- Ejercicio 6.3. Definir la funciÃ³n todosPares tal que (todosPares n)
-- se verifica si todos los nÃºmeros abundantes menores o iguales que n
-- son pares. Por ejemplo,
--    todosPares 10    ==  True
--    todosPares 100   ==  True
--    todosPares 1000  ==  False
-- ---------------------------------------------------------------------

todosPares n = and [ even x | x <- numerosAbundantesMenores n]

todosPares' n = all (even) (numerosAbundantesMenores n)

-- ---------------------------------------------------------------------
-- Ejercicio 6.4. Definir la constante primerAbundanteImpar que calcule
-- el primer nÃºmero natural abundante impar. Determinar el valor de
-- dicho nÃºmero.
-- ---------------------------------------------------------------------

primerAbundanteImpar = head [ x | x <- [1,3..], numeroAbundante x]
primerAbundanteImpar'  = head [x | x<-[1..], odd x && numeroAbundante x ]

-- Su cÃ¡lculo es

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la funciÃ³n suma tal (suma n) es la suma de los
-- n primeros nÃºmeros. Por ejemplo,
--    suma 3  ==  6
-- ---------------------------------------------------------------------

suma n = sum [x | x <- [1..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 8. Los triÃ¡ngulo aritmÃ©tico se forman como sigue
--     1
--     2  3
--     4  5  6
--     7  8  9 10
--    11 12 13 14 15
--    16 17 18 19 20 21
-- Definir la funciÃ³n linea tal que (linea n) es la lÃ­nea n-Ã©sima de los
-- triÃ¡ngulos aritmÃ©ticos. Por ejemplo, 
--    linea 4  ==  [7,8,9,10]
--    linea 5  ==  [11,12,13,14,15]
-- ---------------------------------------------------------------------

linea n = [ suma (n-1)+1..suma n ]

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la funciÃ³n triangulo tal que (triangulo n) es
-- el triÃ¡ngulo aritmÃ©tico de altura n. Por ejemplo,
--    triangulo 3  ==  [[1],[2,3],[4,5,6]]
--    triangulo 4  ==  [[1],[2,3],[4,5,6],[7,8,9,10]]
-- ---------------------------------------------------------------------

triangulo n = [linea x | x <-[1..n] ]

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la funciÃ³n circulo tal que (circulo n) es la
-- cantidad de pares de nÃºmeros naturales (x,y) que se encuentran dentro
-- del cÃ­rculo de radio n. Por ejemplo, 
--    circulo 3  ==  9
--    circulo 4  ==  15
--    circulo 5  ==  22
-- La ecuaciÃ³n de los puntos que estÃ¡n en la circunferencia de centro (0,0)
--  y radio n es: x^2 + y^2 = n^2. Los puntos del interior cumplen la 
-- desigualdad: x^2 + y^2 < n^2 
-- ---------------------------------------------------------------------

circulo n = length [(x,y) | x <- [0..n], y <- [0..n], x^2 + y^2 < n^2]

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la funciÃ³n ocurrenciasDelMaximo tal que
-- (ocurrenciasDelMaximo xs) es el par formado por el mayor de los
-- nÃºmeros de xs y el nÃºmero de veces que este aparece en la lista
-- xs, si la lista es no vacÃ­a y es (0,0) si xs es la lista vacÃ­a. Por
-- ejemplo,  
--    ocurrenciasDelMaximo [1,3,2,4,2,5,3,6,3,2,1,8,7,6,5]  ==  (8,1)
--    ocurrenciasDelMaximo [1,8,2,4,8,5,3,6,3,2,1,8]        ==  (8,3)
--    ocurrenciasDelMaximo [8,8,2,4,8,5,3,6,3,2,1,8]        ==  (8,4)
-- ---------------------------------------------------------------------

ocurrenciasDelMaximo xs = (maximum xs, length [x | x<-xs, x==maximum xs])

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir, por comprensiÃ³n, la funciÃ³n tienenS tal que
-- (tienenS xss) es la lista de las longitudes de las cadenas de xss que
-- contienen el caracter 's' en mayÃºsculas o minÃºsculas. Por ejemplo, 
--    tienenS ["Este","es","un","examen","de","hoy","Suerte"]  ==  [4,2,6]
--    tienenS ["Este"]                                         ==  [4]
--    tienenS []                                               ==  []
--    tienenS [" "]                                            ==  []
-- ---------------------------------------------------------------------

tienenS xss = [ length xs | xs <- xss, (elem 's' xs || elem 'S' xs)]

-- ---------------------------------------------------------------------
-- Ejercicio 13. Decimos que una lista estÃ¡ algo ordenada si para todo
-- par de elementos consecutivos se cumple que el primero es menor o
-- igual que el doble del segundo. Definir, por comprensiÃ³n, la funciÃ³n
-- (algoOrdenada xs) que se verifica si la lista xs estÃ¡ algo ordenada. 
-- Por ejemplo, 
--    algoOrdenada [1,3,2,5,3,8]  ==  True
--    algoOrdenada [3,1]          ==  False
-- ---------------------------------------------------------------------

algoOrdenada xs = and [x <= 2*y | (x,y) <- zip xs (tail xs)]

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir, por comprensiÃ³n, la funciÃ³n tripletas tal 
-- que (tripletas xs) es la listas de tripletas de elementos
-- consecutivos de la lista xs. Por ejemplo,
--    tripletas [8,7,6,5,4] == [[8,7,6],[7,6,5],[6,5,4]]
--    tripletas "abcd"      == ["abc","bcd"]
--    tripletas [2,4,3]     == [[2,4,3]]
--    tripletas [2,4]       == []
-- ---------------------------------------------------------------------

tripletas xs = [ [ xs!!n, xs!!(n+1),xs!!(n+2)] | n <- [0..(length xs -3)]]

-- ---------------------------------------------------------------------
-- Ejercicio 15. Definir la función tresConsecutivas tal que
-- (tresConsecutivas x ys) se verifica si x ocurre tres veces seguidas
-- en la lista ys. Por ejemplo,
--    tresConsecutivas 3 [1,4,2,3,3,4,3,5,3,4,6]  ==  False
--    tresConsecutivas 'a' "abcaaadfg"            ==  True
-- ---------------------------------------------------------------------

tresConsecutivas x ys = elem [x,x,x] (tripletas ys)

-- ---------------------------------------------------------------------
-- Ejercicio 16.1. Definir la funciÃ³n unitarios tal (unitarios n) es
-- la lista de nÃºmeros [n,nn, nnn, ....]. Por ejemplo. 
--    take 7 (unitarios 3) == [3,33,333,3333,33333,333333,3333333]
--    take 3 (unitarios 1) == [1,11,111]
-- ---------------------------------------------------------------------

unitarios x = [x*(div (10^y-1) 9) | y <- [1..]]

-- ---------------------------------------------------------------------
-- Ejercicio 16.2. Definir la función multiplosUnitarios tal que
-- (multiplosUnitarios x y n) es la lista de los n primeros mÃºltiplos de
-- x cuyo único dígito es y. Por ejemplo,
--    multiplosUnitarios 7 1 2  == [111111,111111111111]
--    multiplosUnitarios 11 3 5 == [33,3333,333333,33333333,3333333333]
-- ---------------------------------------------------------------------

 
ultimoDigito :: Integer -> Integer 
ultimoDigito n = n `rem` 10

multiplosUnitarios x y n = take n [z | z <- unitarios y, mod z x == 0]

-- ---------------------------------------------------------------------
-- Ejercicio 17. Definir la funciÃ³n primosEntre tal que (primosEntre x y)
-- es la lista de los nÃºmero primos entre x e y (ambos inclusive). Por
-- ejemplo, 
--    primosEntre 11 44  ==  [11,13,17,19,23,29,31,37,41,43]
-- ---------------------------------------------------------------------
primosEntre x y = [z | z<-[x..y], primo z]

primo :: Integer -> Bool
primo n = length (factores (fromIntegral n)) == 1

-- ---------------------------------------------------------------------
-- Ejercicio 18. Definir la funciÃ³n cortas tal que (cortas xs) es la
-- lista de las palabras mÃ¡s cortas (es decir, de menor longitud) de la
-- lista xs. Por ejemplo,
--    ghci> cortas ["hoy", "es", "un", "buen", "dia", "de", "sol"]
--    ["es","un","de"]
-- ---------------------------------------------------------------------

cortas xs = [x | x <-xs, length x == n]
            where n = minimum [length x | x <- xs]

-- ---------------------------------------------------------------------
-- Ejercicio 19. Un entero positivo n es libre de cuadrado si no es
-- divisible por ningún m^2 > 1. Por ejemplo, 10 es libre de cuadrado
-- (porque 10 = 2*5) y 12 no lo es (ya que es divisible por 2^2). 
-- Definir la funciÃ³n libresDeCuadrado tal que (libresDeCuadrado n) es
-- la lista de los primeros n nÃºmeros libres de cuadrado. Por ejemplo,
--    libresDeCuadrado 15  ==  [1,2,3,5,6,7,10,11,13,14,15,17,19,21,22]
-- ---------------------------------------------------------------------

libresDeCuadrado n = take n [x | x <- [1,2..], eslibreCuadrado x]

eslibreCuadrado n = null [m | m <- [2..n], rem n (m^2) == 0]

-- ---------------------------------------------------------------------
-- Ejercicio 20. Definir la función masOcurrentes tal que
-- (masOcurrentes xs) es la lista de los elementos de xs que ocurren el
-- mÃ¡ximo nÃºmero de veces. Por ejemplo,
--    masOcurrentes [1,2,3,4,3,2,3,1,4] == [3,3,3]
--    masOcurrentes [1,2,3,4,5,2,3,1,4] == [1,2,3,4,2,3,1,4]
--    masOcurrentes "Salamanca"         == "aaaa"
-- ---------------------------------------------------------------------
ocurrencias :: Eq a => a -> [a] -> Int
ocurrencias m xs = length [x | x <- xs, x==m]

masOcurrentes xs = [x | x <- xs, ocurrencias x xs == max]
        where max = maximum [ocurrencias x xs| x<-xs]

-- ---------------------------------------------------------------------
-- Ejercicio 21.1. Definir la funciÃ³n numDiv tal que (numDiv x) es el
-- nÃºmero de divisores del nÃºmero natural x. Por ejemplo, 
--    numDiv 11 == 2 
--    numDiv 12 == 6 
-- ---------------------------------------------------------------------

numDiv x = length [elem | elem <- [1..x], mod x elem == 0]

-- ---------------------------------------------------------------------
-- Ejercicio 21.2. Definir la función entre tal que (entre a b c) es la
-- lista de los naturales entre a y b con, al menos, c divisores. Por
-- ejemplo,  
--    entre 11 16 5 == [12, 16]
-- ---------------------------------------------------------------------

entre a b c = [x | x<- [a..b], (numDiv x) >= c]

-- ---------------------------------------------------------------------
-- Ejercicio 22. Definir la función conPos tal que (conPos xs) es la
-- lista obtenida a partir de xs especificando las posiciones de sus
-- elementos. Por ejemplo, 
--    conPos [1,5,0,7] == [(1,0),(5,1),(0,2),(7,3)]
-- ---------------------------------------------------------------------

conPos xs = zip xs [0.. length xs]

-- ---------------------------------------------------------------------
-- Ejercicio 23. Definir la funciÃ³n tal que (pares cs) es la cadena
-- formada por los caracteres en posiciÃ³n par de cs. Por ejemplo, 
--    pares "el cielo sobre berlin" == "e il or eln"
-- ---------------------------------------------------------------------

pares cs = [fst x | x <- ltuplas, even (snd x)]
        where ltuplas = conPos cs
-- ---------------------------------------------------------------------
-- Ejercicio 24.1. Una terna (x,y,z) de enteros positivos es pitagÃ³rica
-- si x^2 + y^2 = z^2. Usando una lista por comprensiÃ³n, definir la
-- funciÃ³n 
--    pitagoricas :: Int -> [(Int,Int,Int)]
-- tal que (pitagoricas n) es la lista de todas las ternas pitagÃ³ricas
-- cuyas componentes estÃ¡n entre 1 y n. Por ejemplo, 
--    pitagoricas 10  ==  [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]
-- ---------------------------------------------------------------------

pitagoricas :: Int -> [(Int,Int,Int)]
pitagoricas n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- ---------------------------------------------------------------------
-- Ejercicio 24.2. Definir la funciÃ³n 
--    numeroDePares :: (Int,Int,Int) -> Int
-- tal que (numeroDePares t) es el nÃºmero de elementos pares de la terna
-- t. Por ejemplo,
--    numeroDePares (3,5,7)  ==  0
--    numeroDePares (3,6,7)  ==  1
--    numeroDePares (3,6,4)  ==  2
--    numeroDePares (4,6,4)  ==  3
-- ---------------------------------------------------------------------

numeroDePares :: (Int,Int,Int) -> Int
numeroDePares (x,y,z) = length [num | num <-[x,y,z], even num]

-- ---------------------------------------------------------------------
-- Ejercicio 24.3. Definir la funciÃ³n
--    conjetura :: Int -> Bool
-- tal que (conjetura n) se verifica si todas las ternas pitagÃ³ricas
-- cuyas componentes estÃ¡n entre 1 y n tiene un nÃºmero impar de nÃºmeros
-- pares. Por ejemplo,
--    conjetura 10  ==  True
-- ---------------------------------------------------------------------

conjetura :: Int -> Bool
conjetura n = and [odd $ numeroDePares num | num <- pitagoricas n]

-- ---------------------------------------------------------------------
-- Ejercicio 25. Definir, por comprensión, la función
--    sumaConsecutivos :: [Int] -> [Int]
-- tal que (sumaConsecutivos xs) es la suma de los pares de elementos
-- consecutivos de la lista xs. Por ejemplo,
--    sumaConsecutivos [3,1,5,2]  ==  [4,6,7]
--    sumaConsecutivos [3]        ==  []
-- ---------------------------------------------------------------------

sumaConsecutivos :: [Int] -> [Int]
sumaConsecutivos xs = [x + y | (x,y) <- zip xs (tail xs)]
-- ---------------------------------------------------------------------
-- Ejercicio 26. Los polinomios pueden representarse de forma dispersa o
-- densa. Por ejemplo, el polinomio 6x^4-5x^2+4x-7 se puede representar
-- de forma dispersa por [6,0,-5,4,-7] y de forma densa por
-- [(4,6),(2,-5),(1,4),(0,-7)].  
-- 
-- Definir la funciÃ³n 
--    densa :: [Int] -> [(Int,Int)]
-- tal que, si xs es la forma dispersa de un polinomio, (densa xs) es la 
-- forma densa. 
-- representaciÃ³n dispersa es xs. Por ejemplo, 
--   densa [6,0,-5,4,-7]  ==  [(4,6),(2,-5),(1,4),(0,-7)]
--   densa [6,0,0,3,0,4]  ==  [(5,6),(2,3),(0,4)]
-- ---------------------------------------------------------------------

densa :: [Int] -> [(Int,Int)]
densa xs = [(x,y)| (x,y) <- zip lista xs, y/=0]
        where lista = [(length xs -1), (length xs -2)..0]

-- ---------------------------------------------------------------------
-- Ejercicio 27. La funciÃ³n 
--    pares2 :: [a] -> [b] -> [(a,b)]
-- definida por
--    pares2 xs ys = [(x,y) | x <- xs, y <- ys]
-- toma como argumento dos listas y devuelve la listas de los pares con
-- el primer elemento de la primera lista y el segundo de la
-- segunda. Por ejemplo,
--    ghci> pares2 [1..3] [4..6]
--    [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
-- 
-- Definir, usando dos listas por comprensiÃ³n con un generador cada una,
-- la funciÃ³n 
--    pares2' :: [a] -> [b] -> [(a,b)]
-- tal que pares2' sea equivalente a pares2.
-- 
-- Indicación: Utilizar la función predefinida concat y encajar una
-- lista por comprensiÃ³n dentro de la otra. 
-- ---------------------------------------------------------------------

-- La definiciÃ³n de pares es
pares2 :: [a] -> [b] -> [(a,b)]
pares2 xs ys = [(x,y) | x <- xs, y <- ys]

-- La redefinición de pares es
pares2' :: [a] -> [b] -> [(a,b)]
pares2' xs ys = concat [[(x,y) | y <- ys] | x<-xs]

-- ---------------------------------------------------------------------
-- Ejercicio 28. La bases de datos sobre actividades de personas pueden
-- representarse mediante listas de elementos de la forma (a,b,c,d),
-- donde a es el nombre de la persona, b su actividad, c su fecha de
-- nacimiento y d la de su fallecimiento. Un ejemplo es la siguiente que
-- usaremos a lo largo de este ejercicio,
-- ---------------------------------------------------------------------

personas :: [(String,String,Int,Int)]
personas = [("Cervantes","Literatura",1547,1616),
            ("Velazquez","Pintura",1599,1660),
            ("Picasso","Pintura",1881,1973),
            ("Beethoven","Musica",1770,1823),
            ("Poincare","Ciencia",1854,1912),
            ("Quevedo","Literatura",1580,1654),
            ("Goya","Pintura",1746,1828),
            ("Einstein","Ciencia",1879,1955),
            ("Mozart","Musica",1756,1791),
            ("Botticelli","Pintura",1445,1510),
            ("Borromini","Arquitectura",1599,1667),
            ("Bach","Musica",1685,1750)]

-- ---------------------------------------------------------------------
-- Ejercicio 28.1. Definir la funciÃ³n nombres tal que (nombres bd) es
-- la lista de los nombres de las personas de la base de datos bd. Por
-- ejemplo,  
--    ghci> nombres personas
--     ["Cervantes","Velazquez","Picasso","Beethoven","Poincare",
--      "Quevedo","Goya","Einstein","Mozart","Botticelli","Borromini","Bach"]
-- ---------------------------------------------------------------------

nombres :: [(String,String,Int,Int)] -> [String]
nombres bd = [x | (x,_,_,_) <- bd]

-- ---------------------------------------------------------------------
-- Ejercicio 28.2. Definir la funciÃ³n musicos tal que (musicos bd) es
-- la lista de los nombres de los mÃºsicos de la base de datos bd. Por
-- ejemplo,  
--    ghci> musicos personas
--    ["Beethoven","Mozart","Bach"]
-- ---------------------------------------------------------------------

musicos :: [(String,String,Int,Int)] -> [String]
musicos bd = [x | (x,m,_,_) <- bd, m=="Musica"]

-- ---------------------------------------------------------------------
-- Ejercicio 28.3. Definir la función seleccion tal que (seleccion bd m) 
-- es la lista de los nombres de las personas de la base de datos bd
-- cuya actividad es m. Por ejemplo,  
--    ghci> seleccion personas "Pintura"
--    ["Velazquez","Picasso","Goya","Botticelli"]
-- ---------------------------------------------------------------------

seleccion :: [(String,String,Int,Int)] -> String -> [String]
seleccion bd m = [x | (x,p,_,_) <- bd, p==m]

-- ---------------------------------------------------------------------
-- Ejercicio 28.4. Definir, usando el apartado anterior, la funciÃ³n
-- musicos' tal que (musicos' bd) es la lista de los nombres de los
-- mÃºsicos de la base de datos bd. Por ejemplo,  
--    ghci> musicos' personas
--    ["Beethoven","Mozart","Bach"]
-- ---------------------------------------------------------------------

musicos' :: [(String,String,Int,Int)] -> [String]
musicos' bd = seleccion bd "Musica"

-- ---------------------------------------------------------------------
-- Ejercicio 28.5. Definir la funciÃ³n vivas tal que (vivas bd a) es la
-- lista de los nombres de las personas de la base de datos bd  que
-- estaban vivas en el aÃ±o a. Por ejemplo,  
--    ghci> vivas personas 1600
--    ["Cervantes","Velazquez","Quevedo","Borromini"]
-- ---------------------------------------------------------------------

vivas :: [(String,String,Int,Int)] -> Int -> [String]
vivas ps a = [ n  | (n,_,c,m) <- ps, a>c && a<m]

-- ---------------------------------------------------------------------
-- Ejercicio 29.1. En este ejercicio se consideran listas de ternas de
-- la forma (nombre, edad, población). 
-- 
-- Definir la funciÃ³n puedenVotar tal que (puedenVotar t) es la
-- lista de las personas de t que tienen edad para votar. Por ejemplo,
--    ghci> :{
--    *Main| puedenVotar [("Ana", 16, "Sevilla"), ("Juan", 21, "Coria"), 
--    *Main|              ("Alba", 19, "Camas"), ("Pedro",18,"Sevilla")]
--    *Main| :}
--    ["Juan","Alba","Pedro"]
-- ---------------------------------------------------------------------

puedenVotar t = [a | (a,b,c) <- t, b>=18]

-- ---------------------------------------------------------------------
-- Ejercicio 29.2. Definir la funciÃ³n puedenVotarEn tal que (puedenVotar
-- t p) es la lista de las personas de t que pueden votar en la
-- poblaciÃ³n p. Por ejemplo, 
--    ghci> :{
--    *Main| puedenVotarEn [("Ana", 16, "Sevilla"), ("Juan", 21, "Coria"), 
--    *Main|                ("Alba", 19, "Camas"),("Pedro",18,"Sevilla")] 
--    *Main|               "Sevilla"
--    *Main| :}
--    ["Pedro"]
-- ---------------------------------------------------------------------

puedenVotarEn t p = [a | (a,b,c) <- t, p==c, b>=18]

-- ---------------------------------------------------------------------
-- Ejercicio 30. Dos listas xs, ys de la misma longitud son
-- perpendiculares si el producto escalar de ambas es 0, donde el
-- producto escalar de dos listas de enteros xs e ys viene
-- dado por la suma de los productos de los elementos correspondientes.
-- 
-- Definir la funciÃ³n perpendiculares tal que (perpendiculares xs yss)
-- es la lista de los elementos de yss que son perpendiculares a xs.
-- Por ejemplo,
--    ghci> perpendiculares [1,0,1] [[0,1,0], [2,3,1], [-1,7,1],[3,1,0]]
--    [[0,1,0],[-1,7,1]]
-- ---------------------------------------------------------------------

perpendiculares xs yss = [ys | ys <-yss, productoEscalar xs ys == 0]


-- ---------------------------------------------------------------------
-- Ejercicio 31.1. Un nÃºmero natural n
-- es especial si para todo divisor d de n, d+(n/d) es primo. Definir la
-- funciÃ³n  
--    especial :: Integer -> Bool
-- tal que (especial x) se verifica si x es especial. Por ejemplo,
--    especial 30  ==  True
--    especial 20  ==  False
-- ---------------------------------------------------------------------


especial :: Integer -> Bool
especial x = and [primo (fromIntegral (d + div x' d)) | d <- factores x']
  where x' = fromIntegral x

-- ---------------------------------------------------------------------
-- Ejercicio 31.2. Definir la función 
--    sumaEspeciales :: Integer -> Integer
-- tal que (sumaEspeciales n) es la suma de los nÃºmeros especiales
-- menores o iguales que n. Por ejemplo, 
--    sumaEspeciales 100  ==  401
-- ---------------------------------------------------------------------

sumaEspeciales :: Integer -> Integer
sumaEspeciales n = sum [x | x <- [1..n], especial x]

-- ---------------------------------------------------------------------
-- Ejercicio 32.1. Un número es muy compuesto si tiene más divisores que
-- sus anteriores. Por ejemplo, 12 es muy compuesto porque tiene 6
-- divisores (1, 2, 3, 4, 6, 12) y todos los nÃºmeros del 1 al 11 tienen
-- menos de 6 divisores.  
-- 
-- Definir la funciÃ³n
--    esMuyCompuesto :: Int -> Bool
-- tal que (esMuyCompuesto x) se verifica si x es un nÃºmero muy
-- compuesto. Por ejemplo,
--    esMuyCompuesto 24  ==  True
--    esMuyCompuesto 25  ==  False
-- Calcular  el menor nÃºmero muy compuesto de 4 cifras.
-- ---------------------------------------------------------------------

esMuyCompuesto :: Int -> Bool
esMuyCompuesto x = and [numDiv x > numDiv num|  num <-[1..x-1]] 

esMuyCompuestoMinimo = head [x | x<- [1000..9999], esMuyCompuesto x]


-- ---------------------------------------------------------------------
-- Ejercicio 32.2. Definir la funciÃ³n
--    muyCompuesto :: Int -> Int
-- tal que (muyCompuesto n) es el n-Ã©simo nÃºmero muy compuesto. Por
-- ejemplo, 
--    muyCompuesto 10  ==  180
-- ---------------------------------------------------------------------

muyCompuesto :: Int -> Int
muyCompuesto n = [x | x<- [1..], esMuyCompuesto x]!!10

-- ---------------------------------------------------------------------
-- Ejercicio 33. Definir la funciÃ³n  
--     todosIguales :: Eq a => [a] -> Bool
-- tal que (todosIguales xs) se verifica si los elementos de la 
-- lista xs son todos iguales. Por ejemplo,   
--     todosIguales [1..5]    == False
--     todosIguales [2,2,2]   == True
--     todosIguales ["a","a"] == True
-- ---------------------------------------------------------------------

todosIguales:: Eq a => [a] -> Bool
todosIguales xs = null xs || and [x == head xs| x<-xs]

-- ---------------------------------------------------------------------
-- Ejercicio 34.1. Las bases de datos de alumnos matriculados por
-- provincia y por especialidad se pueden representar como sigue 
--    matriculas :: [(String,String,Int)]
--    matriculas = [("Almeria","Matematicas",27),
--                  ("Sevilla","Informatica",325),
--                  ("Granada","Informatica",296),
--                  ("Huelva","Matematicas",41),
--                  ("Sevilla","Matematicas",122),
--                  ("Granada","Matematicas",131),
--                  ("Malaga","Informatica",314)]
-- Es decir, se indica que por ejemplo en Sevilla hay 325 alumnos
-- matriculados en InformÃ¡tica. 
-- 
-- Definir la funciÃ³n 
--    totalAlumnos :: [(String,String,Int)] -> Int
-- tal que (totalAlumnos bd) es el total de alumnos matriculados,
-- incluyendo todas las provincias y todas las especialidades, en la
-- base de datos bd. Por ejemplo,
--    totalAlumnos matriculas == 1256
-- ---------------------------------------------------------------------

matriculas :: [(String,String,Int)]
matriculas = [("Almeria","Matematicas",27),
              ("Sevilla","Informatica",325),
              ("Granada","Informatica",296),
              ("Huelva","Matematicas",41),
              ("Sevilla","Matematicas",122),
              ("Granada","Matematicas",131),
              ("Malaga","Informatica",314)]

totalAlumnos :: [(String,String,Int)] -> Int
totalAlumnos bd =  sum [c | (_,_,c) <- bd]

-- ---------------------------------------------------------------------
-- Ejercicio 34.2. Definir la funciÃ³n 
--    totalMateria :: [(String,String,Int)] -> String -> Int
-- tal que (totalMateria bd m) es el nÃºmero de alumnos de la base de
-- datos bd matriculados en la materia m. Por ejemplo, 
--    totalMateria matriculas "Informatica" == 935
--    totalMateria matriculas "Matematicas" == 321
--    totalMateria matriculas "Fisica"      == 0
-- ---------------------------------------------------------------------

totalMateria :: [(String,String,Int)] -> String -> Int
totalMateria bd m = sum [c | (_,b,c) <- bd, m==b]

-- ---------------------------------------------------------------------
-- Ejercicio 35. Dada una lista de nÃºmeros enteros, definiremos el
-- mayor salto como el mayor valor de las diferencias (en valor
-- absoluto) entre nÃºmeros consecutivos de la lista. Por ejemplo, dada
-- la lista [2,5,-3] las distancias son 
--    3 (valor absoluto de la resta 2 - 5) y
--    8 (valor absoluto de la resta de 5 y (-3))
-- Por tanto, su mayor salto es 8. No estÃ¡ definido el mayor salto para
-- listas con menos de 2 elementos
--
-- Definir la funciÃ³n 
--    mayorSalto :: [Integer] -> Integer
-- tal que (mayorSalto xs) es el mayor salto de la lista xs. Por
-- ejemplo, 
--    mayorSalto [1,5]              == 4
--    mayorSalto [10,-10,1,4,20,-2] == 22
-- ---------------------------------------------------------------------

-- mayorSalto :: [Integer] -> Integer
mayorSalto xs = maximum [abs (x-y)| (x,y) <- zip xs (tail xs)]