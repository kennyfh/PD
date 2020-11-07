-- PD-PrÃ¡ctica 5
-- Definiciones por recursiÃ³n.
-- Departamento de Ciencias de la ComputaciÃ³n e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir, por recursiÃ³n, la funciÃ³n
--    sumaCuadradosImpares :: [Integer] -> Integer
-- tal que (sumaCuadradosImparesR xs) es la suma de los cuadrados de los
-- nÃºmeros impares de la lista xs. Por ejemplo,
--    sumaCuadradosImparesR [1,2,3]  ==  10
-- ---------------------------------------------------------------------
sumaCuadradosImparesR [] = 0
sumaCuadradosImparesR (x:xs)
    | odd x = x^2 + sumaCuadradosImparesR xs
    | otherwise  = sumaCuadradosImparesR xs

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir, usando recursiÃ³n, la funciÃ³n
--    entre :: Integer -> Integer -> [Integer]
-- tal que (entreL m n) es la lista de los nÃºmeros entre m y n. Por
-- ejemplo, 
--    entreL 2 5  ==  [2,3,4,5]
-- ---------------------------------------------------------------------

entreL m n = entre_aux m n []

entre_aux m n xs
    | m > n = xs
    | otherwise = entre_aux (m+1) (n) (xs ++ [m])
-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir, por recursiÃ³n, la funciÃ³n
--    sumaPositivosRec :: [Int] -> Int
-- tal que (sumaPositivosRec xs) es la suma de los nÃºmeros positivos de
-- xs. Por ejemplo, 
--    sumaPositivosRec [0,1,-3,-2,8,-1,6]  ==  15
-- ---------------------------------------------------------------------
sumaPositivosRec [] = 0
sumaPositivosRec (x:xs)
    | (signum x) >= 0 = x + sumaPositivosRec xs
    | otherwise = sumaPositivosRec xs
-- ---------------------------------------------------------------------
-- Ejercicio 4. El doble factorial de un nÃºmero n se define por 
--    n!! = n*(n-2)* ... * 3 * 1, si n es impar
--    n!! = n*(n-2)* ... * 4 * 2, si n es par
--    1!! = 1
--    0!! = 1    
-- Por ejemplo,
--    8!! = 8*6*4*2   = 384
--    9!! = 9*7*5*3*1 = 945
-- Definir, por recursiÃ³n, la funciÃ³n
--    dobleFactorial :: Integer -> Integer
-- tal que (dobleFactorial n) es el doble factorial de n. Por ejemplo,
--    dobleFactorial 8  ==  384
--    dobleFactorial 9  ==  945
-- ---------------------------------------------------------------------
dobleFactorial 0 = 1
dobleFactorial 1 = 1
dobleFactorial n = n * dobleFactorial (n-2)

-- ---------------------------------------------------------------------
-- Ejercicio 5. La distancia de Hamming entre dos listas es el
-- nÃºmero de posiciones en que los correspondientes elementos son
-- distintos. Por ejemplo, la distancia de Hamming entre "roma" y "loba"
-- es 2 (porque hay 2 posiciones en las que los elementos
-- correspondientes son distintos: la 1Åž y la 3Åž).
--    
-- Definir la funciÃ³n
--    distancia :: Eq a => [a] -> [a] -> Int
-- tal que (distancia xs ys) es la distancia de Hamming entre xs e
-- ys. Por ejemplo,
--    distancia "romano" "comino"  ==  2
--    distancia "romano" "camino"  ==  3
--    distancia "roma"   "comino"  ==  2
--    distancia "roma"   "camino"  ==  3
--    distancia "romano" "ron"     ==  1
--    distancia "romano" "cama"    ==  2
--    distancia "romano" "rama"    ==  1
-- ---------------------------------------------------------------------
distancia [] ys = 0
distancia xs [] = 0
distancia (x:xs) (y:ys)
    | x /= y = 1 + (distancia xs ys)
    | otherwise =  distancia xs ys

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir por recursiÃ³n la funciÃ³n 
--    sustituyeImpar :: [Int] -> [Int]
-- tal que (sustituyeImpar xs) es la lista obtenida sustituyendo cada
-- nÃºmero impar de xs por el siguiente nÃºmero par. Por ejemplo,
--    sustituyeImpar [2,5,7,4]  ==  [2,6,8,4]
-- --------------------------------------------------------------------- 
sustituyeImpar [] = []
sustituyeImpar (x:xs)
    | even x = x:[] ++ sustituyeImpar xs
    | otherwise = (x+1):[] ++ sustituyeImpar xs

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir, por recursiÃ³n, la funciÃ³n
--    digitosR :: Integer -> [Integer]
-- tal que (digitosR n) es la lista de los dÃ­gitos del nÃºmero n. Por
-- ejemplo, 
--    digitosR 320274  ==  [3,2,0,2,7,4]
-- ---------------------------------------------------------------------
digitosR 0 = []
digitosR n = digitosR (div n 10) ++ [(mod n 10)]
-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir por recursiÃ³n la funciÃ³n
--    potencia :: Integer -> Integer -> Integer
-- tal que (potencia x n) es x elevado al nÃºmero natural n. Por ejemplo,  
--    potencia 2 3  ==  8
-- ---------------------------------------------------------------------
potencia _ 0 = 1
potencia x n = 2 * (potencia x (n-1))
-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir por recursiÃ³n la funciÃ³n
--    replicate' :: Int -> a -> [a]
-- tal que (replicate' n x) es la lista formado por n copias del
-- elemento x. Por ejemplo,
--    replicate' 3 2  ==  [2,2,2]
-- ---------------------------------------------------------------------
replicate' 0 _ = []
replicate' n x = x:[] ++  replicate' (n-1) x
-- ---------------------------------------------------------------------
-- Ejercicio 10. Dados dos nÃºmeros naturales, a y b, es posible
-- calcular su mÃ¡ximo comÃºn divisor mediante el Algoritmo de
-- Euclides. Este algoritmo se puede resumir en la siguiente fÃ³rmula:
--    mcd(a,b) = a,                   si b = 0
--             = mcd (b, a mÃ³dulo b), si b > 0
-- 
-- Definir la funciÃ³n 
--    mcd :: Integer -> Integer -> Integer
-- tal que (mcd a b) es el mÃ¡ximo comÃºn divisor de a y b calculado
-- mediante el algoritmo de Euclides. Por ejemplo,
--    mcd 30 45  ==  15
-- ---------------------------------------------------------------------
mcd a b
    | b == 0 = a
    | b > 0  = mcd b (mod a b)
-- ---------------------------------------------------------------------
-- Ejercicio 11. En un templo hindÃº se encuentran tres varillas de
-- platino. En una de ellas, hay 64 anillos de oro de distintos radios,
-- colocados de mayor a menor.
-- 
-- El trabajo de los monjes de ese templo consiste en pasarlos todos a
-- la tercera varilla, usando la segunda como varilla auxiliar, con las
-- siguientes condiciones: 
--   * En cada paso sÃ³lo se puede mover un anillo.
--   * Nunca puede haber un anillo de mayor diÃ¡metro encima de uno de
--     menor diÃ¡metro.
-- La leyenda dice que cuando todos los anillos se encuentren en la
-- tercera varilla, serÃ¡ el fin del mundo.  
-- 
-- Definir la funciÃ³n 
--    numPasosHanoi :: Integer -> Integer
-- tal que (numPasosHanoi n) es el nÃºmero de pasos necesarios para
-- trasladar n anillos. Por ejemplo, 
--    numPasosHanoi 2   ==  3
--    numPasosHanoi 7   ==  127
--    numPasosHanoi 64  ==  18446744073709551615
-- ---------------------------------------------------------------------

-- Sean A, B y C las tres varillas. La estrategia recursiva es la
-- siguiente: 
-- * Caso base (N=1): Se mueve el disco de A a C.
-- * Caso inductivo (N=M+1): Se mueven M discos de A a C. Se mueve el disco
--   de A a B. Se mueven M discos de C a B.
-- Por tanto,
numPasosHanoi n
    | n ==1 = 1
    | otherwise = 2*numPasosHanoi(n-1) +1
-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir por recursiÃ³n la funciÃ³n
--    and' :: [Bool] -> Bool
-- tal que (and' xs) se verifica si todos los elementos de xs son
-- verdadero. Por ejemplo,
--    and' [1+2 < 4, 2:[3] == [2,3]]  ==  True
--    and' [1+2 < 3, 2:[3] == [2,3]]  ==  False
-- ---------------------------------------------------------------------
and' [] = True
and' (x:xs) = x && (and' xs) 

-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir por recursiÃ³n la funciÃ³n
--    elem' :: Eq a => a -> [a] -> Bool
-- tal que (elem' x xs) se verifica si x pertenece a la lista xs. Por
-- ejemplo, 
--    elem' 3 [2,3,5]  ==  True
--    elem' 4 [2,3,5]  ==  False
-- ---------------------------------------------------------------------
elem' x (y:ys)
    | null ys =  False
    | x==y = True || elem' x ys
    | otherwise = False || elem' x ys

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir por recursiÃ³n la funciÃ³n
--    last' :: [a] -> a
-- tal que (last xs) es el Ãºltimo elemento de xs. Por ejemplo,
--    last' [2,3,5]  =>  5
-- ---------------------------------------------------------------------
last' (x:xs)
    | null xs = x
    | otherwise = last' xs
-- ---------------------------------------------------------------------
-- Ejercicio 15. Definir por recursiÃ³n la funciÃ³n
--    concat' :: [[a]] -> [a]
-- tal que (concat' xss) es la lista obtenida concatenando las listas de
-- xss. Por ejemplo,
--    concat' [[1..3],[5..7],[8..10]]  ==  [1,2,3,5,6,7,8,9,10]
-- ---------------------------------------------------------------------
concat' [xs] =  xs
concat' (xs:xss) =  xs ++ concat' xss
-- ---------------------------------------------------------------------
-- Ejercicio 16. Definir por recursiÃ³n la funciÃ³n
--    selecciona :: [a] -> Int -> a
-- tal que (selecciona xs n) es el n-Ã©simo elemento de xs. Por ejemplo,
--    selecciona [2,3,5,7] 2  ==  5 
-- ---------------------------------------------------------------------
selecciona (x:xs) n
    | n == 0 = x
    | null xs =  error "f"
    | otherwise =  selecciona xs (n-1)
-- ---------------------------------------------------------------------
-- Ejercicio 17. Definir por recursiÃ³n la funciÃ³n
--    take' :: Int -> [a] -> [a]
-- tal que (take' n xs) es la lista de los n primeros elementos de
-- xs. Por ejemplo, 
--    take' 3 [4..12]  =>  [4,5,6]
-- ---------------------------------------------------------------------
take' 0 xs = []
take' n (x:xs) =  x:[] ++  take' (n-1) xs
-- ---------------------------------------------------------------------
-- Ejercicio 18. Definir por recursión la funciÃ³n
--    mezcla :: Ord a => [a] -> [a] -> [a] 
-- tal que (mezcla xs ys) es la lista obtenida mezclando las listas
-- ordenadas xs e ys. Por ejemplo,  
--    mezcla [2,5,6] [1,3,4]  ==  [1,2,3,4,5,6]
-- ---------------------------------------------------------------------
mezcla xs [] = xs
mezcla [] ys = ys
mezcla (x:xs) (y:ys)
    | x < y =  x:(mezcla xs (y:ys))
    | otherwise = y:(mezcla (x:xs) ys)
-- ---------------------------------------------------------------------
-- Ejercicio 19. Definir la funciÃ³n 
--    mitades :: [a] -> ([a],[a]) 
-- tal que (mitades xs) es el par formado por las dos mitades en que se
-- divide xs tales que sus longitudes difieren como mÃ¡ximo en uno. Por
-- ejemplo, 
--    mitades [2,3,5,7,9]  ==  ([2,3],[5,7,9])
-- ---------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- Ejercicio 20. Definir por recursiÃ³n la funciÃ³n 
--    ordMezcla :: Ord a => [a] -> [a]
-- tal que (ordMezcla xs) es la lista obtenida ordenado xs por mezcla
-- (es decir, considerando que la lista vacÃ­a y las listas unitarias
-- estÃ¡n ordenadas y cualquier otra lista se ordena mezclando las dos
-- listas que resultan de ordenar sus dos mitades por separado). Por
-- ejemplo, 
--    ordMezcla [5,2,3,1,7,2,5]  ==  [1,2,2,3,5,5,7]
-- ---------------------------------------------------------------------

    
-- ---------------------------------------------------------------------
-- Ejercicio 21. Definir por recursiÃ³n la funciÃ³n
--    borra :: Eq a => a -> [a] -> [a]
-- tal que (borra x xs) es la lista obtenida borrando una ocurrencia de
-- x en la lista xs. Por ejemplo, 
--    borra 1 [1,2,1]  ==  [2,1]
--    borra 3 [1,2,1]  ==  [1,2,1]
-- ---------------------------------------------------------------------