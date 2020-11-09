-- PD 2019-20: Practica 6.2
-- Funciones de orden superior y definiciones por plegados (II)
-- Departamento de Ciencias de la ComputaciÃ³n e Inteligencia Artificial
-- Universidad de Sevilla
-- ============================================================================

-- ============================================================================
-- LibrerÃ­as auxiliares
-- ============================================================================
import Data.Char
import Data.List
import Data.Numbers.Primes
-- ----------------------------------------------------------------------------
-- Ejercicio 1. Se considera la funciÃ³n
--      resultadoPos :: (a -> Integer) -> [a] -> [a]
-- tal que (resultadoPos f xs) es la lista de los elementos de la lista
-- xs tales que el valor de la funciÃ³n f sobre ellos es positivo. Por ejemplo,
--   resultadoPos head [[-1,2],[-9,4],[2,3]]       ==  [[2,3]]
--   resultadoPos sum [[1,2],[9],[-8,3],[],[3,5]]  ==  [[1,2],[9],[3,5]]
-- 
-- Define esta funciÃ³n
-- 1) por comprensiÃ³n,
-- 2) por orden superior (map, filter, ...),
-- 3) por recursiÃ³n,
-- 4) por plegado (con 'foldr').
-- -----------------------------------------------------------------------------

-- 1)
resultadoPosComp f xss = [xs | xs <- xss, ((f xs) > 0)]

-- 2)
resultadoPosOrdSup f xss = filter ((>0) . f) xss

-- 3) 
resultadoPosRec f [] = []
resultadoPosRec f (xs:xss)
    | (f xs) > 0 =  xs:(resultadoPosRec f xss)
    | otherwise = resultadoPosRec f xss
-- 4)

resultadoPosPL f = foldr (\xs acc -> if ((f xs) > 0) then xs:(acc) else acc) []

-- ----------------------------------------------------------------------------
-- Ejercicio 2. Se considera la funciÃ³n
--     intercala :: Int -> [Int] -> [Int]
-- tal que (intercala y xs) es la lista que resulta de intercalar el elemento
-- y delante de todos los elementos de la lista xs que sean menores que y.
-- Por ejemplo,
--   intercala 5 [1,2,6,3,7,9]  ==  [5,1,5,2,6,5,3,7,9]
--   intercala 5 [6,7,9,8]      ==  [6,7,9,8]
--
-- Define esta funciÃ³n
-- 1) por comprensiÃ³n,
-- 2) por orden superior (map, filter, ...)
-- 3) por recursiÃ³n,
-- 4) por plegado (con 'foldr').
-- ----------------------------------------------------------------------------
-- 1)
intercalaComp y xs = concat [f x | x <- xs]
        where f x = if x < y then [y,x] else [x]

-- 2) 
intercalaOrdSup y xs = concat $ map f xs
        where f x  = if x < y then [y,x] else [x]

-- 3)
intercalaRe y xs = intercalaRe_aux y xs []
intercalaRe_aux _ [] acum = acum
intercalaRe_aux y (x:xs) acum
    | x < y = intercalaRe_aux y xs (acum ++ [y,x])
    | otherwise = intercalaRe_aux y xs (acum ++ [x])

-- 4)
intercalaPL y = foldr (\x acc -> if (x < y) then [y,x] ++ acc else [x] ++ acc) []

-- ----------------------------------------------------------------------------
-- Ejercicio 3. Se considera la funciÃ³n
--    dec2ent :: [Integer] -> Integer
-- tal que (dec2ent xs) es el nÃºmero entero cuyas cifras ordenadas son los
-- elementos de la lista xs. Por ejemplo,
--   dec2ent [2,3,4,5]  ==  2345
--   dec2ent [1..9]     ==  123456789
--
-- Defie esta funciÃ³n
-- 1) por comprensiÃ³n,
-- 2) por orden superior (map, filter, ...)
-- 3) por recursiÃ³n,
-- 4) por plegado (con 'foldr').
-- ----------------------------------------------------------------------------

    
-- ----------------------------------------------------------------------------
-- Ejercicio 4. Se considera la funciÃ³n
--     diferencia :: Eq a => [a] -> [a] -> [a]
-- tal que (diferencia xs ys) es la diferencia entre los conjuntos xs e
-- ys; es decir, el conjunto de los elementos de la lista xs que no se
-- encuentran en la lista ys. Por ejemplo,
--   diferencia [2,3,5,6] [5,2,7]  ==  [3,6]
--   diferencia [1,3,5,7] [2,4,6]  ==  [1,3,5,7]
--   diferencia [1,3] [1..9]       ==  []
--
-- Define esta funciÃ³n
-- 1) por comprensiÃ³n,
-- 2) por orden superior (map, filter, ...)
-- 3) por recursiÃ³n,
-- 4) por plegado (con 'foldr').
-- ----------------------------------------------------------------------------

-- ----------------------------------------------------------------------------
-- Ejercicio 5. Se considera la funciÃ³n
--   primerosYultimos :: [[a]] -> ([a],[a])
-- tal que (primerosYultimos xss) es el par formado por la lista de los
-- primeros elementos de las listas no vacÃ­as de xss y la lista de los
-- Ãºltimos elementos de las listas no vacÃ­as de xss. Por ejemplo,
--   primerosYultimos [[1,2],[5,3,4],[],[9]]  ==  ([1,5,9],[2,4,9])
--   primerosYultimos [[1,2],[1,2,3],[1..4]]  ==  ([1,1,1],[2,3,4])

--
-- Define esta funciÃ³n
-- 1) por comprensiÃ³n,
-- 2) por orden superior (map, filter, ...)
-- 3) por recursiÃ³n,
-- 4) por plegado (con 'foldr').
-- ----------------------------------------------------------------------------


-- ----------------------------------------------------------------------------
-- Ejercicio 6. Una lista hermanada es una lista de nÃºmeros estrictamente
-- positivos en la que cada elemento tiene algÃºn factor primo en comÃºn con el
-- siguiente, en caso de que exista, o alguno de los dos es un 1. Por ejemplo,
-- [2,6,3,9,1,5] es una lista hermanada.

-- Se considera la funciÃ³n
--    hermanada :: [Int] -> Bool
-- tal que (hermanada xs) comprueba que la lista xs es hermanada segÃºn la
-- definiciÃ³n anterior. Por ejemplo,
--    hermanada [2,6,3,9,1,5]  ==  True
--    hermanada [2,3,5]        ==  False
--
-- Se pide definir esta funciÃ³n
-- 1) por comprensiÃ³n,
-- 2) por orden superior (map, filter, ...)
-- 3) por recursiÃ³n,
-- 4) por plegado (con 'foldr').
-- ----------------------------------------------------------------------------
-- Nota: Usa la funciÃ³n 'gcd'
-- ----------------------------------------------------------------------------



-- ----------------------------------------------------------------------------
-- Ejercicio 7. Un elemento de una lista es permanente si ninguno de los que
-- vienen a continuaciÃ³n en la lista es mayor que Ã©l. Consideramos la funciÃ³n
--   permanentes :: [Int] -> [Int]
-- tal que (permanentes xs) es la lista de los elementos permanentes de la
-- lista xs. Por ejemplo,
--   permanentes [80,1,7,8,4]  ==  [80,8,4]

-- Se pide definir esta funciÃ³n
-- 1) por comprensiÃ³n,
-- 2) por orden superior (map, filter, ...)
-- 3) por recursiÃ³n,
-- 4) por plegado (con 'foldr').
-- ---------------------------------------------------------------------------
-- Nota: Usa la funciÃ³n 'tails' de Data.List.
-- ----------------------------------------------------------------------------


               
-- ---------------------------------------------------------------------
-- Ejercicio 8. Un nÃºmero entero positivo n es muy primo si es n primo
-- y todos los nÃºmeros que resultan de ir suprimimiendo la Ãºltima cifra
-- tambiÃ©n son primos. Por ejemplo, 7193 es muy primo pues los nÃºmeros
-- 7193, 719, 71 y 7 son todos primos. 
-- 
-- Define la funciÃ³n 
--    muyPrimo :: Integer -> Bool
-- que (muyPrimo n) se verifica si n es muy primo. Por ejemplo,
--    muyPrimo 7193  == True
--    muyPrimo 71932 == False
-- --------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- Â¿CuÃ¡ntos nÃºmeros de cinco cifras son muy primos?
-- ---------------------------------------------------------------------

-- El cÃ¡lculo es

-- ---------------------------------------------------------------------

