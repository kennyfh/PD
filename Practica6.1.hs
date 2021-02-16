-- PD-Practica 6.1
-- Funciones de orden superior y definiciones por plegados.
-- Departamento de Ciencias de la ComputaciÃ³n e I.A.
-- Universidad de Sevilla
-- =====================================================================
import Test.QuickCheck
-- ---------------------------------------------------------------------
-- Ejercicio 1. Redefinir por recursiÃ³n la funciÃ³n
--    takeWhile :: (a -> Bool) -> [a] -> [a]
-- tal que (takeWhile p xs) es la lista de los elemento de xs hasta el
-- primero que no cumple la propiedad p. Por ejemplo,
--    takeWhile' (<7) [2,3,9,4,5]  ==  [2,3]
-- ---------------------------------------------------------------------
 
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
    | f x = x : (takeWhile' f xs)
    | otherwise = []

takeWhile'' f = foldr (\x acc -> if f x then x : acc else []) []
-- ---------------------------------------------------------------------
-- Ejercicio 2. Redefinir por recursiÃ³n la funciÃ³n
--    dropWhile :: (a -> Bool) -> [a] -> [a]
-- tal que (dropWhile p xs) es la lista de eliminando los elemento de xs
-- hasta el primero que no cumple la propiedad p. Por ejemplo,
--    dropWhile' (<7) [2,3,9,4,5]  ==  [9,4,5]
-- ---------------------------------------------------------------------
 
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (x:xs)
    | f x = dropWhile' f xs
    | otherwise = x:xs

dropWhile'' f = foldl (\acc x -> if (f x) && (null acc) then [] else acc ++ [x]) []

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Redefinir, usando foldr, la funciÃ³n concat. Por ejemplo, 
--    concat' [[1,3],[2,4,6],[1,9]]  ==  [1,3,2,4,6,1,9]
-- ---------------------------------------------------------------------
 
concat' :: [[a]] -> [a]
concat' = foldr (++) []

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Comprobar con QuickCheck que la funciones concat',
-- y concat son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_concat :: [[Int]] -> Bool
prop_concat xss = concat xss == concat' xss

-- La comprobaciÃ³n es

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Comprobar con QuickCheck que la longitud de 
-- (concat' xss) es la suma de las longitudes de los elementos de xss.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_longConcat :: [[Int]] -> Bool
prop_longConcat xss = (length (concat' xss)) == sum (map length xss)

-- La comprobaciÃ³n es

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la funciÃ³n segmentos con su signatura
-- tal que (segmentos p xs) es la lista de los segmentos de xs cuyos
-- elementos verifican la propiedad p. Por ejemplo,
--    segmentos even [1,2,0,4,9,6,4,5,7,2]  ==  [[2,0,4],[6,4],[2]]
--    segmentos odd  [1,2,0,4,9,6,4,5,7,2]  ==  [[1],[9],[5,7]]
-- ---------------------------------------------------------------------

segmentos p xs
    | null xs = []
    | null $ cumple = segmentos p $ tail xs
    | otherwise = [cumple] ++ (segmentos p $ dropWhile' p xs)
    where cumple =  takeWhile' p xs
 
-- ---------------------------------------------------------------------
-- Ejercicio 5. La funciÃ³n 
--    divideMedia :: [Double] -> ([Double],[Double])
-- dada una lista numÃ©rica, xs, calcula el par (ys,zs), donde ys 
-- contiene los elementos de xs estrictamente menores que la media, 
-- mientras que zs contiene los elementos de xs estrictamente mayores 
-- que la media. Por ejemplo, 
--    divideMedia [6,7,2,8,6,3,4] ==  ([2.0,3.0,4.0],[6.0,7.0,8.0,6.0])
--    divideMedia [1,2,3]         ==  ([1.0],[3.0])
-- Definir la funciÃ³n divideMedia por filtrado y por recursiÃ³n. 
-- ---------------------------------------------------------------------
 
-- La definiciÃ³n por filtrado es
divideMediaF :: [Double] -> ([Double],[Double])
divideMediaF xs = ((filter (<media xs) xs), (filter (>media xs) xs))

media ::  [Double] -> Double
media ns = sum ns / fromIntegral(length ns)
 
-- La definición por recursión es
divideMediaR :: [Double] -> ([Double],[Double])
divideMediaR xs = divideMediaAuxR [] [] media xs
    where media = sum xs / fromIntegral(length xs)

divideMediaAuxR ys zs media [] = (ys,zs)
divideMediaAuxR ys zs media (x:xs)
    | x < media = divideMediaAuxR (ys ++ [x]) zs media xs
    | x > media = divideMediaAuxR ys (zs ++ [x]) media xs
    | otherwise =  divideMediaAuxR ys zs media xs
 
-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la funciÃ³n
--    agrupa :: Eq a => [[a]] -> [[a]]
-- tal que (agrupa xss) es la lista de las listas obtenidas agrupando
-- los primeros elementos, los segundos, ... Por
-- ejemplo, 
--    agrupa [[1..6],[7..9],[10..20]]  ==  [[1,7,10],[2,8,11],[3,9,12]]
--    agrupa []                        ==  []
-- ---------------------------------------------------------------------
 
agrupa :: Eq a => [[a]] -> [[a]]
agrupa [] =  []
agrupa [x,y] = map (\(a,b) -> a:[b]) (zip x y)
agrupa (xs:xss) = map (\(a,b) -> a:b) (zip xs (agrupa xss))
-- ---------------------------------------------------------------------
-- Ejercicio 7. Se considera la funciÃ³n 
--    filtraAplica :: (a -> b) -> (a -> Bool) -> [a] -> [b]
-- tal que (filtraAplica f p xs) es la lista obtenida aplicÃ¡ndole a los
-- elementos de xs que cumplen el predicado p la funciÃ³n f. Por ejemplo,
--    filtraAplica (4+) (<3) [1..7]  =>  [5,6]
-- Se pide, definir la funciÃ³n
-- 1. por comprensiÃ³n,
-- 2. usando map y filter,
-- 3. por recursiÃ³n y
-- 4. por plegado (con foldr).
-- ---------------------------------------------------------------------
 
-- La definiciÃ³n con lista de comprensiÃ³n es
filtraAplica_1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplica_1 f p xs= [f x | x <- xs, p x]

-- La definiciÃ³n con map y filter es
filtraAplica_2 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplica_2 f p xs = map f (filter p xs)

-- La definiciÃ³n por recursiÃ³n es
filtraAplica_3 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplica_3 _ _ [] = []
filtraAplica_3 f p (x:xs)
    | p x = (f x):(filtraAplica_3 f p xs)
    | otherwise =  filtraAplica_3 f p xs

-- La definiciÃ³n por plegado es
filtraAplica_4 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplica_4 f p= foldr (\x y -> if p x then (f x):y else y) []
 
-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir, usando recursión, plegado, y acumulador, la 
-- funciÃ³n
--    inversa :: [a] -> [a]
-- tal que (inversa xs) es la inversa de la lista xs. Por ejemplo,
--    inversa [3,5,2,4,7]  ==  [7,4,2,5,3]
-- ---------------------------------------------------------------------

inversaR :: [a] -> [a]
inversaR [] = []
inversaR (x:xs) = (inversaR xs) ++ [x]

inversaP :: [a] -> [a]
inversaP = foldr (\x acum -> acum++[x]) []

inversaAC :: [a] -> [a]
inversaAC xs = inversaAC_aux xs []

inversaAC_aux [] ac = ac
inversaAC_aux (x:xs) ac = inversaAC_aux xs (x:ac) 

inversaPI :: [a] -> [a]
inversaPI = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 9. La funciÃ³n de plegado foldl estÃ¡ definida por
--    foldl :: (a -> b -> a) -> a -> [b] -> a
--    foldl f ys xs = aux ys xs
--        where aux ys []     = ys
--              aux ys (x:xs) = aux (f ys x) xs
-- Definir, mediante plegado con foldl, la funciÃ³n
--    inversaP' :: [a] -> [a]
-- tal que (inversaP' xs) es la inversa de la lista xs. Por ejemplo,
--    inversaP' [3,5,2,4,7]  ==  [7,4,2,5,3]
-- ---------------------------------------------------------------------

inversaP' :: [a] -> [a]
inversaP' = foldl (\y x -> x:y) []

-- ---------------------------------------------------------------------
-- Ejercicio 10. Redefinir, por recursiÃ³n y plegado la funciÃ³n map. 
-- ---------------------------------------------------------------------

mapR :: (a -> b) -> [a] -> [b]
mapR _ [] = []
mapR f (x:xs) = (f x):(mapR f xs)

mapP :: (a -> b) -> [a] -> [b]
mapP f = foldr (\x y -> (f x):y) []

-- ---------------------------------------------------------------------
-- Ejercicio 11. Redefinir, usando foldl y foldr la funciÃ³n filter. Por
-- ejemplo, 
--    filter (<4) [1,7,3,2]  =>  [1,3,2]
-- ---------------------------------------------------------------------

-- filterL :: (a -> Bool) -> [a] -> [a]
-- filterL = foldl (\acum x -> if f x then acum ++[x] else acum) []


-- filterR :: (a -> Bool) -> [a] -> [a]
-- filterR f = foldr (\x acum -> if f x then x:acum else acum) []

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir, mediante recursiÃ³n, plegado, acumulador, y 
-- plegado con foldl la funciÃ³n
--    sumll :: Num a => [[a]] -> a
-- tal que (sumll xss) es la suma de las sumas de las listas de xss. 
-- Por ejemplo, 
--    sumll [[1,3],[2,5]]  ==  11
-- ---------------------------------------------------------------------

sumllR :: Num a => [[a]] -> a
sumllR [] = 0
sumllR (xs:xss) = (sum xs) + (sumllR xss)

sumllP :: Num a => [[a]] -> a
sumllP = foldr (\xs acum -> (sum xs) +  acum) 0

sumllA :: Num a => [[a]] -> a
sumllA = undefined

sumllAP :: Num a => [[a]] -> a
sumllAP = foldl (\y x -> sum x + y) 0

-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir, mediante recursiÃ³n y plegado, la funciÃ³n
--    borra :: Eq a => a -> a -> [a]
-- tal que (borra y xs) es la lista obtenida borrando las ocurrencias de
-- y en xs. Por ejemplo, 
--    borra 5 [2,3,5,6]    ==  [2,3,6]
--    borra 5 [2,3,5,6,5]  ==  [2,3,6]
--    borra 7 [2,3,5,6,5]  ==  [2,3,5,6,5]
-- ---------------------------------------------------------------------

borraR :: Eq a => a -> [a] -> [a]
borraR = undefined

borraP :: Eq a => a -> [a] -> [a]
borraP = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir, mediante recursiÃ³n y plegado la funciÃ³n
--    diferencia :: Eq a => [a] -> [a] -> [a]
-- tal que (diferencia xs ys) es la diferencia del conjunto xs e ys; es
-- decir el conjunto de los elementos de xs que no pertenecen a ys. Por
-- ejemplo,  
--    diferencia [2,3,5,6] [5,2,7]  ==  [3,6]
-- ---------------------------------------------------------------------

diferenciaR :: Eq a => [a] -> [a] -> [a]
diferenciaR = undefined

diferenciaP :: Eq a => [a] -> [a] -> [a]
diferenciaP = undefined

-- -------------------------------------------------------------------
-- Ejercicio 15. Definir mediante plegado la funciÃ³n 
--    producto :: Num a => [a] -> a
-- tal que (producto xs) es el producto de los elementos de la lista
-- xs. Por ejemplo, 
--    producto [2,1,-3,4,5,-6] == 720
-- ---------------------------------------------------------------------

producto :: Num a => [a] -> a
producto = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 16. Definir mediante plegado la funciÃ³n 
--    productoPred :: Num a => (a -> Bool) -> [a] -> a
-- tal que (productoPred p xs) es el producto de los elementos de la
-- lista xs que verifican el predicado p. Por ejemplo, 
--    productoPred even [2,1,-3,4,-5,6] == 48
-- ---------------------------------------------------------------------

productoPred :: Num a => (a -> Bool) -> [a] -> a
productoPred = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 17.1. Definir, mediante recursiÃ³n, la funciÃ³n
--    maximumR :: Ord a => [a] -> a
-- tal que (maximumR xs) es el mÃ¡ximo de la lista xs. Por ejemplo,
--    maximumR [3,7,2,5]                  ==  7
--    maximumR ["todo","es","falso"]      ==  "todo"
--    maximumR ["menos","alguna","cosa"]  ==  "menos"
-- 
-- Nota: La funciÃ³n maximumR es equivalente a la predefinida maximum.
-- ---------------------------------------------------------------------

maximumR :: Ord a => [a] -> a
maximumR = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 17.2. La funciÃ³n de plegado foldr1 estÃ¡ definida por 
--    foldr1 :: (a -> a -> a) -> [a] -> a
--    foldr1 _ [x]    =  x
--    foldr1 f (x:xs) =  f x (foldr1 f xs)
-- 
-- Definir, mediante plegado con foldr1, la funciÃ³n
--    maximumP :: Ord a => [a] -> a
-- tal que (maximumR xs) es el mÃ¡ximo de la lista xs. Por ejemplo,
--    maximumP [3,7,2,5]                  ==  7
--    maximumP ["todo","es","falso"]      ==  "todo"
--    maximumP ["menos","alguna","cosa"]  ==  "menos"
-- 
-- Nota: La funciÃ³n maximumP es equivalente a la predefinida maximum.
-- ---------------------------------------------------------------------

maximumP :: Ord a => [a] -> a
maximumP = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 18. Definir, por plegado, la funciÃ³n
-- sumaDivP :: Int -> [Int] -> Int
-- tal que (SumaDivP x xs) es la suma de los cuadrados de los
-- elementos de xs que son divisibles por x. Por ejemplo,
-- sumaDivP 3 [1..7] == 45
-- sumaDivP 2 [1..7] == 56
-- ---------------------------------------------------------------------

sumaDivP :: Int -> [Int] -> Int
sumaDivP = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 19.1. Definir, con la funciÃ³n all, la funciÃ³n
--    relacionadosA :: (a -> a -> Bool) -> [a] -> Bool
-- tal que (relacionadosA r xs) se verifica si para todo par (x,y) de
-- elementos consecutivos de xs se cumple la relaciÃ³n r. Por ejemplo,
--    relacionadosA (<) [2,3,7,9]                ==  True
--    relacionadosA (<) [2,3,1,9]                ==  False
-- ---------------------------------------------------------------------

relacionadosA :: (a -> a -> Bool) -> [a] -> Bool
relacionadosA = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 19.2. Definir, con la funciÃ³n foldr, la funciÃ³n
--    relacionadosP :: (a -> a -> Bool) -> [a] -> Bool
-- tal que (relacionadosP r xs) se verifica si para todo par (x,y) de
-- elementos consecutivos de xs se cumple la relaciÃ³n r. Por ejemplo,
--    relacionadosP (<) [2,3,7,9]                ==  True
--    relacionadosP (<) [2,3,1,9]                ==  False
-- ---------------------------------------------------------------------

relacionadosP :: (a -> a -> Bool) -> [a] -> Bool
relacionadosP = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 19.3.
-- Una lista se dirÃ¡ muy creciente si cada elemento es mayor estricto
-- que el triple del siguiente. 
-- Empleando tan solo (relacionadosA p xs), define el predicado 
--          muyCreciente :: [Integer] -> Bool
-- tal que (muyCreciente xs) se verifica si xs es muy creciente. Por
-- ejemplo:
-- muyCreciente [1,5,23,115]  == True
-- muyCreciente [1,2,7,14]    == False
-- muyCreciente [7]           == True
-- muyCreciente []            == True
-- ---------------------------------------------------------------------

muyCreciente :: [Integer] -> Bool
muyCreciente xs = undefined