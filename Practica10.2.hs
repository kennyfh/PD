-- PD 2020-21. Vectores y matrices: ejercicios de ex�menes.
-- Departamento de Ciencias de la Computaci�n e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- En esta relaci�n se presenta una recopilaci�n de ejercicios vectores
-- y matrices propuestos en exómenes de la asignatura I1M.

-- ---------------------------------------------------------------------
-- � Librer�as auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.Array
import Data.List

-- Nota. En la relación usaremos los tipos de los vectores y las matrices 
-- definidos por 

type Vector a = Array Int a
type Matriz a = Array (Int,Int) a

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función 
--    esTriangularS :: Num a => Matriz a -> Bool
-- tal que (esTriangularS p) se verifica si p es una matriz triangular
-- superior. Por ejemplo, 
--    ghci> esTriangularS (listArray ((1,1),(3,3)) [1,2,1,0,4,7,0,0,5])
--    True
--    ghci> esTriangularS (listArray ((1,1),(3,3)) [1,2,3,1,2,4,1,2,5])
--    False
-- ---------------------------------------------------------------------

esTriangularS:: (Num a, Eq a) => Matriz a -> Bool
esTriangularS p = all (==0) [p!(i,j) | i<-[2..numFilas p], j<-[1..((numColumnas p)-1)], j<i]

numFilas :: Num a => Matriz a -> Int
numFilas m = fst $ snd $ bounds m

numColumnas:: Num a => Matriz a -> Int
numColumnas m = snd $ snd $ bounds m
-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la funci�n
--    antidiagonal :: (Num a, Eq a) => Matriz a -> Bool
-- tal que (antidiagonal m) se verifica si es cuadrada y todos los
-- elementos de m que no est�n en su diagonal secundaria son nulos. Por
-- ejemplo,   
--    ghci> antidiagonal (listArray ((1,1),(3,3)) [0,0,4, 0,6,0, 0,0,0])
--    True
--    ghci> antidiagonal (listArray ((1,1),(3,3)) [7,0,4, 0,6,0, 0,0,5])
--    False
-- ---------------------------------------------------------------------

m1, m2 :: Matriz Int
m1 = listArray ((1,1),(3,3)) [0,0,4, 0,6,0, 0,0,0]
m2 = listArray ((1,1),(3,3)) [7,0,4, 0,6,0, 0,0,5]

antidiagonal :: (Num a, Eq a) => Matriz a -> Bool 
antidiagonal p = (esCuadrada p) && (all (==0) [p!(i,j) | i<-[1..numFilas p], j<-[1..numColumnas p], (not $ elem (i,j) lista)])
    where lista = zip [(numFilas p),((numFilas p)-1)..1] [1..numColumnas p]

esCuadrada :: (Num a, Eq a) => Matriz a -> Bool
esCuadrada p = (numFilas p) == (numColumnas p)


-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la funci�n
--    esEscalar:: Num a => Matriz a -> Bool
-- tal que (esEscalar p) se verifica si p es una matriz es escalar; es
-- decir, diagonal con todos los elementos de la diagonal principal
-- iguales. Por ejemplo,
--    esEscalar (listArray ((1,1),(3,3)) [5,0,0,0,5,0,0,0,5])  ==  True
--    esEscalar (listArray ((1,1),(3,3)) [5,0,0,1,5,0,0,0,5])  ==  False
--    esEscalar (listArray ((1,1),(3,3)) [5,0,0,0,6,0,0,0,5])  ==  False
-- ---------------------------------------------------------------------

esEscalar:: (Num a, Eq a) => Matriz a -> Bool
esEscalar p = (all (==0) [p!(i,j) | (i,j)<-lNDiagonal]) && (todosiguales [p!(i,j) | (i,j) <-lDiagonal])
    where lDiagonal = [(i,j) | i<-[1..numFilas p], j<-[1..numColumnas p], i==j]
          lNDiagonal = [(i,j) | i<-[1..numFilas p], j<-[1..numColumnas p], i/=j]


todosiguales xs = all (==(xs!!0)) xs

-- listaDiagonal p = [(i,j) | i<-[1..numFilas p], j<-[1..numColumnas p], i==j]
-- listaNoDiagonal p = [(i,j) | i<-[1..numFilas p], j<-[1..numColumnas p], i/=j]

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la funci�n 
--    aplicaT :: (Ix a, Num b) => Array a b -> (b -> c) -> Array a c
-- tal que (aplicaT t f) es la tabla obtenida aplicado la funci�n f a
-- los elementos de la tabla t. Por ejemplo,
--    ghci> aplicaT (array (1,5) [(1,6),(2,3),(3,-1),(4,9),(5,20)]) (+1)
--    array (1,5) [(1,7),(2,4),(3,0),(4,10),(5,21)]
--    ghci> :{
--    *Main| aplicaT (array ((1,1),(2,3)) [((1,1),3),((1,2),-1),((1,3),0),((2,1),0),((2,2),0),((2,3),-1)]) (*2)
--    *Main| :}
--    array ((1,1),(2,3)) [((1,1),6),((1,2),-2),((1,3),0),
--                         ((2,1),0),((2,2),0),((2,3),-2)]
-- ---------------------------------------------------------------------

aplicaT :: (Ix a, Num b) => Array a b -> (b -> c) -> Array a c
aplicaT t f = array (bounds t) (zip (indices t) [f x | x<- elems t])

-- ---------------------------------------------------------------------
-- Ejercicio 5. Dada una matriz num�rica A de dimensiones (m,n) y una
-- matriz booleana B de las mismas dimensiones, y dos funciones f y g,
-- la transformada de A respecto de B, f y g es la matriz C (de las
-- mismas dimensiones), tal que, para cada celda (i,j):   
--    C(i,j) = f(A(i,j)) si B(i,j) es verdadero
--    C(i,j) = g(A(i,j)) si B(i,j) es falso
-- Por ejemplo, si A y B son las matrices
--    |1 2|   |True  False|  
--    |3 4|   |False True |
-- respectivamente, y f y g son dos funciones tales que f(x) = x+1 y
-- g(x) = 2*x, entonces la transformada de A respecto de B, f y g es
--    |2 4|
--    |6 5|
--     
-- Definir la funci�n
--    transformada :: Matriz a -> Matriz Bool -> (a -> b) -> (a -> b) -> Matriz b
-- tal que (transformada a b f g) es la transformada de A respecto de B,
-- f y g. Por ejemplo,
--  ghci> let a = listArray ((1,1),(2,2)) [1,2,3,4] :: Matriz Int
--  ghci> let b = listArray ((1,1),(2,2)) [True,False,False,True] :: Matriz Bool
--  ghci> transformada a b (+1) (*2)
--  array ((1,1),(2,2)) [((1,1),2),((1,2),4),((2,1),6),((2,2),5)]
-- ---------------------------------------------------------------------

transformada :: Matriz a -> Matriz Bool -> (a -> b) -> (a -> b) -> Matriz b
transformada a b f g = array (bounds a) [((i,j),funcion (a!(i,j)) (b!(i,j))) | i<-[1..numf], j<-[1..numc]]
    where funcion x1 x2
            | x2==True = f x1
            | otherwise = g x1
          numf = fst $ snd $ bounds a
          numc = snd $ snd $ bounds a
-- ---------------------------------------------------------------------
-- Ejercicio 6.1. Un vector se denomina estoc�stico si todos sus
-- elementos son mayores o iguales que 0 y suman 1.  
-- 
-- Definir la funci�n 
--    vectorEstocastico :: Vector Float -> Bool
-- tal que (vectorEstocastico v) se verifica si v es estoc�stico. Por
-- ejemplo,  
--    vectorEstocastico (listArray (1,5) [0.1, 0.2, 0, 0, 0.7]) == True
--    vectorEstocastico (listArray (1,5) [0.1, 0.2, 0, 0, 0.9]) == False
-- ---------------------------------------------------------------------

vectorEstocastico :: Vector Float -> Bool
vectorEstocastico v = (sum lista == 1.0) && (all (>=0) lista)
    where lista = elems v

-- ---------------------------------------------------------------------
-- Ejercicio 6.2. Una matriz se denomina estoc�stica si sus columnas
-- son vectores estoc�sticos.  
-- 
-- Definir la funci�n 
--    matrizEstocastica :: Matriz Float -> Bool
-- tal que (matrizEstocastico p) se verifica si p es estoc�stica. Por
-- ejemplo,  
--    matrizEstocastica (listArray ((1,1),(2,2)) [0.1,0.2,0.9,0.8]) == True
--    matrizEstocastica (listArray ((1,1),(2,2)) [0.1,0.2,0.3,0.8]) == False

-- 11 12 13
-- 21 22 23 
-- 31 32 33
-- ---------------------------------------------------------------------

matrizEstocastica :: Matriz Float -> Bool   
matrizEstocastica p = all (==True) [vectorEstocastico (columnaMat j p) | j<-[1..numc]]
    where numf = fst $ snd $ bounds p
          numc = snd $ snd $ bounds p

columnaMat :: Num a => Int -> Matriz a -> Vector a
columnaMat j p = listArray (1,n) xs
    where xs = [p!(i,j) | i<-[1..numFilas p]]
          n =  length xs

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la funci�n 
--    maximos :: Matriz Int -> [Int]
-- tal que (maximos p) es la lista de los m�ximos locales de la matriz
-- p; es decir de los elementos de p que son mayores que todos sus
-- vecinos. Por ejemplo,  
--    ghci> maximos (listArray ((1,1),(3,4)) [9,4,6,5,8,1,7,3,0,2,5,4])
--    [9,7]
-- ya que los m�ximos locales de la matriz
--    |9 4 6 5|
--    |8 1 7 3|
--    |0 2 5 4|
-- son 9 y 7.
-- ---------------------------------------------------------------------
maximos :: Matriz Int -> [Int]
maximos p = [p!(i,j)| ((i,j),k) <- assocs p, all (<k) (vecinos p (i,j))]
    where vecinos p (i,j) = [p!(a,b) | a <- [i-1..i+1], 
                                       b <- [j-1..j+1],
                                       a `notElem` [0,n+1],
                                       b `notElem` [0,m+1],
                                       (a,b) /= (i,j)]
          (n,m) = snd $ bounds p
-- maximos :: Matriz Int -> [Int]
-- maximos p =  filter (/=(-1)) [mayoresvecinos (p!(i,j)) (vecinos p (i,j)) | i<-[1..nf], j<-[1..nc]]
--     where (nf,nc) = snd $ bounds p


-- mayoresvecinos :: Int -> [Int] -> Int
-- mayoresvecinos v xs
--     | f v xs==True = v
--     | otherwise = -1
--     where f v xs = all (<v) xs

-- vecinos p (i,j) = [p!(a,b) | a <- [i-1..i+1], 
--                                        b <- [j-1..j+1],
--                                        a `notElem` [0,n+1],
--                                        b `notElem` [0,m+1],
--                                        (a,b) /= (i,j)]
--     where (n,m) = snd $ bounds p
-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la funci�n 
--    algunMenor :: Matriz Int -> [Int]
-- tal que (algunMenor p) es la lista de los elementos de p que tienen
-- alg�n vecino menor que �l. Por ejemplo,  
--    algunMenor (listArray ((1,1),(3,4)) [9,4,6,5,8,1,7,3,4,2,5,4])
--    [9,4,6,5,8,7,4,2,5,4]          
-- pues s�lo el 1 y el 3 no tienen ning�n vecino menor en la matriz
--    |9 4 6 5|
--    |8 1 7 3|
--    |4 2 5 4|
-- ---------------------------------------------------------------------        

algunMenor :: Matriz Int -> [Int]
algunMenor p = [p!(i,j) | ((i,j),k) <- assocs p, any (<k) (vecinos p (i,j))]
    where vecinos p (i,j) = [p!(a,b) | a<-[(i-1)..(i+1)], b<-[(j-1)..(j+1)], a `notElem` [0,n+1],b `notElem` [0,m+1], (i,j)/=(a,b)]
          (n,m) = snd $ bounds p
    

-- ---------------------------------------------------------------------
-- Ejercicio 9.1. Definir la funci�n
--      proporcional :: (Fractional a, Eq a) => Vector a -> Vector a -> Bool
-- tal que (proporcional v1 v2) verifica si los vectores v1 y v2 son proporcionales, 
-- es decir, v1 == k*v2, con un k un número escalar cualquiera. Por ejemplo,
--    ghci> let p1 = listArray ((1,1),(3,3)) [1,0,0,0,0,1,0,1,0] :: Matriz Float
--    ghci> let v1 = listArray (1,3) [0,-1,1] :: Vector Float
--    ghci> let v2 = listArray (1,3) [1,2,1] :: Vector Float
--    proporcional v1 v1                           = True
--    proporcional v1 v2                           = False
--    proporcional v1 (listArray (1,3) [0,-5,5])   = True
--    proporcional v1 (listArray (1,3) [0,-5,4])   = False
--    proporcional (listArray (1,3) [0,-5,5]) v1   = True
--    proporcional v1 (listArray (1,3) [0,0,0])    = True
--    proporcional (listArray (1,3) [0,0,0]) v1    = False

v1, v2 :: Vector Float
v1 = listArray (1,3) [0,-1,1]
v2 = listArray (1,3) [1,2,1]

proporcional :: (Fractional a, Eq a) => Vector a -> Vector a -> Bool
proporcional v1 v2
            | esVectorCero v1 || esVectorCero v2 = False
            | otherwise = all (\(a,b) -> b == k*a) vec
            where vec = [(a,b) | i<-[1..n], let a = v1!i, let b=v2!i, a/=0, b/=0]
                  k = if (null vec) then 1 else ((snd $ head $ vec)/ (fst $ head $ vec))
                  n = snd $ bounds v1

esVectorCero :: (Fractional a, Eq a) => Vector a -> Bool
esVectorCero v1 = all (==0) (elems v1)

-- ---------------------------------------------------------------------
-- Ejercicio 9.2. Definir la funci�n
--    esAutovector :: (Fractional a, Eq a) => 
--                    Vector a -> Matriz a -> Bool
-- tal que (esAutovector v p) compruebe si v es un autovector de p
-- (es decir, el producto de v por p es un vector proporcional a
-- v). Por ejemplo, 
--    ghci> let p1 = listArray ((1,1),(3,3)) [1,0,0,0,0,1,0,1,0] :: Matriz Float
--    ghci> let v1 = listArray (1,3) [0,-1,1] :: Vector Float
--    ghci> let v2 = listArray (1,3) [1,2,1] :: Vector Float
--    ghci> esAutovector v1 p1 
--    True
--    ghci> esAutovector v2 p1 
--    False
-- ---------------------------------------------------------------------
p2 :: Matriz Float
p2 = listArray ((1,1),(3,3)) [1,0,0,0,0,1,0,1,0]

v3,v4 :: Vector Float
v3 = listArray (1,3) [0,-1,1]
v4 = listArray (1,3) [1,2,1]

esAutovector :: (Fractional a, Eq a) => Vector a -> Matriz a -> Bool
esAutovector v p = proporcional v (producto v p)


producto v p = array (1,m) [(j,sum $ zipWith (*) (elems v) (vectorMat j p)) | j<-[1..m]]
    where (_,m) = snd $ bounds p

vectorMat j p= [p!(i,j) | i<-[1..n]]
    where (n,m) = snd $ bounds p

-- ---------------------------------------------------------------------
-- Ejercicio 9.3. Definir la funci�n
--    autovalorAsociado :: (Fractional a, Eq a) => 
--                         Matriz a -> Vector a -> Maybe a
-- tal que si v es un autovector de p, calcule el autovalor asociado.
-- Por ejemplo,
--    ghci> let p1 = listArray ((1,1),(3,3)) [1,0,0,0,0,1,0,1,0] :: Matriz Float
--    ghci> let v1 = listArray (1,3) [0,-1,1] :: Vector Float
--    ghci> let v2 = listArray (1,3) [1,2,1] :: Vector Float
--    autovalorAsociado p1 v1 == Just (-1.0)
--    autovalorAsociado p1 v2 == Nothing
-- ---------------------------------------------------------------------

autovalorAsociado :: (Fractional a, Eq a) => 
                     Matriz a -> Vector a -> Maybe a
autovalorAsociado p v= undefined
-- ------------------------------------------------------------------
-- Ejercicio 10. Definir la funci�n
--    sumaVecinos :: Matriz Int -> Matriz Int
-- tal que (sumaVecinos p) es la matriz obtenida al escribir en la 
-- posicion (i,j) la suma de los todos vecinos del elemento que ocupa 
-- el lugar (i,j) en la matriz p. Por ejemplo,
--    ghci> sumaVecinos (listArray ((1,1),(3,3)) [0,1,3, 1,2,0, 0,5,7])
--    array ((1,1),(3,3)) [((1,1),4),((1,2), 6),((1,3), 3),
--                         ((2,1),8),((2,2),17),((2,3),18),
--                         ((3,1),8),((3,2),10),((3,3), 7)]
-- ------------------------------------------------------------------

sumaVecinos :: Matriz Int -> Matriz Int
sumaVecinos p =  array (bounds p) [((i,j),sum $ vecinos p (i,j))| i<-[1..n], j<-[1..m]]
    where vecinos p (i,j) = [p!(a,b) | a<-[(i-1)..(i+1)], b<-[(j-1)..(j+1)], a `notElem` [0,n+1], b `notElem` [0,m+1], (a,b)/=(i,j)]
          (n,m) = snd $ bounds p

-- ---------------------------------------------------------------------
-- Ejercicio 11.1. Una matriz tridiagonal es aquella en la que s�lo hay
-- elementos distintos de 0 en la diagonal principal o en las diagonales
-- por encima y por debajo de la diagonal principal. Por ejemplo, 
--    ( 1 2 0 0 0 0 )
--    ( 3 4 5 0 0 0 )
--    ( 0 6 7 8 0 0 )
--    ( 0 0 9 1 2 0 )
--    ( 0 0 0 3 4 5 )
--    ( 0 0 0 0 6 7 )
-- 
-- Definir la funci�n 
--    creaTridiagonal :: Int -> Matriz Int
-- tal que (creaTridiagonal n) es la siguiente matriz tridiagonal
-- cuadrada con n filas y n columnas:
--    ( 1 1 0 0 0 0 ... 0  0  )
--    ( 1 2 2 0 0 0 ... 0  0  )
--    ( 0 2 3 3 0 0 ... 0  0  )
--    ( 0 0 3 4 4 0 ... 0  0  )
--    ( 0 0 0 4 5 5 ... 0  0  )
--    ( 0 0 0 0 5 6 ... 0  0  )
--    ( ..................... )
--    ( 0 0 0 0 0 0 ... n  n  )
--    ( 0 0 0 0 0 0 ... n n+1 )
-- Por ejemplo,
--    ghci> creaTridiagonal 4
--    array ((1,1),(4,4)) [((1,1),1),((1,2),1),((1,3),0),((1,4),0),
--                         ((2,1),1),((2,2),2),((2,3),2),((2,4),0),
--                         ((3,1),0),((3,2),2),((3,3),3),((3,4),3),
--                         ((4,1),0),((4,2),0),((4,3),3),((4,4),4)]
-- ----------------------------------------------------------------------------

-- creaTridiagonal :: Int -> Matriz Int
-- creaTridiagonal n = array ((1,1),(n,n)) (zip lpos [f i j | (i,j)<-lpos])
--     where f i j
--             | (i,j) `elem` (listaPos n) = 12
--             | otherwise = 0
--           lpos = [(i,j) | i<-[1..n], j<-[1..n]]

-- encdebDiag n (i,j) = [(i,b) | b<-[(j-1)..(j+1)],b `notElem` [0,n+1], (i,b)/=(i,j)] 

-- listaPos n = xs ++ concat [encdebDiag n (i,j) | (i,j)<-xs]
--     where xs = zip [1..] [1..n]

creaTridiagonal :: Int -> Matriz Int
creaTridiagonal n = array ((1,1),(n,n)) [((i, j), f i j) | i <- [1..n], j <- [1..n]]
    where
        f i j
            | i == j = i
            | abs (i-j) < 2 = min i j
            | otherwise = 0
-- ----------------------------------------------------------------------------
-- Ejercicio 11.2. Definir la funci�n 
--    esTridiagonal :: Matriz Int -> Bool
-- tal que (esTridiagonal p) se verifica si la matriz p es tridiagonal. Por 
-- ejemplo,
--    esTridiagonal (creaTridiagonal 5)               ==  True
--    esTridiagonal (listArray ((1,1),(3,3)) [1..9])  ==  False
-- ----------------------------------------------------------------------------

esTridiagonal :: Matriz Int -> Bool
esTridiagonal p = all (==True) [p!(i,j) == (f i j) | i<-[1..nf], j<-[1..nc]]
    where f i j
            | i==j = i
            | (abs (i-j)) < 2 = min i j
            | otherwise = 0
          (nf,nc) = snd $ bounds p 
