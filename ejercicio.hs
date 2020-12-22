-- ============================================================================

-- Ejercicio 2. (2,5 ptos) Los árboles binarios con datos en nodos internos y

-- hojas se pueden representar con el siguiente tipo de dato:

import Data.List

data Arbol a = H a

             | N a (Arbol a) (Arbol a)

               deriving Show



-- Por ejemplo, el siguiente árbol binario:

--

--           1

--          / \

--         /   \

--        4     7

--       / \   / \

--      5   6 8   3

--     / \

--    2   9

--

-- se representa como



ejArbol :: Arbol Integer
ejArbol = N 1 (N 4 (N 5 (H 2) (H 9)) (H 6)) (N 7 (H 8) (H 3))



-- Asumiendo que todos los elementos del árbol son distintos entre sí, definir

-- la función

--   caminoEntreHojas :: (Eq a) => a -> a -> Arbol a -> [a]

-- tal que '(caminoEntreHojas a b t)' es la lista que contiene los nodos por

-- los que pasa el camino más corto desde la hoja con el valor 'a' hasta la hoja

-- con valor 'b' en el árbol 't', si es que ambos valores están presentes en el

-- árbol. Si alguno de los dos valores no está presente en el árbol el resultado

-- debe ser la lista vacía. Por ejemplo,

--   caminoEntreHojas 2 3 ejArbol  ==  [2,5,4,1,7,3]

--   caminoEntreHojas 3 2 ejArbol  ==  [3,7,1,4,5,2]

--   caminoEntreHojas 9 2 ejArbol  ==  [9,5,2]

--   caminoEntreHojas 6 8 ejArbol  ==  [6,4,1,7,8]

--   caminoEntreHojas 0 3 ejArbol  ==  []

--   caminoEntreHojas 2 0 ejArbol  ==  []

-- ----------------------------------------------------------------------------

{--
data Arbol a = H a

             | N a (Arbol a) (Arbol a)

               deriving Show
--}

caminoEntreHojas ::  (Eq a) => a -> a -> Arbol a -> [a]
caminoEntreHojas a b (H _) = []
caminoEntreHojas a b (N v i d)
    | (not.null) cai && (not.null) cbi = caminoEntreHojas a b i
    | (not.null) cad && (not.null) cbd = caminoEntreHojas a b d
    | (null cai && null cad) || (null cbi && null cbd) = []
    | otherwise = (reverse ((cai++cad))) ++ [v] ++ cbi ++ cbd
    where cai = camino a i
          cad = camino a d
          cbi = camino b i
          cbd = camino b d

-- Primero: una funcion que compruebe si una hoja está en un árbol
pertenece :: (Eq a) => a -> Arbol a -> Bool
pertenece a (H a1) = a1 == a 
pertenece a (N _ a1 a2) = pertenece a a1 || pertenece a a2

-- Segundo: usarlo para encontrar el nodo donde las hojas se "separan"

-- Tercero: calcular un camino desde un nodo hasta una hoja

camino :: (Eq a) => a -> Arbol a -> [a]
camino x (H a)
    | x == a = [x]
    | otherwise = []
camino x (N a i d)
    | pertenece x i = [a] ++ (camino x i)
    | pertenece x d = [a] ++ (camino x d)
    | otherwise = []