import Data.List



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
data Arbol a = H a

             | N a (Arbol a) (Arbol a)

               deriving Show

ejArbol :: Arbol Integer
ejArbol = N 1 (N 4 (N 5 (H 2) (H 9)) (H 6)) (N 7 (H 8) (H 3))

pertenece :: (Eq a) => a -> Arbol a -> Bool
pertenece a1 (H a) = a1 == a
pertenece a1 (N _ izq der) =  (pertenece a1 izq) || (pertenece a1 der)

camino :: (Eq a) => a -> Arbol a -> [a]
camino x (H a)
    | x == a = [a]
    | otherwise = []

camino x (N a izq der)
  | pertenece x izq = [a] ++ (camino x izq) 
  | pertenece x der = [a] ++ (camino x der)
  | otherwise = []

  -- Asumiendo que todos los elementos del árbol son distintos entre sí, definir
-- la función
--   caminoEntreHojas :: (Eq a) => a -> a -> Arbol a -> [a]

--   caminoEntreHojas 2 3 ejArbol  ==  [2,5,4,1,7,3]

caminoEntreHojas :: (Eq a) => a -> a -> Arbol a -> [a]
caminoEntreHojas a b (N n i d)
    | ((not.null) caizq) && ((not.null) cbizq) = caminoEntreHojas a b i
    | ((not.null) cader) && ((not.null) cbder) =  caminoEntreHojas a b d
    | (null caizq && null cbder) || (null cbizq && null cader) = []
    | otherwise = (reverse ((caizq++cader))) ++ [n] ++ cbizq ++ cbder
    where caizq = camino a i
          cbizq = camino b i
          cader = camino a d
          cbder = camino b d