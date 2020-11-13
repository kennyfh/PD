-- Nombre: KENNY JESÚS
-- Apellidos: FLORES HUAMÁN
-- UVUS: kenflohua

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 2 (3 puntos). El centro de masas de un sistema físico discreto
-- de cuerpos es el punto del espacio que se comporta como si en él estuviera
-- aplicada la resultante de las fuerzas externas al sistema.
--
-- Representaremos un conjunto de n cuerpos en un espacio mediante una lista
-- de n pares de la forma (mi,(ai,bi,ci)) donde (ai,bi,ci) es la posición
-- en el espacio tridimensional y mi es la masa puntual del cuerpo i-ésimo.
-- Las coordenadas del centro de masas (a,b,c) se calculan como:
--    a = (a1*m1+a2*m2+ ... an*mn)/(m1+m2+...mn)
--    b = (b1*m1+b2*m2+ ... bn*mn)/(m1+m2+...mn)
--    c = (c1*m1+c2*m2+ ... cn*mn)/(m1+m2+...mn)
--
-- Ejercicio 2.1. (1,5 puntos de 3)
-- Define la función (centroDeMasas xs) tal que reciba una lista de
-- cuerpos como descrito arriba, y devuelva la coordenada del centro de masas.
-- La función debe ser polimórfica, aceptando tipos de clase Floating.
-- Si la lista está vacía, debe dar un error como el mostrado abajo.
-- Por ejemplo:       m1                m2           m3 
--  > centroDeMasas [(3.8,(-1,3,0.9)),(5.9,(0,0,0)),(0.9,(1.5,3,0))] 
--  (-0.2311320754716981,1.3301886792452828,0.32264150943396225)
--  > centroDeMasas []
--  *** Exception: No hay cuerpos
-- ---------------------------------------------------------------------

-- centroDeMasas [] = error "No hay cuerpos" -- no hay cuerpos
-- centroDeMasas xss = [apa xss] ++ [apb xss] ++ [apc xss] -- suma de los 3 componentes

-- apa xss = (sum [ x * y | (x,y) <- calculo]) / (fromIntegral summasas) -- calculamos a
--     where tupla = [xs | (m,xs) <- xss] -- tupla de elementos de  a
--           ms = [fst xs | xs <- xss] -- calculo de las masas
--           calculo = zip [a | (a,_,_) <- tupla] ms -- tupla de (ai,ms) para ir multiplicandolo
--           summasas = sum [fst xs | xs <- xss] -- suma de todas las masas

-- apb xss = (sum [ x * y | (x,y) <- calculo]) / (fromIntegral summasas)
--     where tupla = [xs | (m,xs) <- xss]
--           ms = [fst xs | xs <- xss]
--           calculo = zip [b | (_,b,_) <- tupla] ms
--           summasas = sum [fst xs | xs <- xss]

-- apc xss = (sum [ x * y | (x,y) <- calculo]) / (fromIntegral summasas)
--     where tupla = [xs | (m,xs) <- xss]
--           ms = [fst xs | xs <- xss]
--           calculo = zip [c | (_,_,c) <- tupla] ms
--           summasas = sum [fst xs | xs <- xss]

-- Calculamos en todos los componentes de a, b y c y devolvemos la lista con cada componente
-- Voy a comentarlo porque falla

-- ---------------------------------------------------------------------
-- Ejercicio 2.2 (1,5 puntos de 3)
-- Comprueba con quickcheck la siguiente propiedad: para todo conjunto
-- de n cuerpos, con n>1, cuyas masas son positivas (estrictamente mayor que
-- cero), las coordenadas del centro de masas están dentro del rango de
-- las coordenadas de los cuerpos; es decir, la componente a de la coordenada
-- del centro de masas está entre el mínimo y el máximo (ambos inclusive)
-- de las componentes ai de los cuerpos (ídem para las componentes b y c).
-- Por ejemplo:
-- > quickCheck prop_masas
-- *** Gave up! Passed only 26 tests; 1000 discarded tests.
-- ---------------------------------------------------------------------

prop_masas = undefined