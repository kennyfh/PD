-- Nombre: KENNY JESÚS
-- Apellidos: FLORES HUAMÁN 
-- UVUS: kenflohua
-- ----------------------------------------------------------------------
-- Ejercicio 1 (1,5 puntos)
-- Definir el operador infijo (~>=) tal que reciba números de cualquier 
-- tipo de la clase fractional, y devuelva un Booleano indicando si el 
-- primer argumento es aproximadamente igual al segundo con una precisión
-- del 0.001, o bien mayor que él. Se debe dar explícitiamente la misma
-- precedencia que a la comparación (>=) y asociatividad por la derecha.
-- Por ejemplo:
-- > 2 ~>= 2.001 == True
-- > 2 ~>= 2.001 && 1.01 ~>= 1 == True
-- > 1-2.1 ~>= 2.3 || 2 ~>= 2.01 == False
-- ----------------------------------------------------------------------
(~>=) :: fractional -> fractional -> Bool
x ~>= y = (abs fromIntegral(x-y)) == 0.001 || x > y

{-
Lo que estamos haciendo es que si la diferencia del primer elemento con el segundo
elemento en valor absoluto da igual a 0.001 o el primero sea mayor que el segundo,
eso va a devolver True


-}