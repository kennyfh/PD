-- Nombre: KENNY JESUS
-- Apellidos: FLORES HUAMÁN
-- UVUS: kenflohua

-- ---------------------------------------------------------------------
-- Ejercicio 3. (3 puntos)
-- Dada una señal codificada en una lista de números reales,
-- necesitamos buscar los máximos locales, los cuales se calculan como
-- sigue: dado un valor n, un máximo local es aquel elemento que es 
-- mayor que los n elementos anteriores y los n posteriores a él en la
-- lista; además, el primer y último elemento de la lista no pueden ser
-- un máximo local.
-- Define la función (maximosLocales n xs) tal que devuelva una lista
-- con los máximos locales en xs según el valor n. Los máximos locales
-- se darán mediante pares (i,v), donde i es la posición en la lista
-- (comenzando a contar por 1), y v es el valor. 
-- Por ejemplo:
--  > maximosLocales 3 [0.1,0.2,0.1,0.04,0.1,0.2,0.25,0.2]
--  [(2,0.2),(7,0.25)]
--  > maximosLocales 4 [0.1,0.2,0.1,0.04,0.1,0.2,0.25,0.2]
--  [(7,0.25)]
--  > maximosLocales 3 [0.1,0.2]
--  []
--  > maximosLocales 3 []       
--  []
-- ---------------------------------------------------------------------

maximosLocales _ [] = [] -- caso base: para cualquier n con una lista vacía devuelve una lista vacia
maximosLocales _ [x,y] = [] -- El primer y último elemento de la lista no pueden ser
-- un máximo local, por lo que si solo hay 2 elementos en la lista, no hay maximos locales


{-
Como no me va a dar tiempo a implementarlo lo voy explicar con mis palabras:
Agarraríamos por cada elemento de la lista, agarraríamos los 3 anteriores y los 3 posteriores,
comenzando desde la posición 2 hasta la longitud -1 de la lista, y comprobaríamos si es el
máximo (usando el método maximum) de esa lista, si es el máximo, significa que es el mayor de todos
por lo que se añade a la lista de máximos, si no, pues se pasa al siguiente y se va probando eso

maximosLocales n xs = maximosLocales_aux n lista acum
        where lista = zip [1..] xs -- lista de tuplas donde (i,v) i = posicion, v = valor

maximosLocales_aux n [] acum = acum -- caso vacío, se devuelve acumulador
maximosLocales_aux n (x:xs) acum
    | x == maximum lista = maximosLocales_aux n xs (acum ++ [x]) -- si el elemento es maximo de la lista, se añade a acum 
    | otherwise = maximosLocales_aux n xs (acum) -- si no, pues se pasa al siguiente y no se añade en acumulador

    where lista = (take n xs) ++ reverse (take n xs) -- esto sería ir agarrando la parte izquierda y derecha
                                                            --del elemento x, pero, sé que no es correcto

-}