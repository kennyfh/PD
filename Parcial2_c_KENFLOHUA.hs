-- Programación Declarativa 2020/21
-- Grado de Ingeniería Informática - Tecnologías Informáticas
-- Parcial 2 (parte c)                            22 de Diciembre de 2020
-- ----------------------------------------------------------------------
-- Apellidos: Flores Huamán
-- Nombre: Kenny Jesús
-- UVUS: Kenflohua
-- ----------------------------------------------------------------------

import System.Environment (getArgs)
import Control.Exception (catch, SomeException)
import System.Directory
import Text.CSV
import Text.Printf
import Data.List
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 5. (3 puntos)
-- Escribir un programa compilable y ejecutable desde la terminal que reciba
-- como parámetros la ruta de un fichero de entrada CSV y el nombre de 2
-- columnas. Es decir, la llamada es:
--   ./programa [entrada_csv] [col1] [col2]
-- El programa debe:

--   a) cargar el contenido del fichero CSV en 'entrada_csv' en una variable
--     de tipo Tabla. Por simplicidad vamos a asumir que todas las columnas del
--     CSV siempre contienen valores numéricos. Tendrás que hacer la transformación
--     a valores numéricos a todos los valores. (1 punto)

--   b) calcular una tabla de agrupación con 'agrupaPor' (como en el ejercicio
--     3.b, si no lo has hecho, puedes usar la definición en Nota 1) para las
--     columnas 'col1', 'col2' y usando como función de agregación la media 
--     aritmética. (1 punto)

--   c) elige una de las siguientes opciones (hasta 1 punto):
--       * imprimir por pantalla la tabla resultado como se muestra en los 
--         ejemplos de abajo. (0,5 puntos)
--       * además de imprimir por pantalla la tabla, escribir en un fichero
--         con nombre out.csv el resultado resultado. (1 punto)
--
-- Se valorará más si se controlan los posible errores: existencia de ficheros, 
-- existencia de columnas en la tabla y número de parámetros en la llamada.
--
-- Nota 1:
-- Si no has podido definir la función agrupaPor del 3.b, usa la siguiente 
-- definición de juguete (sin repercutir en la nota):
agrupaPor _ c1 c2 _ = T "agrupado" [c1,c2] [[24,25],[1102,4090]] 2
 
-- Ejemplos de uso:
--
-- $ ./programa clima.csv Cielo Velocidad_Viento 
-- Cielo   Velocidad_Viento
-- 0.0     25.6
-- 1.0     22.0
-- 2.0     12.8
--
-- Si has usado la función agrupaPor de juguete de arriba, el ejemplo es
-- $ ./programa clima.csv Cielo Velocidad_Viento 
-- Cielo   Velocidad_Viento
-- 24      1102
-- 25      4090
--
-- Ejemplos valorables para máxima nota:
--
-- $ ./programa clima.csv Cielo
-- Los parámetros indicados no son correctos.
-- El formato es el siguiente:
--    programa [entrada csv] [col1] [col2]
-- donde
--   [entrada csv] -> es el archivo csv de entrada
--   [col1] y [col2] -> son la columnas de agrupación
--
-- $ ./programa clima.csv Cielo Vel
-- La columnas indicadas no existen en el CSV de entrada
--
-- $ ./programa clim.csv Cielo Temperatura
-- El fichero de entrada indicado no existe
--
-- ---------------------------------------------------------------------

-- Pueden serte útil estas funciones

-- Transponer los registros leídos de un CSV, de filas a columnas.
traspuesta :: [[a]] -> [[a]]
traspuesta registros = [[(registros!!f)!!c | f <-[0..(length registros)-1]]  | c <- [0..(length (head registros))-1]]

-- Imprimir mensajes de error capturados
control_error err = print "Se ha producido un error." >> print (err::SomeException) 

data Tabla a = T String [String] [[a]] Int deriving Show


main :: IO ()
main = do
  args <- getArgs
  if length args >0 then do
      let filename = head args
      contents <- readFile filename
      let csv = parseCSV filename contents
        filas = case csv of
            (Left _) -> []
            (Right lineas) -> lineas
        filasValidas =  filas
       procesaCabecera (head filasValidas) 
  else do 
    putStrLn "Debe indicar el nombre de archivo."

  return ()

{-
ERROR:
Lo que estoy intentando hacer es si tiene más de un argumento, lea el contenido del fichero
agarrando el primer argumento que hay 

posteriormente habría que tratar para intentar introducir esas lineas en una Tabla T

-}

  

--Cielo,Temperatura,Humedad,Viento,JugarTenis,Velocidad_Viento
-- agrupaPor _ c1 c2 _ = T "agrupado" [c1,c2] [[24,25],[1102,4090]] 2
procesaCabecera t = sequence_ (map (\ (x,y)-> putStrLn $ concat $ [show x," : ",y]) (zip [1..] t))
