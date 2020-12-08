-- PD-Práctica 8.1 
-- Tipos: Definiciones básicas de tipos de datos algebráicos
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================
import Data.Maybe
-- ---------------------------------------------------------------------
-- Ejercicio 1. Haciendo uso de la decraración de tipos (type) define un
-- tipo nuevo, Punto2D, para los Puntos del Plano (de 2 dimensiones).
-- ---------------------------------------------------------------------
type Punto2D = (Float,Float)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Usando el tipo Punto2D, define un vector delimitado por
-- un par de puntos, Vector2D.
-- ---------------------------------------------------------------------
type Vector2D = (Punto2D,Punto2D)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función vector2Dcoor, que reciba un Vector2D
-- y devuelva un par que describa sus coordenadas. Si el vector está
-- formado por los puntos p1 y p2, entonces el calculo del par se
-- calcula como sigue:
--   * la primera componente es la diferencia de la primera componente
--     de p2 menos del p1.
--   * la segunda componente es la diferencia de la segunda componente
--     de p2 menos del p1.
-- ---------------------------------------------------------------------
vector2Dcoor :: Vector2D -> Punto2D
vector2Dcoor ((x1,y1),(x2,y2)) = (x2-x1,y2-y1)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función productoEscalar, tal que reciba dos
-- vectores Vector2D, y devuelva su producto escalar, calculado como
-- sigue: la suma de la multiplicación de las componentes de las
-- coordenadas de los vectores.
-- ---------------------------------------------------------------------
productoEscalar :: Vector2D -> Vector2D -> Float
productoEscalar v1 v2 = x1*x2 + y1*y2
        where (x1,y1) = vector2Dcoor v1
              (x2,y2)= vector2Dcoor v2
            
-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función norma, tal que reciba un vector tipo
-- Vector2D y devuelva el módulo del vector, definido como la raíz
-- cuadrada del producto escalar del vector por sí mismo.
-- ---------------------------------------------------------------------
norma :: Vector2D -> Float
norma v1 = undefined
   
-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función paralelos, que reciba dos vectores
-- tipo Vector2D y devuelva si los vectores son paralelos. El cálculo
-- necesario para ello es comprobar que el valor absoluto del producto
-- escalar de los dos vectores dividido por la multiplicación de la
-- norma de cada uno, sea igual a 1.
-- ---------------------------------------------------------------------
paralelos :: Vector2D -> Vector2D -> Bool
paralelos v1 v2 = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 7. Define tipos de datos (data) para almacenar información
-- sobre el calendario: días de la semana, meses, y estaciones del año.
-- ---------------------------------------------------------------------
type Calendario = (Dia, Mes, Estacion)
-- no hace falta ponerle deriving porque lo hereda todo de los data

data Dia = Lunes | Martes | Miercoles | Jueves | Viernes
    deriving (Show, Eq,Ord) -- El orden por defecto es el definido por el constructor

data Mes = Enero | Febrero | Marzo | Abril | Mayo | Junio | Julio | Agosto | Septiembre | Octubre | Noviembre | Diciembre
    deriving (Show,Eq,Ord)

data Estacion = Primavera | Verano | Otoño | Invierno
    deriving (Show,Eq,Ord)

hoy :: Calendario
hoy = (Jueves, Diciembre, Otoño)

-- ---------------------------------------------------------------------
-- Ejercicio 8. Haciendo uso del tipo Maybe, define una función de
-- división segura (que al dividir por 0 no lance una excepción).
-- ---------------------------------------------------------------------
divisionSegura :: Float -> Float -> Maybe Float
divisionSegura m 0 = Nothing 
divisionSegura m n = Just (m / n)

divisionSeguraImpr :: IO()
divisionSeguraImpr = do  
    putStrLn "Introduzca 2 numeros reales"  
    xs <- getLine
    ys <- getLine
    let x =  read xs
    let y =  read ys
    if(divisionSegura x y) == Nothing then
        putStrLn "Error: No se puede dividir entre cero" 
    else do -- do -> dos o mas lineas
        let Just valor = divisionSegura x y
        putStrLn "El resultado es "
        putStrLn (show valor)

-- ---------------------------------------------------------------------
-- Ejercicio 9. Haciendo uso del tipo Maybe, define una función que
-- devuelva las raices de una ecuación de segundo grado.
-- ---------------------------------------------------------------------


