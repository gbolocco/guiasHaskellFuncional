-- 1.Definir una función que sume una lista de números. Nota: Investigar sum 

--sum :: (Num a) => [a] -> a, Funcion SUM, suma todos los elementos de una lista

{-
frecuenciaCardiaca = [80, 100, 120, 128, 130, 123, 125] 
    2.
        a)Definir la función promedioFrecuenciaCardiaca, que devuelve el promedio de la frecuencia cardíaca.
-}

promedioFrecuenciaCardiaca :: (Integral c, Foldable t) => t c -> c
promedioFrecuenciaCardiaca frecuencias = (`div` 7).sum $ frecuencias

-- b) Definir la función frecuenciaCardiacaMinuto/1, que recibe m que es el minuto en el cual quiero
--conocer la frecuencia cardíaca, m puede ser a los 10, 20, 30 ,40,..hasta 60. 

frecuenciaCardiacaMinuto :: [a] -> Int -> a
frecuenciaCardiacaMinuto frecuencias m = frecuencias !! div m 10

{-
    3.Definir la función esCapicua/1, si data una lista de listas, me devuelve si la concatenación de las sublistas es una lista capicua
-}
esCapicua :: Eq a => [a] -> Bool
esCapicua palabra = palabra == reverse palabra
-- esCapicua "neuquen" -> TRUE

{-
    4.Definir la función esCapicua/1, si data una lista de listas, me devuelve si la concatenación de las sublistas es una lista capicua
-}

-- ========== [Orden Superior] ==========

-- 1.Definir la función existsAny/2, que dadas una función booleana y una tupla de tres elementos
--devuelve True si existe algún elemento de la tupla que haga verdadera la función. 

existsAny :: Foldable t => (a -> Bool) -> t a -> Bool
existsAny funcion tupla = any funcion tupla

--existsAny even [1,2,3] -> True

-- 2.Definir la función mejor/3, que recibe dos funciones y un número,
--y devuelve el resultado de la función que dé un valor más alto.

enMejor :: (Ord a) => (a -> a) -> (a -> a) -> a -> a
enMejor func1 func2 numero
    | func1 numero > func2 numero = func1 numero
    | otherwise = func2 numero

-- 3.Definir la función aplicarPar/2, que recibe una función y un par,
--y devuelve el par que resulta de aplicar la función a los elementos del par.

aplicarPar :: (a -> b) -> [a] -> [b]
aplicarPar func par = map func par

--aplicarPar (**3) [2,3] -> [8,27]

--4.Definir la función parDeFns/3, que recibe dos funciones y un valor,
--y devuelve un par ordenado que es el resultado de aplicar las dos funciones al valor.

parDeFns :: (t -> a) -> (t -> b) -> t -> (a, b)
parDeFns func1 func2 valor = (func1 valor, func2 valor)

-- ========== [Orden Superior + Listas] ==========

--1: Definir la función esMultiploDeAlguno/2, que recibe un número y una lista
--y devuelve True si el número es múltiplo de alguno de los números de la lista. 

esMultiploDe :: Integral b => b -> b -> Bool
esMultiploDe numerador divisor = (==0).mod numerador $ divisor

esMultiploDeAlguno numero lista = any (esMultiploDe numero) lista 

--2: Armar una función p romedios/1, que dada una lista de listas me devuelve la
--lista de los promedios de cada lista-elemento

promedio :: Fractional a => [a] -> a
promedio lista = sum lista / fromIntegral (length lista)

promedioListas :: Fractional a => [[a]] -> [a]
promedioListas lista = map promedio lista

--3: Armar una función promediosSinAplazos que dada una lista de listas me devuelve
--la lista de los promedios de cada lista-elemento, excluyendo los que sean menores a 4 que no se cuentan

filtrarNotas :: [Float] -> [Float]
filtrarNotas lista = filter (>=4) lista

promediosSinAplazos :: [[Float]] -> [Float]
--promediosSinAplazos lista = map promedio (map filtrarNotas lista)
promediosSinAplazos lista = (map promedio).(map filtrarNotas) $ lista --Misma funcion con composicion y aplicacion parcial

--4: Definir la función mejoresNotas, que dada la información
--de un curso devuelve la lista con la mejor nota de cada alumno.

mejoresNotas :: Ord a => [[a]] -> [a]
mejoresNotas lista = map maximum lista

--5: Definir la función aprobó/1, que dada la lista de las notas de un alumno devuelve True si el alumno aprobó.
--Se dice que un alumno aprobó si todas sus notas son 6 o más.

aproboNotas lista = not.(any (<6)) $ lista -- Por notas
aproboPromedio = not.(< 6).(promedio ) --Por Promedio

--6: Definir la función aprobaron/1, que dada la información de un curso devuelve la información de los alumnos que aprobaron.

aprobaron :: [[Float]] -> [[Float]]
aprobaron lista = filter aproboPromedio lista

--7: Definir la función divisores/1, que recibe un número y devuelve la lista de divisores.
divisores numero = filter (esMultiploDe numero) [1..numero]

--8: Definir la función exists/2, que dadas una función booleana y una lista devuelve True
--si la función da True para algún elemento de la lista.

exists funcion lista = any funcion lista -- Exists es igual a la funcion ANY, no entiendo que pide la consigna

--9: Definir la función hayAlgunNegativo/2, que dada una lista de números y un (…algo…)
--devuelve True si hay algún nro. negativo

hayAlgunNegativo :: [Int] -> a -> Bool
hayAlgunNegativo lista _ = any (<0) lista --  hayAlgunNegativo [1,2,3,-3] "Hola mundo" -> True

--10: Definir la función aplicarFunciones/2, que dadas una lista de funciones y un valor cualquiera,
--devuelve la lista del resultado de aplicar las funciones al valor.
                    
                --Funciones - valor - resultados
aplicarFunciones :: [a -> b] -> a -> [b]
aplicarFunciones listaDeFunciones valor = map (\f -> f valor) listaDeFunciones

-- aplicarFunciones [even,(*2),(-1),abs] 2 == FALTA TERMINAR ==

--11: Definir la función sumaF/2, que dadas una lista de funciones y un número, devuelve la suma del resultado de aplicar las funciones al número.

sumaF :: Num b =>[(a -> b)] -> a -> b
sumaF fs n = sum (aplicarFunciones fs n)

{-
--12: Un programador Haskell está haciendo las cuentas para un juego de fútbol virtual
(como el Hattrick o el ManagerZone). En un momento le llega la información sobre
la habilidad de cada jugador de un equipo, que es un número entre 0 y 12, y la orden
de subir la forma de todos los jugadores en un número entero;
p.ej., subirle 2 la forma a cada jugador. Ahora, ningún jugador puede tener más
de 12 de habilidad; si un jugador tiene 11 y la orden es subir 2, pasa a 12, no a 13;
si estaba en 12 se queda en 12. Escribir una función subirHabilidad/2 que reciba un número
(que se supone positivo sin validar) y una lista de números, y le suba la habilidad
a cada jugador cuidando que ninguno se pase de 12
-}

subirHabilidad :: [Int] -> Int -> [Int]
subirHabilidad jugadores aumento = map (noSupera12 . (+ aumento) ) jugadores

--funcion min
noSupera12 :: Int -> Int
noSupera12 nuevaHabilidad
    | nuevaHabilidad <= 12 = nuevaHabilidad
    | otherwise = 12

{-
-- 13: Ahora el requerimiento es más genérico: hay que cambiar la habilidad de cada jugador
según una función que recibe la vieja habilidad y devuelve la nueva.
Armar: una función flimitada que recibe una función f y un número n,
y devuelve f n garantizando que quede entre 0 y 12 (si f n < 0 debe devolver 0,
si f n > 12 debe devolver 12)
-}

fLimitada :: (Int -> Int) -> [Int] -> [Int]
fLimitada f jugadores = map (dentroDelRango . f ) jugadores

dentroDelRango :: Int -> Int
dentroDelRango nuevaHabilidad
    | nuevaHabilidad > 12 = 12
    | nuevaHabilidad < 0 = 0
    | otherwise = nuevaHabilidad

--ghci> fLimitada (+(-7)) [6,3,4,11] ==> [0,0,0,4]

--b) Usar cambiarHabilidad/2 para llevar a 4 a los que tenían menos de 4,
--dejando como estaban al resto

--llevarHabilidadA4 :: [Int] -> [Int]
llevarHabilidadA4 jugadores = map dentroDelRangoCuatro jugadores

dentroDelRangoCuatro :: Int -> Int
dentroDelRangoCuatro nuevaHabilidad
    | nuevaHabilidad > 12 = 12
    | nuevaHabilidad <= 4 = 4
    | otherwise = nuevaHabilidad

--14: Investigar lo que hace la función takeWhile/2, que está incluida en el prelude.
--Preguntar primero el tipo, y después hacer pruebas. Ayudarse con el nombre. 

-- TIPO: takeWhile :: (a -> Bool) -> [a] -> [a]
-- EJEMPLO: ghci> takeWhile even [2,4,6,8,1,2,4,6,8] ==> [2,4,6,8]
{-
Lo que realiza la funcion "takeWhile" es recibir una funcion del tipo "(a -> Bool)"
como por ejemplo:
    -even
    -odd
    -esVocal
    -esMultiploDeN
    -etc.
Y tambien recibe una lista de tipo "a", lo que hace es tomar los primeros elementos
de la lista que cumplan con la condicion de la funcion. Hasta encontrar un elemento de la
lista que no cumpla.
-}

--15: a) primerosPares/1

primerosPares :: [Int] -> [Int]
primerosPares numeros = takeWhile even numeros

--b) primerosDivisores (reutilizamos la funcion "esMultiploDe")

primerosDivisores ::Integral b => [b] -> b ->[b]
primerosDivisores numeros divisor = takeWhile (flip esMultiploDe divisor) numeros

--c) primerosNoDivisores/2 (le compongo la funcion "not")

primerosNoDivisores ::Integral b => [b] -> b ->[b]
primerosNoDivisores numeros divisor = takeWhile (not.flip esMultiploDe divisor) numeros

--16: No entendi q pide

--17: a)
crecimientoAnual edad  
    | edad <= 10 = 24-(edad*2)
    | edad > 10 && edad <= 15 = 4
    | edad > 15 && edad <= 17 = 2
    | edad > 17 && edad <= 19 = 1
    | otherwise = 0
--b)
crecimiento = [24,22,20,18,16,14,12,10,8,6,4,4,4,4,4,4,2,2,1,1,0]
crecimientoEntreEdades :: Int -> Int -> Int
crecimientoEntreEdades edadA edadB = sum (take (edadB - edadA) (drop edadA crecimiento))
--c)
alturasEnUnAnio :: Int -> [Int] -> [Int]
alturasEnUnAnio edad personas = map (+ crecimientoAnual edad) personas
--d)
alturaEnEdades edadActual altura edades = map ((+altura).(crecimientoEntreEdades edadActual )) edades

--18: a)
rachasLluvia :: [Integer] -> [[Integer]]
rachasLluvia [] = []
rachasLluvia xs = extraerRachas xs
  where
    extraerRachas :: [Integer] -> [[Integer]]
    extraerRachas [] = []
    extraerRachas (y:ys)
        | y == 0    = extraerRachas (dropWhile (== 0) ys)
        | otherwise = (y : takeWhile (/= 0) ys) : extraerRachas (dropWhile (== 0) (dropWhile (/= 0) ys))
--b)
mes = [0,2,5,1,34,2,0,21,0,0,0,5,9,18,4,0]
mayorRachaDeLluvias m = maximum (map length (rachasLluvia m))

--19: Definir una función que sume una lista de números. Nota: Resolverlo utilizando foldl/foldr. 
sumatoria :: (Foldable t, Num b) => t b -> b
sumatoria lista = foldr (+) 0 lista
--20: Definir una función que resuelva la productoria de una lista de números. Nota: Resolverlo utilizando foldl/foldr. 
productoria :: (Foldable t, Num b) => t b -> b
productoria lista = foldr (*) 1 lista
--21:Definir la función dispersion, que recibe una lista de números y devuelve la dispersión de los valores, o sea máximo - mínimo. Nota: Probar de utilizar foldr.  
dispersion lista = (foldr max (minimum lista) lista)-(foldr min (maximum lista) lista)