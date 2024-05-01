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