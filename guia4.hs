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

esMultiploDeAlguno :: Integral a => a -> [a] -> [Bool]
esMultiploDeAlguno numero lista = map (esMultiploDe numero) lista 