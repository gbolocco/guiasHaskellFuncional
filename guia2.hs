import Data.Time.Format.ISO8601 (Format)
import GHC.Base (BCO)
{-  1.Definir una función siguiente, que al invocarla con un número cualquiera me devuelve el resultado
de sumar a ese número el 1. -}

siguiente :: Int -> Int
siguiente numero = numero + 1

{-  2.Definir la función mitad que al invocarla con un número cualquiera
me devuelve la mitad de dicho número -}

mitad:: Float -> Float
mitad numero = numero/2

{-  3.Definir una función inversa, que invocando a la función con un número
cualquiera me devuelva su inversa.  -}

inversa:: Float -> Float
inversa numero = 1/numero

{-  4.Definir una función triple, que invocando a la función con un número
cualquiera me devuelva el triple del mismo.  -}

triple:: Int -> Int
triple numero = (*) 3 numero

{-  5.Definir una función esNumeroPositivo, que invocando a la función con un
número cualquiera me devuelva true si el número es positivo y false en caso contrario.   -}

esNumeroPositivo :: (Num a, Ord a) => a -> Bool
esNumeroPositivo x = x > 0

{-  6.Resolver la función del ejercicio 2 de la guía anterior esMultiploDe/2,
utilizando aplicación parcial y composición.  -}

esMultiploDe :: Integral b => b -> b -> Bool
esMultiploDe numerador divisor = (==0).mod numerador $ divisor

{-  7.Resolver la función del ejercicio 5 de la guía anterior esBisiesto/1,
utilizando aplicación parcial y composición.  -}

esBisiesto :: Integer -> Bool
esBisiesto = (||) <$> (flip esMultiploDe 400) <*> ((&&) <$> (flip esMultiploDe 4) <*> (not . (flip esMultiploDe 100)))

{-  8.Resolver la función inversaRaizCuadrada/1,
que da un número n devolver la inversa su raíz cuadrada.  -}

inversaRaizCuadrada :: Float -> Float
inversaRaizCuadrada numero = inversa.sqrt $ numero

{-  9.Definir una función incrementMCuadradoN, que invocándola con 2 números m y n,
incrementa un valor m al cuadrado de n  -}

incrementMCuadradoN :: Floating a => a -> a -> a
incrementMCuadradoN m n = (+m).(**2) $ n

{-  10.Definir una función esResultadoPar/2, que invocándola con número n y otro m,
devuelve true si el resultado de elevar n a m es par.  -}

esResultadoPar :: (RealFrac b, Floating b) => b -> b -> Bool
esResultadoPar m n = even.(truncate.(**n)) $ m