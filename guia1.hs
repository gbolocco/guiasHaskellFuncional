
-- Punto 1 --
esMultiploDeTres :: Int -> Bool
esMultiploDeTres numero = (mod numero 3) == 0

-- Punto 2 --   
esMultiplo :: Int -> Int -> Bool
esMultiplo multiplo numero = (mod multiplo numero) == 0

-- Punto 3 --
cubo :: Int -> Int
cubo numero = numero * numero * numero

-- Punto 4 --
area :: Int -> Int -> Int
area base altura = base * altura

-- Punto 5 --
esBisiesto :: Int -> Bool
esBisiesto año = (año `mod` 400 == 0) || ((año `mod` 4 == 0) && (not (año `mod` 100 == 0)))

--Punto 6 --
celsiusToFahr :: Float -> Float
celsiusToFahr gradosC = (gradosC * 9) / 5 + 32

-- Punto 7 --
fahrToCelsius :: Float -> Float
fahrToCelsius gradosF = ((gradosF - 32) * 5) / 9

-- Punto 8 --
haceFrio :: Float -> Bool
haceFrio grados = fahrToCelsius grados <= 8

-- Punto 9 --
mcm :: Int -> Int -> Int
mcm a b = (a * b) `div` gcd' a b

gcd' :: Int -> Int -> Int
gcd' a 0 = a
gcd' a b = gcd' b (mod a b)

-- Punto 10 --
dispersion :: Int -> Int -> Int -> Int
dispersion dia1 dia2 dia3 = max dia1 (max dia2 dia3) - min dia1 (min dia2 dia3)

diasParejos :: Int -> Int -> Int -> Bool
diasParejos dia1 dia2 dia3 = dispersion dia1 dia2 dia3 <= 30

diasLocos :: Int -> Int -> Int -> Bool
diasLocos dia1 dia2 dia3 = dispersion dia1 dia2 dia3 >= 100

diasNormales :: Int -> Int -> Int -> Bool
diasNormales dia1 dia2 dia3 = not (diasParejos dia1 dia2 dia3) &&  not (diasLocos dia1 dia2 dia3)

-- Punto 11 --
pesoPino :: Float -> Float
pesoPino altura
    | altura <= 300 = altura * 3
    | altura > 300   = 300 * 3 + (altura - 300) * 2

esPesoUtil :: Float -> Bool
esPesoUtil altura = pesoPino altura < 1000 && pesoPino altura >= 400

-- Punto 12 --