--Pactica 0: Lógica computacional.

--Definición de naturales.
data Nat = Cero | Suc Nat deriving Show

--Definición de la función suma.
suma :: Nat -> Nat -> Nat
suma Cero n = n
suma (Suc m) n = Suc $ suma m n

--Definición de la función multiplicación.
prod :: Nat -> Nat -> Nat
prod Cero n = Cero
prod n Cero = Cero
prod (Suc m) n = suma n $ prod m n

--Definimos la función Mayor que ">"
mayorQue :: Nat -> Nat -> Bool
mayorQue Cero Cero = False
mayorQue Cero n = False
mayorQue n Cero = True
mayorQue (Suc m) (Suc n) = mayorQue m n

--Definimos la función Menor que "<"
menorQue :: Nat -> Nat -> Bool
menorQue Cero Cero = False
menorQue Cero n = True
menorQue n Cero = False
menorQue (Suc m) (Suc n) = menorQue m n

--Definimos la función igual "==" para saber si dos números naturales son iguales.
igual :: Nat -> Nat -> Bool
igual Cero Cero = True
igual Cero n = False
igual n Cero = False
igual (Suc m) (Suc n) = igual m n

--Definimos la función power para sacar la potencia de un número.
power :: Int -> Int -> Int
power n 0 = 1
power n 1 = n
power n m = n * (power n (m-1))

--Definimos la función power2 para sacar la potencia de un numero bajo otro esquema
power2 :: Int -> Int -> Int
power2 n k = if (mod k 2 > 0) then (n * (power n (k-1))) 
       	     else (power n (2 * (div k 2)))

--Definición de reversa de listas []
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]

--Definimos la función suma de una lista de enteros.
sumal :: [Int] -> Int
sumal [] = 0
sumal (x:xs) = x + sumal xs

--Definimos la función para tomar los primeros n elementos de la lista.
toma :: Int -> [a] -> [a]
toma 0 [] = []
toma 0 (x:xs) = []
toma n [] = []
toma n (x:xs) = [x] ++ (toma (n-1) xs)

--Definimos la función para desechar los primeros n elementos de una lista.
tira :: Int -> [a] -> [a]
tira 0 (x:xs) = (x:xs)
tira n [] = []
tira n (x:xs) = (tira (n-1) xs)

--Definimos la función para ver cuantas veces está un elemento en una lista.
cuantas :: Eq a => a -> [a] -> Int
cuantas a [] = 0
cuantas a (x:xs) = if a == x then 1 + (cuantas a (xs))
	  	      	     else (cuantas a (xs))

--Definimos la función que nos devuelve una lista con el objeto y su frecuencia.
frec :: Eq a => [a] -> [(a, Int)]
frec = (frecHelper [] 1)
    where frecHelper rep n []  = []
       	  frecHelper rep n [a]  = if not (elem a rep) then [(a,n)] else []
	  frecHelper rep n (x:xs) = if (elem x rep) then (frecHelper rep n xs) 
	  	     else (frecHelper rep ((cuantas x xs)+1) [x]) ++ (frecHelper (rep++[x]) n xs)

--Definimos la función que nos regresa los elementos que se encuentan una sola vez.
unaVez :: Eq a => [a] -> [a]
unaVez = unaVezHelper []
    where unaVezHelper var [] = []
    	  unaVezHelper var [a] = [a]
    	  unaVezHelper var (x:xs) = if (elem x var || elem x xs) 
	  	       	   	  then (unaVezHelper ([x] ++ var) xs) else ([x] ++ (unaVezHelper var xs))

--PUNTO EXTRA:
--Definimos una función que nos de los n primeros números primos.
primosHasta :: Int -> [Int]
primosHasta n = (primosHastaHelper [2..n] 1)
	    where primosHastaHelper [] n = []
	    	  primosHastaHelper [a] n = if (n >= a) then [a] else 
		  		    (if (not (mod a n > 0) && n /= 1) then [] else primosHastaHelper [a] (n+1))
		  primosHastaHelper (x:xs) n = (primosHastaHelper [x] 1) ++ (primosHastaHelper xs 1)

--Fin de pract1.hs
--José Ricardo Rodríguez Abreu