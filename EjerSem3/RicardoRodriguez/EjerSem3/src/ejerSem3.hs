{-Facultad de Ciencias UNAM - Lógica Computacional 2014-2 
		  Profesor: Dr. Favio Ezequiel Miranda 
		  Ayudante: José Manuel Reyes Snyder
		  Laboratorio: C. Moisés Vázquez Reyes
                  Alumno: José Ricardo Rodríguez Abreu -}

--Para términos
type Nombre = String
data Term = V Nombre | F Nombre [Term] deriving (Show, Eq)

--Funciones que asignan un estado a cada variable.
type Estado = Nombre -> Int

--Interpretación para símbolos de función.
type IntF = Nombre -> [Int] -> Int



--Interpretación de términos
interp :: Estado -> IntF -> Term -> Int
interp est intf (V n) = est n
interp est intf (F nom term) = intf nom (recursInterp est intf term) where
    recursInterp _ _ [] = []
    recursInterp est intf (x:xs) = [(interp est intf x)] ++ (recursInterp est intf xs)
    

est1 :: Estado 
est1 "x" = 14
est1 "y" = 21
est1 "z" = 10
est1 _ = 0 

iF1 :: IntF 
iF1 "a" = \_ -> 1
iF1 "g" = \[a,b,c] -> a+b+c
iF1 "f" = \[a,b,c] -> (a+b)*c
iF1 "h" = \[a,b] -> a*b
iF1 _ = \_ -> 0


--Pruebas:
--Debe de dar 21
prueba1 = interp est1 iF1 (F "h" [F "a" [], V "y"]) 
--Debe de dar 588
prueba2 = interp est1 iF1 (F "f" [V "x", V "x", V "y"]) 
--Debe de dar 6209
prueba3 = interp est1 iF1 (F "g" [V "x", V "y", F "h" [V "y", F "h" [V "y", V "x"]]]) 
-- Debe de dar 0
prueba4 = interp est1 iF1 (F "h" [ F "f" [V "a", V "b", V "c"], V "x"]) 
--Debe dar 210
prueba5 = interp est1 iF1 (F "h" [V "y", F "g" [F "m" [], F "o" [], V "z"]]) 


