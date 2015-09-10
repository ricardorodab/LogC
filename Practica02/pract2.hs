{-Facultad de Ciencias UNAM - Lógica Computacional 2015-2 
		  Profesor: Dr. Favio Ezequiel Miranda 
		  Ayudante: José Manuel Reyes Snyder
		  Laboratorio: C. Moisés Vázquez Reyes
                  Alumno: José Ricardo Rodríguez Abreu. -}

--Para términos
type Nombre = String
data Term = V Nombre | F Nombre [Term] deriving (Show, Eq)

--Lógica de primer orden
data Form = TrueF |           -- ⊤
			FalseF|           -- ⊥
			Pr Nombre [Term]| -- P(t1,t2,⋯,tn) 
			Eq Term Term | 	  -- t1 = t2
			Neg Form |        -- ¬ φ
			Conj Form Form |  -- φ ⋀ ψ
			Disy Form Form |  -- φ ⋁ ψ
			Imp Form Form |   -- φ → ψ
			Equi Form Form |  -- φ ↔ ψ
			All Nombre Form | -- ∀x φ
			Ex Nombre Form 	 {-- ∃x φ  --} deriving (Show, Eq) 

--Estados de variables en el universo 'a'
type Estado a = Nombre -> a

--Interpretación para símbolos de función
type IntF a = Nombre -> [a] -> a

--Interpretación para símbolos de relación
type IntR a = Nombre -> [a] -> Bool


--Actualización un estado en una variable 
actEst :: Estado a -> Nombre -> a -> Estado a
actEst est x m = \z -> if x == z then m else est z
                 


--Interpretación de términos
iTerm :: Estado a -> IntF a -> Term -> a
iTerm est intf (V nom) = est nom
iTerm est intf (F nom term) =  intf nom (recursiTerm est intf term) where
    recursiTerm _ _ [] = []
    recursiTerm est intf (x:xs) = [(iTerm est intf x)] ++ (recursiTerm est intf xs)

--Interpretación de fórmulas
iForm :: Eq a => [a] -> Estado a -> IntF a -> IntR a -> Form -> Bool
iForm ar est intf intr FalseF = False
iForm ar est intf intr TrueF = True
iForm ar est intf intr (Eq m n) = (iTerm est intf m) == (iTerm est intf n)
iForm ar est intf intr (Neg p) = not (iForm ar est intf intr p)
iForm ar est intf intr (Conj p q) = (iForm ar est intf intr p) && (iForm ar est intf intr q)
iForm ar est intf intr (Disy p q) = (iForm ar est intf intr p) || (iForm ar est intf intr q)
iForm ar est intf intr (Imp p q) = (not (iForm ar est intf intr p)) || (iForm ar est intf intr q)
iForm ar est intf intr (Equi p q) = (iForm ar est intf intr (Imp p q)) && (iForm ar est intf intr (Imp q p))
iForm ar est intf intr (Pr n term) = (intr n (arregloA est intf term)) where
    arregloA est intf [] = []
    arregloA est intf (x:xs) = [(iTerm est intf x)] ++ (arregloA est intf xs)
iForm ar est intf intr (All n form) = (todoM ar) where
    todoM [] = True
    todoM (x:xs) = (iForm ar (actEst est n x) intf intr form) && (todoM xs)
iForm ar est intf intr (Ex n form) = (algunM ar) where
    algunM [] = False
    algunM (x:xs) = (iForm ar (actEst est n x) intf intr form) || (algunM xs)

--Universo
m :: [Int]
m = [x | x <- [0..1000], x `mod` 2 == 0]

--Estado de variables
est :: Estado Int 
est "x" = 2
est "y" = 10
est "z" = 6
est "w" = 4
est _ = 0

--Interpretación de fórmulas
iF :: IntF Int
iF "f" = \[a,b] -> a+b
iF "g" = \[a,b] -> a-b
iF _ = \_ -> 0

--Interpretación de predicados
iR :: IntR Int
iR "P" = \[x,y] -> x < y
iR "R" = \[x,y,z] -> x+y == z
iR "C" = \[x] -> x == 0 
iR _ = \_ -> False


--PRUEBAS:

-- [False]
-- ∀x∊m(∃y∊m(x + y = 0)	
prueba1 = iForm m est iF iR $ All "x" $ Ex "y" $ Pr "C" [F "f" [V "x", V "y"]]

-- [True]
-- x+x = z-x	[x ↦ 2, z ↦ 6]
prueba2 = iForm m est iF iR $ Pr "R" [V "x", V "x", F "g" [V "z", V "x"]]

-- [True]
-- ∀x ∊ m(∀y ∊ m(x+y = 0 → x = 0 ⋀ y = 0))
prueba3 = iForm m est iF iR $ All "x" $ All "y" $ Imp (Pr "R" [V "x", V "y", V "c"]) (Conj (Pr "C" [V "x"]) (Pr "C" [V "y"]))

-- [True]
-- (x < y) ⋀ (z + w = y)	[x ↦ 2, y ↦ 10, z ↦ 6, w ↦ 4]
prueba4 = iForm m est iF iR $ Conj (Pr "P" [V "x", V "y"]) (Pr "R" [V "z", V "w", V "y"])

-- [True]
-- ∀x∊m(∃y∊m(x - y = 0)	
prueba5 = iForm m est iF iR $ All "x" $ Ex "y" $ Pr "C" [F "g" [V "x", V "y"]]