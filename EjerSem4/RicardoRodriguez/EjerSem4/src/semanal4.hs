{-Facultad de Ciencias UNAM - Lógica Computacional 2015-2 
		  Profesor: Dr. Favio Ezequiel Miranda 
		  Ayudante: José Manuel Reyes Snyder
		  Laboratorio: C. Moisés Vázquez Reyes
                  Alumno : José Ricardo Rodríguez Abreu -}

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


--Una figura es un círculo, un triángulo o un cuadrado.
data Fig = Circ | Trian | Cuadr deriving (Show, Eq)
--Hay tres tamaños: pequeño, mediano, grande.
data Tam = Peq | Med | Gran deriving (Show, Eq)
--Las primeras dos coordenadas son la posición en el plano de las 
--figuras, la tercera el tipo de figura y la cuarta su tamaño.
type Figura = (Int, Int, Fig, Tam) 


est :: Estado Figura
est = \_ -> (0,0,Circ,Peq)

iF :: IntF Figura
iF = \_ -> \_ -> (0,0,Circ,Peq) 

--Interpretación de predicados
iR :: IntR Figura
iR "C" = \[(x,y,z,w)] -> z == Circ
iR "T" = \[(x,y,z,w)] -> z == Trian
iR "Q" = \[(x,y,z,w)] -> z == Cuadr
iR "G" = \[(x,y,z,w)] -> w == Gran
iR "M" = \[(x,y,z,w)] -> w == Med
iR "P" = \[(x,y,z,w)] -> w == Peq
iR "N" = \[(x1,y1,z1,w1),(x2,y2,z2,w2)] -> y1 > y2
iR "S" = \[(x1,y1,z1,w1),(x2,y2,z2,w2)] -> y1 < y2
iR "E" = \[(x1,y1,z1,w1),(x2,y2,z2,w2)] -> x1 > x2
iR "O" = \[(x1,y1,z1,w1),(x2,y2,z2,w2)] -> x1 < x2
iR "R" = \[(x1,y1,z1,w1),(x2,y2,z2,w2)] -> x1 == x2

--La lista de figuras en el plano.
mundoFiguras :: [Figura]
mundoFiguras = [(5,5,Circ,Peq),(3,4,Circ,Gran),
                (5,4,Trian,Gran),(7,4,Circ,Peq),
                (8,4,Cuadr,Gran),(2,3,Cuadr,Peq),
                (5,3,Cuadr,Peq),(8,3,Circ,Peq),
                (3,2,Circ,Gran),(8,2,Trian,Peq),
                (3,1,Trian,Gran)]

--[False]
--Ningún cuadrado está al norte de un círculo grande.
prueba1 = iForm mundoFiguras est iF iR $ Neg $ Ex "x" $ Conj (Pr "Q" [V "x"])
			 (Ex "y" $ Conj (Conj (Pr "C" [V "y"]) (Pr "G" [V "y"])) (Pr "N" [V "x", V "y"]))

--[True]
--Todos los círculos medianos están al oeste de un mismo triángulo grande.
prueba2 = iForm mundoFiguras est iF iR $ Ex "x" $ Conj (Conj (Pr "T" [V "x"]) (Pr "G" [V "x"]))
                         (All "y" $ (Imp (Conj (Pr "C" [V "y"]) (Pr "M" [V "y"])) (Pr "O" [V "y", V "x"])))

--[False]
--Todos los cuadrados pequeños están al sur de cualquier triángulo.
prueba3 = iForm mundoFiguras est iF iR $ All "x" $ Imp (Conj (Pr "Q" [V "x"]) (Pr "P" [V "x"]))
                         (All "y" $ Imp (Pr "T" [V "y"]) (Pr "S" [V "x", V "y"]))

--[False]
--Si dos cuadrados están en el mismo renglón, entonces cualquier triángulo al sur de ambos es mediano.
prueba4 = iForm mundoFiguras est iF iR $ All "x" $ All "y" $ Imp (Conj (Conj (Pr "Q" [V "x"]) (Pr "Q" [V "y"])) (Pr "R" [V "x", V "y"]))
                         (All "z" $ Imp (Conj (Pr "T" [V "z"]) (Conj (Pr "S" [V "z",V "x"]) (Pr "S" [V "z",V "y"]))) (Pr "M" [V "z"]))
