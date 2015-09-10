{-Facultad de Ciencias UNAM - Lógica Computacional 2015-2 
		  Profesor: Dr. Favio Ezequiel Miranda 
		  Ayudante: José Manuel Reyes Snyder
		  Laboratorio: C. Moisés Vázquez Reyes
                  Alumno: José Ricardo Rodríguez Abreu-}

--Tipo de dato fórmula.
data Prop = Ff  				 -- ⊥ 
           |Tt  				 -- ⊤
			  |VarP NVar 		 -- Variables
			  |Neg Prop  		 -- ¬ φ
			  |Conj Prop Prop  -- φ ⋀ ψ
			  |Disy Prop Prop  -- φ ⋁ ψ
			  |Imp Prop Prop   -- φ → ψ
			  |Equiv Prop Prop deriving Eq -- φ ↔ ψ


--Tipo para representar variables.
data NVar =	A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving (Show, Eq)

--Hacemos parte de la familia Show al tipo Prop.
instance Show Prop where
	show (VarP x) = show x
	show (Neg (VarP x)) = "~" ++ show x
	show (Neg p) = "~(" ++ show p ++ ")"
	show (Conj p q) = "("++show p++")&("++show q++")"
	show (Disy p q) = "("++show p++")|("++show q++")"
	show (Imp p q) = "("++show p++")-->("++show q++")"
	show (Equiv p q) = "("++show p++")<-->("++show q++")"
	show Tt = "T"
	show Ff = "F"

--Si la variable está en la lista de estado se evalúa a True, False en otro caso.
type Estado = [NVar]


	--EJERCICIOS:

--Función de interpretación.
interp :: Estado -> Prop -> Bool
interp _ Tt = True
interp _ Ff = False
interp s (VarP p) = elem p s
interp s (Neg p) = not(interp s p)
interp s (Conj p q) = (interp s p) && (interp s q)
interp s (Disy p q) = (interp s p) || (interp s q)
interp s (Imp p q) = not(interp s p) || (interp s q)
interp s (Equiv p q) = (interp s p) == (interp s q)

--Devuelve las variables de una fórmula.
vars :: Prop -> [NVar]
vars x = quitaRepetidos (varsHelper x)
    where varsHelper Tt = []
          varsHelper Ff = []
          varsHelper (VarP p) = [p]
          varsHelper (Neg p) = varsHelper p
          varsHelper (Conj p q) = (varsHelper p ++ varsHelper q)
          varsHelper (Disy p q) = (varsHelper p ++ varsHelper q)
          varsHelper (Imp p q) = (varsHelper p ++ varsHelper q)
          varsHelper (Equiv p q) = (varsHelper p ++ varsHelper q)

--Método auxiliar para quitar variables repetidas de una lista.
quitaRepetidos :: Eq a => [a] -> [a]
quitaRepetidos [] = []
quitaRepetidos (x:xs) | elem x xs = (quitaRepetidos xs)
                      | otherwise = [x] ++ (quitaRepetidos xs)

--Calcula la lista potencia de una lista.
subconj :: [a] -> [[a]]
subconj [] = [[]]
subconj (x:xs) = (subconj xs) ++ [x:t | t <- subconj xs]

--Estados posibles de una fórmula.
estados :: Prop -> [Estado]
estados x =  subconj $ vars x

--Nos devuelve todos los estados posibles de una lista de fórmulas.
estadosConj :: [Prop] -> [Estado]
estadosConj f = subconj $ quitaRepetidos $ varsF f
  where varsF [] = []
        varsF (x:xs) = (vars x) ++ (varsF xs)

--Nos devuelve los estados que satisfacen al conjunto de fórmulas.
modelosConj :: [Prop] -> [Estado]
modelosConj prop = modelosConjHelper (estadosConj prop) prop
    where modelosConjHelper est [] = []
          modelosConjHelper [] _ = []
          modelosConjHelper (x:xs) prop | (probador x prop) = [x] ++ (modelosConjHelper xs prop)
                                        | otherwise = (modelosConjHelper xs prop)
          probador est [] = True
          probador est (x:xs) | (interp est x) = (probador est xs)
                              | otherwise = False
                                                          
          --PRUEBAS:
--Debe dar False.
prueba1 = interp [P] $ Conj (Imp (VarP P) (VarP Q)) (Imp (VarP Q) (VarP P)) 

--Debe dar True.
prueba2 = interp [P,Q] $ Conj (Imp (VarP P) (VarP Q)) (Imp (VarP Q) (VarP P)) 

--Debe dar True.
prueba3 = interp [A,C,M,I,P,T] $ Imp (Conj (Equiv Tt (VarP R)) (Disy (VarP P) (VarP Q))) (Equiv Tt (VarP R))

--Debe contener veinticuatro estados. 
--24?? MÁS BIEN DEBE SER UN NÚMERO DE LA FORMA 2^N NO?
prueba4 = estados $ Equiv (Disy (VarP P) (VarP Q)) (Conj (Imp (VarP S) (VarP P)) (VarP R))

--Debe contener treinta y dos estados.
prueba5 = estados $ Imp (Disy (VarP Q) (VarP R)) (Conj (Imp (VarP S) (VarP T)) (VarP P))

--Debe de dar [[],[Q],[R],[R,Q],[P],[P,Q],[P,R],[P,R,Q]].
prueba6 = estadosConj [Conj (Imp (VarP P) (VarP R)) (Imp (VarP Q) (VarP P)), 
							 Conj (Imp (VarP R) (VarP Q)) (Imp (VarP Q) (VarP P)), 
								Imp (Disy (VarP P) (VarP Q)) (Conj (Imp (VarP P) (VarP P)) (VarP P))]

--Debe de dar [[],[D],[C],[C,D],[B],[B,D],[B,C],[B,C,D],[A],[A,D],[A,C],[A,C,D],[A,B],[A,B,D],[A,B,C],[A,B,C,D]].
prueba7 = estadosConj [Imp (Disy (VarP A) (VarP B)) (Conj (Imp (VarP C) (VarP D)) (VarP A)), 
							 Conj (Imp (VarP B) (VarP C)) (Imp (VarP D) (VarP A))]

--Debe dar False.
--FALSE??? Más bien da []????
prueba8 = modelosConj [Conj (VarP A) (Neg $ VarP A), Disy (VarP A) (Tt), 
								Conj (Imp (VarP P) (VarP R)) (Imp (VarP Q) (VarP P))]

--Debe dar [[A,B,C]].
prueba9 = modelosConj [ Conj (VarP A) (VarP B), Imp (VarP A) (VarP C), Imp (VarP B) (VarP C)]

--Debe dar [[B,C],[A,B,C]].
prueba10 = modelosConj [ Disy (VarP A) (VarP B), Imp (VarP A) (VarP B), Imp (VarP B) (VarP C)]


