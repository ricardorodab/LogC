{-Facultad de Ciencias UNAM - Lógica Computacional 2015-2 
		  Profesor: Dr. Favio Ezequiel Miranda 
		  Ayudante: José Manuel Reyes Snyder
		  Laboratorio: C. Moisés Vázquez Reyes
                  Alumno: José Ricardo Rodríguez Abreu-}

--Para términos:
type Nombre = String
data Term = V Nombre | F Nombre [Term] deriving (Show, Eq)

--Lógica de primer orden:
data Form = TrueF |        -- ⊤
			FalseF|           -- ⊥
			Pr Nombre [Term]| -- P(t1,t2,⋯,tn) 
			Eq Term Term |    -- t1 = t2
			Neg Form |        -- ¬ φ
			Conj Form Form |  -- φ ⋀ ψ
			Disy Form Form |  -- φ ⋁ ψ
			Imp Form Form |   -- φ → ψ
			Equi Form Form |  -- φ ↔ ψ
			All Nombre Form | -- ∀x φ
			Ex Nombre Form   {-- ∃x φ  --} deriving (Show, Eq) 


--Algunos ejemplos:

-- P(x,y)
f1 = Pr "P" [V "x", V "y"]

-- Q(a,h(w))
f2 = Pr "Q" [V "a", F "h" [V "w"]]

-- R(x) → x = b
f3 = Imp (Pr "R" [V "x"]) (Eq (V "x") (F "b" []))

-- ¬R(z) ⋁ (S(a) ⋀ f(x) = z)
f4 = Disy (Neg $ Pr "R" [V "z"]) (Conj (Pr "S" [V "a"]) (Eq (F "f" [V "x"]) (V "z")))

-- ∀xQ(x,g(y))
f5 = All "x" $ Pr "Q" [V "x", F "g" [V "y"]]

-- ∀x∃yP(x,y)
f6 = All "x" $ Ex "y" $ Pr "P" [V "x", V "y"] 

--EJERCICIOS:

--1)
--Variables libres en términos
varT :: Term -> [Nombre]
varT n = (quitaRep (varTHelper n) []) where
    quitaRep [] n = n
    quitaRep (x:xs) lista | elem x lista = quitaRep xs lista
                          | otherwise = quitaRep xs (lista++[x])
    varTHelper (V n) = [n]
    varTHelper (F n m) = varTHelper2 m 
    varTHelper2 [] = []
    varTHelper2 (x:xs) = (varTHelper x)++(varTHelper2 xs)

--2)
--Variables libres en una fórmula
fv :: Form -> [Nombre]
fv n = (quitaRep (fnHelper n []) []) where
    fnHelper (Pr n m) lig = fnPr m lig
    fnHelper (Eq n m) lig = fnLigadas ((varT n) ++ (varT m)) lig []
    fnHelper (Neg n) lig = fnHelper n lig
    fnHelper (Conj n m) lig = (fnHelper n lig) ++ (fnHelper m lig)
    fnHelper (Disy n m) lig = (fnHelper n lig) ++ (fnHelper m lig)
    fnHelper (Imp n m) lig = (fnHelper n lig) ++ (fnHelper m lig)
    fnHelper (Equi n m) lig = (fnHelper n lig) ++ (fnHelper m lig)
    fnHelper (All nom n) lig = (fnHelper n (lig++[nom]))
    fnHelper (Ex nom n) lig = (fnHelper n (lig++[nom]))
    fnHelper p lig = []
    fnLigadas [] lista noLigadas = noLigadas
    fnLigadas (x:xs) lista noLigadas | elem x lista = fnLigadas xs lista noLigadas
                          | otherwise = fnLigadas xs lista (noLigadas++[x])
    fnPr [] lis = []
    fnPr (x:xs) lis = (fnLigadas (varT x) lis []) ++ (fnPr xs lis)
    quitaRep [] n = n
    quitaRep (x:xs) lista | elem x lista = quitaRep xs lista
                          | otherwise = quitaRep xs (lista++[x])


--Tipo substitución
type Subst = [(Nombre,Term)]

--3)
--Sustitución en un término
apsubT :: Term -> Subst -> Term
apsubT (V n) m = apsubTHelperV (V n) m where 
    apsubTHelperV n []  = n 
    apsubTHelperV (V n) (x:xs) | apsubTHelperVOpera n x = apsubCambia x
                               | otherwise = (apsubTHelperV (V n) xs)
    apsubTHelperVOpera n1 (n2,t) = n1 == n2
    apsubCambia (n2,t) = t                         
apsubT (F n m) s = (F n (apsubTHelperF m s)) where
    apsubTHelperF [] s = []
    apsubTHelperF (x:xs) s = [apsubT x s] ++ (apsubTHelperF xs s)

--4)
--Sustitución en una fórmula
apsubF :: Form -> Subst -> Form
--apsubF n = apSubFHelper n sus where
apsubF (Pr n m) sus = (Pr n (apsubFHelperPr m sus)) where
    apsubFHelperPr [] sus = []
    apsubFHelperPr (x:xs) sus = [(apsubT x sus)]++ (apsubFHelperPr xs sus)
apsubF (Eq n m) sus = (Eq (apsubT n sus) (apsubT m sus))
apsubF (Neg n) sus = (Neg (apsubF n sus))
apsubF (Conj n m) sus = (Conj (apsubF n sus) (apsubF m sus))
apsubF (Disy n m) sus = (Disy (apsubF n sus) (apsubF m sus))
apsubF (Imp n m) sus = (Imp (apsubF n sus) (apsubF m sus))
apsubF (Equi n m) sus = (Equi (apsubF n sus) (apsubF m sus))
apsubF n sus = n

--5)
--Clausura universal de una fórmula NOTA: En esta me sale All x All z y en la prueba viene All z All x
aCl :: Form -> Form
aCl n = aClHelper n (fv n) where
    aClHelper n [] = n
    aClHelper n (x:xs) = (All x (aClHelper n xs))

--Pruebas:

--1) Tiene que dar ["x","y","z"]
prueba1 = varT $ F "f" [V "x", V "y", F "g" [V "z"], F "h" [V "x", V "y"]]

--2) Tiene que dar ["o","y","e","s"]
prueba2 = varT $ F "m" [ V "o" , V "y", F "r" [], F "u" [], F "l" [V "e", V "s"]]

--3) Tiene que dar ["z", "w"]
prueba3 = fv $ Ex "x" $ Ex "y" $ Conj (Pr "P" [V "x", V "z"]) (Pr "Q" [V "y", F "a" [], V "w"])

--4) Tiene que dar ["x","a","y"]
prueba4 = fv $ Disy (All "w" $ Ex "a" $ Pr "Q" [V "a", F "h" [V "w"], V "x"]) 
							(Conj (Imp (Equi (TrueF) (FalseF)) (Pr "P" [V "a"]) ) (All "x" $ Pr "P" [V "x", V "y"]))

--5) Tiene que dar F "f" [V "x1",V "y1",V "z"]
prueba5 = apsubT (F "f" [V "x", V "y", V "z"]) [("x", V "x1"), ("y", V "y1"), ("z", V "z")]

--6) Tiene que dar F "g" [V "z",F "h" [F "a" [],F "a" []],V "p"]
prueba6 = apsubT (F "g" [V "a", F "h" [V "m", F "a" []], V "p"]) 
					[("a", V "z"), ("c", V "x"), ("m", F "a" []), ("1", V "a"), ("p", V "p"), ("t", V "s") ]

--7) Tiene que dar Neg (Conj (Disy (Pr "Q" [V "y"]) (Pr "R" [V "z",F "f" [F "f" [V "z"]]])) (Pr "P" [F "f" [V "z"]]))
prueba7 = apsubF (Neg $ Conj (Disy (Pr "Q" [V "x"]) (Pr "R" [V "z",F "f" [V "y"]])) (Pr "P" [V "y"])) 
																							[("x", V "y"), ("y", F "f" [V "z"])]

--8) Tiene que dar All "x" (Ex "y" (Equi (Pr "P" [V "x"]) (Pr "Q" [V "y"])))
prueba8 = apsubF (All "x" $ Ex "y" $ Equi (Pr "P" [V "x"]) (Pr "Q" [V "y"])) [("x", F "a" []), ("y", F "f" [V "x"])]

--9) Tiene que dar All "z" (All "x" (All "y" (Neg (Equi (Pr "Q" [V "x",V "y"]) 
--	(Imp (Pr "P" [F "f" [V "x",V "z"]]) (Pr "Q" [V "y"]))))))
--NOTA: En esta me sale All x All z y en la prueba viene All z All x
prueba9 = aCl $ All "y" $ Neg $ Equi (Pr "Q" [V "x", V "y"]) (Imp (Pr "P" [F "f" [V "x", V "z"]]) (Pr "Q" [V "y"])) 

--10) Tiene que dar All "z" (Equi TrueF (All "x" (Ex "y" (Disy (Pr "Q" [V "x",V "y"]) 
--																					(Neg (Pr "Q" [F "f" [V "x",V "z"]]))))))
prueba10 = aCl $ Equi (TrueF) (All "x" $ Ex "y" $ Disy (Pr "Q" [V "x", V "y"]) (Neg $ Pr "Q" [F "f" [V "x", V "z"]]))


