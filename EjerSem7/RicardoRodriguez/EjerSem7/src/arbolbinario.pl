/* Facultad de Ciencias UNAM - Lógica Computacional 2015-2 
Profesor: Dr. Favio Ezequiel Miranda 
Ayudante: José Manuel Reyes Snyder
Laboratorio: C. Moises Vázquez Reyes 
Alumno: José Ricardo Rodríguez Abreu*/

%Nos dice el numero de hojas de un arbol t.
numHojas(T,N) :- arbolHojas(T,N).

%Nos dice el numero de nodos internos de un arbol t.
numNodInternos(T,N) :- arbolHojas(T,M),arbolNodos(T,S), N is S-M.

%Devuelve una lista con las hojas de un arbol.
hojas(nil,[]).
hojas(t(X,nil,nil),[X]).
hojas(t(_,SI,SD),L) :- hojas(SI,L1),hojas(SD,L2),pega(L1,L2,L).

%Devuelve una lista con los nodos internos de un arbol.
internos(nil,[]).
internos(t(_,nil,nil),[]).
internos(t(X,SI,SD),[X|XS]) :- internos(SI,L1),internos(SD,L2),pega(L1,L2,XS).

%Tomo prestado del ejercicio semanal 5 estas funciones.
%Para concatenar dos listas 
pega([],Y,Y).
pega([X|XS],YS, [X | ZS]) :- pega(XS,YS,ZS).
