/* Facultad de Ciencias UNAM - Lógica Computacional 2015-2 
Profesor: Dr. Favio Ezequiel Miranda 
Ayudante: José Manuel Reyes Snyder
Laboratorio: C. Moises Vázquez Reyes 
Alumno: José Ricardo Rodríguez Abreu*/

%Nos dice si es un arbol de busqueda dada una lista.
arbBinBusq(L,T) :- arbol(L,T, nil).

%Crea el arbol de busqueda elemento por elemento.
arbol([],T,T).
arbol([X|XS],T,TS) :- ubicacion(X,TS,TN),arbol(XS,T,TN).

%Busca y coloca cada elemento de la lista donde debe ir en el arbol.
ubicacion(X,nil,t(X,nil,nil)). 
ubicacion(X,t(P,I1,D),t(P,I2,D)) :- X =< P, ubicacion(X,I1,I2).
ubicacion(X,t(P,I,D1),t(P,I,D2)) :- X >= P, ubicacion(X,D1,D2).

%Da la lista en forma de preorder.
preOrden(T,L) :- reversa(L2,L),preOrden(T,L2,[]).
preOrden(nil,L,L).
preOrden(t(P,I,D),L,LN) :- agrega(P,LN,L2),preOrden(I,L3,L2),preOrden(D,L,L3).

%Da la lista en forma de inorder.
inOrden(T,L) :- reversa(L2,L),inOrden(T,L2,[]).
inOrden(nil,L,L).
inOrden(t(P,I,D),L,LN) :- inOrden(I,L2,LN),agrega(P,L2,L3),inOrden(D,L,L3).

%Da la lista en forma de posorder.
posOrden(T,L) :- reversa(L2,L),posOrden(T,L2,[]).
posOrden(nil,L,L).
posOrden(t(P,I,D),L,LN) :- posOrden(I,L2,LN),posOrden(D,L3,L2),agrega(P,L3,L).

%Funcion que agrega elementos a nuestra lista.
agrega(X,Y ,[X|Y]). 

%Tomo prestado del ejercicio semanal 5 estas funciones.
%Para concatenar dos listas 
pega([],Y,Y).
pega([X|XS],YS, [X | ZS]) :- pega(XS,YS,ZS).
%Nos dice si una lista es la reversa de la otra.
reversa([],[]).
reversa([X|XS],Y) :- reversa(XS,S),pega(S,[X],Y).
