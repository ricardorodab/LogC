/* Facultad de Ciencias UNAM - Lógica Computacional 2015-2 
Profesor: Dr. Favio Ezequiel Miranda 
Ayudante: José Manuel Reyes Snyder
Laboratorio: C. Moises Vázquez Reyes 
Alumno: José Ricardo Rodríguez Abreu*/

%Tira los primeros N elemenos de una lista L y devuelve el resultado R.
drop(0,L,L).
drop(N,[_|XS],R) :- drop(M,XS,R),N is M+1.

%Toma los N primeros elementos de la lista L y devuelve el resultado R.
take(0,_,[]).
take(N,[X|XS],YS) :- take(M,XS,ZS),pega([X],ZS,YS),N is M+1.

%Nos dice cuantas veces aparece X en la lista L.
veces(_,[],0).
veces(X,[Y|YS],N) :- X == Y,veces(X,YS,M),N is M+1.
veces(X,[_|YS],N) :- veces(X,YS,N).

%Nos devuelve el elemento X en la posicion N en la lista L.
get(0,[X|_],X).
get(N,[_|XS],Y) :- get(M,XS,Y),N is M+1.

%Tomo prestado del ejercicio semanal 5 estas funciones.
%Para concatenar dos listas 
pega([],Y,Y).
pega([X|XS],YS, [X | ZS]) :- pega(XS,YS,ZS).

