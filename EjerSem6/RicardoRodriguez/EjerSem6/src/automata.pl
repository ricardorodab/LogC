/* Facultad de Ciencias UNAM - Lógica Computacional 2015-2 
Profesor: Dr. Favio Ezequiel Miranda 
Ayudante: José Manuel Reyes Snyder
Laboratorio: C. Moises Vázquez Reyes 
Alumno: José Ricardo Rodríguez Abreu*/

mealy([],[]).
mealy([X|XS],M) :- X == 0, reversa(M,MN), cero(XS,MN,[]).
mealy([X|XS],M) :- X == 1, reversa(M,MN), uno(XS,MN,[]).

mealy([],M,M).
mealy([X|XS],M,N) :- X == 0, cero(XS,M,N).
mealy([X|XS],M,N) :- X == 1, uno(XS,M,N).

cero([],M,M).
cero([X|XS],M,N) :- X == 0, agrega(a,N,MN), mealy(XS,M,MN).
cero([X|XS],M,N) :- X == 1, agrega(b,N,MN), mealy(XS,M,MN).

uno([],M,M).
uno([X|XS],M,N) :- X == 0, agrega(c,N,MN), mealy(XS,M,MN).
uno([X|XS],M,N) :- X == 1, agrega(d,N,MN), mealy(XS,M,MN).

%Funcion que agrega elementos a nuestra lista.
agrega(X,Y ,[X|Y]). 

%Tomo prestado del ejercicio semanal 5 estas funciones.
%Para concatenar dos listas 
pega([],Y,Y).
pega([X|XS],YS, [X | ZS]) :- pega(XS,YS,ZS).
%Nos dice si una lista es la reversa de la otra.
reversa([],[]).
reversa([X|XS],Y) :- reversa(XS,S),pega(S,[X],Y).
