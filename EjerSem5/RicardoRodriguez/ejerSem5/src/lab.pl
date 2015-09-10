/* Facultad de Ciencias UNAM - Lógica Computacional 2015-2 
Profesor: Dr. Favio Ezequiel Miranda 
Ayudante: José Manuel Reyes Snyder
Laboratorio: C. Moises Vázquez Reyes 
Alumno: José Ricardo Rodríguez Abreu*/

%Nos dice cuándo algo es natural
nat(cero).
nat(suc(N)) :- nat(N).

%Para sumar naturales
suma(cero,N,N).
suma(suc(N),M,suc(R)) :- suma(N,M,R).

%Para multiplicar naturales
prod(cero,_,cero).
prod(suc(N),M,R) :- suma(NM,M,R),prod(N,M,NM).

%Algunas relaciones
gato(tom).
raton(jerry).
come(tom, jerry).
animal(oso).
animal(X) :- gato(X).
animal(X) :- raton(X).
peligroso(X) :- animal(X), (X = oso; come(X,_)).


%LISTAS

%Para obtener la cabeza de una lista
cabeza([X|_], X). 

%Para obtener la cola de una lista
cola([_|Y],Y).

%Para concatenar dos listas 
pega([],Y,Y).
pega([X|XS],YS, [X | ZS]) :- pega(XS,YS,ZS).

%Nos da el factorial de un numero
factorial(cero, suc(cero)).
factorial(suc(N), M) :- prod(suc(N),K,M), factorial(N,K).

%Potencia de un numero.
potencia(_,cero,suc(cero)).
potencia(X,suc(N),R) :- prod(X,K,R), potencia(X,N,K).

%Nos dice si un numero es menor que otro
menor(cero,suc(_)).
menor(suc(N),suc(M)) :- menor(N,M).

%Nos dice si un numero es mayor que otro.
mayor(suc(_),cero).
mayor(suc(N),suc(M)) :- mayor(N,M).

%Nos dice si dos numeros son iguales.
igual(cero,cero).
igual(suc(N),suc(M)) :- igual(N,M).

%Nos dice si un elemento es parte de la lista.
elem(X, [X|_]).
elem(X, [_ | XS]) :- elem(X,XS).

%Nos dice si una lista es la reversa de la otra.
reversa([],[]).
reversa([X|XS],Y) :- reversa(XS,S),pega(S,[X],Y).

%Nos dice si una lista es un palindromo.
palindromo(X) :- reversa(X,X).

%Nos regresa el ultimo elemento.
ultimo(X,[X]).
ultimo(X,[_|XS]) :- ultimo(X,XS).

%Nos dice si la longitud es la de una lista.
long([],cero).
long([_|XS],suc(N)) :- long(XS,N).
