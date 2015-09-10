%El mundo de los bloques.

:- dynamic on/2. %Hace que es dinámico

on(a,b).
on(b,c).
on(c,piso).

on(d,piso).

on(e,f).
on(f, piso).

on(g,h).
on(h,i).
on(i,piso).

%1. Estar bloqueado.
blocked(X) :- on(_,X).

%2. Estar en el tope
onTop(X) :- \+ blocked(X).

%3 Mover al piso un bloque.
move(X,piso) :- onTop(X),retract(on(X,_)),assertz(on(X,piso)).

%4 Mover al bloque 'a' encima del bloque 'b'.
move(X,Y) :- onTop(X),onTop(Y),retract(on(X,_)),assertz(on(X,Y)).
