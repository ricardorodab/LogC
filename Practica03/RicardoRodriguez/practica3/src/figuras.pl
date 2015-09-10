%El mundo de los bloques.

:- dynamic on/2.

on(a,b).
on(b,c).
on(c,piso).

on(d,piso).

on(e,f).
on(f,piso).

on(g,h).
on(h,i).
on(i,piso).


%1. Estar bloqueado.
blocked(X) :- on(_,X).

%2. Estar en el tope.
onTop(piso).
onTop(X) :- \+ blocked(X).

%3. Mover al piso un bloque.
move(X,piso) :- onTop(X),retract(on(X,_)),assertz(on(X, piso)).

%4. Mover al bloque 'a' encima del bloque 'b'.
move(X,Y) :- onTop(X),onTop(Y),retract(on(X, _)), assertz(on(X,Y)).

%5. Nos da el tope, el fondo de la pila.
bottom(X,Y) :- onTop(X),on(Y,piso),bottom_aux(X,Y).

%5.5. Auxiliar para bottom.
bottom_aux(X,X) :- onTop(X),on(X,piso).
bottom_aux(X,Y) :- on(X,Y),on(Y,piso).
bottom_aux(X,Y) :- on(X,Z),bottom_aux(Z,Y).

%6. Mueve una pila encima la otra de forma ordenada.
move_ordered(X,Y) :- bottom(X,Z),move_reversed(X,piso),move_reversed(Z,Y).

%7. Mueve una pila encima de otra, queda en reversa.
move_reversed(piso,_).
move_reversed(X,Y) :- on(X,Z),move(X,Y),move_reversed(Z,X).


%EVALUACION:
%move_reversed(a,d),move(g,piso),move(e,h),move(g,f),move_reversed(c,g),move_ordered(e,d).
%on(e,h),on(h,i),on(i,d),on(d,a),on(a,b),on(b,c),on(c,g),on(g,f),on(f,piso).
