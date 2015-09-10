%JOSÉ RICARDO RODRIGUEZ ABREU.

%Constante 'yo' del hombre.
%Constante 'mi_esposa' de su esposa viuda.
%Constante 'hijastra' para su hijastra.
%Constante 'mi_padre' de su padre viudo.
%Constante 'nueva_niña' de su nieta.
%Constante 'nuevo_niño' de su hijo.


esposos(yo,mi_esposa).                  %Casó con una viuda.
esposos(mi_padre,hijastra).             %Mi padre se caso con mi hijastra.

esposos(X,Y) :- padre_de(X,Z),padre_de(Y,Z).

padre_de(mi_esposa,hijastra).           %La viuda tenía una hija.
padre_de(hijastra,yo).                  %Yo soy hijastro de mi hijastra.
padre_de(hijastra,nueva_niña).          %Mi hija tiene una nueva niña
padre_de(mi_esposa,nuevo_niño).         %Mi mujer tuvo un niño.
padre_de(yo,hijastra).                  %Soy padre de mi hijastra-madre.
%DEFINICIÓN DE PADRE:
padre_de(X,Y) :- hijo_de(Y,X).

suegro_de(mi_esposa,mi_padre).          %Mi esposa es suegra de mi padre.
suegro_de(mi_padre,mi_esposa).          %Mi padre es suegro de mi esposa.
%DEFINICIÓN DE SUEGRO:
suegro_de(X,Y) :- padre_de(X,Z),esposos(Z,Y).

yerno_de(yo,mi_padre).                  %Mi padre es mi yerno
%DEFINICIÓN DE YERNO:
yerno_de(X,Y) :- padre_de(Y,Z),esposos(Z,X).

nuera_de(mi_esposa,hijastra).          %Mi mujer es nuera de su hija.
%DEFINICIÓN DE NUERA:
nuera_de(X,Y) :- padre_de(Y,Z),esposos(Z,X).

cuñado_de(nuevo_niño,mi_padre).        %Cuñado de mi padre.
%DEFINICIÓN DE CUÑADO:
cuñado_de(X,Y) :- (hermanos(X,Z),esposos(Z,Y)).
cuñado_de(X,Y) :- (hermanos(Y,Z),esposos(Z,X)). 

hermanos(yo,nueva_niña).               %Soy hermano de la nueva hija de mi papá.
hermanos(nuevo_niño,hijastra).         %Hermano de mi hija-madre.
%DEFINICIÓN DE HERMANOS:
hermanos(X,Y) :- padre_de(Z,X),padre_de(Z,Y).

abuelo_de(yo,nueva_niña).              %Soy abuelo de la nueva hija de mi hijastra.
abuelo_de(hijastra,nuevo_niño).        %Nieto de su hermana.
%DEFINICIÓN DE ABUELO:
abuelo_de(X,Y) :- padre_de(X,Z),padre_de(Z,Y).

tio_de(nuevo_niño,mi_padre).           %Mi tio.
%DEFINICIÓN DE TIO:
tio_de(X,Y) :- padre_de(Z,Y),hermanos(X,Z).


hijo_de(mi_esposa,yo).                %Soy padre de mi mujer.
hijo_de(mi_padre,yo).                 %Soy padre de mi padre.  
