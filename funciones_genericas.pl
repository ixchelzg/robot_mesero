:- initialization(main).
:- op(15, xfx, '=>').
=>(X,Y).

main:- consult('main.pl'),consult('consultas.pl').

/* -- Funcion para leer estado de bd -- */
rb(Y):- open_kb('bd.txt',Y).

/* -- Funcion para guardar el cambio de estado en bd -- */
commit(BD):- save_kb('bd.txt',BD).

% Regresan ya sea el lado izquierdo o derecho del operador (siempre y cuando estos sean átomos).
primerTermino(X,Y) :- X = =>(Y,_).
segundoTermino(X,Y):- X = =>(_,Y).

% Busca en una lista de atributos si alguno coincide con el pedido y regresa su valor en caso de que sea así.
valor(_,[],_):- fail.
valor(X,[H|_],Y):- primerTermino(H,X), segundoTermino(H,Y), !.
valor(X,[H|T],Y):- primerTermino(H,Z), X\=Z, valor(X,T,Y).

cabeza([H|_],Y):- Y=H.
cabeza(X,Y):- Y=X.
cola([_|T],Y):- Y=T.
cola(X,Y):- Y=X.

sus(_,_,[],[]).
sus(X,Y,[X|T],[Y|S]):-!,sus(X,Y,T,S).
sus(X,Y,[Z|T],[Z|S]):-sus(X,Y,T,S).

eliminaClase(X,[X|T],T).
eliminaClase(X,[H|T],[H|T1]):-
	eliminaClase(X,T,T1).

concatenar([],L,L).
concatenar([X|L1],L2,[X|L3]):-concatenar(L1,L2,L3).

set([],[]).
set([H|T],[H|Out]) :-
	primerTermino(H,Y),
	Cp= Y=>_,
	not(member(Cp,T)),
    set(T,Out).
set([H|T],Out) :-
	primerTermino(H,Y),
	Cp= Y=>_,
    member(Cp,T),
    set(T,Out).

/*regresa una clase X , q busca en BD=[H|T], en P*/
quieroClase(X,[H|T],P):-
	nth0(2,H,Props),
	cabeza(Props,S),
	segundoTermino(S,X)->
		P=H ; quieroClase(X,T,P).

/* id de un objeto, regresa el objeto. */
quieroLugar(X,[H|T],P):-
	nth0(0,H,Name),
	segundoTermino(Name,X)->
		P=H ; quieroLugar(X,T,P).

/*regresa la probabilidad P de moverse a un lugar, hacia lugar X, buscando en una lista de lugares [H|T]*/
quieroProb(X,[H|T],P):-
	member(lugar=>X,H), member(probabilidad=>Pr,H)->
		P=Pr ; quieroProb(X,T,P).

/*quieroProbLugar(Clase/Objeto completo de lugar en que estoy, id Lugar al que me voy a mover, Probabilidad)*/
quieroProbLugar(X,Y,P):-
	nth0(3,X,Mov),
	member(movimiento=>Ls,Mov),
	quieroProb(Y,Ls,P).


/*regresa la probabilidad P de realizar la accion X buscando en una lista de acciones q le pasas [H|T]*/
quieroProbAccion(X,[H|T],P):-
	member(nombre=>X,H), member(probabilidad=>Pr,H)->
		P=Pr ; quieroProbAccion(X,T,P).

/*quieroProbLugar(Clase/Objeto completo, accion, ej.: buscar/ agarrar, Probabilidad)*/
quieroProbAccionObjeto(Ob,Ac,P):-
	nth0(2,Ob,Props),
	nth0(1,Props,Acs),
	member(acciones=>As, Acs),
	quieroProbAccion(Ac,As,P).

/*ubicacion actual del robot*/
ubicacion_actual(Ub):-
	rb(W), 
	quieroClase(robot,W,R),
	nth0(3,R,U),
	member(ubicacion=>X,U),
	Ub=X.

/*ubicacion del objeto Nm = nombre dle objeto, Ub, id de la ubicacion del objeto */
ubicar_objeto(Nm,Ub):-
	rb(W), 
	quieroClase(Nm,W,R),
	nth0(3,R,Rels),
	member(ubicacion=>X,Rels),
	Ub=X.

/* estado de brazos actuales del robot*/
brazos_robot(X):-
	rb(W), 
	extensionDeUnaClase(robot,Y),
	nth0(0,Y,B1),nth0(1,Y,B2),
	quieroClase(B1,W,Brazo1),quieroClase(B2,W,Brazo2),
	nth0(3,Brazo1,Rb1),nth0(3,Y,Rb2),
	length(Rb1,N1),length(Rb2,N2),
	(N1 == 0 ; N2 ==0)-> X=true; X=false.

/*modificaPropiedad(Propiedad, Clase/Objeto, Valor Nuevo, Regresa la BD con la prop modificada en BD)*/
modificaPropiedad(P,X,P1,BD):-
	rb(W), 
	quieroClase(X,W,Cl),
	nth0(3,Cl,Props),
	member(P=>Xv,Props),
	sus(P=>Xv,P=>P1,Props,S), sus(Props,S,Cl,SC), sus(Cl,SC,W,BD).


/*-------------------------------Funciones del robot-----------------------------------------*/

/*
	Para saber si se realizo una accion o no
	accion_realizada(Probalidad,True/False)
*/
accion_realizada(P,T):-
	random(R),
	(R >= P)-> T=false; T=true.

/* Para mover al robot al lugar L */
mover(L, BD):-
	rb(W), 
	ubicacion_actual(Ub),
	quieroLugar(Ub,W,Uba),
	quieroClase(L,W,Lu),
	nth0(0,Lu,Nm),
	segundoTermino(Nm,X), /*saco el id del lugar al q me voy a mover*/
	quieroProbLugar(Uba,X,P),
	accion_realizada(P,T),
	(T == true)-> modificaPropiedad(ubicacion, robot, X, BD) ; write('no me pude mover :('), fail.


/* Para mover al robot al lugar L */
buscar(O,P):-
	rb(W), 
	ubicacion_actual(Ub),
	ubicar_objeto(O,Ub),
	quieroClase(O,W,Obj),
	quieroProbAccionObjeto(Obj,buscar,P),
	accion_realizada(P,T),
	(T == true)-> P = true ; P=false.

