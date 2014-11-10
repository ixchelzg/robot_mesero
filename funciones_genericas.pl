:- initialization(main).
:- op(15, xfx, '=>').
=>(X,Y).

main:- consult('main.pl').
	
rb(Y):- open_kb('bd.txt',Y).
guardarBD(Y):- save_kb('bd.txt',Y).

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

quieroClase(X,[H|T],P):-
	nth0(2,H,Props),
	cabeza(Props,S),
	segundoTermino(S,X)->
		P=H ; quieroClase(X,T,P).


quieroLugar(X,[H|T],P):-
	nth0(0,H,Name),
	segundoTermino(Name,X)->
		P=H ; quieroLugar(X,T,P).


/*regresa la probabilidad P de moverse a un lugar, desde otro lugar X, buscando en una lista de lugares [H|T]*/
quieroProb(X,[H|T],P):-
	member(lugar=>X,H), member(probabilidad=>Pr,H)->
		P=Pr ; quieroProb(X,T,P).

/*quieroProbLugar(Clase/Objeto completo de lugar en que estoy, id Lugar al que me voy a mover, Probabilidad)*/
quieroProbLugar(X,Y,P):-
	nth0(3,X,Mov),
	member(movimiento=>Ls,Mov),
	quieroProb(Y,Ls,P).

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

/*ubicacion actual del robot*/
ubicacion_actual(Ub):-
	rb(W), 
	quieroClase(robot,W,R),
	nth0(3,R,U),
	member(ubicacion=>X,U),
	Ub=X.

/*modificaPropiedad(Propiedad, Clase/Objeto, Valor Nuevo)*/
modificaPropiedad(P,X,P1):-
	rb(W), 
	quieroClase(X,W,Cl),
	nth0(3,Cl,Props),
	member(P=>Xv,Props),
	sus(P=>Xv,P=>P1,Props,S), sus(Props,S,Cl,SC), sus(Cl,SC,W,S1),
	guardarBD(S1).


/*-------------------------------Funciones del robot-----------------------------------------*/

/*
	Para saber si se realizo una accion o no
	accion_realizada(Probalidad,True/False)
*/
accion_realizada(P,T):-
	random(R),
	(R >= P)-> T=false; T=true.

/*
	Para mover al robot al lugar L 
*/
mover(L):-
	rb(W), 
	ubicacion_actual(Ub),
	quieroLugar(Ub,W,Uba),
	quieroClase(L,W,Lu),
	nth0(0,Lu,Nm),w
	segundoTermino(Nm,X), /*saco el id del lugar al q me voy a mover*/
	quieroProbLugar(Uba,X,P),
	accion_realizada(P,T),
	(T == true)-> modificaPropiedad(ubicacion, robot, X) ; write('no me pude mover :('), fail.

