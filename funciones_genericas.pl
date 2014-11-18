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
quieroProb(X,[H|T],P, Time, Reward):-
	member(lugar=>X,H), member(probabilidad=>Pr,H), member(costo=>Tm,H), member(recompensa=>Rw,H) ->
		P=Pr, Time=Tm,  Reward = Rw; quieroProb(X,T,P,Time, Reward).

/*quieroProbLugar(Clase/Objeto completo de lugar en que estoy, id Lugar al que me voy a mover, Probabilidad)*/
quieroProbLugar(X,Y,P,Time, Reward):-
	nth0(3,X,Mov),
	member(movimiento=>Ls,Mov),
	quieroProb(Y,Ls,P,Time, Reward).


/*regresa la probabilidad P de realizar la accion X buscando en una lista de acciones q le pasas [H|T]*/
quieroProbAccion(X,[H|T],P, Time, Reward):-
	member(nombre=>X,H), member(probabilidad=>Pr,H), member(costo=>Tm,H), member(recompensa=>Rw,H)->
		P=Pr, Time = Tm, Reward = Rw ; quieroProbAccion(X,T,P, Time, Reward).

/*quieroProbAccionObjeto(Clase/Objeto completo, accion, ej.: buscar/ agarrar, Probabilidad, Tiempo q toma, Reward q da)*/
quieroProbAccionObjeto(Ob,Ac,P, Time, Reward):-
	nth0(2,Ob,Props),
	member(acciones=>Acs,Props),
	quieroProbAccion(Ac,Acs,P, Time, Reward).

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
brazos_robot(R):-
	rb(W), 
	extensionDeUnaClaseInicio(robot,Y),
	nth0(0,Y,B1),nth0(1,Y,B2),
	quieroClase(B1,W,Brazo1),quieroClase(B2,W,Brazo2),
	nth0(3,Brazo1,Rb1),nth0(3,Brazo2,Rb2),
	nth0(2,Brazo1,Nm1),nth0(2,Brazo2,Nm2),
	member(nombre=>X1,Nm1), member(nombre=>X2, Nm2),
	length(Rb1,N1),length(Rb2,N2),
	( 	(N1 == 0 ),R = X1; (N2 == 0),R = X2
	); R=false
	.


/* estado de brazos actuales del robot*/
brazos_robot_actual(Obj,Brazo):-
	rb(W), 
	extensionDeUnaClaseInicio(robot,Y),
	nth0(0,Y,B1),nth0(1,Y,B2),
	quieroClase(B1,W,Brazo1),quieroClase(B2,W,Brazo2),
	nth0(3,Brazo1,Rb1),nth0(3,Brazo2,Rb2),
	nth0(2,Brazo1,Nm1),nth0(2,Brazo2,Nm2),
	length(Rb1,N1),length(Rb2,N2),
	(	dif(N1,0), member(agarro=>Obj,Rb1), member(nombre=>X1,Nm1), Brazo = X1 ; 
		dif(N2,0), member(agarro=>Obj,Rb2), member(nombre=>X1,Nm2), Brazo = X1 ;
		Brazo = false
	);
	Brazo = false .


/*agarrar objeto O, colocarlo en brazo X y, recibo BD actual en W, regreso la base modificada en BD*/
agarrar_objeto(O,X,W,BD):-
	modificaRelacion(ubicacion, O, X,W, BD1),
	anadeRelacion(X,[agarro=>O],BD1,BD).

/*-------------------------------Funciones del robot-----------------------------------------*/

/*
	Para saber si se realizo una accion o no
	accion_realizada(Probalidad,True/False)
*/
accion_realizada(P,T):-
	random(R),
	(R >= P)-> T=false; T=true.

/* Para mover al robot al lugar L, BD=New database, Time=Tiempo q lleva */
mover(L, BD, OldTime,Time,OldReward, Reward):-
	rb(W), 
	ubicacion_actual(Ub),
	quieroLugar(Ub,W,Uba),
	quieroClase(L,W,Lu),
	nth0(0,Lu,Nm),
	segundoTermino(Nm,X), /*saco el id del lugar al q me voy a mover*/
	quieroProbLugar(Uba,X,P, Tm, Rw),
	accion_realizada(P,T),
	Time is OldTime+Tm, 
	(T == true)-> 
		Reward is OldReward+Rw, modificaPropiedad(ubicacion, robot, X, BD) ; 
		write('no me pude mover :('), BD = false,  Reward is OldReward.


/* Para buscar el objeto O, P = true or false/ lo hizo o no?, Time=Tiempo q lleva, Oldtime pa pasarle el viejo */
buscar(O,P, OldTime,Time, OldReward, Reward):-
	rb(W), 
	quieroClase(O,W,Obj),
	ubicacion_actual(Ub),
	ubicar_objeto(O,Ub),
	quieroProbAccionObjeto(Obj,buscar, Pro,Tm, Rw),
	Time is OldTime+Tm,
	accion_realizada(Pro,T),
	(T == true)-> 
		( P=true, Reward is OldReward+Rw ;
		P=false,  Reward is OldReward )
	;
	write('no pude buscar, en este lugar no hay nada'), P = false, Reward is OldReward, Time is 0.

/*colocar el objeto O en la mano libre del robot, si es q la tiene, BD modificada si realizo la accion. */
agarrar(O, BD, OldTime, Time,OldReward, Reward):-
	rb(W),
	ubicacion_actual(Ub), 
	ubicar_objeto(O,Ub),
	quieroClase(O,W,Obj),
	brazos_robot(R),
	quieroProbAccionObjeto(Obj,agarrar, P,Tm, Rw),Time is OldTime+Tm,
	dif(R,false),
	accion_realizada(P,T),
	((T == true) -> agarrar_objeto(O,R,W,BD), Reward is OldReward+Rw ; write('no pude agarrar :('), Reward is OldReward, BD = false)
	;
	write('no pude agarrar :( tengo las manos ocupadas'), BD = false, Reward is OldReward+0, Time is 0.

/*colocar el objeto O en el lugar en el que esta el robot, BD modificada si realizo la accion. */
colocar(O, BD, OldTime,Time,OldReward, Reward):-
	rb(W),
	quieroClase(O,W,Obj),
	nth0(0,Obj,IDO),
	segundoTermino(IDO,Y),
	brazos_robot_actual(Y,Bra),
	dif(Bra,false),
	ubicacion_actual(Ub),
	quieroLugar(Ub,W,Lug),
	nth0(2,Lug,Props),
	member(nombre=>L,Props),
	quieroProbAccionObjeto(Obj,entregar, P,Tm, Rw),
	Time is OldTime+Tm,
	accion_realizada(P,T),
	(
		(T == true)->
		Reward is OldReward+Rw, modificaRelacion(ubicacion, O, L, W, BD1), eliminaRelacion(agarro, Bra, BD1, BD);
		write('no pude colocar :('), BD = false, Reward is OldReward
	);
	write('no puedo colocar, no tengo eso en las manos'), BD = false, Reward is OldReward, Time is 0.



ejecutar([],_,_,_):-!.
ejecutar([H],StartTime,Time,Reward):-
	nth0(0,H,Accion),
	nth0(1,H,CoL), write(Accion),nl, write(CoL),nl,
	(
	(Accion == buscar),
		buscar(CoL,P,StartTime,NT,Reward,NR), NewTime is Time-NT,
		( 
			(P == true),ejecutar([],NT,NewTime,NR);
			(P == false),( (NT == 0), (NR == 0), write(CoL),nl, ejecutar([],NT,NewTime,NR); 
			ejecutar([H],NT,NewTime,NR) )
		)
	; 
	(Accion == mover),
		mover(CoL, BD, StartTime,NT,Reward,NR), NewTime is Time-NT,
		( (BD == false),ejecutar([H],NT,NewTime,NR); commit(BD),ejecutar([],NT,NewTime,NR) )
	;
	(Accion == agarrar),
		agarrar(CoL,BD,StartTime,NT,Reward,NR), NewTime is Time-NT,
		( (BD == false),ejecutar([H],NT,NewTime,NR) ; 
		  (BD == false),(NT == 0),(NR == 0), write('no puedo agarrar el objeto de ahi'), write(CoL),nl, ejecutar([],NT,NewTime,NR); 
		  commit(BD) ,ejecutar([],NT,NewTime,NR)
		)
	;
	(Accion == colocar),
		colocar(CoL,BD,StartTime,NT,Reward,NR), NewTime is Time-NT,
		( (BD == false),ejecutar([H],NT,NewTime,NR); commit(BD),ejecutar([],NT,NewTime,NR) )
	; 
	(Time == 0),write('ya no queda tiempo!'),nl,fail
	;
	write('algo salio mal :( '),nl,fail
	)
	.

ejecutar([H|T],StartTime,Time,Reward):-
	nth0(0,H,Accion),
	nth0(1,H,CoL), write(Accion),nl, write(CoL),nl,
	(
	(Accion == buscar),
		buscar(CoL,P,StartTime,NT,Reward,NR), NewTime is Time-NT,
		( 
			(P == true), ejecutar(T,NT,NewTime,NR);
			(P == false),(NT == 0), (NR == 0), write('no puedo buscar el objeto ahi'), write(CoL),nl, ejecutar([],NT,NewTime,NR); 
			(P == false),ejecutar([H|T],NT,NewTime,NR)
		)
	; 
	(Accion == mover),
		mover(CoL, BD, StartTime,NT,Reward,NR), NewTime is Time-NT,
		( (BD == false),ejecutar([H],NT,NewTime,NR); commit(BD),ejecutar(T,NT,NewTime,NR) )
	;
	(Accion == agarrar),
		agarrar(CoL,BD,StartTime,NT,Reward,NR), NewTime is Time-NT,
		( (BD == false),ejecutar([H|T],NT,NewTime,NR) ; 
		  (BD == false),(NT == 0),(NR == 0), write(CoL),nl, ejecutar([],NT,NewTime,NR); 
		  commit(BD) ,ejecutar(T,NT,NewTime,NR)
		)
	;
	(Accion == colocar),
		colocar(CoL,BD,StartTime,NT,Reward,NR), NewTime is Time-NT,
		( (BD == false),ejecutar([H|T],NT,NewTime,NR); commit(BD),ejecutar(T,NT,NewTime,NR) )
	; 
	(Time == 0),write('ya no queda tiempo!'),nl,fail
	;
	write('algo salio mal :( '),nl,fail
	)
	.