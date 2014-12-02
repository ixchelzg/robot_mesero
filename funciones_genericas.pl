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

delete_all_ocurrences(X, [X|T], R):-delete_all_ocurrences(X, T, R). 
delete_all_ocurrences(X, [H|T], [H|R]):-delete_all_ocurrences(X, T, R).
delete_all_ocurrences(_, [], []).

slice([X|_],1,1,[X]).
slice([X|Xs],1,K,[X|Ys]) :- K > 1, 
   K1 is K - 1, slice(Xs,1,K1,Ys).
slice([_|Xs],I,K,Ys) :- I > 1, 
   I1 is I - 1, K1 is K - 1, slice(Xs,I1,K1,Ys).

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

fill([], _, 0).
fill([X|Xs], X, N) :- N0 is N-1, fill(Xs, X, N0).

findlen(Y,[],X):- X=0,!.    
findlen(Y,[X|Tail],Count):-
        (
          X=[Y,_],
          findlen(Y,Tail,Prev),
          Count = Prev + 1
        )
        ;findlen(Y,Tail,Count).

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
quieroProb(X,[],P,Time,Reward).
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
ubicacion_actual(Ub,W):-
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

probNreward_mover(Lugar,BD,Prob,Rew,Cost):-
  ubicacion_actual(Ub,BD),
  quieroLugar(Ub,BD,Uba),
  quieroClase(Lugar,BD,Lu),
  nth0(0,Lu,Nm),
  segundoTermino(Nm,X), /*saco el id del lugar al q me voy a mover*/
  quieroProbLugar(Uba,X,Prob,Cost,Rew).

probNreward_grab(Objeto,BD,Prob,Rew,Cost):-
  quieroClase(Objeto,BD,Obj),
  quieroProbAccionObjeto(Obj,buscar, P1,T1, R1),
  quieroProbAccionObjeto(Obj,agarrar, P2,T2, R2),
  Prob is P1+P2, Rew is R1+R2, Cost is T1+T2.

stuff_belongs_where(Stuff,Where,BD):-
   quieroClase(Stuff,BD,StuffClass),
   nth0(3,StuffClass,RelsStuff), member(ubicacion=>X,RelsStuff),
   quieroLugar(X,BD,WhereClass),
   nth0(2,WhereClass,PropsWhere),
   member(nombre=>Y, PropsWhere),
   Where = Y.

place_has_stuff(X,ToF):-
  rb(W),
  quieroClase(X,W,Lugar),
  extensionDeUnaClaseInicio(cosas,Y),
  nth0(0,Lugar,Id),
  segundoTermino(Id,IdLug),
  place_has_stuff_(Y,IdLug,W,ToF1),
  ToF = ToF1.

place_has_stuff_([],Place,W,ToF):- ToF = false,!.
place_has_stuff_([H|T],Place,W,ToF):-
  quieroClase(H,W,Obj),
  nth0(3,Obj,Rels),
  member(ubicacion=>X,Rels),
  (X == Place-> ToF = true,! ; 
  place_has_stuff_(T,Place,W,ToF)).

stuff_belongs(Obj,Lug,ToF):-
  rb(W),
  quieroClase(Obj,W,Objeto),
  nth0(3,Objeto,Rels),
  quieroClase(Lug,W,Lugar),
  nth0(0,Lugar,IdL),
  segundoTermino(IdL,IdLug),
  member(ubicacion=>X,Rels),
  (X == IdLug-> ToF = true,! ; ToF = false,! ).

is_member(X,Y,T):-
  ( member(X,Y) -> T=true ; T=false).

delete_one(_, [], []).
delete_one(Term, [Term|Tail], Tail):- !.
delete_one(Term, [Head|Tail], [Head|Result]) :-
  delete_one(Term, Tail, Result).

/* estado de brazos actuales del robot*/
% si R = falso, el robot no tiene brazos libres y por lo tnatno no puede agarrar%
brazos_robot(R,W):- 
	extensionDeUnaClaseInicio(robot,Y),
	nth0(0,Y,B1),nth0(1,Y,B2),
	quieroClase(B1,W,Brazo1),quieroClase(B2,W,Brazo2),
	nth0(3,Brazo1,Rb1),nth0(3,Brazo2,Rb2),
	nth0(2,Brazo1,Nm1),nth0(2,Brazo2,Nm2),
	nth0(0,Brazo1,Id1),nth0(0,Brazo2,Id2),
	segundoTermino(Id1,Id1_),segundoTermino(Id2,Id2_),
	member(nombre=>X1,Nm1), member(nombre=>X2, Nm2),
	member(agarro=>N1,Rb1),member(agarro=>N2,Rb2),
	( 	( N1 == Id1_ ),R = X1,! ; ( N2 == Id2_ ),R = X2,! ; R=false,!) .

/* estado de brazos actuales del robot*/
brazos_robot_actual(Obj,Brazo,W):-
	extensionDeUnaClaseInicio(robot,Y),
	nth0(0,Y,B1),nth0(1,Y,B2),
	quieroClase(B1,W,Brazo1),quieroClase(B2,W,Brazo2),
	nth0(3,Brazo1,Rb1),nth0(3,Brazo2,Rb2),
	nth0(2,Brazo1,Nm1),nth0(2,Brazo2,Nm2),
	length(Rb1,N1),length(Rb2,N2),
	(	dif(N1,0), member(agarro=>Obj,Rb1), member(nombre=>X1,Nm1), Brazo = X1,! ; 
		dif(N2,0), member(agarro=>Obj,Rb2), member(nombre=>X1,Nm2), Brazo = X1,! ;
		Brazo = false,!
	);
	Brazo = false,!.

/*agarrar objeto O, colocarlo en brazo X y, recibo BD actual en W, regreso la base modificada en BD*/
agarrar_objeto(O,X,W,BD):-
	modificaRelacion(ubicacion, O, X,W, BD1),
	modificaRelacion(agarro,X,O,BD1,BD).
	%anadeRelacion(X,[agarro=>O],BD1,BD).


/*-------------------------------Funciones del robot-----------------------------------------*/

/*
	Para saber si se realizo una accion o no
	accion_realizada(Probalidad,True/False)
*/
accion_realizada(P,T):-
	random(R),
	(R >= P)-> T=false; T=true.

/* Para mover al robot al lugar L, BD=New database, Time=Tiempo q lleva */
mover(L, BD, OldTime,Time,OldReward, Reward,W):-
	ubicacion_actual(Ub,W),
	quieroLugar(Ub,W,Uba),
	quieroClase(L,W,Lu),
	nth0(0,Lu,Nm),
	segundoTermino(Nm,X), /*saco el id del lugar al q me voy a mover*/
	quieroProbLugar(Uba,X,P, Tm, Rw),
	accion_realizada(P,T),
	Time is OldTime+Tm, 
	(T == true)-> 
		Reward is OldReward+Rw, modificaPropiedad(ubicacion, robot, X,W, BD) ; 
		write('no me pude mover :('),nl, BD = false,  Reward is OldReward.


/* Para buscar el objeto O, P = true or false/ lo hizo o no?, Time=Tiempo q lleva, Oldtime pa pasarle el viejo */
buscar(O,P, OldTime,Time, OldReward, Reward,W):-
	quieroClase(O,W,Obj),
	ubicacion_actual(Ub,W),
	ubicar_objeto(O,Ub),
	quieroProbAccionObjeto(Obj,buscar, Pro,Tm, Rw),
	Time is OldTime+Tm,
	accion_realizada(Pro,T),
	(
	T==true,(P=true, Reward is OldReward+Rw); 
	T==false,(P=false, Reward is OldReward)
	);
	write('no pude buscar, en este lugar no hay nada'),nl, P = false, Reward is OldReward, Time is 0.

/*colocar el objeto O en la mano libre del robot, si es q la tiene, BD modificada si realizo la accion. */
agarrar(O, BD, OldTime, Time,OldReward, Reward,W):-
	ubicacion_actual(Ub,W), 
	ubicar_objeto(O,Ub),
	quieroClase(O,W,Obj),
	brazos_robot(R,W),
	write(' --------- ::::::: '),write(R),nl,nl,
	quieroProbAccionObjeto(Obj,agarrar, P,Tm, Rw),Time is OldTime+Tm,
	dif(R,false),
	accion_realizada(P,T),
	(
	T == true,agarrar_objeto(O,R,W,BD), Reward is OldReward+Rw ;
	T == false, write('no pude agarrar :('),nl, Reward is OldReward, BD = false 
	);
	write('no pude agarrar :( tengo las manos ocupadas'),nl, BD = false, Reward is OldReward+0, Time is 0.

/*colocar el objeto O en el lugar en el que esta el robot, BD modificada si realizo la accion. */
colocar(O, BD, OldTime,Time,OldReward, Reward,W):-
	quieroClase(O,W,Obj),
	nth0(0,Obj,IDO),
	segundoTermino(IDO,Y),
	brazos_robot_actual(Y,Bra,W),
	dif(Bra,false),
	ubicacion_actual(Ub,W),
	quieroLugar(Ub,W,Lug),
	nth0(2,Lug,Props),
	member(nombre=>L,Props),
	quieroProbAccionObjeto(Obj,entregar, P,Tm, Rw),
	Time is OldTime+Tm,
	accion_realizada(P,T),
	(
	T == true,Reward is OldReward+Rw, modificaRelacion(ubicacion, O, L, W, BD1), modificaRelacion(agarro,Bra,Bra,BD1,BD);
	T == false,write('no pude colocar :('),nl, BD = false, Reward is OldReward
	);
	write('no puedo colocar, no tengo eso en las manos!'),nl, BD = false, Reward is OldReward, Time is 0.



ejecutar([],StartTime,Time,Reward,KW,RealDB):-write('fin :)'),!.
ejecutar([H],StartTime,Time,Reward,KW,RealDB):-
	nth0(0,H,Accion), StartTime1 is abs(StartTime), ScnTime is Time-StartTime1,
	nth0(1,H,CoL), write(' -> Accion: '),write(Accion), write(' => '), write(CoL), write(' , el tiempo que tenemos es: '),write(ScnTime),nl, 
	(
	ScnTime =< 0,
	write('se nos acabo el tiempo!'),nl,!
	;
	(Accion == buscar),
		buscar(CoL,P,StartTime,NT,Reward,NR,KW), NewTime is Time-NT, LeTime is NT-StartTime,
		write('Empezo en: '),write(StartTime),write(' y le tomo en buscar: '),write(LeTime),nl,
		write('Tiempo que queda: '),write(NewTime),nl,write('Recompensa:'),write(NR),nl,
		( 
			(P == true),ejecutar([],NT,Time,NR,KW,RealDB);
			(P == false),( (NT == 0), write(CoL),nl, ejecutar([],NT,Time,NR,KW,RealDB); 
			write(' voy a intentar buscar otra vez .. '),nl,ejecutar([H],NT,Time,NR,KW,RealDB) )
		)
	; 
	(Accion == mover),
		( (var(RealDB))-> DataBase = KW ; DataBase = RealDB ),
		mover(CoL, BD, StartTime,NT,Reward,NR,DataBase), NewTime is Time-NT, LeTime is NT-StartTime, 
		write('Empezo en: '),write(StartTime),write(' y le tomo en moverse: '),write(LeTime),nl,
		write('Tiempo que queda: '),write(NewTime),nl,write('Recompensa: '),write(NR),nl,
		( (BD == false),write(' voy a intentar moverme otra vez .. '),nl,ejecutar([H],NT,Time,NR,KW,RealDB); ejecutar([],NT,Time,NR,KW,BD) )
	;
	(Accion == agarrar),
		( (var(RealDB))-> DataBase = KW ; DataBase = RealDB ),
		agarrar(CoL,BD,StartTime,NT,Reward,NR,DataBase), NewTime is Time-NT,LeTime is NT-StartTime,
		write('Empezo en: '),write(StartTime),write(' y le tomo en agarrar: '),write(LeTime),nl,
		write('Tiempo que queda: '),write(NewTime),nl,write('Recompensa: '),write(NR),nl,
		( 
		  (BD == false),(NT == 0), write('no puedo agarrar el objeto de ahi'), write(CoL),nl, ejecutar([],NT,Time,NR,KW,RealDB); 
		  (BD == false),write(' voy a intentar agarrar otra vez .. '),nl,ejecutar([H],NT,Time,NR,KW,RealDB) ; 
		  ejecutar([],NT,Time,NR,KW,BD)
		)
	;
	(Accion == colocar),
		( (var(RealDB))-> DataBase = KW ; DataBase = RealDB ),
		colocar(CoL,BD,StartTime,NT,Reward,NR,DataBase), NewTime is Time-NT,LeTime is NT-StartTime, 
		write('Empezo en: '),write(StartTime),write(' y le tomo en colocar: '),write(LeTime),nl,
		write('Tiempo que queda: '),write(NewTime),nl,write('Recompensa: '),write(NR),nl,
		( (BD == false),write(' voy a intentar colocar otra vez .. '),nl,ejecutar([H],NT,Time,NR,KW,RealDB); ejecutar([],NT,Time,NR,KW,BD) )
	; 
	write('algo salio mal :( '),nl,!
	)
	.

ejecutar([H|T],StartTime,Time,Reward,KW,RealDB):-
	nth0(0,H,Accion), StartTime1 is abs(StartTime), ScnTime is Time-StartTime1,
	nth0(1,H,CoL), write(' -> Accion: '),write(Accion), write(' => '), write(CoL), write(' , el tiempo que tenemos es: '),write(ScnTime),nl, 
	(
	ScnTime =< 0,
	write('se nos acabo el tiempo!'),nl,!
	;
	(Accion == buscar),
		buscar(CoL,P,StartTime,NT,Reward,NR,KW), NewTime is Time-NT,LeTime is NT-StartTime,
		write('Empezo en:'),write(StartTime),write(' y le tomo en buscar:'),write(LeTime),nl,
		write('Tiempo que queda: '),write(NewTime),nl,write('Recompensa: '),write(NR),nl,
		( 
			(P == true), ejecutar(T,NT,Time,NR,KW,RealDB);
			(P == false),(NT == 0), write('no puedo buscar el objeto ahi :('), write(CoL),nl, ejecutar([],NT,NewTime,NR,KW,RealDB); 
			(P == false),write(' voy a intentar buscar otra vez .. '),nl,ejecutar([H|T],NT,Time,NR,KW,RealDB)
		)
	; 
	(Accion == mover),
		( (var(RealDB))-> DataBase = KW ; DataBase = RealDB ),
		mover(CoL, BD, StartTime,NT,Reward,NR,DataBase), NewTime is Time-NT, LeTime is NT-StartTime,
		write('Empezo en: '),write(StartTime),write(' y le tomo en moverse: '),write(LeTime),nl,
		write('Tiempo que queda: '),write(NewTime),nl,write('Recompensa: '),write(NR),nl,
		( (BD == false),write(' voy a intentar moverme otra vez .. '),nl,ejecutar([H|T],NT,Time,NR,KW,RealDB); ejecutar(T,NT,Time,NR,KW,BD) )
	;
	(Accion == agarrar),
		( (var(RealDB))-> DataBase = KW ; DataBase = RealDB ),
		agarrar(CoL,BD,StartTime,NT,Reward,NR,DataBase), NewTime is Time-NT,LeTime is NT-StartTime,
		write('Empezo en:'),write(StartTime),write(' y le tomo en agarrar:'),write(LeTime),nl,
		write('Tiempo que queda:'),write(NewTime),nl,write('Recompensa: '),write(NR),nl,
		( 
		  (BD == false),(NT == 0), ejecutar([],NT,Time,NR,KW,RealDB); 
		  (BD == false),write(' voy a intentar agarrar otra vez .. '),nl,ejecutar([H|T],NT,Time,NR,KW,RealDB) ; 
		  ejecutar(T,NT,Time,NR,KW,BD)
		)
	;
	(Accion == colocar),
		( (var(RealDB))-> DataBase = KW ; DataBase = RealDB ),
		colocar(CoL,BD,StartTime,NT,Reward,NR,DataBase), NewTime is Time-NT,LeTime is NT-StartTime,
		write('Empezo en: '),write(StartTime),write(' y le tomo en colocar: '),write(LeTime),nl,
		write('Tiempo que queda: '),write(NewTime),nl,write('Recompensa: '),write(NR),nl,
		( 
			(BD == false),write(' voy a intentar colocar otra vez .. '),nl,ejecutar([H|T],NT,Time,NR,KW,RealDB); 
			ejecutar(T,NT,Time,NR,KW,BD) 
		)
	; 
	write('algo salio mal :( '),nl,fail,!
	)
	.
	