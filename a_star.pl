:- initialization(main).
:- op(15, xfx, '=>').
=>(X,Y).

main:- consult('main.pl'),consult('consultas.pl'),consult('funciones_genericas.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Implementacion de A* %%
%%  para el robot        %%
%%  mesero.              %%
%%  IIMAS, UNAM, 2014    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-- W es la base de datos, la lista del inicio es lo q se quiere q haga el robot y L regresa la lista de acciones totales
allnodes([],W,L):- L=[],!.

allnodes([H|T],W,L):-
  nth0(0,H,Objeto),
  nth0(1,H,Destino),
  quieroClase(Objeto,W,ObjClass),
  nth0(3,ObjClass,RelsObj),
  member(ubicacion=>IdUbObj,RelsObj),
  quieroLugar(IdUbObj,W,UbObjClass),
  nth0(2,UbObjClass,PropsUbObj),
  member(nombre=>UbName,PropsUbObj),
  NewList = [ [mover,UbName],[grab,Objeto],[mover,Destino],[colocar,Objeto] ],
  allnodes(T,W,NewList1),
  append(NewList1,NewList,L),!.


stufflist([],L):- L=[],!.
stufflist([H|T],L):-
  nth0(0,H,Objeto),
  nth0(1,H,Destino),
  stufflist(T,NewList1),
  append([Objeto],NewList1,L),!.

clean_([],L):- L=[],!.
clean_([H|T],L):-
  H=[X,Y],
  delete_all_ocurrences(H,T,L1),
  clean_(L1,L2),
  append([H],L2,L),!.

% clean_plan(ThePlan__,Reward__,MeanestPlan,MeanestReward),
clean_plan([],[],[],MeanestPlan,MeanestReward,MeanestTime):- MeanestReward=[],MeanestTime=[], MeanestPlan=[],!.
clean_plan([H|T],[H1|T1],[H2|T2],MeanestPlan,MeanestReward,MeanestTime):-
  length(T,Len),
  ( (Len>=2)-> 
    (nth0(1,T,MH),
    ((MH == H)-> 
      ( delete_one(H,T,L1),
        nth0(1,T1,MH1),nth0(1,T2,MH2),
        delete_one(MH1,T1,L11), delete_one(MH2,T2,L12),
        Cola1 = L1, Cola2 = L11, Cola3 = L12 );
      ( Cola1 = T, Cola2 = T1,Cola3 = T2 )
    ),
    clean_plan(Cola1,Cola2,Cola3,MeanestPlan1,MeanestR,MeanestT),
    append([H],MeanestPlan1,MeanestPlan),append([H2],MeanestT,MeanestTime),
    append([H1],MeanestR,MeanestReward),!)
    ;
    (clean_plan(T,T1,T2,MeanestPlan1,MeanestR,MeanestT),
    append([H],MeanestPlan1,MeanestPlan),append([H2],MeanestT,MeanestTime),
    append([H1],MeanestR,MeanestReward),!)
  ).

start_plan(L,W,CTime,Time,Reward,ThePlan):-
  allnodes(L,W,Plan),
  clean_(Plan,CleanPlan),
  length(Plan,Len),
  Len1 is Len-1,
  fill(List,a,Len1),
  find_best_first(CleanPlan, Bests),
  stufflist(L,StuffList),
  find_best_first_(Bests,BestCandidates,Heus,OldReward,CTimes,StuffList,W),
  last(BestCandidates,BestCandidate), last(Heus,Heu), last(CTimes,BestTime),
  BestCandidate = [XXX,YYY],
  ( (XXX == mover)-> mover(YYY,DataBase,W) ; DataBase = W ),
  %, (DataBase1 == false-> DataBase=W ; DataBase=DataBase1)%
  %write(' -- >>> DataBase  ... '),nl,nl, write(DataBase),nl,nl,nl,
  delete_one(BestCandidate,Plan,CleanPlan_),
  delete_one(BestCandidate,CleanPlan,CleanPlan_1),
  planned_(List,CleanPlan_,CleanPlan_,NodesLeft,ThePlan_,BestCandidate,Current1,Rewards, CTime_,  ALLTIMES, Time,DataBase,NewBD),
  %expand(CleanPlan_,BestCandidate,LastTime,Time,Heuristic,NewCandidate,W),
  %CTime__ is CTime_+BestTime,  
  append([[BestCandidate]|[ThePlan_]],ThePlan__),
  append([Heu],Rewards,Reward__),
  append([BestTime],ALLTIMES,ALLTIMES1),
  clean_plan(ThePlan__,Reward__,ALLTIMES1,MeanestPlan,MeanestReward,MeanestTime),
  sumlist(MeanestTime,ATimes),
  ( (ATimes > Time)-> reverse(MeanestTime,ListAT), deleterange(ListAT,Time,NewListTimes); reverse(MeanestTime,NewListTimes) ),
  length(NewListTimes,LENNLT),
  slice(MeanestPlan,1,LENNLT,ThePlan),
  slice(MeanestReward,1,LENNLT,Reward),
  sumlist(Reward,Rewardss),
  write(' FIRST OPT :: '), write(ThePlan__),nl,
  write('BESTPLAN :: '), write(ThePlan),nl,
  write(' Rewardsss :: '), write(Reward__),nl,
  write(' Reward :: '), write(Rewardss),nl,!.

deleterange([],Time,L) :- L=[],!.
deleterange([H|T],Time,L) :- sumlist([H|T],X), X > Time, deleterange(T,Time,L).
deleterange([H|T],Time,L) :- sumlist([H|T],X), X =< Time, deleterange(T,Time,D), L=[H|D].

planned_([], AllNodesEv, AllNodes, NodesLeft, Plan, Current, Current1, Heuristics, TimeLeft, ALLTIMES, Time,BD,NewBD):- write('done planning .. '),Plan=[],Heuristics=[],ALLTIMES=[],!.
planned_([H|T],AllNodesEv, AllNodes, NodesLeft, Plan, Current, Current1, Heuristics,TimeLeft,  ALLTIMES, Time,BD,NewBD):-
  ( var(NodesLeft)-> LosNodos = AllNodes ; LosNodos = NodesLeft ),
  ( var(NewBD)-> DataBase = BD ; DataBase = NewBD ),
  ( var(Current1) -> 
    expand(AllNodesEv,LosNodos,Current, LastTime, Time, Heuristic, NewCandidate,NL1,DataBase,NewBD1) ; 
    expand(AllNodesEv,LosNodos,Current1, LastTime, Time, Heuristic, NewCandidate,NL1,DataBase,NewBD1)
  ),
  Current12 = NewCandidate, 
  planned_( T, AllNodesEv,AllNodes, NL1, Plan1,Current, Current12,Heuristics1, TimeLeft1,  ALLTIMES1,Time,BD,NewBD1),
  %TimeLeft is TimeLeft1+LastTime,
  append([LastTime],ALLTIMES1,ALLTIMES),
  append([NewCandidate],Plan1,Plan),
  append([Heuristic],Heuristics1,Heuristics),!.

expand(AllTheNodes,AllNodes,Current,CTime,Time,Heuristic,NewCandidate,NodesLeft,BD,NewBD):-
  nth0(0,Current,Accion), nth0(1,Current,OoL),
  (
    ( 
      Accion == mover, place_has_stuff(OoL,ToF), 
      ( (ToF == true)-> 
          find_all_mover(OoL,AllNodes,Candidates), candidate(AllTheNodes,Candidates,CTimes,Time,Heus,OldHeu,BestCandidate,BD,NewstBD),
          last(BestCandidate,LBC), last(Heus,LBH), last(CTimes,LBT), last(NewstBD,NewNewBD)
          ;
          find_all_mover_col(AllTheNodes,OoL,AllNodes,Candidates), candidate(AllTheNodes,Candidates,CTimes,Time,Heus,OldHeu,BestCandidate,BD,NewstBD),
          last(BestCandidate,LBC), last(Heus,LBH),last(CTimes,LBT), last(NewstBD,NewNewBD)
      )
    );
    (
      Accion == grab, 
      find_all_grab(OoL,AllNodes,Candidates,BD),candidate(AllTheNodes,Candidates,CTimes,Time,Heus,OldHeu,BestCandidate,BD,NewstBD),
      last(BestCandidate,LBC), last(Heus,LBH),last(CTimes,LBT), last(NewstBD,NewNewBD)
    );
    (
      Accion == colocar, find_all_colocar(OoL,AllNodes,Candidates),candidate(AllTheNodes,Candidates,CTimes,Time,Heus,OldHeu,BestCandidate,BD,NewstBD),
          last(BestCandidate,LBC), last(Heus,LBH),last(CTimes,LBT), last(NewstBD,NewNewBD)
    )
  ),
  %write('Candidatos para la siguiente accion : : : : : : : : : '),write(Candidates),nl,nl,
  delete_one(LBC,AllNodes,NodesLeft),
  ( NewNewBD == false-> NewBD = BD ; NewBD = NewNewBD),
  NewCandidate = LBC, CTime = LBT, Heuristic = LBH,!.

find_all_colocar(Obj,[],PosArray):- PosArray = [],!.
find_all_colocar(Obj,[H|T],PosArray):-
  (
    (
      H = [mover,X], find_all_colocar(Obj,T,PosArray1),append([H],PosArray1,PosArray),! 
    )
    ;
    ( find_all_colocar(Obj,T,PosArray) )
  ).

find_all_grab(Obj,[],PosArray,BD):- PosArray = [],!.
find_all_grab(Obj,[H|T],PosArray,BD):-
  (
    ( 
      H = [mover,X], 
      find_all_grab(Obj,T,PosArray1,BD),append([H],PosArray1,PosArray),!
    )
    ;
    ( H = [grab,X],
      brazos_robot(R,BD),
      ( (R == false)->
          (
            find_all_grab(Obj,T,PosArray,BD)
          )
          ;
          ( stuff_belongs_where(Obj,Place1,BD),
            stuff_belongs_where(X,Place2,BD),
            ( 
              (Place1 == Place2, dif(Obj,X) )-> 
              find_all_grab(Obj,T,PosArray1,BD),append([H],PosArray1,PosArray),! ; 
              find_all_grab(Obj,T,PosArray,BD) 
            )
          )
      )
    )
    ;
    (
      find_all_grab(Obj,T,PosArray,BD)
    )
  ).

find_best_first([],Bests):- Bests=[],!.
find_best_first([H|T],Bests):-
  H=[mover,X],
  place_has_stuff(X,Test),
  ( 
    ( Test == true)-> 
      (
        find_best_first(T,Bests1), append([H],Bests1,Bests),!
      )
      ;
      (
        find_best_first(T,Bests)
      )
  )
  ;
  find_best_first(T,Bests).


find_all_mover(Place,[],PosArray):- PosArray = [],!.
find_all_mover(Place,[H|T],PosArray):-
  (
    (
      H = [grab,X],
      stuff_belongs(X,Place,Test),
      ( 
        (Test == true ) ->  find_all_mover(Place,T,PosArray1),append([H],PosArray1,PosArray),! ; find_all_mover(Place,T,PosArray)
      )
    )
    ;
    (
      H = [colocar,X],
      place_has_stuff(Place,Test),
      ( 
        (Test == true ) -> find_all_mover(Place,T,PosArray) ; find_all_mover(Place,T,PosArray1),append([H],PosArray1,PosArray),! 
      )
    )
    ;
    (
      find_all_mover(Place,T,PosArray)
    )
  ).

% tiene sentido elegir esta accion de moverme , le paso el lugar al q me quiero mover %
% y si no estoy agarrarndo nada, no importa, si me puedo mover, pero si estoy agarrando algo, %
% debo checar si moverme a una mesa tiene sentido, con respecto a lo q estoy agarrarndo %
makes_sense_to_move(Place,AllTheNodes,DB,NoY):-
  nextto([mover,Place],[XXX,YYY],AllTheNodes),
  %ubicacion_actual(Ub,DB),
  %quieroLugar(Ub,DB,Lug),
  %nth0(2,Lug,Props),
  %member(nombre=>L_,Props),
  %( (L_ == Place)->  NoY = false,! ; true ),
  brazos_robot_agarrando(Cosa1,Cosa2,DB),
  ( XXX == colocar -> 
      is_this_nextto(AllTheNodes,Cosa1,Place,Test1),is_this_nextto(AllTheNodes,Cosa2,Place,Test2),
      (( Test1 == true )-> NoY = true,! ; ( ( Test2 == true ) -> NoY = true,! ; NoY = false,! ) )
      ;
      ( (Cosa1 == false)-> NoY = true,!; ( (Cosa2 == false)-> NoY = true,! ; NoY = false,! ) )
  ).

is_this_nextto(Nodes,Cosa,Place,Test):-
  nextto([mover,Place],[colocar,Cosa],Nodes)->
    Test = true; Test = false.

/* estado de brazos actuales del robot*/
brazos_robot_agarrando(Cosa1,Cosa2,W):- 
  extensionDeUnaClaseInicio(robot,Y),
  nth0(0,Y,B1),nth0(1,Y,B2),
  quieroClase(B1,W,Brazo1),quieroClase(B2,W,Brazo2),
  nth0(3,Brazo1,Rb1),nth0(3,Brazo2,Rb2),
  nth0(2,Brazo1,Nm1),nth0(2,Brazo2,Nm2),
  nth0(0,Brazo1,Id1),nth0(0,Brazo2,Id2),
  segundoTermino(Id1,Id1_),segundoTermino(Id2,Id2_),
  member(nombre=>X1,Nm1), member(nombre=>X2, Nm2),
  member(agarro=>N1,Rb1),member(agarro=>N2,Rb2),
  (( N1 == Id1_ )->  Cosa1 = false ; quieroLugar(N1,W,Cosa), nth0(2,Cosa,Props), member(nombre=>CosaNm,Props), Cosa1 = CosaNm),
  (( N2 == Id2_ )-> Cosa2 = false,! ; quieroLugar(N2,W,Cosa_), nth0(2,Cosa_,Props_), member(nombre=>CosaNm_,Props_), Cosa2 = CosaNm_,!). 

find_all_mover_col(AllNodes,Place,[],PosArray):- PosArray = [],!.
find_all_mover_col(AllNodes,Place,[H|T],PosArray):-
  (
    ( H = [colocar,X] )->
    (
      ( 
        nextto([mover,Place],H,AllNodes) ->  
          ( find_all_mover_col(AllNodes,Place,T,PosArray1),append([H],PosArray1,PosArray),! )
          ; 
          ( nextto([mover,NewPlace],H,AllNodes) ->
             find_all_mover_col(AllNodes,Place,T,PosArray) ; 
             find_all_mover_col(AllNodes,Place,T,PosArray1),append([H],PosArray1,PosArray),!
          )
      )
    )
    ;
    ( 
      find_all_mover_col(AllNodes,Place,T,PosArray)
    )
  ).

%find the best next candidate !! 
% encuentra al mejor candidato de una lista
candidate(AllTheNodes,[],CTime,Time,Heu,OldHeu,BestCandidate,BD,NewBD):- BestCandidate = [], Heu = [], CTime = [], NewBD = [],!.
candidate(AllTheNodes,[H|T],CTime,Time,Heu,OldHeu,BestCandidate,BD,NewBD):- 
      nth0(0,H,Accion), nth0(1,H,OoL),
      (
        ( 
          Accion == mover, probNreward_mover(OoL,BD,Prob,Rew,Cost),heuristic(Prob,Rew,Cost,Heu1)
        );
        (
          Accion == grab, probNreward_grab(OoL,BD,Prob,Rew,Cost),heuristic(Prob,Rew,Cost,Heu1)
        );
        (
          Accion == colocar, quieroClase(OoL,BD,Obj),quieroProbAccionObjeto(Obj,entregar, Prob,Cost,Rew),heuristic(Prob,Rew,Cost,Heu1)
        )
      )
      ,
      (
        ( 
          ( var(OldHeu) ) -> ThisHeu is 0 ; ThisHeu is OldHeu ),
          ((ThisHeu < Heu1)-> 
          ( 
            ( (Accion == grab)-> 
              ( agarrar(OoL,NewBD1,BD) )
              ; 
              ( (Accion == mover)-> 
                (
                 
                  makes_sense_to_move(OoL,AllTheNodes,BD,NoY), 
                  ( (NoY == true)-> mover(OoL,NewBD1,BD) ; NewBD1 = false )
                )
                ; 
                ( 
                  (Accion == colocar)-> colocar(OoL,NewBD1,BD);  NewBD1 = false 
                ) 
              )
            ),
            ( NewBD1 == false -> 
              ( candidate(AllTheNodes,T,CTime,Time,Heu,OldHeu,BestCandidate,BD,NewBD))
              ; 
              (
                candidate(AllTheNodes,T,NT,Time,Heu2,Heu1,BestCandidate1,BD,NewstBD),
                append([NewBD1],NewstBD,NewBD),
                append([Cost],NT,CTime),
                append([Rew],Heu2,Heu),
                append([H],BestCandidate1,BestCandidate),!
              )
            )
          )
          ;
          ( candidate(AllTheNodes,T,CTime,Time,Heu,OldHeu,BestCandidate,BD,NewBD) ))
      ).

find_best_first_([],TheBest,Reward,OldReward,CTIMEs,StuffList,BD):- TheBest=[],Reward=[],!.
find_best_first_([H|T],TheBest,Reward,OldReward,CTIMEs,StuffList,BD):-
  nth0(0,H,Accion), nth0(1,H,OoL),
  probNreward_mover_first(OoL,StuffList,Reward_,_,BD),
  last(Reward_,Reward1),
  quieroClase(inicio,BD,Inicio),
  quieroClase(OoL,BD,Lug1),
  nth0(0,Lug1,IdLug1_),
  segundoTermino(IdLug1_,IdLug1),
  quieroProbLugar(Inicio,IdLug1,Pro,Time,Rew),
  ( (var(OldReward)) -> ThisR is 0 ; ThisR is OldReward ),
  ( (ThisR < Reward1)->
      (
        find_best_first_(T,Best_,Reward__,Reward1,CTIMEs1,StuffList,BD),
        append([H],Best_,TheBest),
        append([Time],CTIMEs1,CTIMEs),
        append([Rew],Reward__,Reward),!
      )
      ;
      (
        find_best_first_(T,TheBest,Reward,OldReward,CTIMEs,StuffList,BD)
      )
  ).

probNreward_mover_first(Lugar,[],Reward,OldReward,BD):- Reward=[],!.
probNreward_mover_first(Lugar,[H|T],Reward,OldReward,BD):-
  stuff_belongs(H,Lugar,Test),
  ((Test == false)-> 
    probNreward_mover_first(Lugar,T,Reward,OldReward,BD) ; 
    (
      (( var(OldReward) ) -> ThisR is 0 ; ThisR is OldReward ),
      probNreward_grab(H,BD,Prob,Rew,Cost),heuristic(Prob,Rew,Cost,Heu),
      (( Heu > ThisR)-> 
          probNreward_mover_first(Lugar,T,Reward_,Heu,BD),
          append([Heu],Reward_,Reward),! 
          ;
          probNreward_mover_first(Lugar,T,Reward,OldReward,BD)         
      )
    )
  ).


heuristic(Prob,Rew,Cost,Heu):-
  ( dif(Rew,0.0) ) ->  Heu is (Prob*(Rew-Cost)) ; Heu is (Prob*(100-Cost)) .

/* Para mover al robot al lugar L, BD=New database, Time=Tiempo q lleva */
mover(L, BD,W):-
  quieroClase(L,W,Lu),
  nth0(0,Lu,Nm),
  segundoTermino(Nm,X),
  modificaPropiedad(ubicacion, robot, X,W, BD),!; BD= false,!.

/*colocar el objeto O en la mano libre del robot, si es q la tiene, BD modificada si realizo la accion. */
agarrar(O, BD,W):-
  ubicacion_actual(Ub,W), 
  ubicar_objeto(O,Ub),
  quieroClase(O,W,Obj),
  brazos_robot(R,W),
  dif(R,false),
  modificaRelacion(agarro,R,O,W,BD),!
  ;
  BD= false,!.


/*colocar el objeto O en el lugar en el que esta el robot, BD modificada si realizo la accion. */
colocar(O, BD, W):-
  quieroClase(O,W,Obj),
  nth0(0,Obj,IDO),
  segundoTermino(IDO,Y),
  brazos_robot_actual(Y,Bra,W),
  dif(Bra,false),
  ubicacion_actual(Ub,W),
  quieroLugar(Ub,W,Lug),
  nth0(2,Lug,Props),
  member(nombre=>L,Props),
  modificaRelacion(agarro,Bra,Bra,W,BD),!;
  BD = false,!.
