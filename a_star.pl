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
  append(NewList1,NewList,L).

clean_([],L):- L=[],!.
clean_([H|T],L):-
  delete_all_ocurrences(H,T,L1),
  clean_(L1,L2),
  append([H],L2,L).

start_plan(L,W,CTime,Time,Reward,ThePlan):-
  allnodes(L,W,Plan),
  write(Plan),nl,
  clean_(Plan,CleanPlan),
  write(CleanPlan),nl,
  length(CleanPlan,Len),
  Len1 is Len-1,
  fill(List,a,Len1),
  find_best_first(CleanPlan, Bests),
  write('Bests >>>'),nl,write(Bests),nl,
  candidate(Bests, CTimes, Time, Heus,OldHeu, BestCandidates, W),
  last(BestCandidates,BestCandidate), last(Heus,Heu), last(CTimes,BestTime),
  write('BestCandidate  ... '), write(BestCandidate),nl,
  write(List),nl,
  delete(CleanPlan,BestCandidate,CleanPlan_),
  write('CleanPlan_ >'),write(CleanPlan_),nl,
  write('CleanPlan >'),write(CleanPlan),nl,
  planned_(List,CleanPlan_,NodesLeft,ThePlan_,BestCandidate,Current1,Rewards, CTime_, Time,W),
  %expand(CleanPlan_,BestCandidate,LastTime,Time,Heuristic,NewCandidate,W),
  append([[BestCandidate]|[ThePlan_]],ThePlan),
  append([Heu],Rewards,Reward),
  CTime is CTime_+BestTime,
  write('BESTPLAN :: '), write(ThePlan),nl,
  write(' ----- :: '), write(CTimes),nl,
  write(' EL TIEMPO INIT :: '), write(BestTime),nl,
  write(' ----- :: '), write(Reward),nl,!.

planned_([], AllNodes, NodesLeft, Plan, Current, Current1, Heuristics, TimeLeft, Time,BD):- Plan = [],Heuristics=[], TimeLeft=0,!.
planned_([H|T],AllNodes, NodesLeft, Plan, Current, Current1, Heuristics,TimeLeft, Time,BD):-
  write('me meto o nel '),nl,
  write('NodesLeft : '),write(NodesLeft),nl,
  ( var(NodesLeft)-> LosNodos = AllNodes ; LosNodos = NodesLeft ),
  ( var(Current1) -> 
    write(' cur 1 var '),nl,expand(LosNodos,Current, LastTime, Time, Heuristic, NewCandidate,NL1,BD) ; 
    write(' cur 1 no var '),nl,write('::'),write(Current1),nl,expand(LosNodos,Current1, LastTime, Time, Heuristic, NewCandidate,NL1,BD)
  ),
  write('  [Current] :: '),write(Current),nl,
  write('  [Current1] :: '),write(Current1),nl,
  write('  [LastTime] :: '),write(LastTime),nl,  
  Current12 = NewCandidate, 
  write(' >> [Current1] :: '),write(Current12),nl,

  write('  [NewCandidate] :: '),write(NewCandidate),nl,
  planned_( T, AllNodes, NL1, Plan1,Current, Current12,Heuristics1, TimeLeft1,Time,BD ),
  TimeLeft is TimeLeft1+LastTime,
  append([NewCandidate],Plan1,Plan),
  append([Heuristic],Heuristics1,Heuristics).

expand(AllNodes,Current,CTime,Time,Heuristic,NewCandidate,NodesLeft,BD):-
  nth0(0,Current,Accion), nth0(1,Current,OoL),
  write(Accion),nl,write(OoL),nl,
  write('AllNodes inside EXPAND PREDICATE -- :: '),nl,write(AllNodes),nl,
  (
    ( 
      Accion == mover, place_has_stuff(OoL,ToF), 
      ( (ToF == true)-> 
          find_all_mover(OoL,AllNodes,Candidates), candidate(Candidates,CTimes,Time,Heus,OldHeu,BestCandidate,BD),
          last(BestCandidate,LBC), last(Heus,LBH), last(CTimes,LBT)
          ;
          find_all_mover_col(AllNodes,OoL,AllNodes,Candidates), candidate(Candidates,CTimes,Time,Heus,OldHeu,BestCandidate,BD),
          last(BestCandidate,LBC), last(Heus,LBH),last(CTimes,LBT)
      )
    );
    (
      Accion == grab,
      findlen(grab,AllNodes,Y),
      find_all_grab(OoL,AllNodes,Candidates,BD,Y), candidate(Candidates,CTimes,Time,Heus,OldHeu,BestCandidate,BD),
      write(Candidates),nl,
      last(BestCandidate,LBC), last(Heus,LBH),last(CTimes,LBT)
    );
    (
      Accion == colocar, find_all_colocar(OoL,AllNodes,Candidates), candidate(Candidates,CTimes,Time,Heus,OldHeu,BestCandidate,BD),
          last(BestCandidate,LBC), last(Heus,LBH),last(CTimes,LBT)
    )
  ),
  write(LBC),nl,write(LBT),nl, 
  NewCandidate = LBC, CTime = LBT, Heuristic = LBH,
  delete(AllNodes,LBC,NodesLeft),!.

find_all_colocar(Obj,[],PosArray):- PosArray = [],!.
find_all_colocar(Obj,[H|T],PosArray):-
  (
    ( H = [mover,X],
      place_has_stuff(X,Test),
      ((Test == true)->  find_all_colocar(Obj,T,PosArray) ; find_all_colocar(Obj,T,PosArray1),append([H],PosArray1,PosArray) )
    )
    ;
    ( find_all_colocar(Obj,T,PosArray) )
  ).

find_all_grab(Obj,[],PosArray,BD,Number):- PosArray = [],!.
find_all_grab(Obj,[H|T],PosArray,BD,Number):-
  ( H = [mover,X], find_all_grab(Obj,T,PosArray1,BD,Number),append([H],PosArray1,PosArray) )
  ;
  ( H = [grab,X],
    stuff_belongs_where(Obj,Place1,BD),
    stuff_belongs_where(X,Place2,BD),
    ( ( Place1 == Place2,dif(Obj,X),Number > 0 )-> find_all_grab(Obj,T,PosArray1,BD,Number),append([H],PosArray1,PosArray) ; find_all_grab(Obj,T,PosArray,BD,Number) )
  )
  ;
  (
    find_all_grab(Obj,T,PosArray,BD,Number)
  ).

find_best_first([],Bests):- Bests=[],!.
find_best_first([H|T],Bests):-
  H=[mover,X],
  place_has_stuff(X,Test),
  ( 
    ( Test == true)-> 
      (
        find_best_first(T,Bests1), append([H],Bests1,Bests) 
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
        (Test == true ) ->  find_all_mover(Place,T,PosArray1),append([H],PosArray1,PosArray) ; find_all_mover(Place,T,PosArray)
      )
    )
    ;
    (
      H = [colocar,X],
      place_has_stuff(Place,Test),
      ( 
        (Test == true ) -> find_all_mover(Place,T,PosArray) ; find_all_mover(Place,T,PosArray1),append([H],PosArray1,PosArray) 
      )
    )
    ;
    (
      find_all_mover(Place,T,PosArray)
    )
  ).

find_all_mover_col(AllNodes,Place,[],PosArray):- PosArray = [],!.
find_all_mover_col(AllNodes,Place,[H|T],PosArray):-
  (
    ( H = [colocar,X] )->
    (
      write(X),nl,nl,
      ( 
        nextto([mover,Place],H,AllNodes) ->  
          ( find_all_mover_col(AllNodes,Place,T,PosArray1),append([H],PosArray1,PosArray) )
          ; 
          ( nextto([mover,NewPlace],H,AllNodes) ->
             find_all_mover_col(AllNodes,Place,T,PosArray) ; 
             find_all_mover_col(AllNodes,Place,T,PosArray1),append([H],PosArray1,PosArray)
          )
      )
    )
    ;
    ( 
      write('no colocar'),nl,nl,
      find_all_mover_col(AllNodes,Place,T,PosArray)
    )
  ).

%find the best next candidate !! 
% encuentra al mejor candidato de una lista
candidate([],CTime,Time,Heu,OldHeu,BestCandidate,BD):- BestCandidate = [], Heu = [], CTime = [],!.
candidate([H|T],CTime,Time,Heu,OldHeu,BestCandidate,BD):- 
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
          write(Heu),nl,nl,
          ( var(OldHeu) ) -> ThisHeu is 0 ; ThisHeu is OldHeu ),
         write(Prob),write(Rew),write(Cost),nl,nl,
        write(ThisHeu),nl,nl,write(Heu1),nl,nl,
        (ThisHeu < Heu1)-> 
          ( %NT is CTime+Cost,
            candidate(T,NT,Time,Heu2,Heu1,BestCandidate1,BD),
            append([Cost],NT,CTime),
            append([Heu1],Heu2,Heu),
            append([H],BestCandidate1,BestCandidate) 
          )
          ;
          ( candidate(T,CTime,Time,Heu,OldHeu,BestCandidate,BD) )
      ).

heuristic(Prob,Rew,Cost,Heu):-
  ( dif(Rew,0.0) ) ->  Heu is Prob*Rew ; Heu is Prob*100 .