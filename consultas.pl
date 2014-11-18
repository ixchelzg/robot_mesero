
% Regresan partes de la tupla.
regresaId([H|T],Y):- valor(id,[H],Y).
regresaId_padre([H|T],Y):- valor(id_padre,T,Y).
regresaPropiedades([H|T],Y):- T=[HT|TT], 
								TT = [Y|TTT].
regresaRelaciones([H|T],Y):- T=[HT|TT], 
								TT = [HTT|Y].

% Regresa el nombre asociado a una tupla.
regresaNombre(X,Y):- regresaPropiedades(X,S), 
								valor(nombre,S,Y).

% Regresa el nombre mas el operador asociado a una tupla.
regresaNombreCompleto(X,Y):- regresaPropiedades(X,S), 
								S=[H|T], Y = H.

% Regresa una tupla entera de la base de datos según un nombre.
regresaTuplaPorNombreInicio(X,Y):- rb(W), 
								regresaTuplaPorNombre(X,W,Y),!.
regresaTuplaPorNombre(X,[],Y):-fail.
regresaTuplaPorNombre(X,[H|T],Y):- regresaNombre(H,S), 
								X==S, 
								Y = H, !
								; 
								regresaTuplaPorNombre(X,T,Y), !.

% Regresa una tupla entera de la base de datos según un id.
regresaTuplaPorIdInicio(X,Y):- rb(W), 
								regresaTuplaPorId(X,W,Y),!.
regresaTuplaPorId(X,[],Y):-fail.
regresaTuplaPorId(X,[H|T],Y):- regresaId(H,S), 
								X==S, 
								Y = H, !
								; 
								regresaTuplaPorId(X,T,Y), !.  
% Regresa todos los objetos hijos directos o indirectos de una clase dada.
extensionDeUnaClaseInicio(X,Y):- rb(W), 
								regresaTuplaPorNombre(X,W,S), 
								regresaId(S,L), 
								extensionDeUnaClase(L,W,Y),!.
extensionDeUnaClase(X,[],Y):- Y = [].
extensionDeUnaClase(X,[H|T],Y):- 
								regresaId_padre(H,S), 
								X==S,
								regresaNombre(H,N),
								regresaId(H,I), 
								string_chars(I,J), 
								J=[C|V], 
								C == 'c',
								extensionDeUnaClaseInicio(N,P),
								extensionDeUnaClase(X,T,R), 
								append(P,R,Y),!
								;
								regresaId_padre(H,S), 
								X==S,
								regresaNombre(H,N),
								regresaId(H,I), 
								string_chars(I,J), 
								J=[C|V], 
								C == 'o',
								extensionDeUnaClase(X,T,R), 
								append([N],R,Y),!
								; 
								extensionDeUnaClase(X,T,Y),!.


modificaRelacion(P,X,P1, W,BD):-
	quieroClase(X,W,Cl), quieroClase(P1,W,Cl1),
	nth0(3,Cl,Rels), nth0(0,Cl1,IDCl1), segundoTermino(IDCl1,Id),
	member(P=>Xv,Rels),
	sus(P=>Xv,P=>Id,Rels,S), sus(Rels,S,Cl,H1), sus(Cl,H1,W,BD).


/*modificaPropiedad(Propiedad, Clase/Objeto, Valor Nuevo, Regresa la BD con la prop modificada en BD)*/
modificaPropiedad(P,X,P1,BD):-
	rb(W), 
	quieroClase(X,W,Cl),
	nth0(3,Cl,Props),
	member(P=>Xv,Props),
	sus(P=>Xv,P=>P1,Props,S), sus(Props,S,Cl,SC), sus(Cl,SC,W,BD).


eliminaRelacion(X,Y,W,BD):-
	quieroClase(Y,W,P),
	crearNuevaListaRels(P,X,W,BD).

crearNuevaListaRels(X,Y,W,BD):-
	nth0(3,X,Rels),
	member(Y=>XX,Rels),
	eliminaClase(Y=>XX,Rels,P1),
	sus(Rels,P1,X,H1),
	sus(X,H1,W,BD).


% Lo mismo pero para Relaciones
anadeRelacion(Nom,Rels,X,BD) :-
	[H|T] = Rels,
	(esCorrecto(Rels) -> true; nl,write('Escribe las relaciones de la forma x=>y'),nl,false),
	quieroClase(Nom,X,Cla),
	([] \= Cla -> true; nl,write('Esa Clase u Objeto no existe'),nl,false),
	nth0(3,Cla,Re),
	nth0(0,Rels,PP),
	segundoTermino(PP,YY),
	primerTermino(PP,XX),
	quieroClase(YY,X,RelClas),
	nth0(0,RelClas,Id),
	segundoTermino(Id,IdL),
	Rels1=[XX=>IdL],
	append(Re,Rels1,Rel),
	nth0(3,Cla,E,R),
	nth0(3,L,Rel,R),
	select(Cla,X,L,BD),!.

esCorrecto([]).
esCorrecto([H|T]) :-
	H = =>(X,Y),
	esCorrecto(T).