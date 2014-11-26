%%  Algoritmo Genético
%%  Ixchel, Vladimir, Eduardo
%%  Algoritmo Genetico para generar soluciones al problema del robot de servicio.

%% Esto Establece la generación aleatoria inicial y llama la parte cíclica del algoritmo.
algoritmoGenetico(NumeroDeIndividuos,Tamano,ProbabilidadDeCruzamiento,ProbabilidadDeMutacion,Generaciones,AccionesIdeales,MejorIndividuo):-
													regresaTodaLaActividad(AccionesIdeales,ListaDeActividadesPosibles),
													%%write('ListaDeActividadesPosibles: '),
													%%write(ListaDeActividadesPosibles),
													%%nl,
													length(ListaDeActividadesPosibles,NumeroDeAcciones),
													generaPoblacionAleatoria(NumeroDeIndividuos,Tamano,NumeroDeAcciones,Poblacion),
													cicloBasico(NumeroDeIndividuos,Tamano,NumeroDeAcciones,ProbabilidadDeCruzamiento,ProbabilidadDeMutacion,Generaciones,Poblacion,ListaDeActividadesPosibles,MejorIndividuoSinTraducir),
													MejorIndividuoSinTraducir = [Fitness|Acciones],
													traduceIndividuo(Acciones,ListaDeActividadesPosibles,MejorIndividuoSinFitness),
													MejorIndividuo = [Fitness|MejorIndividuoSinFitness],
													!.

traduceIndividuo([],ListaDeActividadesPosibles,IndividuoTraducido):-
													IndividuoTraducido = [],
													!.
traduceIndividuo([Siguiente|Resto],ListaDeActividadesPosibles,IndividuoTraducido):-
													traduceAccion(Siguiente,ListaDeActividadesPosibles,AccionTraducida),
													traduceIndividuo(Resto,ListaDeActividadesPosibles,IndividuoTraducidoIncompleto),
													append([AccionTraducida],IndividuoTraducidoIncompleto,IndividuoTraducido),
													!.

traduceAccion(NumeroDeActividad,[],AccionTraducida):-
													AccionTraducida = [],
													!.
traduceAccion(NumeroDeActividad,[SiguienteAccitividad|RestoDeActividades],AccionTraducida):-
													SiguienteAccitividad = [NumeroDeActividad|Actividad],
													Actividad = [A,_,D,_,_,_,_,_],
													AccionTraducida = [A,D],
													!
													;
													traduceAccion(NumeroDeActividad,RestoDeActividades,AccionTraducida),
													!.									

%% Esto es el ciclo básico del algoritmo genético.
cicloBasico(NumeroDeIndividuos,Tamano,NumeroDeAcciones,ProbabilidadDeCruzamiento,ProbabilidadDeMutacion,0,Poblacion,ListaDeActividadesPosibles,MejorIndividuo):-
													MejorIndividuo = [0,0],
													!.
cicloBasico(NumeroDeIndividuos,Tamano,NumeroDeAcciones,ProbabilidadDeCruzamiento,ProbabilidadDeMutacion,Generaciones,Poblacion,ListaDeActividadesPosibles,MejorIndividuo):-
													Generaciones > 0,
													obtieneEstadoDelRobot(Localizacion,BrazoDerecho,BrazoIzquierdo,Encontrado),
													!,
													evaluaPoblacion(Poblacion,ListaDeActividadesPosibles,Localizacion,BrazoDerecho,BrazoIzquierdo,Encontrado,PoblacionEvaluada),
													ordenaPoblacion(PoblacionEvaluada,PoblacionOrdenada),
													PoblacionOrdenada = [MejorIndividuoActual|ElResto],
													cruzaPoblacion(PoblacionOrdenada,ProbabilidadDeCruzamiento,PoblacionCruzada),
													mutaPoblacion(PoblacionCruzada,ProbabilidadDeMutacion,NumeroDeAcciones,PoblacionMutada),
													%% Hacemos estoporque estamos usando elitismo total.
													append(PoblacionOrdenada,PoblacionMutada,PoblacionDoble),
													evaluaPoblacion(PoblacionDoble,ListaDeActividadesPosibles,Localizacion,BrazoDerecho,BrazoIzquierdo,Encontrado,PoblacionDobleEvaluada),
													ordenaPoblacion(PoblacionDobleEvaluada,PoblacionDobleOrdenada),
													%% Aqui regresamos a la población del tamaño original con los mejores individuos.
													split_at(NumeroDeIndividuos,PoblacionDobleOrdenada,PoblacionSuperior,PoblacionInferior),
													Generacioncitas is Generaciones - 1,
													cicloBasico(NumeroDeIndividuos,Tamano,NumeroDeAcciones,ProbabilidadDeCruzamiento,ProbabilidadDeMutacion,Generacioncitas,PoblacionSuperior,ListaDeActividadesPosibles,MejorIndividuoPreliminar),
													mejor(MejorIndividuoActual,MejorIndividuoPreliminar,MejorIndividuo),
													% write('Generacion: '),
													% write(Generaciones),
													% write(', Mejor Individuo: '),
													% write(MejorIndividuoActual),
													% nl,
													!.

%% Esto regresa el individuo que tiene el fitness más grande.
mejor(PrimerIndividuo,SegundoIndividuo,MaximoIndividuo):-
													PrimerIndividuo = [PrimerFitness|PrimerosGenes],
													SegundoIndividuo = [SegundoFitness|SegundosGenes],
													PrimerFitnessEvaluado is PrimerFitness + 0,
													SegundoFitnessEvaluado is SegundoFitness + 0,
													PrimerFitnessEvaluado >= SegundoFitnessEvaluado,
													MaximoIndividuo = PrimerIndividuo,
													!
													;
													MaximoIndividuo = SegundoIndividuo,
													!.					

%% Esto va a generar una poblacion aleatoria.
generaPoblacionAleatoria(0,Tamano,NumeroDeAcciones,Poblacion):- 
													Poblacion = [],
													!.
generaPoblacionAleatoria(NumeroDeIndividuos,Tamano,NumeroDeAcciones,Poblacion):-
													NumeroDeIndividuos > 0,
													NumeritoDeIndividuos is NumeroDeIndividuos - 1,
													generaPoblacionAleatoria(NumeritoDeIndividuos,Tamano,NumeroDeAcciones,PoblacionAnterior),
													generaUNindividuoAleatorio(Tamano,NumeroDeAcciones,Individuo),
													append([0],Individuo,IndividuoConFitness),
													append(PoblacionAnterior,[IndividuoConFitness],Poblacion),
													!.

%% Esto van a generar un individuo aleatorio.
generaUNindividuoAleatorio(0,NumeroDeAcciones,Individuo):- 
													Individuo = [],
													!.
generaUNindividuoAleatorio(Tamano,NumeroDeAcciones,Individuo):-
 													Tamano > 0,	
													Tamanito is Tamano - 1,
													generaUNindividuoAleatorio(Tamanito,NumeroDeAcciones,IndividuoAnterior),
													random_between(1,NumeroDeAcciones,Aleatorio),
													append(IndividuoAnterior,[Aleatorio],Individuo),
													!.

%% Esto saca el fitness de la población entera.
evaluaPoblacion([],ListaDeActividadesPosibles,Localizacion,BrazoDerecho,BrazoIzquierdo,Encontrado,PoblacionEvaluada):-
													PoblacionEvaluada = [],
													!.
evaluaPoblacion([PrimerIndividuo|RestoDeIndividuos],ListaDeActividadesPosibles,Localizacion,BrazoDerecho,BrazoIzquierdo,Encontrado,PoblacionEvaluada):-
													evaluaPoblacion(RestoDeIndividuos,ListaDeActividadesPosibles,Localizacion,BrazoDerecho,BrazoIzquierdo,Encontrado,PoblacionEvaluadaIncompleta),
													PrimerIndividuo = [Fitness|Genes],
													evaluaIndividuo(Genes,ListaDeActividadesPosibles,Localizacion,BrazoDerecho,BrazoIzquierdo,Encontrado,'movimiento','inicio',Fitness2),
													append(PoblacionEvaluadaIncompleta,[[Fitness2|Genes]],PoblacionEvaluada),
													!.

obtieneEstadoDelRobot(Localizacion,BrazoDerecho,BrazoIzquierdo,Encontrado):-
													extensionDeUnaClaseInicio('robot',PartesDelRobot),
													regresaTuplaPorNombreInicio('robot',Robot),
													Robot = [_,_,_,[ClaveLocalizacionSinExtraer|_]],
													segundoTermino(ClaveLocalizacionSinExtraer,ClaveLocalizacion),
													regresaTuplaPorIdInicio(ClaveLocalizacion,Lugar),
													regresaNombre(Lugar,Localizacion),
													PartesDelRobot = [BraIzq,BraDer],
													regresaTuplaPorNombreInicio(BraDer,BrazoDer),
													BrazoDer = [_,_,_,[ClaveBrazoDerechoSinExtraer|_]],
													segundoTermino(ClaveBrazoDerechoSinExtraer,ClaveBrazoDerecho),
													regresaTuplaPorIdInicio(ClaveBrazoDerecho,BrazoDere),
													regresaNombre(BrazoDere,BrazoDerecho),
													regresaTuplaPorNombreInicio(BraIzq,BrazoIzq),
													BrazoIzq = [_,_,_,[ClaveBrazoIzquierdoSinExtraer|_]],
													segundoTermino(ClaveBrazoIzquierdoSinExtraer,ClaveBrazoIzquierdo),
													regresaTuplaPorIdInicio(ClaveBrazoIzquierdo,BrazoIzqer),
													regresaNombre(BrazoIzqer,BrazoIzquierdo),
													Encontrado = 'False',
													!.

%% Esto le saca el fitness a un individuo solo.
evaluaIndividuo([],ListaDeActividadesPosibles,_,_,_,_,PrerrequisitoVerbo,PrerrequisitoObjeto,Fitness):- 
													Fitness is 0,
													!.
evaluaIndividuo([Cabeza|Cola],ListaDeActividadesPosibles,Localizacion,BrazoDerecho,BrazoIzquierdo,Encontrado,PrerrequisitoVerbo,PrerrequisitoObjeto,Fitness):-
													
													% que regrese lo pedido y no otras cosas
													% que sea congruente
													nth1(Cabeza,ListaDeActividadesPosibles,Actividad),
													Actividad = [_,Verbo,_,Objeto,_,_,Recompensa,PrerrequisitoVerbo,PrerrequisitoObjeto],
													evaluaIndividuo(Cola,ListaDeActividadesPosibles,Localizacion,BrazoDerecho,BrazoIzquierdo,Encontrado,Verbo,Objeto,FitnessIncompleto),
													Fitness is Recompensa + FitnessIncompleto,
													!
													;
													evaluaIndividuo(Cola,ListaDeActividadesPosibles,Localizacion,BrazoDerecho,BrazoIzquierdo,Encontrado,Verbo,Objeto,FitnessIncompleto),
													Fitness is + FitnessIncompleto,
													!.



%% Esto revisa que las precondiciones de cierta accion se cumplan para que el plan sea posible


%% Esto ordena de mayor a menor una población según el fitness (primer entrada) de cada individuo.
ordenaPoblacion(PoblacionEvaluada,PoblacionOrdenada):-
													msort(PoblacionEvaluada,PoblacionOrdenadaInvertida),
													reverse(PoblacionOrdenadaInvertida,PoblacionOrdenada),
													!.

%% Esto Reproduce los genomas según el esquema de reproduccion deterministica (el mejor con el peor, el segundo mejor con el segundo mejor, etc..).
cruzaPoblacion([],ProbabilidadDeCruzamiento,PoblacionCruzada):- 
													PoblacionCruzada = [],
													!.
cruzaPoblacion(PoblacionOrdenada,ProbabilidadDeCruzamiento,PoblacionCruzada):-
													PoblacionOrdenada = [ElMejor|LosOtros],
													reverse(LosOtros,LosOtrosInvertidos),
													LosOtrosInvertidos = [ElPeor|LosRestantesInvertidos],
													reverse(LosRestantesInvertidos,LosRestantes),
													cruzaPoblacion(LosRestantes,ProbabilidadDeCruzamiento,PoblacionCruzadaIncompleta),
													cruzaIndividuos(ElMejor,ElPeor,Hijo1,Hijo2,ProbabilidadDeCruzamiento),
													append(PoblacionCruzadaIncompleta,[Hijo1,Hijo2],PoblacionCruzada),
													!.

%% Esto toma 2 individuos y regresa 2 individuos con los genes mezclados.
cruzaIndividuos(ElMejor,ElPeor,Hijo1,Hijo2,ProbabilidadDeCruzamiento):-
													random(Rand),
													Aleatorio is Rand + 0,
													ProbabilidadDeCruzamiento >= Aleatorio,
													length(ElMejor,Largo), 
													Fin is Largo - 2,
													random_between(2,Fin,PuntoDeCorte),
													split_at(PuntoDeCorte,ElMejor,PrincipioDeElMejor,FinalDeElMejor),
													split_at(PuntoDeCorte,ElPeor,PrincipioDeElPeor,FinalDeElPeor),
													append(PrincipioDeElMejor,FinalDeElPeor,Hijo1),
													append(PrincipioDeElPeor,FinalDeElMejor,Hijo2),
													!
													;
													Hijo1 = ElMejor,
													Hijo2 = ElPeor,
													!.

%% Esto cambia algún gen de todos los individuos.
mutaPoblacion([],ProbabilidadDeMutacion,NumeroDeAcciones,PoblacionMutada):-
													PoblacionMutada = [],
													!.
mutaPoblacion([ElPrimero|LosDemas],ProbabilidadDeMutacion,NumeroDeAcciones,PoblacionMutada):-
													mutaPoblacion(LosDemas,ProbabilidadDeMutacion,NumeroDeAcciones,PoblacionMutadaIncompleta),
													mutaIndividuos(ElPrimero,ProbabilidadDeMutacion,NumeroDeAcciones,ElPrimeroMutado),
													append([ElPrimeroMutado],PoblacionMutadaIncompleta,PoblacionMutada),
													!.

%% Esto Cambia algún gen de un individuo.
mutaIndividuos(ElPrimero,ProbabilidadDeMutacion,NumeroDeAcciones,ElPrimeroMutado):-
													random(Rand),
													Aleatorio is Rand + 0,
													ProbabilidadDeMutacion >= Aleatorio,
													length(ElPrimero,Largo), 
													Fin is Largo - 2,
													random_between(0,Fin,PuntoDeCorte),
													split_at(PuntoDeCorte,ElPrimero,PrincipioDeElPrimero,FinalDeElPrimero),
													random_between(1,NumeroDeAcciones,Aleatorio),
													FinalDeElPrimero = [ElQueVamosAReemplazar|LosQueVamosADejarEnPaz],
													append(PrincipioDeElPrimero,[Aleatorio|LosQueVamosADejarEnPaz],ElPrimeroMutado),
													!
													;
													ElPrimero = ElPrimeroMutado,
													!.

% Regresa toda la actividad posible del robot
regresaTodaLaActividad(AccionesIdeales,Y):-
													regresaTodasLasAccionesInicio(AccionesIdeales,Y1),
													regresaTodosLosMovimientosInicio(Y2),
													append(Y1,Y2,Y3),
													numeraLasActividades(1,Y3,Y),
													!.

numeraLasActividades(_,[],Y):-					
													Y = [],
													!.
numeraLasActividades(Indice,[X|Xs],Y):-			
													X = [HX|TX],
													TX = ['movimiento',U,V,_,_,_,_,_],
													dif(U,V),
													YXs = [Indice|TX],
													Siguiente is Indice + 1,
													numeraLasActividades(Siguiente,Xs,Yt),
													append([YXs],Yt,Y),
													!
													;
													X = [HX|TX],
													TX = ['buscar',_,_,_,_,_,_,_],
													Xs = [X2,X3|LasDemasAcciones],
													X2 = [HX2|TX2],
													X3 = [HX3|TX3],
													TX3 = [_,_,_,_,_,_,_,U],
													dif(U,'No pedido'),
													YXs = [Indice|TX],
													Indice2 is Indice + 1,
													YX2s = [Indice2|TX2],
													Indice3 is Indice2 + 1,
													YX3s = [Indice3|TX3],
													Siguiente is Indice3 + 1,
													numeraLasActividades(Siguiente,LasDemasAcciones,Yt),
													append([YXs,YX2s,YX3s],Yt,Y),
													!
													;
													numeraLasActividades(Indice,Xs,Y),
													!.

% Regresa todas la acciones posibles del robot
regresaTodasLasAccionesInicio(AccionesIdeales,Y):-
													extensionDeUnaClaseInicio('cosas',X),
													regresaTodasLasAcciones(AccionesIdeales,X,Y),
													!.

regresaTodasLasAcciones(AccionesIdeales,[],Y):- 
													Y = [],
													!.
regresaTodasLasAcciones(AccionesIdeales,[X|Xs],Y):-
													regresaAccionesDeUnaCosa(AccionesIdeales,X,Ys),
													regresaTodasLasAcciones(AccionesIdeales,Xs,Yt),
													append(Ys,Yt,Y), 
													!.

regresaAccionesDeUnaCosa(AccionesIdeales,X,Y):-
													regresaTuplaPorNombreInicio(X,Z),
													Z = [_,_,_,L],
													L = [Cabeza|Cola],
													segundoTermino(Cabeza,Ub),
													regresaTuplaPorIdInicio(Ub,P),
													regresaNombre(P,Ubicacion),
													regresaAcciones(Z,M),
													M = [H,T],
													segundoTermino(T,B),
													tablaAcciones(AccionesIdeales,X,B,Ubicacion,Y),
													!.

regresaAcciones(X,Y):-
													X=[_,_,Y|_],
													!.

tablaAcciones(AccionesIdeales,Dato,[],Ubicacion,Y):- 
													Y=[],
													!.
tablaAcciones(AccionesIdeales,Dato,[X|Xs],Ubicacion,Y):-													
													X = [X1,X2,X3,X4],
													segundoTermino(X1,Accion),
													Accion = 'buscar',
													segundoTermino(X2,Probadilidad),
													segundoTermino(X3,Costo),
													segundoTermino(X4,Recompensa),
													T = [0,Accion,Ubicacion,Dato,Probadilidad,Costo,Recompensa,'movimiento',Ubicacion],
													tablaAcciones(AccionesIdeales,Dato,Xs,Ubicacion,Yi),
													append([T],Yi,Y),
													!
													;
													X = [X1,X2,X3,X4],
													segundoTermino(X1,Accion),
													Accion = 'agarrar',
													segundoTermino(X2,Probadilidad),
													segundoTermino(X3,Costo),
													segundoTermino(X4,Recompensa),
													T = [0,Accion,Ubicacion,Dato,Probadilidad,Costo,Recompensa,'buscar',Dato],
													tablaAcciones(AccionesIdeales,Dato,Xs,Ubicacion,Yi),
													append([T],Yi,Y),
													!
													;
													buscaDestino(AccionesIdeales,Dato,Destino),
													X = [X1,X2,X3,X4],
													segundoTermino(X1,Accion),
													Accion = 'entregar',
													segundoTermino(X2,Probadilidad),
													segundoTermino(X3,Costo),
													segundoTermino(X4,Recompensa),
													T = [0,Accion,Ubicacion,Dato,Probadilidad,Costo,Recompensa,'movimiento',Destino],
													tablaAcciones(AccionesIdeales,Dato,Xs,Ubicacion,Yi),
													append([T],Yi,Y),
													!.

buscaDestino([],Dato,Destino):- 
													Destino='No pedido',
													!.
buscaDestino([AccionActual,AccionSiguiente|RestoDeLasAcciones],Dato,Destino):-
													AccionActual = [VerboActual,ObjetoActual],
													AccionSiguiente = [VerboSiguiente,ObjetoSiguiente],
													VerboSiguiente = 'entregar',
													ObjetoSiguiente = Dato,
													Destino = ObjetoActual,
													!
													;
													AccionActual = [VerboActual,ObjetoActual],
													AccionSiguiente = [VerboSiguiente,ObjetoSiguiente],
													VerboSiguiente = 'entregar',
													dif(ObjetoSiguiente,Dato),
													buscaDestino(RestoDeLasAcciones,Dato,Destino),
													!
													;
													buscaDestino([AccionSiguiente|RestoDeLasAcciones],Dato,Destino),
													!.


% Regresa todos los movimientos posibles del robot
regresaTodosLosMovimientosInicio(Y):-
													extensionDeUnaClaseInicio('lugares',X),
													regresaTodosLosMovimientos(X,Y),
													!.


regresaTodosLosMovimientos([],Y):- 
													Y = [],
													!.
regresaTodosLosMovimientos([X|Xs],Y):-
													regresaMovimientosDeUnLugar(X,Ys),
													regresaTodosLosMovimientos(Xs,Yt),
													append(Ys,Yt,Y), 
													!.

% Regresa los movimientos de un lugar
regresaMovimientosDeUnLugar(X,Y):-
													regresaTuplaPorNombreInicio(X,Z),
													regresaMovimientos(Z,M),
													M=[H|T],
													H=[C|V],
													segundoTermino(C,B),
													tablaMovimientos(X,B,Y),
													!.

regresaMovimientos(X,Y):-
													X=[_,_,_|Y],
													!.

% Regresa Tabla de movimientos de un lugar
tablaMovimientos(Dato,[],Y):- 
													Y=[],
													!.
tablaMovimientos(Dato,[X|Xs],Y):-
													X = [X1,X2,X3,X4],
													segundoTermino(X1,Clave),
													regresaTuplaPorIdInicio(Clave,K),
													regresaNombre(K,Nombre),
													segundoTermino(X2,Probadilidad),
													segundoTermino(X3,Costo),
													segundoTermino(X4,Recompensa),
													T = [0,'movimiento',Dato,Nombre,Probadilidad,Costo,Recompensa,'movimiento',Dato],
													tablaMovimientos(Dato,Xs,Yi),
													append([T],Yi,Y),
													!.

%% Función split_at de hprolog.pl (me gustaría poder cargarlo pero no encuentro como).
split_at(0,L,[],L) :- !.
split_at(N,[H|T],[H|L1],L2) :-
 	M is N -1,
 	split_at(M,T,L1,L2).

% Uso esto para poder picarle w en swi-prolog para que me de todas las respuestas.
ho(hom).
ho(bar).
h(Y):- ho(Y).

% Inicialización.
:- initialization(main).
main:- consult('main.pl'),consult('consultas.pl').

% Operador de la Base del Dato.
:- op(15, xfx, '=>').
=>(X,Y).

% Regresan partes de la tupla.
regresaId([H|T],Y):- valor(id,[H],Y).
regresaId_padre([H|T],Y):- valor(id_padre,T,Y).
regresaPropiedades([H|T],Y):- T=[HT|TT], 
								TT = [Y|TTT].
regresaRelaciones([H|T],Y):- T=[HT|TT], 
								TT = [HTT|Y].

% Regresa el nombre asociado a una tupla.
regresaNombre(X,Y):- 
									regresaPropiedades(X,S), 
									valor(nombre,S,Y).

% Regresa el nombre mas el operador asociado a una tupla.
regresaNombreCompleto(X,Y):- 
									regresaPropiedades(X,S), 
									S=[H|T], Y = H.

% Regresa una tupla entera de la base de datos según un nombre.
regresaTuplaPorNombreInicio(X,Y):- 
									rb(W), 
									regresaTuplaPorNombre(X,W,Y),!.
regresaTuplaPorNombre(X,[],Y):-fail.
regresaTuplaPorNombre(X,[H|T],Y):- 
									regresaNombre(H,S), 
									X==S, 
									Y = H, !
									; 
									regresaTuplaPorNombre(X,T,Y), !.

% Regresa una tupla entera de la base de datos según un id.
regresaTuplaPorIdInicio(X,Y):- 
									rb(W), 
									regresaTuplaPorId(X,W,Y),!.
regresaTuplaPorId(X,[],Y):-fail.
regresaTuplaPorId(X,[H|T],Y):- 
									regresaId(H,S), 
									X==S, 
									Y = H, !
									; 
									regresaTuplaPorId(X,T,Y), !. 

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

% Regresa todos los objetos hijos directos o indirectos de una clase dada.
extensionDeUnaClaseInicio(X,Y):- 
									rb(W), 
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

% Contiene la base (después la cambiaré para que la base la saque de un archivo)
rb(Y):- Y = [
	[
		id=>c1,
		id_padre=>c0,
		[nombre=>robot],
		[ubicacion=>o5]
	],
	[
		id=>o2,
		id_padre=>c1,
		[nombre=>brazoIzquierdo],
		[cargando=>o2]
	],
	[
		id=>o3,
		id_padre=>c1,
		[nombre=>brazoDerecho],
		[cargando=>o3]
	],
	[
		id=>c4,
		id_padre=>c0,
		[nombre=>lugares],
		[]
	],
	[
		id=>o5,
		id_padre=>c4,
		[nombre=>inicio],
		[movimiento=>[
			[lugar=>o5,probabilidad=>1.00,costo=>0.0,recompensa=>0.0],
			[lugar=>o6,probabilidad=>0.98,costo=>6.0,recompensa=>0.0],
			[lugar=>o7,probabilidad=>0.98,costo=>8.0,recompensa=>0.0],
			[lugar=>o8,probabilidad=>0.98,costo=>9.0,recompensa=>0.0],
			[lugar=>o9,probabilidad=>0.98,costo=>10.0,recompensa=>0.0],
			[lugar=>o10,probabilidad=>0.98,costo=>9.0,recompensa=>0.0]             
			]
		]
	],
	[
		id=>o6,
		id_padre=>c4,
		[nombre=>estante1],
		[movimiento=>[
			[lugar=>o5,probabilidad=>0.98,costo=>6.0,recompensa=>0.0],
			[lugar=>o6,probabilidad=>1.00,costo=>0.0,recompensa=>0.0],
			[lugar=>o7,probabilidad=>0.97,costo=>12.0,recompensa=>0.0],
			[lugar=>o8,probabilidad=>0.97,costo=>15.0,recompensa=>0.0],
			[lugar=>o9,probabilidad=>0.96,costo=>16.0,recompensa=>0.0],
			[lugar=>o10,probabilidad=>0.97,costo=>12.0,recompensa=>0.0]             
			]
		]
	],
	[
		id=>o7,
		id_padre=>c4,
		[nombre=>estante2],
		[movimiento=>[
			[lugar=>o5,probabilidad=>0.98,costo=>8.0,recompensa=>0.0],
			[lugar=>o6,probabilidad=>0.97,costo=>12.0,recompensa=>0.0],
			[lugar=>o7,probabilidad=>1.00,costo=>0.0,recompensa=>0.0],
			[lugar=>o8,probabilidad=>0.98,costo=>6.0,recompensa=>0.0],
			[lugar=>o9,probabilidad=>0.98,costo=>8.0,recompensa=>0.0],
			[lugar=>o10,probabilidad=>0.96,costo=>16.0,recompensa=>0.0]             
			]
		]
	],
	[
		id=>o8,
		id_padre=>c4,
		[nombre=>mesa1],
		[movimiento=>[
			[lugar=>o5,probabilidad=>0.98,costo=>9.0,recompensa=>0.0],
			[lugar=>o6,probabilidad=>0.97,costo=>15.0,recompensa=>0.0],
			[lugar=>o7,probabilidad=>0.98,costo=>6.0,recompensa=>0.0],
			[lugar=>o8,probabilidad=>1.00,costo=>0.0,recompensa=>0.0],
			[lugar=>o9,probabilidad=>0.99,costo=>2.0,recompensa=>0.0],
			[lugar=>o10,probabilidad=>0.97,costo=>13.0,recompensa=>0.0]             
			]
		]
	],
	[
		id=>o9,
		id_padre=>c4,
		[nombre=>mesa2],
		[movimiento=>[
			[lugar=>o5,probabilidad=>0.98,costo=>10.0,recompensa=>0.0],
			[lugar=>o6,probabilidad=>0.96,costo=>16.0,recompensa=>0.0],
			[lugar=>o7,probabilidad=>0.98,costo=>8.0,recompensa=>0.0],
			[lugar=>o8,probabilidad=>0.99,costo=>2.0,recompensa=>0.0],
			[lugar=>o9,probabilidad=>1.00,costo=>0.0,recompensa=>0.0],
			[lugar=>o10,probabilidad=>0.97,costo=>12.0,recompensa=>0.0]             
			]
		]
	],
	[
		id=>o10,
		id_padre=>c4,
		[nombre=>mesa3],
		[movimiento=>[
			[lugar=>o5,probabilidad=>0.98,costo=>9.0,recompensa=>0.0],
			[lugar=>o6,probabilidad=>0.97,costo=>12.0,recompensa=>0.0],
			[lugar=>o7,probabilidad=>0.96,costo=>16.0,recompensa=>0.0],
			[lugar=>o8,probabilidad=>0.97,costo=>13.0,recompensa=>0.0],
			[lugar=>o9,probabilidad=>0.97,costo=>12.0,recompensa=>0.0],
			[lugar=>o10,probabilidad=>1.00,costo=>0.0,recompensa=>0.0]             
			]
		]
	],
	[
		id=>c11,
		id_padre=>c0,
		[nombre=>cosas],
		[]
	],
	[
		id=>o12,
		id_padre=>c11,
		[
			nombre=>hamburguesa,
			acciones=>[
				[nombre=>buscar,probabilidad=>0.95,costo=>5.0,recompensa=>50.0],
				[nombre=>agarrar,probabilidad=>0.90,costo=>8.0,recompensa=>100.0],
				[nombre=>entregar,probabilidad=>0.99,costo=>5.0,recompensa=>300.0]
			]
		],
		[ubicacion=>o6]
	],
	[
		id=>o13,
		id_padre=>c11,
		[
			nombre=>sandwich,
			acciones=>[
				[nombre=>buscar,probabilidad=>0.80,costo=>5.0,recompensa=>50.0],
				[nombre=>agarrar,probabilidad=>0.85,costo=>9.0,recompensa=>100.0],
				[nombre=>entregar,probabilidad=>0.99,costo=>5.0,recompensa=>300.0]
			]
		],
		[ubicacion=>o6]
	],
	[
		id=>o14,
		id_padre=>c11,
		[
			nombre=>refresco,
			acciones=>[
				[nombre=>buscar,probabilidad=>0.96,costo=>2.0,recompensa=>50.0],
				[nombre=>agarrar,probabilidad=>0.95,costo=>2.0,recompensa=>80.0],
				[nombre=>entregar,probabilidad=>0.99,costo=>3.0,recompensa=>300.0]
			]
		],
		[ubicacion=>o7]
	],
	[
		id=>o15,
		id_padre=>c11,
		[
			nombre=>agua,
			acciones=>[
				[nombre=>buscar,probabilidad=>0.70,costo=>15.0,recompensa=>50.0],
				[nombre=>agarrar,probabilidad=>0.95,costo=>10.0,recompensa=>80.0],
				[nombre=>entregar,probabilidad=>0.99,costo=>10.0,recompensa=>300.0]
			]
		],
		[ubicacion=>o7]
	],
	[
		id=>o16,
		id_padre=>c11,
		[
			nombre=>cafe,
			acciones=>[
				[nombre=>buscar,probabilidad=>0.85,costo=>10.0,recompensa=>50.0],
				[nombre=>agarrar,probabilidad=>0.70,costo=>20.0,recompensa=>15.0],
				[nombre=>entregar,probabilidad=>0.85,costo=>20.0,recompensa=>400.0]
			]
		],
		[ubicacion=>o7]
	]
].