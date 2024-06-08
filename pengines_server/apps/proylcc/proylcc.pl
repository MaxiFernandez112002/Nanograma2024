:-module(proylcc,[ put/9 ]).

:-use_module(library(lists)).
:- use_module(library(clpfd)).


% replace(?X, +XIndex, +Y, +Xs, -XsY)
%
% XsY is the Cumpleult of replacing the occurrence of X in position XIndex of Xs by Y.
/*
CB: cuando el indice es 0 entonces reemplaza el elemento X de la lista [X|Xs] por la Y, quedando como dato de salida [Y|Ys]
CR: Llama recursivamente disminuyendo el indice hasta que el indice sea 0 y luegp reemplaza donde estaba la X por la Y
*/

replace(X, 0, Y, [X|Xs], [Y|Xs]).	%si tenemos una "x" la reemplazamos por un "#" y si tenemos un "#" reemplazamos por una "x", idem para el espacio vacio

% replace(X, 0, X, [X|Xs], [" "|Xs]).	%si estamos intentado reemplazar una "x" por una "x" o un "#" por un "#" entonces reemplazamos por un espacio vacio

replace(X, XIndex, Y, [Xi|Xs], [Xi|XsY]):-
    XIndex > 0,
    XIndexS is XIndex - 1,
    replace(X, XIndexS, Y, Xs, XsY).


% put(+Contenido, +Pos, +FilasPistas, +ColsPistas, +Grilla, -NuevaGrilla, -FilaSat, -ColSat).

/*
Fila = fila
put/8 actualiza una matriz (Grilla) en una posición específica con un nuevo valor (Contenido), la matriz de salida sera NuevaGrilla
Contenido contenido que es # o X
Pos es una lista [Fila, Columna], indicando la posición donde se desea colocar Contenido
Fila Pistas y col Pistas son las pistas de las filas y col Cumplepectivamente
FilaSat es 1 si las fila de Pos satisface las pistas asociadas, y 0 en caso contrario, ColSat es analogo
cambiar los 0 por 2 var 
NonogramaCompletado = 1 si el juego se gano
*/

put(Contenido, [FilaNumero, ColNumero], PistasFilas, PistasColumnas, Grilla, NuevaGrilla, FilaSat, ColSat, NonogramaCompletado):-

	% NuevaGrilla es el Cumpleultado de reemplazar la fila vieja por la nueva modificada
	replace(Fila, FilaNumero, NewFila, Grilla, NuevaGrilla),	%lo q hace este 1er replace es conseguir la fila q se busca y la misma pero modificada

	(replace(Celda, ColNumero, _, Fila, NewFila),
	Celda == Contenido		/*Si estas pintando y lo pCumpleionas de nuevo lo despinta*/
		;
	replace(_Celda, ColNumero, Contenido, Fila, NewFila)),

	verificar_fila(FilaNumero,PistasFilas,NuevaGrilla,FilaSat),		% Verifica si para la fila se cumple lo indicado en su Cumplepectiva lista fila de pistas, 
																	% en caso de cumplirse FilaSat es 1, caso contrario 0.
	verificar_columna(ColNumero,PistasColumnas,NuevaGrilla, ColSat),% Verifica si para la columna se cumple lo indicado en su Cumplepectiva lista columna de pistas, 
																	% en caso de cumplirse ColSat es 1, caso contrario 0.

	%Si se cumple la pista de la fila y la de la col ent comprobamos si se gano el nonograma
	%FilaSat is 1,
	%ColSat is 1, 
	
	comprobar_grilla(NuevaGrilla, PistasFilas, PistasColumnas, _TodasFilasSat, _TodasColSat, NonogramaCompletado).


/*
CASO BUENO 
proylcc:put("#", [1,3],  
	[[3], [1,2], [4], [5], [5]],
	[[2], [5], [1,3], [5], [4]],
	[["X","#","#","#","X"], 		
 	["X","#","X","X","#"],
 	["X","#","#","#","#"],		
 	["#","#","#","#","#"],
 	["#","#","#","#","#"]],
	NuevaGrilla,
	FilaSat,
	ColSat,
	NonogramaCompletado
	 ).

CASO MALO
proylcc:put("#", [1,3], 
	[[3], [1,2], [4], [5], [5]],
	[[2], [5], [1,3], [5], [4]],
	[["X","#","#","#","#"], 		
 	["X","#","X","X","#"],
 	["X","#","#","#","#"],		
 	["#","#","#","#","#"],
 	["#","#","#","#","#"]],
	NuevaGrilla,
	FilaSat,
	ColSat,
	NonogramaCompletado
	 ).

verificar_fila(+Posicion,+ListaPistas,+GrillaCumple,-N)
verifica que la fila/columna tenga sus pistas satisfechas.
1==TRUE, 0==FALSE
Ejemplo: verificar_fila(2, 4, NuevaGrilla, 0/1)
	
	nth0(+Indice, +Lista, -ElemEnPosIndex)

Primero se entra a verificar_fila(IndiceFila, PistasFilas, GrillaCumple, 1)
y en caso de que no se verifique el predicado se va a verificar_fila(_,_,_,0).
*/

obtener_fila(Grilla,Numerofila,Fila):- nth0(NumeroFila, Grilla, Fila).

verificar_fila(IndiceFila, PistasFilas, GrillaCumple, 1):-
	nth0(IndiceFila, PistasFilas, PistaDeFila),				% Obtiene las pistas (o la pista) de la fila 
	nth0(IndiceFila, GrillaCumple, Filadegrilla),				% Obtiene la fila corCumplepondiente a la posicion fila, de la grilla
    verificar_pistas_en_lista(PistaDeFila, Filadegrilla).	% Verifica que la fila de la grilla cumpla con las pistas de la misma
	

verificar_fila(_,_,_,0).									



/*
obtener_columna_acum(+Grilla, +NumCol, -ColumnaCumpleultante)
CB: cuando la lista de filas está vacía entonces la columna es la lista vacia.
CR: Si la grilla no esta vacia, entonces en la 1er lista se busca el elem que se encuentre en la columna deseada y se lo agrega al comienzo de la columna de salida,
luego se llama recursivamente con la cola de la grilla
*/

obtener_columna(Grilla, Col, Columna) :-
	obtener_columna_acum(Grilla, Col, [], ColumnaAux),
	invertir_lista(ColumnaAux, Columna).

obtener_columna_acum([], _, ColumnaAcum, ColumnaAcum).   

obtener_columna_acum([Fila|Grilla], Col, ColumnaAcum, Columna) :-
	nth0(Col, Fila, Elem),  % Obtenemos el elemento en la posición Col de la fila actual.
	obtener_columna_acum(Grilla, Col, [Elem|ColumnaAcum], Columna).  % Llamada recursiva con el acumulador actualizado.



% Predicado para invertir una lista
invertir_lista([], []). % La lista vacía invertida es también una lista vacía.

invertir_lista([X|Xs], ListaInvertida) :-
    invertir_lista(Xs, CumpletoInvertido), 		 
    append(CumpletoInvertido, [X], ListaInvertida).

/*Tenemos que recorrer la grilla de atras para adelante, para ello necesitaremos saber la cant de columnas y pedir de la ult columna - 1 el elem de la columna deseada*/





verificar_columna(IndiceColumna, PistasCol, GrillaCumple, 1) :-
	nth0(IndiceColumna, PistasCol, FiladePistas),
	obtener_columna(GrillaCumple, IndiceColumna, ColumnaDeGrilla),
	verificar_pistas_en_lista(FiladePistas, ColumnaDeGrilla).											

verificar_columna(_,_,_,0).								

/*CASO MALO
proylcc:verificar_columna(2,[[2],[5],[1,3],[5],[4]] ,[
		["X","#","#","#","X"],
		["X","#","X","#","#"],
		["X","#","X","#","#"],
		["X","#","#","#","#"],
		["#","#","#","#","#"]],1).

CASO BUENO
proylcc:verificar_columna(2,[[2],[5],[1,3],[5],[4]] ,[
		["X","#","#","#","X"],
		["X","#","X","#","#"],
		["X","#","#","#","#"],
		["#","#","#","#","#"],
		["#","#","#","#","#"]],1).

*/
	
/*	verificar_pistas_en_lista(+Pistas, +FiladeGrilla)

verificar_pistas_en_lista verifica que se cumpla que no haya mas #s que pistas dadas
CB: Si ya se descontaron las pistas dadas (hay 0 pistas) no tiene que haber #s en la lista
CR: Si hay pistas y el primer elem de la lista es # entonces se verifica que cumpla si hay cierta cantidad de # consecutivos, si cumple luego se llama recursivamente 
con las pistas Cumpletantes y la lista Cumpletante
CR2: Si hay pistas y hay lista, y el 1er elem de la lista no es # entonces se llama recursivamente con las mismas pistas y la cola de la lista.
*/

verificar_pistas_en_lista([],ListaFila):-
	not(member("#",ListaFila)).

verificar_pistas_en_lista([X|Pistas], [Y|ListaFilaS]):-
	Y == "#",
	verificar_pconsecutivos(X, [Y|ListaFilaS], Cumpletante),	%en caso de q se cumpla q haya p consecutivos retorna la lista Cumpletante
	verificar_pistas_en_lista(Pistas, Cumpletante).

verificar_pistas_en_lista(Pistas, [Y|ListaFilaS]):- 
	Y \== "#", 				   % Dada la lista de pistas, y el primer elemento de ListaFilaS (lista de fila)
	verificar_pistas_en_lista(Pistas, ListaFilaS).



/*
 verificar_pconsecutivos( +NumeroPista, +FilaARecorrer, -FilaCumpletante)
 CB: si hay 0 pistas que verificar y no hay mas lista por recorrer ent las pistas se cumple
 CB2: si no hay pista y si hay lista entonces si el primer elem de la lista no es # entonces cumple con que haya p #s consecutivos
 CR: si hay pista entonces si el 1er elem de la lista es # entonces descontar pista y llamamos recursivamente con lista'.
 lista' es lista sin su 1er elem .
 FilaCumpletante es la porcion de lista que no se recorrio aun.
proylcc: verificar_pistas_en_lista([1,2,1],["X","#","X","#","#","X","#"]).
*/

verificar_pconsecutivos(0,[],[]).														   

verificar_pconsecutivos(0,[X|FilaCumpletante],FilaCumpletante):-
	X \== "#".

verificar_pconsecutivos(N,[X|FilaCumpletante],FilaCumpletante2):- 
	X == "#", 
	N > 0, 
	Naux is N-1,   
	verificar_pconsecutivos(Naux,FilaCumpletante,FilaCumpletante2).

/*
probar con 
trace,proylcc:verificar_pconsecutivos(3,["#","#","#"],FilaCumpletante).
True retorna lista vacia
trace,proylcc:verificar_pconsecutivos(3,["X","#","#","#","X"],FilaCumpletante).
True retorna lista vacia
trace,proylcc:verificar_pconsecutivos(3,["#","#","#","X","X"],FilaCumpletante).
True retorna lista=["X"]
trace,proylcc:verificar_pconsecutivos(3,["#","#","#","#","X"],FilaCumpletante).
False
*/



comprobar_grilla(Grilla, PistasFilas, PistasCol, FilaSat, ColSat, 1):-
	contar_filas(Grilla, CantFilas),
	contar_columnas(Grilla, CantColumnas),
	comprobar_todas_filas(Grilla, FilaSat, PistasFilas, CantFilas),			%empieza comprobando las filas desde la primera (la 0)
	comprobar_todas_columnas(Grilla, ColSat, PistasCol, CantColumnas),		%empieza a comprobar las columnas desde la primera (la 0)
	FilaSat is 1,																
	ColSat is 1.
	%NonogramaCompletado is 1.														

comprobar_grilla(_, _, _, 0, 0, 0 ).

/*
comprobar_todas_filas comprobara que se cumplan las pistas de todas las filas
comprobar_todas_filas(+Grilla, -FilaSat, +NumeroFila, +PistasFilas)
*/

comprobar_todas_filas(_, _, _, 0).

comprobar_todas_filas(Grilla, FilaSat, PistasFilas, CantFilas):-
	Aux is CantFilas - 1,
	Aux >= 0,																		
	verificar_fila(Aux, PistasFilas, Grilla, FilaSat),
	comprobar_todas_filas(Grilla, FilaSat, PistasFilas, Aux).


/*
COLSAT = 0 = FALSE
COLSAT = 1 = TRUE
comprobar_todas_columnas comprobara que se cumplan las pistas de todas las columnas
comprobar_todas_columnas (+Grilla, -ColSat, +PistasCol, +CantCol)
*/

comprobar_todas_columnas(_, _, _, 0).

/*PONIENDO EN VEZ DE COLSAT UN 1 ANDA BIEN*/
comprobar_todas_columnas(Grilla, ColSat, PistasCol, CantColumnas):-
	Aux is CantColumnas - 1,
	Aux >= 0,																		
	verificar_columna(Aux, PistasCol, Grilla, ColSat),
	comprobar_todas_columnas(Grilla, ColSat, PistasCol, Aux).



/*Cuenta las filas*/

contar_filas([], 0). % Caso base: la lista está vacía, no hay listas dentro.

contar_filas([_|T], Cont) :-
	contar_filas(T, Aux), % Llamada recursiva para el Cumpleto de la lista
	Cont is Aux + 1. % Incrementa el contador si H es una lista


% Predicado para contar la cantidad de columnas en una lista de listas
contar_columnas([], 0). % Caso base: la lista está vacía, no hay columnas.

contar_columnas([H|_], Cont) :-
    length(H, Cont). % Obtener la longitud de la primera ColaListaa



comprobar_todas_filas_react(_, _, 0,[]).

comprobar_todas_filas_react(Grilla, PistasFilas, CantFilas, [FilaSat|FilaConPistas]):-
	Aux is CantFilas - 1, 
	Aux >= 0,																		
	verificar_fila(Aux, PistasFilas, Grilla, FilaSat),
	comprobar_todas_filas_react(Grilla, PistasFilas, Aux, FilaConPistas).	


comprobar_todas_columnas_react(_, _, 0,[]).
	
comprobar_todas_columnas_react(Grilla, PistasCol, CantColumnas,[ColSat|ColumnaConPistas]):-
	Aux is CantColumnas - 1,
	Aux >= 0,																		
	verificar_columna(Aux, PistasCol, Grilla, ColSat),
	comprobar_todas_columnas_react(Grilla, PistasCol, Aux, ColumnaConPistas).


comprobar_grilla_react(Grilla, PistasFilas, PistasCol, FilaConPistas, ColumnaConPistas):-
	contar_filas(Grilla, CantFilas),
	contar_columnas(Grilla, CantColumnas),
	comprobar_todas_filas_react(Grilla, PistasFilas, CantFilas, FilaConPistasInvertida),			%empieza comprobando las filas desde la primera (la 0)
	comprobar_todas_columnas_react(Grilla, PistasCol, CantColumnas, ColumnaConPistasInvertida),
	invertir_lista(FilaConPistasInvertida, FilaConPistas),
	invertir_lista(ColumnaConPistasInvertida, ColumnaConPistas).	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Determina si una fila o columna satisface su pista corCumplepondiente.
% checkPista(Fila/Columna,PistasFilas/PistasColumnas,Cumple).
% Se utiliza una "cascara" que se encarga de omitir los elementos que no son # y luego se usa verifica_pista_aux


% Si queda un solo elemento y no hay mas pistas que satisfacer entonces debe ser vacio.
%																							checkPista([Elem],[0],1):- no_es_hashtag(Elem).
% Si no queda ningún elemento y no hay mas pistas que satisfacer entonces satisface.
verifica_pista([Elem],[0],1):- no_es_hashtag(Elem).
verifica_pista([],[0],1).
verifica_pista([],[],1).
% Si el elemento es vacío entonces se llama con el siguiente elemento de la lista.
verifica_pista([Elem|ColaLista],Pista,Cumple):- 
	no_es_hashtag(Elem),
 	verifica_pista(ColaLista,Pista,Cumple).
% Si es el elemento no es vacío, se encarga verifica_pista_aux.
verifica_pista([Elem|ColaLista],Pista,Cumple):- Elem == "#",
 	verifica_pista_aux([Elem|ColaLista],Pista,Cumple).
% Cualquier otro caso es falso.
verifica_pista(_,_,0).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Determina si una fila o columna satisface su pista corCumplepondiente, esta fila o columna debe comenzar con un elemento no "vacío"
% verifica_pista_aux(Fila/Columna,PistasFilas/PistasColumnas,Cumple).
% Si queda un solo elemento y no hay mas pistas que satisfacer entonces debe ser vacio.

verifica_pista_aux([Elem],[0],1):- no_es_hashtag(Elem).
% Si no queda ningún elemento y no hay mas pistas que satisfacer entonces satisface.
verifica_pista_aux([],[0],1).
% Si el elemento es #, entonces se decrementa el valor de la pista y se llama recursivamente con el siguiente elemento de la fila o columna.
verifica_pista_aux([Elem|ColaLista],[Pista|ColaPista],Cumple):-
	Elem == "#",
 	PistaAux is (Pista-1),
 	verifica_pista_aux(ColaLista,[PistaAux|ColaPista],Cumple),
 	!.
% Si el elemento es vacío y el valor de la pista está en 0, significa que cumplió con al menos una parte de las pistas, entonces se llama con la parte siguiente. ___________________________________________________________________________________________________________________
verifica_pista_aux([Elem|ColaLista],[Pista|ColaPista],Cumple):-
	no_es_hashtag(Elem),
	Pista is 0,
	verifica_pista(ColaLista,ColaPista,Cumple).


% Si se alcanza este caso siginifca que no hay mas pistas que Cumpleolver, por lo que todos los elementos que quedan deben estar vacios.
verifica_pista_aux([Elem|ColaLista],[],Cumple):- 
no_es_hashtag(Elem), 
verifica_pista_aux(ColaLista,[0],Cumple).

% Cualquier otro caso es falso.
verifica_pista_aux(_,_,0).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Se considera vacío a los elementos no instanciados o que son X
% no_es_hashtag(Elem).
%
no_es_hashtag(Elem):- not(ground(Elem)).
no_es_hashtag("X").
no_es_hashtag([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dado un indice y un arreglo de pistas, se obtiene la pista corCumplepondiente.
% obtener_pista(Index,Pista,Cumple).
%
obtener_pista(Index, Pista, Cumple):- nth0(Index, Pista, Cumple).

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dado un numero de columna y un arreglo de columnas, se obtiene la pista corCumplepondiente.
% obtener_columna(ColN,PistasColumnas,Cumple).
%
obtener_columna([],_,[]).
obtener_columna([L1|Ls],ColN, [Elem|Col]):- nth0(ColN, L1, Elem), obtener_columna(Ls,ColN, Col).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dado un numero de pista y un arreglo de pistas, se obtiene la pista corCumplepondiente.
% obtener_fila(RowN,PistasFilas,Cumple).
%
obtener_fila(Grilla,RowN,Cumple):- nth0(RowN, Grilla, Cumple).
*/
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Se almacena en Cumple las columnas que satisfacen las pistas de una grilla dada.
% verifica_pistas_columna(Grilla,Length,Length,ColPista,Cumple).

% Si la longitud es igual al contador entonces no hay que "recorrer" más.
verifica_pistas_columna(_Grilla,Length,Length,_ColPista,[]).
% Caso recursivo, si el contador no es igual a la longitud, entonces se obtiene la fila del contador, se verifica que esté bien y se obtiene para el Cumpleto de filas
verifica_pistas_columna(Grilla,Index,Length,[C|CSub],[Cumple|ColArray]):-
    not(Index is Length),
    obtener_columna(Grilla,Index,Col),
    verifica_pista(Col,C,Cumple),
    IndexAux is Index + 1,
    verifica_pistas_columna(Grilla,IndexAux,Length,CSub,ColArray).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Determina si todos los elementos de la Lista son iguales a E
% todas_iguales(Elemento,Lista).
todas_iguales(_E,[]).
todas_iguales(E,[X|Xs]):- X == E, todas_iguales(E,Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dada una pista se genera la posible solución que la satisface.
% generar_posibles_soluciones(ListaPosible,Pista).

%Caso base
generar_posibles_soluciones([],[]).
%Caso recursivo: insertamos en la lista a devolver un "#" y llamamos al metodo auxiliar.
generar_posibles_soluciones([Elem|ColaLista],Pista):- Elem = "#" , generar_posibles_soluciones_aux([Elem|ColaLista],Pista).

generar_posibles_soluciones([Elem|ColaLista],Pista):- Elem = "X" , generar_posibles_soluciones(ColaLista,Pista).

%Caso base: si la lista está vacía y las pistas ya son 0.
generar_posibles_soluciones_aux([],[0]).
%Caso recursivo, insertamos en la lista a devolver un "#" y si la pista no es 0, entonces llamamos recursivamente.
generar_posibles_soluciones_aux([Elem|ColaLista],[Pista|ColaPista]):- 
	Elem = "#",
	Pista \= 0, 
	PistaAux is Pista-1, 
	generar_posibles_soluciones_aux(ColaLista,[PistaAux|ColaPista]).

generar_posibles_soluciones_aux([Elem|ColaLista],[Pista|ColaPista]):- 
	Elem = "X", 
	Pista is 0, 
	generar_posibles_soluciones(ColaLista,ColaPista).


/*
trace,proylcc: generar_posibles_soluciones([ _, _ , _ , _ , _ ], [3]).

trace,proylcc: generar_posibles_soluciones([ _, "X" , _ , _ , _ ], [3]).

*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Se genera una fila con los movimiento que se sabe que son correctos.
% fila_correcta(ListaActual,Pista,Longitud,Salida).
fila_correcta(Actual,Pista,Longitud,Salida):-
    findall(Actual,(length(Actual,Longitud),generar_posibles_soluciones(Actual,Pista)),Todas),
    interseccion(Todas,Longitud,Salida).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Es un método "cascara" en el cual a partir de una lista de listas, se obtiene en Salida la intersección de las mismas.
% interseccion(Posibles,Longitud,Salida).
interseccion(Posibles,Longitud,Salida):-
    LongitudAux is Longitud - 1,
    interseccion_aux(Posibles,LongitudAux,[],Salida).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Es un método auxiliar recursivo que es utilizado por intersección.
% El parametro almacenado empieza vacío si es la primer llamada.
% interseccionAux(Posibles,Longitud,Almacenado,Salida).

%Caso base:
interseccion_aux(_,-1,Aux,Aux).
interseccion_aux(_,-1,_,_).

%Caso recursivo
interseccion_aux(Posibles,N,LAux,Salida):- 
    obtener_columna(Posibles,N,Iesimos),  %Obtenemos de cada lista posible el elemento N
    todas_iguales("X",Iesimos),   
    append(["X"],LAux,Aux),   %Si todos son X, entonces lo agregamos a la lista acumulado
    NAux is N -1,
    interseccion_aux(Posibles,NAux,Aux,Salida).

interseccion_aux(Posibles,N,LAux,Salida):- 
    obtener_columna(Posibles,N,Iesimos),   %Obtenemos de cada lista posible el elemento N
    todas_iguales("#",Iesimos), 
    append(["#"],LAux,Aux),   %Si todos son #, entonces lo agregamos a la lista acumulado
    NAux is N -1,
    interseccion_aux(Posibles,NAux,Aux,Salida).

%Si hay alguna distinta
interseccion_aux(Posibles,N,In,Out):-
    append([_],In,Aux), %Agrego al final de la lista.
    NAux is N-1,
    interseccion_aux(Posibles,NAux,Aux,Out).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dada una longitud y unas pistas determina si la suma de valoCumple y espacios es igual a la longitud.
% cumple_condicion(Pista,Longitud).
%caso base
cumple_condicion([0],0).

%Caso en el que estamos en un espacio, Cumpletamos y seguimos.
cumple_condicion([0|Ps],Longitud):-
	L is Longitud - 1,
    cumple_condicion(Ps,L).

%Caso normal
cumple_condicion([P|Ps],Longitud):-
    not(P is 0),
	L is Longitud - 1,   
    PAux is P - 1,
    cumple_condicion([PAux|Ps],L).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Método cascara el cual a partir de una grilla genera una nueva con las pistas que satisfacen cumple_condicion(Pista,Longitud)
% primer_pasada(GrillaIn,PistaFila,PistasColumna,GrillaSalida).
primer_pasada(GrillaIn,PistasFila,PistasColumna,GrillaFinal):-
    length(PistasFila,LongitudFilas),
    primer_pasada_aux(GrillaIn, PistasFila, GrillaSalidaFilas,LongitudFilas),
 	transpose(GrillaSalidaFilas, GrillaTraspuesta),
    length(PistasFila,LongitudColumnas),
    primer_pasada_aux(GrillaTraspuesta, PistasColumna, GrillaSalidaColumnas,LongitudColumnas),
    transpose(GrillaSalidaColumnas, GrillaFinal).

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Método auxiliar que sirve para generar el método primera pasada.
% primer_pasadaAux(Grilla,Pista,Salida,Longitud).

%Caso Base: No hay pistas para analizar
primer_pasada_aux(_,[],[],_).
%Casos recursivo 1: Cumple con la condicion de primer_pasada.
primer_pasada_aux([Fila|ColaFila],[Pista|ColaPista],[FilaSalida|ColaSalida],Longitud):-
	cumple_condicion(Pista,Longitud),
	fila_correcta(Fila,Pista,Longitud,FilaSalida),
	primer_pasada_aux(ColaFila,ColaPista,ColaSalida,Longitud).
	
%Caso recursivo 2: No cumple con la condicion.
primer_pasada_aux([Fila|ColaFila],[_Pista|ColaPista],[Fila|ColaSalida],Longitud):-
	primer_pasada_aux(ColaFila,ColaPista,ColaSalida,Longitud).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Genera una grilla con los movimientos que son correctos mientras la lista recibida y la de salida sean distintas.
% segunda_pasada(GrillaIn,PistaFila,PistasColumna,GrillaSalida).

%Caso base, la grilla está completa.
segunda_pasada(GrillaIn, PistasFila, _PistasColumna,GrillaIn):-
    length(PistasFila,L),
    grilla_completa(GrillaIn,L).

%Caso recursivo
segunda_pasada(GrillaIn, PistasFila, PistasColumna,GrillaOut):-
    %%Calculo grilla cautas.
   grilla_correcta(GrillaIn,PistasFila,PistasColumna,GrillaAux),(
        %Si son iguales, asigno el valor de salida.                                                  
          (grillas_iguales(GrillaIn,GrillaAux), GrillaOut = GrillaAux);
        %Sino, vuelvo a llamar.
          segunda_pasada(GrillaAux,PistasFila,PistasColumna,GrillaOut)
   ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Determina si dos grillas son iguales
% grillas_iguales(Grilla1,Grilla2).
grillas_iguales([], []).
grillas_iguales([Fila1|Subgrilla1], [Fila2|Subgrilla2]):-
      filas_iguales(Fila1, Fila2),
      grillas_iguales(Subgrilla1, Subgrilla2).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Determina si dos filas son iguales
% grillas_iguales(Fila1,Fila2).
filas_iguales([], []).
filas_iguales([Elemento1|Subfila1],[Elemento2|Subfila2]):-
      var(Elemento1),
      var(Elemento2),
      filas_iguales(Subfila1, Subfila2).

filas_iguales([Elemento1|Subfila1], [Elemento2|Subfila2]):-
      Elemento1 == Elemento2,
      filas_iguales(Subfila1, Subfila2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Determina si una grilla está completa.
% grilla_completa(Grilla,Indice).
grilla_completa(_,0).
grilla_completa(Grilla,Index):-
    I is Index-1,
    obtener_fila(Grilla,I,Fila), 
    elementos_instanciados(Fila),
    grilla_completa(Grilla,I).
            
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Determina si todos los elementos de una lista están instanciados.
% elementos_instanciados(Lista).            
elementos_instanciados([]).
elementos_instanciados([X|Xs]):-
	forall(member(Elem,X), not(no_es_hashtag(Elem))),
	elementos_instanciados(Xs).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Genera una grilla con los movimientos que son correctos
% grilla_correcta(GrillaIN,PistasFilas,PistasColumnas,Salida).  
grilla_correcta(GrillaIN,PistasFilas,PistasColumnas,Salida):-
    length(PistasFilas,LengthFC),
	generar_filas_correctas(GrillaIN,PistasFilas,GrillasFilasCautas,0,LengthFC),
    transpose(GrillasFilasCautas, Traspuesta),
    length(PistasColumnas,LengthPC),
	generar_filas_correctas(Traspuesta,PistasColumnas,GrillasColumnasCautas,0,LengthPC),
    transpose(GrillasColumnasCautas,Salida),!.
                 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Genera una fila con los movimientos que son correctos a partir de una grilla dada.
% generar_filas_correctas(GrillaIN,PistasFilas,Salida,Indice,Longitud).                   

%Caso base:
generar_filas_correctas(_GrillaIN,_Pistas,[],Length,Length).
%Caso recursivo:
generar_filas_correctas(GrillaIN,Pistas,[FilaCorrecta|GrillaOut],Index,Length):-
    obtener_fila(GrillaIN,Index,Fila),
    obtener_pista(Index,Pistas,PistaObtenida),
    length(Fila,L),
    fila_correcta(Fila,PistaObtenida,L,FilaCorrecta),
    I is Index+1, 
    generar_filas_correctas(GrillaIN,Pistas,GrillaOut,I,Length).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Método cáscara el cual a partir de una grilla dada, genera la solución.
% ultima_pasada(GrillaIN,PistasFilas,PistasColumna,Salida).                   

ultima_pasada(GrillaIn, PistasFila, PistasCol, GrillaSalida):-
	ultima_pasada_aux(GrillaIn, PistasFila, PistasCol, [], GrillaSalida).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Método auxiliar que es utilizado por ultima_pasada(GrillaIN,PistasFilas,PistasColumna,Salida). 
% ultima_pasada_aux(GrillaIN,PistasFilas,PistasColumna,Acumulado,Salida). 
ultima_pasada_aux(_GrillaIn,[],PistasColumna,Acumulado,GrillaSalida):-
    length(PistasColumna,LengthCol),
    verifica_pistas_columna(Acumulado,0,LengthCol,PistasColumna,CheckColumna),
    todas_iguales(1,CheckColumna),
    GrillaSalida = Acumulado.

ultima_pasada_aux([Fila|Cumpleto],[_P|CumpletoPistas],PistasColumna,Acumulado,GrillaSalida):- 
	forall(member(Elem,Fila), not(var(Elem))),
    append(Acumulado, [Fila], ListaAux),
    ultima_pasada_aux(Cumpleto,CumpletoPistas, PistasColumna, ListaAux, GrillaSalida).

ultima_pasada_aux([Fila|Cumpleto], [PrimeraPistaFila|CumpletoPistasFila], PistasCol, Acumulado, GrillaSalida):-
	generar_posibles_soluciones(Fila,PrimeraPistaFila), 
	append(Acumulado, [Fila], ListaAux),
	ultima_pasada_aux(Cumpleto, CumpletoPistasFila, PistasCol, ListaAux, GrillaSalida).

           

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Método general para la Cumpleolución del nonograma.
% solucion(GrillaIN,PistasFilas,PistasColumna,Salida). 
solucion(GrillaIn,PistasFila,PistasColumna,GrillaFinal):-
    primer_pasada(GrillaIn,PistasFila,PistasColumna,Grillaprimer_pasada),
    segunda_pasada(Grillaprimer_pasada, PistasFila, PistasColumna,Grillasegunda_pasada),
    ultima_pasada(Grillasegunda_pasada, PistasFila, PistasColumna, GrillaFinal).

/*
proylcc: generar_posibles_soluciones(Lista, [3]).

proylcc: solucion(
[["X", _ , _ , _ , _ ], 		
 ["X", _ ,"X", _ , _ ],
 ["X", _ , _ , _ , _ ],		
 ["#","#","#","#","#"],
 ["#","#","#","#","#"]],
 [[3], [1,2], [4], [5], [5]],	
[[2], [5], [1,3], [5], [4]],
GrillaCumpleuelta).



*/

/*
comprobar_solucion([["X","#","#","#","X"], 		
 ["X","#","X","#","#"],
 ["X","#","#","#","#"],		 
 ["#","#","X","#","#"],
 ["#","#","X","#","#"]
],GrillaSolucionada)

*/



/*
 CASO MAL GRILLA

	proylcc:comprobar_grilla_React([["X","#","#","#","X"], 		
 ["X","#","X","#","#"],
 ["X","#","#","#","#"],		 
 ["#","#","X","#","#"],
 ["#","#","X","#","#"]
],[[3], [1,2], [4], [5], [5]],
[[2], [5], [1,3], [5], [4]],
FilaConPistas, 
ColumnaConPistas).


CASO INTERMEDIO GRILLA
	proylcc:comprobar_grilla_React(
[["X","#","#","#","#"], 		
 ["X","#","X","#","#"],
 ["X","#","#","#","#"],		 
 ["#","#","#","#","#"],
 ["#","#","#","#","#"]
],[[3], [1,2], [4], [5], [5]],
[[2], [5], [1,3], [5], [4]],
FilaConPistas, 
ColumnaConPistas). */

/*

comprobar_todas_filas_React(Grilla, FilaSat, PistasFilas, CantFilas, FilaConPistas)

CASO BUENO FILAS
	proylcc:comprobar_todas_filas_React(
[["X","#","#","#","X"], 		
 ["X","#","X","#","#"],
 ["X","#","#","#","#"],		
 ["#","#","#","#","#"],
 ["#","#","#","#","#"]],FilaSat, [[3], [1,2], [4], [5], [5]],5, FilaConPistas). 

CASO MALO FILAS
proylcc:comprobar_todas_filas_React(
[["X","#","X","#","X"], 		
 ["X","#","#","#","#"],
 ["#","X","X","X","X"],		
 ["#","#","X","#","#"],
 ["#","#","X","#","#"]], [[3], [1,2], [4], [5], [5]],5, FilaConPistas).

CASO INTERMEDIO
proylcc:comprobar_todas_filas_React(
[["X","#","#","#","X"], 		
 ["X","#","X","X","#"],
 ["X","#","#","#","#"],		
 ["#","#","X","#","#"],
 ["#","#","#","#","#"]], [[3], [1,2], [4], [5], [5]],5, FilaConPistas).




 CASO BUENO COLUMNAS

 proylcc:comprobar_todas_columnas_React(
[["X","#","#","#","X"], 		
 ["X","#","X","#","#"],
 ["X","#","#","#","#"],		
 ["#","#","#","#","#"],
 ["#","#","#","#","#"]], [[2], [5], [1,3], [5], [4]],5, ColumnaConPistas).

 CASO INTERMEDIO
proylcc:comprobar_todas_columnas_React(
[["X","#","#","#","X"], 		
 ["X","#","X","X","#"],
 ["X","#","#","#","#"],		
 ["#","#","X","#","#"],
 ["#","#","#","#","#"]], [[2], [5], [1,3], [5], [4]],5, ColumnaConPistas).



CASO BUENO
comprobar_grilla(Grilla, PistasFilas, PistasCol, FilaSat, ColSat, NonogramaCompletado):-

proylcc:comprobar_grilla( 
	[["X","#","#","#","X"], 		
 	["X","#","X","#","#"],
 	["X","#","#","#","#"],		
 	["#","#","#","#","#"],
 	["#","#","#","#","#"]],
	[[3], [1,2], [4], [5], [5]],
	[[2], [5], [1,3], [5], [4]],
	FilaSat,
	ColSat,
	NonogramaCompletado).

CASO MALO:
proylcc:comprobar_grilla( 
	[["X","#","#","#","X"], 		
 	["X","#","X","#","#"],
 	["X","#","#","#","#"],		
 	["X","#","#","X","#"],
 	["#","#","#","#","#"]],
	[[3], [1,2], [4], [5], [5]],
	[[2], [5], [1,3], [5], [4]],
	FilaSat,
	ColSat,
	NonogramaCompletado).

CASO BUENO:
comprobar_todas_filas(Grilla, FilaSat, PistasFilas, CantFilas)

proylcc:comprobar_todas_filas(
	[["X","#","#","#","X"], 		
 	["X","#","X","#","#"],
 	["X","#","#","#","#"],		
 	["#","#","#","#","#"],
 	["#","#","#","#","#"]],
	FilaSat,
	[[3], [1,2], [4], [5], [5]],
	5
	).


 
 */