:- module(proylcc,[ put/8 ]).

:-use_module(library(lists)).


% replace(?X, +XIndex, +Y, +Xs, -XsY)
%
% XsY is the result of replacing the occurrence of X in position XIndex of Xs by Y.
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






% put(+Contenido, +Pos, +FilasClues, +ColsClues, +Grilla, -NuevaGrilla, -FilaSat, -ColSat).
/*
Fila = fila
put/8 actualiza una matriz (Grilla) en una posición específica con un nuevo valor (Contenido), la matriz de salida sera NuevaGrilla
Contenido contenido que es # o X
Pos es una lista [Fila, Columna], indicando la posición donde se desea colocar Contenido
Fila clues y col clues son las pistas de las filas y col respectivamente
FilaSat es 1 si las fila de Pos satisface las pistas asociadas, y 0 en caso contrario, ColSat es analogo
cambiar los 0 por 2 var 
*/

put(Contenido, [FilaNumero, ColNumero], PistasFilas, PistasColumnas, Grilla, NuevaGrilla, FilaSat, ColSat):-

	% NuevaGrilla es el resultado de reemplazar la fila vieja por la nueva modificada
	replace(Fila, FilaNumero, NewFila, Grilla, NuevaGrilla),	%lo q hace este 1er replace es conseguir la fila q se busca y la misma pero modificada

	(replace(Celda, ColNumero, _, Fila, NewFila),
	Celda == Contenido		/*Si estas pintando y lo presionas de nuevo lo despinta*/
		;
	replace(_Celda, ColNumero, Contenido, Fila, NewFila)),

	verificar_fila(FilaNumero,PistasFilas,NuevaGrilla,FilaSat),		% Verifica si para la fila se cumple lo indicado en su respectiva lista fila de pistas, 
																	% en caso de cumplirse FilaSat es 1, caso contrario 0.
	verificar_columna(ColNumero,PistasColumnas,NuevaGrilla, ColSat).% Verifica si para la columna se cumple lo indicado en su respectiva lista columna de pistas, 
																	% en caso de cumplirse ColSat es 1, caso contrario 0.


/*
verificar_fila(+Posicion,+ListaPistas,+GrillaRes,-N)
verifica que la fila/columna tenga sus pistas satisfechas.
1==TRUE, 0==FALSE
Ejemplo: verificar_fila(2, 4, NuevaGrilla, 0/1)
	
	nth0(+Indice, +Lista, -ElemEnPosIndex)

Primero se entra a verificar_fila(IndiceFila, PistasFilas, GrillaRes, 1)
y en caso de que no se verifique el predicado se va a verificar_fila(_,_,_,0).
*/

verificar_fila(IndiceFila, PistasFilas, GrillaRes, 1):-
	nth0(IndiceFila, PistasFilas, PistaDeFila),				% Obtiene las pistas (o la pista) de la fila 
	nth0(IndiceFila, GrillaRes, Filadegrilla),				% Obtiene la fila correspondiente a la posicion fila, de la grilla
    verificar_pistas_en_lista(PistaDeFila, Filadegrilla).	% Verifica que la fila de la grilla cumpla con las pistas de la misma

verificar_fila(_,_,_,0).									% Si termino de recorrer ambas listas y no se verifica las pistas en lista, retorna 0.




/*
obtener_columna_acum(+Grilla, +NumCol, -ColumnaResultante)
CB: cuando la lista de filas está vacía entonces la columna es la lista vacia.
CR: Si la grilla no esta vacia, entonces en la 1er lista se busca el elem que se encuentre en la columna deseada y se lo agrega al comienzo de la columna de salida,
luego se llama recursivamente con la cola de la grilla
*/
obtener_columna(Grilla, Col, Columna) :-
	obtener_columna_acum(Grilla, Col, [], Columna).

obtener_columna_acum([], _, ColumnaAcum, ColumnaAcum).  % 

obtener_columna_acum([Fila|Grilla], Col, ColumnaAcum, Columna) :-
	nth0(Col, Fila, Elem),  % Obtenemos el elemento en la posición Col de la fila actual.
	obtener_columna_acum(Grilla, Col, [Elem|ColumnaAcum], Columna).  % Llamada recursiva con el acumulador actualizado.


verificar_columna(IndiceColumna,PistasFilas,GrillaRes, 1) :-
	nth0(IndiceColumna,PistasFilas,FiladePistas),
	obtener_columna(GrillaRes,IndiceColumna,ColumnaDeGrilla),
	verificar_pistas_en_lista(FiladePistas,ColumnaDeGrilla).

verificar_columna(_,_,_,0).



	
/*	verificar_pistas_en_lista(+Pistas, +FiladeGrilla)

verificar_pistas_en_lista verifica que se cumpla que no haya mas #s que pistas dadas
CB: Si ya se descontaron las pistas dadas (hay 0 pistas) no tiene que haber #s en la lista
CR: Si hay pistas y el primer elem de la lista es # entonces se verifica que cumpla si hay cierta cantidad de # consecutivos, si cumple luego se llama recursivamente 
con las pistas restantes y la lista restante
CR2: Si hay pistas y hay lista, y el 1er elem de la lista no es # entonces se llama recursivamente con las mismas pistas y la cola de la lista.
*/

verificar_pistas_en_lista([],ListaFila):-
	not(member("#",ListaFila)).

verificar_pistas_en_lista([X|PistasS], [Y|ListaFilaS]):-
	Y == "#",
	verificar_pconsecutivos(X, [Y|ListaFilaS], Restante),	%en caso de q se cumpla q haya p consecutivos retorna la lista restante
	verificar_pistas_en_lista(PistasS, Restante).

verificar_pistas_en_lista(Pistas, [Y|ListaFilaS]):- 
	Y \== "#", 				   % Dada la lista de pistas, y el primer elemento de ListaFilaS (lista de fila)
	verificar_pistas_en_lista(Pistas, ListaFilaS).



/*
 verificar_pconsecutivos( +NumeroPista, +FilaARecorrer, -FilaRestante)
 CB: si hay 0 pistas que verificar y no hay mas lista por recorrer ent las pistas se cumple
 CB2: si no hay pista y si hay lista entonces si el primer elem de la lista no es # entonces cumple con que haya p #s consecutivos
 CR: si hay pista entonces si el 1er elem de la lista es # entonces descontar pista y llamamos recursivamente con lista'.
 lista' es lista sin su 1er elem .
 FilaRestante es la porcion de lista que no se recorrio aun.
*/

verificar_pconsecutivos(0,[],[]).														   

verificar_pconsecutivos(0,[X|Filarestante],Filarestante):-
	X \== "#".

verificar_pconsecutivos(N,[X|Filarestante],Filarestante2):- 
	X == "#", 
	N > 0, 
	Naux is N-1,   
	verificar_pconsecutivos(Naux,Filarestante,Filarestante2).
/*
verificar_pconsecutivos(N, [X|Filarestante], Filarestante2):- 
	X \== "#",
	verificar_pconsecutivos(N, Filarestante, Filarestante2).
*/
/*

					X,#,#,#,X

probar con 
trace,proylcc:verificar_pconsecutivos(3,["#","#","#"],FilaRestante).
True retorna lista vacia
trace,proylcc:verificar_pconsecutivos(3,["X","#","#","#","X"],FilaRestante).
True retorna lista vacia
trace,proylcc:verificar_pconsecutivos(3,["#","#","#","X","X"],FilaRestante).
True retorna lista=["X"]
trace,proylcc:verificar_pconsecutivos(3,["#","#","#","#","X"],FilaRestante).
False
*/


/*
Comprobar que se cumplan las pistas de todas las filas y todas las columnas


*/

comprobar_grilla(Grilla, PistasFilas, PistasCol, FilaSat, ColSat):-
	comprobar_todas_filas(Grilla, FilasSat, 0, PistasFilas),			%empieza comprobando las filas desde la primera (la 0)
	comprobar_todas_columnas(Grilla, ColasSat, 0, PistasCol),			%empieza a comprobar las columnas desde la primera (la 0)
	FilasSat = 1,
	ColasSat = 1.

/*
comprobar_todas_filas comprobara que se cumplan las pistas de todas las filas
comprobar_todas_filas(+Grilla, -FilaSat, +NumeroFila, +PistasFilas)
*/
comprobar_todas_filas([CabezaGrilla | ColaGrilla], FilaSat, NumeroFila, PistasFilas):-
	verificar_fila(NumeroFila, PistasFilas, NuevaGrilla, FilaSat),
	NumeroFila is NumeroFila + 1,
	comprobar_todas_filas(ColaGrilla, FilaSat, NumeroFila, PistasFilas).


/*
comprobar_todas_columnas comprobara que se cumplan las pistas de todas las columnas
comprobar_todas_columnas (+Grilla, -ColSat, +NumeroCol, +PistasCol)
*/
comprobar_todas_columnas([CabezaGrilla | ColaGrilla], ColSat, NumeroCol, PistasCol):-
	verificar_columna(NumeroCol, PistasCol, NuevaGrilla, ColSat),
	NumeroCol is NumeroCol + 1,
	comprobar_todas_columnas(ColaGrilla, ColSat, NumeroCol, PistasCol).

/*

				 [2]  [5] [1,3] [5] [4]
			[3]	["X" , _ , _  , _  , _ ], 		
		  [1,2] ["X" , _ ,"X" , _  , _ ],
			[4]	["X" , _ , _  , _  , _ ],		% Grid
			[5]	["#" ,"#","#" , _  , _ ],
			[5]	[ _  , _ ,"#" ,"#" ,"#"]

		put("#", [4,4], [[3],[1,2],[4],[5],[5]], [[2],[5],[1,3],[5],[4]], 
		[["X",_,_,_,_],
		["X",_,"#",_,_],
		["X",_,_,"#",_],
		["#","#","#",_,"#"],
		[_,_,"#","#","#"]], ResGrid, RowSat, ColSat).
	

	comprobar_grilla([
		["X","#","#","#","X"],
		["X","#","X","#","#"],
		["X","#","#","#","#"],
		["#","#","#","#","#"],
		["#","#","#","#","#"]],
		 [[3],[1,2],[4],[5],[5]], 
		 [[2],[5],[1,3],[5],[4]], 
		 FilaSat, ColSat).


		comprobar_todas_filas([CabezaGrilla | ColaGrilla], FilaSat, NumeroFila, PistasFilas):-

		comprobar_todas_filas([
		["X","#","#","#","X"],
		["X","#","X","#","#"],
		["X","#","#","#","#"],
		["#","#","#","#","#"],
		["#","#","#","#","#"]],
		 FilaSat, 
		 0,
		 [[3],[1,2],[4],[5],[5]] 
		 ).


		ask((QueryId=12,((put("#", [2,1], [[3],[1,2],[4],[5],[5]], [[2],[5],[1,3],[5],[4]], [["X",_,"#","#",_],["X",_,"X","#","#"],["X",_,"#","#","#"],["#","#","#","#","#"],["#","#","#","#","#"]], ResGrid, RowSat, ColSat), Success = 1) ; Success = 0)), []) .


put("#", [1,4], [[3],[1,2],[4],[5],[5]], [[2],[5],[1,3],[5],[4]], [["X",_,_,_,_],["X","#","X","#","X"],["X",_,_,_,"#"],["#","#","#",_,_],[_,_,"#","#","#"]], ResGrid, RowSat, ColSat).



*/