:-module(proylcc,[ put/9 ]).

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
NonogramaCompletado = 1 si el juego se gano
*/

put(Contenido, [FilaNumero, ColNumero], PistasFilas, PistasColumnas, Grilla, NuevaGrilla, FilaSat, ColSat, NonogramaCompletado):-

	% NuevaGrilla es el resultado de reemplazar la fila vieja por la nueva modificada
	replace(Fila, FilaNumero, NewFila, Grilla, NuevaGrilla),	%lo q hace este 1er replace es conseguir la fila q se busca y la misma pero modificada

	(replace(Celda, ColNumero, _, Fila, NewFila),
	Celda == Contenido		/*Si estas pintando y lo presionas de nuevo lo despinta*/
		;
	replace(_Celda, ColNumero, Contenido, Fila, NewFila)),

	verificar_fila(FilaNumero,PistasFilas,NuevaGrilla,FilaSat),		% Verifica si para la fila se cumple lo indicado en su respectiva lista fila de pistas, 
																	% en caso de cumplirse FilaSat es 1, caso contrario 0.
	verificar_columna(ColNumero,PistasColumnas,NuevaGrilla, ColSat),% Verifica si para la columna se cumple lo indicado en su respectiva lista columna de pistas, 
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
	

verificar_fila(_,_,_,0).									



/*
obtener_columna_acum(+Grilla, +NumCol, -ColumnaResultante)
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
    invertir_lista(Xs, RestoInvertido), 		 
    append(RestoInvertido, [X], ListaInvertida).

/*Tenemos que recorrer la grilla de atras para adelante, para ello necesitaremos saber la cant de columnas y pedir de la ult columna - 1 el elem de la columna deseada*/





verificar_columna(IndiceColumna, PistasCol, GrillaRes, 1) :-
	nth0(IndiceColumna, PistasCol, FiladePistas),
	obtener_columna(GrillaRes, IndiceColumna, ColumnaDeGrilla),
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
con las pistas restantes y la lista restante
CR2: Si hay pistas y hay lista, y el 1er elem de la lista no es # entonces se llama recursivamente con las mismas pistas y la cola de la lista.
*/

verificar_pistas_en_lista([],ListaFila):-
	not(member("#",ListaFila)).

verificar_pistas_en_lista([X|Pistas], [Y|ListaFilaS]):-
	Y == "#",
	verificar_pconsecutivos(X, [Y|ListaFilaS], Restante),	%en caso de q se cumpla q haya p consecutivos retorna la lista restante
	verificar_pistas_en_lista(Pistas, Restante).

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
	contar_filas(T, Aux), % Llamada recursiva para el resto de la lista
	Cont is Aux + 1. % Incrementa el contador si H es una lista


% Predicado para contar la cantidad de columnas en una lista de listas
contar_columnas([], 0). % Caso base: la lista está vacía, no hay columnas.

contar_columnas([H|_], Cont) :-
    length(H, Cont). % Obtener la longitud de la primera sublista



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



/*
rellenar_lista_izq_sin_hashtag(+Lista, +Pistas , -ListaResultante, 1)
Nos pasan un "1" como ultimo parametro para diferenciar los # de las pistas
la primer pista va a tener #1, la 2da va a tener #2, etc

Rellenar lista desde extremo izquierdo y no hay # en la lista:

Empezamos a colocar la 1er pista y si no llega a colocarla entera, entonces
llenar de "X" las casillas recorridas y empezar a colocar la pista otra vez luego de las "X", 
repetir procedimiento para las otras pistas

*/

rellenar_lista_izq_sin_hashtag([X | ColaLista], [0 | []], [Elem | ListaResultante], Numero, 1):- 		/*Si nos quedamos sin pistas no recorremos mas y ponemos una x al final de la pista rellenada*/
	Elem is "X".

rellenar_lista_izq_sin_hashtag([X | ColaLista], [0 | ColaPista], [Elem | ListaResultante], Numero, 1):-			/*Si nos quedamos sin pista ponemos una x luego de escribir la misma*/
	Elem is "X",
	NumeroAux is Numero +1,
	rellenar_lista_izq_sin_hashtag(ColaLista, ColaPista, ListaResultante , NumeroAux, 1).


rellenar_lista_izq_sin_hashtag([X | ColaLista], [Pista | ColaPista], [Elem | ListaResultante], Numero, 1):-
	X \== "X",
	Elem is "#",
	PistaAux is Pista - 1,
	rellenar_lista_izq_sin_hashtag(ColaLista, [PistaAux | ColaPista], ListaResultante, Numero, 1).

rellenar_lista_izq_sin_hashtag(Lista, Pistas, ListaResultante, Numero, 0):-
	llenar_con_x(Lista, ListaResultante, ListaNoRecorrida).


/*pasar un numero para el 1er hashtag y luego ir sumandole uno*/


rellenar_desde_izquierda(Lista, [Pista | ColaPista], ListaResultante):-
	/*si no hay espacio q rellene de x el comienzo*/
	verificar_espacio_pista(Lista, Pista, HayEspacioPista),
	verificar_siguiente_espacio(Lista, Pista, ListaResultante, HayEspacioPista),
	/*
	hay q hacer funcion en la que :
	-si hay espacio vamos a utilizar los rellenar_lista_izq_sin_hashtag
	-si no hay espacio llamamos a verificar espacio pista nuevamente 
	*/
	rellenar_desde_izquierda(Lista, ColaPista, ListaResultante).




/*
rellenar_desde_izquierda_aux(Lista, [Pista | ColaPista], ListaResultante, 1):-
	rellenar_lista_izq_sin_hashtag(Lista, [Pista | ColaPista], ListaResultante, Numero, 1).

rellenar_desde_izquierda_aux(Lista, [Pista | ColaPista], ListaNoRecorrida, 0):-
	llenar_con_x(Lista, ListaResultante, ListaNoRecorrida).
*/


/*Si HayEspacio entonces procede a escribir la pista sino llamo otra vez a*/

verificar_siguiente_espacio(Lista, Pista, ListaResultante, 1):-
	rellenar_lista_izq_sin_hashtag(Lista,Pista , [Elem | ListaResultante], Numero, 1).
	
verificar_siguiente_espacio(Lista, Pista, ListaResultante, 0):-
	/*rellenar_lista_izq_sin_hashtag(Lista, Pistas, ListaResultante, Numero, 0)*/
	llenar_con_x(Lista, ListaResultante, ListaNoRecorrida),
	verificar_espacio_pista(Lista, ListaNoRecorrida, HayEspacioPista).

	

verificar_espacio_pista_general(Lista, ListaRestante, EspacioPista, 1).  /*Finaliza en que encontro el espacio*/

verificar_espacio_pista_general(Lista, ListaRestante, EspacioPista, 0):-	/*Como no encontro espacio lo llama a seguir buscando espacio*/
	verificar_espacio_pista(Lista, ListaRestante, EspacioPista, HayEspacio).

verificar_espacio_pista_general(Lista , ListaRestante, EspacioPista, HayEspacio):-
	verificar_espacio_pista(Lista, ListaRestante, EspacioPista, HayEspacio).

/*
verificar_espacio_pista(+Lista, +EspacioPista, -ListaRestante, -HayEspacio)
verifica si en la lista hay espacio para la pista
EspacioPista es el numero de la pista o sea el espacio que ocupa la pista
HayEspacio sera 1 si es que hay espacio, 0 en caso contrario
ListaRestante es la lista que hay que corroborar si tiene espacio o no en la proxima iteracion
*/
verificar_espacio_pista([X | ListaRestante], ListaRestante, 0,1).					/*acepta q haya espacio para la pista*/

verificar_espacio_pista([X | ListaRestante], ListaRestante, EspacioPista, 0):-		/*rechaza q haya espacio para la pista*/
	X == "X".

verificar_espacio_pista([X | ListaRestante], ListaRestante, EspacioPista , HayEspacio):-	/*verifica q haya espacio para la pista*/
	X \== "X",
	EspacioPistaAux is EspacioPista -1,
	verificar_espacio_pista([X | ListaRestante], ListaRestante, EspacioPistaAux, HayEspacio).

/*
llenar_con_x(+Lista, -ListaResultante, -ListaNoRecorrida).
Llena la lista de "X" hasta encontrar una X, colocando inclusive la ultima "X", tambien retorna la parte de la lista no recorrida.
*/

llenar_con_x([X | Lista], [Elem |ListaResultante], ListaResultante):-
	X == "X",
	Elem is "X".

llenar_con_x([X | ColaLista], [Elem |ListaResultante], ListaNoRecorrida):-
	X \== "X",
	Elem is "X",
	llenar_con_x(ColaLista,ListaResultante, ListaNoRecorrida).

/*
casillas_a_rellenar(+Lista,-ListaNoRecorrida, +PistaAux, Pista, CantCasillas) 
cuenta la cantidad de casillas a rellenar
PistaAux va a servir para ir descontando el espacio q ocupa la pista a medida q se llenan las casillas
Pista es la pista original
CantCasillas sera la cantidad de casillas a rellenar, al iniciar es 0
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