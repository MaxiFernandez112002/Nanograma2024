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
rellenar_lista_izq_sin_hashtag(+Lista, +Pistas , -ListaResultante, Numero)
Nos pasan un "1" como ultimo parametro para diferenciar los # de las pistas
la primer pista va a tener #1, la 2da va a tener #2, etc

Rellenar lista desde extremo izquierdo y no hay # en la lista:

Empezamos a colocar la 1er pista y si no llega a colocarla entera, entonces
llenar de "X" las casillas recorridas y empezar a colocar la pista otra vez luego de las "X", 
repetir procedimiento para las otras pistas

*/

rellenar_lista_izq_sin_hashtag(["X" | Lista], 0 , HashtagConNumero). 

rellenar_lista_izq_sin_hashtag([], 0 , HashtagConNumero). 		/*Si nos quedamos sin pista no recorremos mas y ponemos una x al final de la pista rellenada*/



rellenar_lista_izq_sin_hashtag([HashtagConNumero | ColaListaSalida], Pista , HashtagConNumero):-
	/*Elem is HashtagConNumero,*/
	PistaAux is Pista - 1,
	rellenar_lista_izq_sin_hashtag(ColaListaSalida, PistaAux, HashtagConNumero).

rellenar_lista_izq_sin_hashtag([X | ColaListaSalida], Pista , HashtagConNumero):-
	/*Elem is HashtagConNumero,*/
	X == "X",
	rellenar_lista_izq_sin_hashtag(ColaListaSalida, Pista, HashtagConNumero).

/*
Caso lista salida no tiene "X"
trace, proylcc: rellenar_lista_izq_sin_hashtag([ _, _ ,_ , _ , _ , _ , _ , _ , _ , _ ], 3 , "#1").
trace, proylcc: rellenar_lista_izq_sin_hashtag([ "X", "X" ,"X" , _ , _ , _ , _ , _ , _ , _ ], 6 , "#1").
trace, proylcc: rellenar_lista_izq_sin_hashtag([ "X", "X" ,"X" , _ , _ , _ , _ , _ , _ , _ ], 4 , "#1").
trace, proylcc: rellenar_lista_izq_sin_hashtag([ "X", "X" ,"X" , _ , _ , _ , _ , _ , _ , _ ], 3 , "#1").
trace, proylcc: rellenar_lista_izq_sin_hashtag([ "X", "X" ,"X" , _ , _ , _ ], 3 , "#1").
*/

/*
llenar_con_x(+Lista, -ListaResultante, -ListaNoRecorrida).
Llena la lista original y la de salida de "X" hasta encontrar una X en la original, colocando inclusive la ultima "X", 
tambien retorna la parte de la lista no recorrida.
*/

llenar_con_x([X | Lista], ["X" |ListaResultante], ListaResultante):-
	X == "X".

llenar_con_x([X | ColaLista], ["X" |ListaResultante], ListaSalidaNoRecorrida):-
	X \== "X",
	llenar_con_x(ColaLista,ListaResultante, ListaSalidaNoRecorrida).

/*
proylcc: llenar_con_x(["X", _ , _ , _ , _ ], ListaSalida).

trace, proylcc: llenar_con_x(["X", _ , _ , _ , _ ], ListaSalida ).

trace, proylcc: llenar_con_x([ _ , _ , "X" , _ , _ ], ListaSalida ).
*/

/*llena una lista con una cantidad de "X" pasada por paramentro
NUmero cantidad de x a poner
 */
llenar_con_x_cantidad_x(["X" |ListaResultante], ListaResultante, 1).

llenar_con_x_cantidad_x(["X" |ListaResultante], ListaSalidaNoRecorrida, Numero):-
	Numero > 0,
	NumeroAux is Numero -1,
	llenar_con_x_cantidad_x(ListaResultante, ListaSalidaNoRecorrida, NumeroAux).
/*
trace, proylcc: llenar_con_x([ _ , _ , _ , _ , _ ], ListaSalidaNoRecorrida, 3 ).
*/

/*
caso bueno tiene que dar q hay espacio:
trace,proylcc: chequear_pista([ _ , _ , _ , _ , _ ], ListaSalida, 3, ListaRestante, HayEspacio).

caso malo:
trace,proylcc: chequear_pista([ _ , _ , "X" , _ , _ ], ListaSalida, 3, ListaRestante, HayEspacio).
*/

/*
buscar_espacio_pista
busca el espacio donde se tiene q empezar a rellenar la lista con la pista dada, cuando no es posible mandar la pista
se llena de x y se llama recursivamente
A la funcion la llaman con el numero 0 q es el q va a contar la cantidad de x a colocar en caso de ser necesario
*/

busco_espacio_pista([], [], Numero, ListaSalida, 0, Pista).		


busco_espacio_pista(["X" |ListaRestante], ListaRestante, Numero, ListaSalida, 0, Pista).							/*la pista entra en la lista*/
/*si hay espacio para la lista y el siguiente elemento es una "X" entonces leo esa "x"*/

busco_espacio_pista( ListaRestante, ListaRestante, Numero, ListaSalida, 0, Pista).

busco_espacio_pista([X | ListaRestante], ListaOriginalRestante, Numero, ListaSalida, EspacioPista, Pista):-
	write("Entre al 2do busco espacio pista"),nl,
	X == "X",
	Numeroaux is Numero +1,
	llenar_con_x_cantidad_x(ListaSalida, ListaSalidaNoRecorrida, Numeroaux),
	busco_espacio_pista(ListaRestante,ListaOriginalRestante, Numeroaux, ListaSalida, Pista, Pista).

busco_espacio_pista([X | ListaRestante],ListaOriginalRestante, Numero, ListaSalida, EspacioPista, Pista):-	/*mientras la casila sea distinta de "x" */
	write("Entre al 3er busco espacio pista"),nl,
	X \== "X",
	EspacioPista >0,
	EspacioPistaAux is EspacioPista -1,
	NumeroAux is Numero + 1,
	busco_espacio_pista(ListaRestante,ListaOriginalRestante, NumeroAux, ListaSalida, EspacioPistaAux, Pista).


/*
caso bueno:
trace,proylcc: busco_espacio_pista([ _ , _ , _ , _ , _ ], 0,  ListaSalida, 2, 2).

caso malo:
trace,proylcc: busco_espacio_pista([ _ , _ , "X" , _ , _ , _ ],0,  ListaSalida, 3, 3).
trace, proylcc: busco_espacio_pista([ _ , "X" , "X" , _ , "X" , _ , _ , _ , _ ],0,  ListaSalida, 3, 3).
trace, proylcc: busco_espacio_pista([ _ , _ , "X" , _ , _ , _ , "X" , _ , "X", _ ],ListaRestante, 0,  ListaSalida, 3, 3).


*/



/*
Cuando se llame a solucionar_lista(+Lista, +Pistas, -ListaSalida, Numero)
se le pasara como numero un "1" 
Para todas las pistas:
	Para la pista actual:
		chequear si hay espacio:
			Si hay espacio ent
				colocar la pista
			Si no hay espacio
				tachar con "x"
				repetir 

*/


solucionar_lista(Lista, [], ListaSalida, _Numero, ListaSalida).

solucionar_lista(Lista, [Pista | ColaPistas], ListaSalida, Numero, ListaSalidaFinal):-
	concatenar_char_numero('#',Numero, NumeroConHashtag),
	busco_espacio_pista(Lista,ListaOriginalRestante, 0, ListaSalida, Pista, Pista),
	rellenar_lista_izq_sin_hashtag(ListaSalida, Pista, NumeroConHashtag),
	NumeroAux is  Numero +1,
	solucionar_lista(ListaOriginalRestante, ColaPistas, ListaSalidaRestante, NumeroAux, ListaSalidaTemporal),
	append(ListaSalida,ListaSalidaTemporal, ListaSalidaFinal).





/*
caso bueno 
trace, proylcc: solucionar_lista([ _ , _ , _ , _  ], [3], ListaSalida, 1, ListaSalidaFinal).

casos mas peludos:
trace, proylcc: solucionar_lista([ _ , _ , _ , _ , _ , _ , _ , _ , _ ], [3,3], ListaSalida, 1, ListaSalidaFinal).
trace, proylcc: solucionar_lista([ _ , _ , _ , _ , _ , _ , _ , _ , _ ], [3,3,1], ListaSalida, 1, ListaSalidaFinal).

trace, proylcc: solucionar_lista([ _ , "X" , _ , _ , _ , _ , _ , _ , _ ], [1,1,1], ListaSalida, 1, ListaSalidaFinal).
trace, proylcc: solucionar_lista([ _ , "X" , _ , _ , _ , _ , _ , _ , _ ], [1,1,1,1], ListaSalida, 1, ListaSalidaFinal).
*/

% Predicado para convertir un número a una cadena
number_to_string(Number, String) :-
    number_codes(Number, Codes),
    atom_codes(String, Codes).

% Predicado para concatenar un carácter y un número
concatenar_char_numero(Char, Numero, Result) :-
    atom_chars(Char, [CharCode]),  					% Convertir el carácter a su representación de código
    number_to_string(Numero, NumeroString),  		% Convertir el número a una cadena
    atom_concat(CharCode, NumeroString, Result). 	% Concatenar el carácter y la cadena




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