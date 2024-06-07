:-module(proylcc,[ put/9 ]).

:-use_module(library(lists)).
:- use_module(library(clpfd)).


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

obtener_fila(Grilla,Numerofila,Fila):- nth0(NumeroFila, Grilla, Fila).

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
proylcc: verificar_pistas_en_lista([1,2,1],["X","#","X","#","#","X","#"]).
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



% Predicado para convertir un número a una cadena
number_to_string(Number, String) :-
    number_codes(Number, Codes),
    atom_codes(String, Codes).

% Predicado para concatenar un carácter y un número
concatenar_char_numero(Char, Numero, Result) :-
    atom_chars(Char, [CharCode]),  					% Convertir el carácter a su representación de código
    number_to_string(Numero, NumeroString),  		% Convertir el número a una cadena
    atom_concat(CharCode, NumeroString, Result). 	% Concatenar el carácter y la cadena

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Determina si una fila o columna satisface su pista correspondiente.
% checkClue(Fila/Columna,PistasFilas/PistasColumnas,RES).
% Se utiliza una "cascara" que se encarga de omitir los elementos que no son # y luego se usa checkClueAux


% Si queda un solo elemento y no hay mas pistas que satisfacer entonces debe ser vacio.
%																							checkClue([Elem],[0],1):- isVoid(Elem).
% Si no queda ningún elemento y no hay mas pistas que satisfacer entonces satisface.
checkClue([Elem],[0],1):- isVoid(Elem).
checkClue([],[0],1).
checkClue([],[],1).
% Si el elemento es vacío entonces se llama con el siguiente elemento de la lista.
checkClue([Elem|Sublist],Clue,RES):- isVoid(Elem) , checkClue(Sublist,Clue,RES).
% Si es el elemento no es vacío, se encarga checkClueAux.
checkClue([Elem|Sublist],Clue,RES):- Elem == "#" , checkClueAux([Elem|Sublist],Clue,RES).
% Cualquier otro caso es falso.
checkClue(_,_,0).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Determina si una fila o columna satisface su pista correspondiente, esta fila o columna debe comenzar con un elemento no "vacío"
% checkClueAux(Fila/Columna,PistasFilas/PistasColumnas,RES).
% Si queda un solo elemento y no hay mas pistas que satisfacer entonces debe ser vacio.

checkClueAux([Elem],[0],1):- isVoid(Elem).
% Si no queda ningún elemento y no hay mas pistas que satisfacer entonces satisface.
checkClueAux([],[0],1).
% Si el elemento es #, entonces se decrementa el valor de la pista y se llama recursivamente con el siguiente elemento de la fila o columna.
checkClueAux([Elem|Sublist],[Clue|Ppost],RES):- Elem == "#", P is (Clue-1), checkClueAux(Sublist,[P|Ppost],RES),!.
% Si el elemento es vacío y el valor de la pista está en 0, significa que cumplió con al menos una parte de las pistas, entonces se llama con la parte siguiente. ___________________________________________________________________________________________________________________
checkClueAux([Elem|Sublist],[Clue|P],RES):- isVoid(Elem), Clue is 0, checkClue(Sublist,P,RES).


% Si se alcanza este caso siginifca que no hay mas pistas que resolver, por lo que todos los elementos que quedan deben estar vacios.
checkClueAux([Elem|Sublist],[],RES):- isVoid(Elem), checkClueAux(Sublist,[0],RES).

% Cualquier otro caso es falso.
checkClueAux(_,_,0).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Se considera vacío a los elementos no instanciados o que son X
% isVoid(Elem).
%
isVoid(Elem):- not(ground(Elem)).
isVoid("X").
isVoid([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dado un indice y un arreglo de pistas, se obtiene la pista correspondiente.
% getClue(Index,Clue,RES).
%
getClue(Index, Clue, RES):- nth0(Index, Clue, RES).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dado un numero de columna y un arreglo de columnas, se obtiene la pista correspondiente.
% getCol(ColN,PistasColumnas,RES).
%
getCol([],_,[]).
getCol([L1|Ls],ColN, [Elem|Col]):- nth0(ColN, L1, Elem), getCol(Ls,ColN, Col).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dado un numero de pista y un arreglo de pistas, se obtiene la pista correspondiente.
% getRow(RowN,PistasFilas,RES).
%
getRow(Grilla,RowN,RES):- nth0(RowN, Grilla, RES).

	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Se almacena en RES las columnas que satisfacen las pistas de una grilla dada.
% checkInitCol(Grilla,Length,Length,ColClue,RES).

% Si la longitud es igual al contador entonces no hay que "recorrer" más.
checkInitCol(_Grilla,Length,Length,_ColClue,[]).
% Caso recursivo, si el contador no es igual a la longitud, entonces se obtiene la fila del contador, se verifica que esté bien y se obtiene para el resto de filas
checkInitCol(Grilla,Index,Length,[C|CSub],[RES|ColArray]):-
    not(Index is Length),
    getCol(Grilla,Index,Col),
    checkClue(Col,C,RES),
    IndexAux is Index + 1,
    checkInitCol(Grilla,IndexAux,Length,CSub,ColArray).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Determina si todos los elementos de la Lista son iguales a E
% allE(Elemento,Lista).
allE(_E,[]).
allE(E,[X|Xs]):- X == E, allE(E,Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dada una pista se genera la posible solución que la satisface.
% generarPosibles(ListaPosible,Pista).

%Caso base
generarPosibles([],[]).
%Caso recursivo: insertamos en la lista a devolver un "#" y llamamos al metodo auxiliar.
generarPosibles([Elem|Sublist],Clue):- Elem = "#" , generarPosiblesAux([Elem|Sublist],Clue).

generarPosibles([Elem|Sublist],Clue):- Elem = "X" , generarPosibles(Sublist,Clue).

%Caso base: si la lista está vacía y las pistas ya son 0.
generarPosiblesAux([],[0]).
%Caso recursivo, insertamos en la lista a devolver un "#" y si la pista no es 0, entonces llamamos recursivamente.
generarPosiblesAux([Elem|Sublist],[Clue|Ppost]):- 
	Elem = "#",
	Clue \= 0, 
	P is (Clue-1), 
	generarPosiblesAux(Sublist,[P|Ppost]).

generarPosiblesAux([Elem|Sublist],[Clue|P]):- 
	Elem = "X", 
	Clue is 0, 
	generarPosibles(Sublist,P).


/*
trace,proylcc: generarPosibles([ _, _ , _ , _ , _ ], [3]).

trace,proylcc: generarPosibles([ _, "X" , _ , _ , _ ], [3]).

*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Se genera una fila con los movimiento que se sabe que son correctos.
% filaCauta(ListaActual,Pista,Longitud,Salida).
filaCauta(Actual,Pista,Length,Out):-
    findall(Actual,(length(Actual,Length),generarPosibles(Actual,Pista)),Todas),
    interseccion(Todas,Length,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Es un método "cascara" en el cual a partir de una lista de listas, se obtiene en Salida la intersección de las mismas.
% interseccion(Posibles,Longitud,Salida).
interseccion(Posibles,Len,Salida):-
    AuxLen is Len - 1,
    interseccion_aux(Posibles,AuxLen,[],Salida).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Es un método auxiliar recursivo que es utilizado por intersección.
% El parametro almacenado empieza vacío si es la primer llamada.
% interseccionAux(Posibles,Longitud,Almacenado,Salida).

%Caso base:
interseccion_aux(_,-1,Aux,Aux).
interseccion_aux(_,-1,_,_).

%Caso recursivo
interseccion_aux(Posibles,N,LAux,Salida):- 
    getCol(Posibles,N,Iesimos),  %Obtenemos de cada lista posible el elemento N
    allE("X",Iesimos),   
    append(["X"],LAux,Aux),   %Si todos son X, entonces lo agregamos a la lista acumulado
    NAux is N -1,
    interseccion_aux(Posibles,NAux,Aux,Salida).

interseccion_aux(Posibles,N,LAux,Salida):- 
    getCol(Posibles,N,Iesimos),   %Obtenemos de cada lista posible el elemento N
    allE("#",Iesimos), 
    append(["#"],LAux,Aux),   %Si todos son #, entonces lo agregamos a la lista acumulado
    NAux is N -1,
    interseccion_aux(Posibles,NAux,Aux,Salida).

%Si hay alguna distinta
interseccion_aux(Posibles,N,In,Out):-
    append([_],In,Aux), %Agrego al final de la lista.
    NAux is N-1,
    interseccion_aux(Posibles,NAux,Aux,Out).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dada una longitud y unas pistas determina si la suma de valores y espacios es igual a la longitud.
% cumpleCondicion(Pista,Longitud).
%caso base
cumpleCondicion([0],0).

%Caso en el que estamos en un espacio, restamos y seguimos.
cumpleCondicion([0|Ps],Length):-
	L is Length - 1,
    cumpleCondicion(Ps,L).

%Caso normal
cumpleCondicion([P|Ps],Length):-
    not(P is 0),
	L is Length - 1,   
    PAux is P - 1,
    cumpleCondicion([PAux|Ps],L).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Método cascara el cual a partir de una grilla genera una nueva con las pistas que satisfacen cumpleCondicion(Pista,Longitud)
% primeraPasada(GrillaIn,PistaFila,PistasColumna,GrillaSalida).
primeraPasada(GrillaIn,PistasFila,PistasColumna,GrillaFinal):-
    length(PistasFila,LongitudFilas),
    primeraPasadaAux(GrillaIn, PistasFila, GrillaSalidaFilas,LongitudFilas),
 	transpose(GrillaSalidaFilas, GrillaTraspuesta),
    length(PistasFila,LongitudColumnas),
    primeraPasadaAux(GrillaTraspuesta, PistasColumna, GrillaSalidaColumnas,LongitudColumnas),
    transpose(GrillaSalidaColumnas, GrillaFinal).

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Método auxiliar que sirve para generar el método primera pasada.
% primeraPasadaAux(Grilla,Pista,Salida,Longitud).

%Caso Base: No hay pistas para analizar
primeraPasadaAux(_,[],[],_).
%Casos recursivo 1: Cumple con la condicion de primerapasada.
primeraPasadaAux([Fila|Resto],[Pista|RestoPista],[FilaSalida|RestoSalida],Longitud):-
	cumpleCondicion(Pista,Longitud),
	filaCauta(Fila,Pista,Longitud,FilaSalida),
	primeraPasadaAux(Resto,RestoPista,RestoSalida,Longitud).
	
%Caso recursivo 2: No cumple con la condicion.
primeraPasadaAux([Fila|Resto],[_Pista|RestoPista],[Fila|RestoSalida],Longitud):-
	primeraPasadaAux(Resto,RestoPista,RestoSalida,Longitud).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Genera una grilla con los movimientos que son correctos mientras la lista recibida y la de salida sean distintas.
% segundaPasada(GrillaIn,PistaFila,PistasColumna,GrillaSalida).

%Caso base, la grilla está completa.
segundaPasada(GrillaIn, PistasFila, _PistasColumna,GrillaIn):-
    length(PistasFila,L),
    grillaCompleta(GrillaIn,L).

%Caso recursivo
segundaPasada(GrillaIn, PistasFila, PistasColumna,GrillaOut):-
    %%Calculo grilla cautas.
   grillaCautas(GrillaIn,PistasFila,PistasColumna,GrillaAux),(
        %Si son iguales, asigno el valor de salida.                                                  
          (grillaIguales(GrillaIn,GrillaAux), GrillaOut = GrillaAux);
        %Sino, vuelvo a llamar.
          segundaPasada(GrillaAux,PistasFila,PistasColumna,GrillaOut)
   ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Determina si dos grillas son iguales
% grillaIguales(Grilla1,Grilla2).
grillaIguales([], []).
grillaIguales([Fila1|Subgrilla1], [Fila2|Subgrilla2]):-
      filasIguales(Fila1, Fila2),
      grillaIguales(Subgrilla1, Subgrilla2).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Determina si dos filas son iguales
% grillaIguales(Fila1,Fila2).
filasIguales([], []).
filasIguales([Elemento1|Subfila1],[Elemento2|Subfila2]):-
      var(Elemento1),
      var(Elemento2),
      filasIguales(Subfila1, Subfila2).

filasIguales([Elemento1|Subfila1], [Elemento2|Subfila2]):-
      Elemento1 == Elemento2,
      filasIguales(Subfila1, Subfila2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Determina si una grilla está completa.
% grillaCompleta(Grilla,Indice).
grillaCompleta(_,0).
grillaCompleta(Grilla,Index):-
    I is Index-1,
    getRow(Grilla,I,Row), 
    allAtomico(Row),
    grillaCompleta(Grilla,I).
            
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Determina si todos los elementos de una lista están instanciados.
% allAtomico(Lista).            
allAtomico([]).
allAtomico([X|Xs]):-
	forall(member(Elem,X), not(isVoid(Elem))),
	allAtomico(Xs).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Genera una grilla con los movimientos que son correctos
% grillaCautas(GrillaIN,PistasFilas,PistasColumnas,Out).  
grillaCautas(GrillaIN,PistasFilas,PistasColumnas,Out):-
    length(PistasFilas,LengthFC),
	generarFilasCautas(GrillaIN,PistasFilas,GrillasFilasCautas,0,LengthFC),
    transpose(GrillasFilasCautas, Traspuesta),
    length(PistasColumnas,LengthPC),
	generarFilasCautas(Traspuesta,PistasColumnas,GrillasColumnasCautas,0,LengthPC),
    transpose(GrillasColumnasCautas,Out),!.
                 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Genera una fila con los movimientos que son correctos a partir de una grilla dada.
% generarFilasCautas(GrillaIN,PistasFilas,Salida,Indice,Longitud).                   

%Caso base:
generarFilasCautas(_GrillaIN,_Pistas,[],Length,Length).
%Caso recursivo:
generarFilasCautas(GrillaIN,Pistas,[RowCauta|GrillaOut],Index,Length):-
    getRow(GrillaIN,Index,Row),
    getClue(Index,Pistas,PistaObtenida),
    length(Row,L),
    filaCauta(Row,PistaObtenida,L,RowCauta),
    I is Index+1, 
    generarFilasCautas(GrillaIN,Pistas,GrillaOut,I,Length).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Método cáscara el cual a partir de una grilla dada, genera la solución.
% pasadaFinal(GrillaIN,PistasFilas,PistasColumna,Salida).                   

pasadaFinal(GrillaIn, PistasFila, PistasCol, GrillaOut):-
	pasadaFinalAux(GrillaIn, PistasFila, PistasCol, [], GrillaOut).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Método auxiliar que es utilizado por pasadaFinal(GrillaIN,PistasFilas,PistasColumna,Salida). 
% pasadaFinalAux(GrillaIN,PistasFilas,PistasColumna,Acumulado,Salida). 
pasadaFinalAux(_GrillaIn,[],PistasColumna,Acumulado,GrillaOut):-
    length(PistasColumna,LengthCol),
    checkInitCol(Acumulado,0,LengthCol,PistasColumna,CheckColumna),
    allE(1,CheckColumna),
    GrillaOut = Acumulado.

pasadaFinalAux([Fila|Resto],[_P|RestoPistas],PistasColumna,Acumulado,GrillaOut):- 
	forall(member(Elem,Fila), not(var(Elem))),
    append(Acumulado, [Fila], ListaAux),
    pasadaFinalAux(Resto,RestoPistas, PistasColumna, ListaAux, GrillaOut).

pasadaFinalAux([Fila|Resto], [PrimeraPistaFila|RestoPistasFila], PistasCol, Acumulado, GrillaOut):-
	generarPosibles(Fila,PrimeraPistaFila), 
	append(Acumulado, [Fila], ListaAux),
	pasadaFinalAux(Resto, RestoPistasFila, PistasCol, ListaAux, GrillaOut).

           

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Método general para la resolución del nonograma.
% solucion(GrillaIN,PistasFilas,PistasColumna,Salida). 
solucion(GrillaIn,PistasFila,PistasColumna,GrillaFinal):-
    primeraPasada(GrillaIn,PistasFila,PistasColumna,GrillaPrimeraPasada),
    segundaPasada(GrillaPrimeraPasada, PistasFila, PistasColumna,GrillaSegundaPasada),
    pasadaFinal(GrillaSegundaPasada, PistasFila, PistasColumna, GrillaFinal).

/*
proylcc: generarPosibles(Lista, [3]).

proylcc: solucion(
[["X", _ , _ , _ , _ ], 		
 ["X", _ ,"X", _ , _ ],
 ["X", _ , _ , _ , _ ],		
 ["#","#","#","#","#"],
 ["#","#","#","#","#"]],
 [[3], [1,2], [4], [5], [5]],	
[[2], [5], [1,3], [5], [4]],
GrillaResuelta).



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