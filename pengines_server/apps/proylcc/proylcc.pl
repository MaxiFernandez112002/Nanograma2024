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

obtener_fila(Grilla,NumeroFila,Fila):- nth0(NumeroFila, Grilla, Fila).

verificar_fila(IndiceFila, PistasFilas, GrillaCumple, Cumple):-
	nth0(IndiceFila, PistasFilas, PistaDeFila),				% Obtiene las pistas (o la pista) de la fila 
	nth0(IndiceFila, GrillaCumple, Filadegrilla),				% Obtiene la fila corCumplepondiente a la posicion fila, de la grilla
    %verificar_pistas_en_lista(PistaDeFila, Filadegrilla).	% Verifica que la fila de la grilla cumpla con las pistas de la misma
	
	verifica_pista(Filadegrilla,PistaDeFila,Cumple).

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


verificar_columna(IndiceColumna, PistasCol, GrillaCumple, Cumple) :-
	nth0(IndiceColumna, PistasCol, FiladePistas),
	obtener_columna(GrillaCumple, IndiceColumna, ColumnaDeGrilla),
	%verificar_pistas_en_lista(FiladePistas, ColumnaDeGrilla).											

	verifica_pista(ColumnaDeGrilla,FiladePistas,Cumple).

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


% verifica_pista(+Lista, +Pista, -Cumple).
% Cumple = 1 si la lista tiene las pistas satisfechas
% Cumple = 0 si la lista no tiene las pistas satisfechas

verifica_pista([Elem],[0],1):- 
    no_esta_instanciado(Elem),
    !.

verifica_pista([],[0],1):- !.

verifica_pista([],[],1):- !.

verifica_pista([Elem|ColaLista],Pista,Cumple):- 
    no_esta_instanciado(Elem),
    verifica_pista(ColaLista,Pista,Cumple),
    !.

verifica_pista([Elem|ColaLista],Pista,Cumple):- 
    Elem == "#",
    verifica_pista_aux([Elem|ColaLista],Pista,Cumple),
    !.

verifica_pista(_,_,0).

/*
proylcc: verifica_pista([_,"X",_,_,_],[3], Cumple).
proylcc: verifica_pista([_,"#","#","#",_],[3], Cumple).
*/

/*
verifica_pista_aux(+Lista,+Pistas,-Cumple)
verifica si una lista satisface sus pistas
*/

verifica_pista_aux([Elem],[0],1):- 
    no_esta_instanciado(Elem),
    !.

verifica_pista_aux([],[0],1):- !.

verifica_pista_aux([Elem|ColaLista],[Pista|ColaPista],Cumple):-
    Elem == "#",
    PistaAux is Pista-1,
    verifica_pista_aux(ColaLista,[PistaAux|ColaPista],Cumple),
    !.

verifica_pista_aux([Elem|ColaLista],[Pista|ColaPista],Cumple):-
    no_esta_instanciado(Elem),
    Pista is 0,
    verifica_pista(ColaLista,ColaPista,Cumple),
    !.

verifica_pista_aux([Elem|ColaLista],[],Cumple):- 
    no_esta_instanciado(Elem), 
    verifica_pista_aux(ColaLista,[0],Cumple),
    !.

verifica_pista_aux(_,_,0).


% no_esta_instanciado(+Elem).
% predicado para chequear que un elemento no este instanciado o que sea "X" o sea lista vacia

no_esta_instanciado(Elem):- not(ground(Elem)).
no_esta_instanciado("X").
no_esta_instanciado([]).


% obtener_pista(+Indice,+Pistas,-PistaSalida).
% Dado un indice y una lista de pistas, se obtiene la pista que se encuentre en el indice correspondiente.

obtener_pista(Indice, Pistas, PistaSalida):- 
	nth0(Indice, Pistas, PistaSalida).

/*
verifica_pistas_columna(+Grilla,+Indice,+Longitud,+Pistas,-PistasCumplidas)
verifica que se cumplan las pistas de cada columna de la grilla
*/
verifica_pistas_columna(_Grilla,Longitud,Longitud,_ColPista,[]) :- !.

verifica_pistas_columna(Grilla,Indice,Longitud,[PistaCol|ColaPistasCol],[PistaCumplida|ColaPistasCumplidas]):-
    not(Indice is Longitud),
    obtener_columna(Grilla,Indice,Col),
    verifica_pista(Col,PistaCol,PistaCumplida),
    IndiceAux is Indice + 1,
    verifica_pistas_columna(Grilla,IndiceAux,Longitud,ColaPistasCol,ColaPistasCumplidas),
    !.


% todas_iguales(+Elemento,+Lista).
% chequea si un elemento es igual al encabezado de la lista y llama recursivamente
todas_iguales(_Elem,[]).

todas_iguales(Elem,[X|Xs]):- 
	X == Elem, 
	todas_iguales(Elem,Xs).

/*
generar_posibles_soluciones(-Lista,+Pistas)
Dada una pista retorna una lista que corresponda con la pista dada
*/
generar_posibles_soluciones([],[]) :- !.

generar_posibles_soluciones([Elem|ColaLista],Pista):- 
    Elem = "#", 
    generar_posibles_soluciones_aux([Elem|ColaLista],Pista),
    !.

generar_posibles_soluciones([Elem|ColaLista],Pista):- 
    Elem = "X", 
    generar_posibles_soluciones(ColaLista,Pista),
    !.

/*
generar_posibles_soluciones_aux(-Lista,+Pistas)
*/

generar_posibles_soluciones_aux([],[0]) :- !.

generar_posibles_soluciones_aux([Elem|ColaLista],[Pista|ColaPista]):- 
    Elem = "#",
    Pista \= 0, 
    PistaAux is Pista-1, 
    generar_posibles_soluciones_aux(ColaLista,[PistaAux|ColaPista]),
    !.

generar_posibles_soluciones_aux([Elem|ColaLista],[Pista|ColaPista]):- 
    Elem = "X", 
    Pista is 0, 
    generar_posibles_soluciones(ColaLista,ColaPista),
    !.


/*
trace,proylcc: generar_posibles_soluciones([ _, _ , _ , _ , _ ], [3]).
trace,proylcc: generar_posibles_soluciones([ _, "X" , _ , _ , _ ], [3]).
*/


/*
fila_correcta(+ListaActual,+Pista,+Longitud,-Salida).
Dada una lista, busca las combinaciones posibles de las pistas y las intersecta para ver cuales coinciden
 */

fila_correcta(ListaActual,Pista,Longitud,Salida):-
    findall(ListaActual,(length(ListaActual,Longitud),generar_posibles_soluciones(ListaActual,Pista)),Todas),interseccion(Todas,Longitud,Salida).
/*
interseccion(+Posibles,+Longitud,-Salida).
Recibe posibles combinaciones de una lista y las intercepta
*/

interseccion(Posibles,Longitud,Salida):-
    LongitudAux is Longitud - 1,
    interseccion_aux(Posibles,LongitudAux,[],Salida).


% interseccionAux(+Posibles,+Longitud,+LAux,-Salida).

interseccion_aux(_, -1, Aux, Aux) :- !.

interseccion_aux(_, -1, _, _) :- !.

interseccion_aux(Posibles, N, LAux, Salida):- 
    obtener_columna(Posibles, N, Iesimos),  
    todas_iguales("X", Iesimos),   
    append(["X"], LAux, Aux),   
    NAux is N - 1,
    interseccion_aux(Posibles, NAux, Aux, Salida),
    !.

interseccion_aux(Posibles, N, LAux, Salida):- 
    obtener_columna(Posibles, N, Iesimos),   
    todas_iguales("#", Iesimos), 
    append(["#"], LAux, Aux),  
    NAux is N - 1,
    interseccion_aux(Posibles, NAux, Aux, Salida),
    !.

interseccion_aux(Posibles, N, In, Out):-
    append([_], In, Aux), % Agrego al final de la lista.
    NAux is N - 1,
    interseccion_aux(Posibles, NAux, Aux, Out),
    !.


% Chequea si la longitud de las pistas más los espacios que habría entre ellas es igual a la longitud L.
% cumple_condicion(+Pistas,+Longitud).
cumple_condicion([0],0).

cumple_condicion([0|ColaPistas],Longitud):-
	LongitudAux is Longitud - 1,
    cumple_condicion(ColaPistas,LongitudAux).

cumple_condicion([Pista|ColaPistas],Longitud):-
    not(Pista is 0),
	LongitudAux is Longitud - 1,   
    PistaAux is Pista - 1,
    cumple_condicion([PistaAux|ColaPistas],LongitudAux).


% primer_pasada(+Grilla,+PistaFila,+PistasColumna,-GrillaSalida).
% va completando las pistas que puede de las filas y las columnas
primer_pasada(Grilla, PistasFila, PistasColumna, GrillaFinal):-
    length(PistasFila, LongitudFilas),
    primer_pasada_aux(Grilla, PistasFila, GrillaSalidaFilas, LongitudFilas),
    transpose(GrillaSalidaFilas, GrillaTraspuesta),
    length(PistasColumna, LongitudColumnas),
    primer_pasada_aux(GrillaTraspuesta, PistasColumna, GrillaSalidaColumnas, LongitudColumnas),
    transpose(GrillaSalidaColumnas, GrillaFinal).


% primer_pasada_aux(+Grilla,+Pistas,-Salida,+Longitud).
primer_pasada_aux(_, [], [], _) :- !.

primer_pasada_aux([Fila|ColaFila], [Pista|ColaPista], [FilaSalida|ColaSalida], Longitud):-
    cumple_condicion(Pista, Longitud),
    fila_correcta(Fila, Pista, Longitud, FilaSalida),
    primer_pasada_aux(ColaFila, ColaPista, ColaSalida, Longitud),
    !.
    
primer_pasada_aux([Fila|ColaFila], [_Pista|ColaPista], [Fila|ColaSalida], Longitud):-
    primer_pasada_aux(ColaFila, ColaPista, ColaSalida, Longitud),
    !.



/*
segunda_pasada(+GrillaIn,+PistaFila,+PistasColumna,-GrillaSalida).

Si la grilla de entrada ya está completa o sea cumple con todas las pistas de las filas,
entonces simplemente devuelve la misma grilla como grilla de salida, caso contrario se genera una grilla con pistas validas,
luego se compara la grilla original con la recién generada, si son iguales entonces ya se obtuvo la grilla de salida, sino se vuelve a llamar recursivamente.
*/

segunda_pasada(GrillaIn, PistasFila, _PistasColumna, GrillaIn):-
    length(PistasFila, L),
    grilla_completa(GrillaIn, L),
    !.

segunda_pasada(GrillaIn, PistasFila, PistasColumna, GrillaOut):-

    grilla_correcta(GrillaIn, PistasFila, PistasColumna, GrillaAux),
    (

        grillas_iguales(GrillaIn, GrillaAux),
        GrillaOut = GrillaAux,
        !; 
        
        segunda_pasada(GrillaAux, PistasFila, PistasColumna, GrillaOut)
    ),
    !.



%Verifica que dos grillas sean iguales
% grillas_iguales(+Grilla1,+Grilla2).
grillas_iguales([], []) :- !.
grillas_iguales([Fila1|Subgrilla1], [Fila2|Subgrilla2]):-
    filas_iguales(Fila1, Fila2),
    grillas_iguales(Subgrilla1, Subgrilla2),
    !.

% Verifica que dos filas sean iguales chequeando si el elemento que encabeza ambas listas es el mismo
% filas_iguales(+Fila1,+Fila2).
filas_iguales([], []) :- !.
filas_iguales([Elemento1|Subfila1], [Elemento2|Subfila2]):-
    var(Elemento1),
    var(Elemento2),
    filas_iguales(Subfila1, Subfila2),
    !.

filas_iguales([Elemento1|Subfila1], [Elemento2|Subfila2]):-
    Elemento1 == Elemento2,
    filas_iguales(Subfila1, Subfila2),
    !.

% Verifica si todas las filas de una grilla estan completamente instanciadas.
% grilla_completa(+Grilla,+Indice).
grilla_completa(_, 0) :- !.
grilla_completa(Grilla, Indice):-
    IndiceAux is Indice - 1,
    obtener_fila(Grilla, IndiceAux, Fila), 
    elementos_instanciados(Fila),
    grilla_completa(Grilla, IndiceAux),
    !.

            


% Verifica que todos los elementos de una lista esten instanciados.
% elementos_instanciados(+Lista).            
elementos_instanciados([]).
elementos_instanciados([X|Xs]):-
	forall(member(Elem,X), not(no_esta_instanciado(Elem))),
	elementos_instanciados(Xs).




% Dada una grilla, las pistas de fila y las pistas de columna retorna una grilla con las pistas que se pueden afirmar
% grilla_correcta(+Grilla,+PistasFilas,+PistasColumnas,-Salida).  
grilla_correcta(Grilla, PistasFilas, PistasColumnas, Salida):-
    length(PistasFilas, LengthFC),
    generar_filas_correctas(Grilla, PistasFilas, GrillasFilasCautas, 0, LengthFC),
    transpose(GrillasFilasCautas, Traspuesta),
    length(PistasColumnas, LengthPC),
    generar_filas_correctas(Traspuesta, PistasColumnas, GrillasColumnasCautas, 0, LengthPC),
    transpose(GrillasColumnasCautas, Salida),
    !.

% Genera una fila de salida con las pistas que se pueden afirmar.
% generar_filas_correctas(+Grilla, +PistasFilas, -Salida, +Indice, +Longitud).

generar_filas_correctas(_, _, [], Longitud, Longitud) :- !.

generar_filas_correctas(Grilla, Pistas, [FilaCorrecta|GrillaCorrecta], Indice, Longitud):-
    obtener_fila(Grilla, Indice, Fila),
    obtener_pista(Indice, Pistas, PistaObtenida),
    length(Fila, L),
    fila_correcta(Fila, PistaObtenida, L, FilaCorrecta),
    IndiceAux is Indice + 1, 
    generar_filas_correctas(Grilla, Pistas, GrillaCorrecta, IndiceAux, Longitud),
    !.

% ultima_pasada(+Grilla,+PistasFilas,+PistasColumna,-Salida).                   
% procesa una grilla basándose en las pistas dadas para las filas y columnas, completando la grilla y verificando que las columnas también cumplan con sus pistas
ultima_pasada(Grilla, PistasFila, PistasCol, GrillaSalida):-
    ultima_pasada_aux(Grilla, PistasFila, PistasCol, [], GrillaSalida),
    !.

% ultima_pasada_aux(+GrillaIN, +PistasFilas, +PistasColumna, +Acumulado, -Salida). 
ultima_pasada_aux(_Grilla, [], PistasColumna, Acumulado, GrillaSalida):-
    length(PistasColumna, LengthCol),
    verifica_pistas_columna(Acumulado, 0, LengthCol, PistasColumna, CheckColumna),
    todas_iguales(1, CheckColumna),
    GrillaSalida = Acumulado,
    !.

ultima_pasada_aux([Fila|Completo], [_Pista|ColaPistas], PistasColumna, Acumulado, GrillaSalida):- 
    forall(member(Elem, Fila), nonvar(Elem)),
    append(Acumulado, [Fila], ListaAux),
    ultima_pasada_aux(Completo, ColaPistas, PistasColumna, ListaAux, GrillaSalida),
    !.

ultima_pasada_aux([Fila|ColaFilas], [PrimeraPistaFila|ColaPistasFila], PistasCol, Acumulado, GrillaSalida):-
    generar_posibles_soluciones(Fila, PrimeraPistaFila), 
    append(Acumulado, [Fila], ListaAux),
    ultima_pasada_aux(ColaFilas, ColaPistasFila, PistasCol, ListaAux, GrillaSalida),
    !.

% solucion(+Grilla,+PistasFilas,+PistasColumna,-Salida). 

solucion(Grilla, PistasFila, PistasColumna, GrillaFinal):-
    primer_pasada(Grilla, PistasFila, PistasColumna, GrillaPrimerPasada),
    !,
    segunda_pasada(GrillaPrimerPasada, PistasFila, PistasColumna, GrillaSegundaPasada),
    !,
    ultima_pasada(GrillaSegundaPasada, PistasFila, PistasColumna, GrillaFinal),
    !.


/*

proylcc: solucion([["X","_","_","_","_"],["X","_","X","_","_"],["X","_","_","_","_"],["#","#","#","#","#"],["#","#","#","#","#"]], [[3],[1,2],[4],[5],[5]], [[2],[5],[1,3],[5],[4]], GrillaSolucionada).
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
