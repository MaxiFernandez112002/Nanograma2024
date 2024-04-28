:- module(proylcc,[ put/8 ]).

:-use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
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

put(Contenido, [FilaNumero, ColNumero], _PistasFilas, _PistasColumnas, Grilla, NuevaGrilla, FilaSat, ColSat):-
	% NuevaGrilla es el resultado de reemplazar la fila vieja por la nueva modificada
	replace(Fila, FilaNumero, NewFila, Grilla, NuevaGrilla),	%lo q hace este 1er replace es conseguir la fila q se busca y la misma pero modificada

	% NewFila is the result of replacing the Celda Celda in position ColNumero of Fila by _,
	% if Celda matches Contenido (Celda is instantiated in the call to replace/5).	
	% Otherwise (;)
	% NewFila is the result of replacing the Celda in position ColNumero of Fila by Contenido (no matter its Contenido: _Celda).			
	(replace(Celda, ColNumero, _, Fila, NewFila),
	Celda == Contenido		/*Si estas pintando y lo presionas de nuevo lo despinta*/
		;
	replace(_Celda, ColNumero, Contenido, Fila, NewFila)).

/*

				 [2]  [5] [1,3] [5] [4]
			[3]	["X" , _ , _  , _  , _ ], 		
		  [1,2] ["X" , _ ,"X" , _  , _ ],
			[4]	["X" , _ , _  , _  , _ ],		% Grid
			[5]	["#" ,"#","#" , _  , _ ],
			[5]	[ _  , _ ,"#" ,"#" ,"#"]

[["X", _ , _ , _ , _ ], 		
 ["X", _ ,"X", _ , _ ],
 ["X", _ , _ , _ , _ ],	
 ["#","#","#", _ , _ ],
 [ _ , _ ,"#","#","#"]
]


	¿Que pasaria si recibimos put(#, [4,0], 3, 2, Grilla, NuevaGrilla, FilaSat, ColSat)?
	
	replace(Fila, 4, NewFila, Grilla, NuevaGrilla)

	¿Que pasa en el replace(Fila, 4, NewFila, Grilla, NuevaGrilla)?
		4 > 0 ? si ent disminuye
		3
		replace(Fila, 3, NewFila, Grilla, NuevaGrilla)
		3 > 0? si ent disminuye
		2
		replace(Fila, 2, NewFila, Grilla, NuevaGrilla)
		2 > 0? si ent disminuye
		1
		replace(Fila, 1, NewFila, Grilla, NuevaGrilla)
		1 > 0? si ent disminuye
		0
		replace(Fila, 0, NewFila, Grilla, NuevaGrilla)

	ask((QueryId=12,((
	put("#", [4,4], [[3],[1,2],[4],[5],[5]], [[2],[5],[1,3],[5],[4]], 
	[["X",_,_,_,_],
	["X",_,"#",_,_],
	["X",_,_,"#",_],
	["#","#","#",_,"#"],
	[_,_,"#","#","#"]], ResGrid, RowSat, ColSat)
	, Success = 1) ; Success = 0)), []) .
	
	en la recursion del replace llama con la grilla entera y cuando llama recursivamente llama con la cola de la grilla 
	[Grilla | ColaGrilla]

*/