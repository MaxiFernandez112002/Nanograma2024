:- module(init, [ init/3 ]).

/**
 * init(-RowsClues, -ColsClues, Grid).
 * Predicate specifying the initial grid, which will be shown at the beginning of the game,
 * including the rows and columns clues.
 */

init(
[[3], [1,2], [4], [5], [5]],	% RowsClues

[[2], [5], [1,3], [5], [4]], 	% ColsClues


[["X", _ , _ , _ , _ ], 		
 ["X", _ ,"X", _ , _ ],
 ["X", _ , _ , _ , _ ],		% Grid
 ["#","#","#","#","#"],
 ["#","#","#","#","#"]
]

/*
[["X","#","#","#","X"], 		
 ["X","#","X","#","#"],
 ["X","#","#","#","#"],		% Grid
 ["#","#","#","#","#"],
 ["#","#","#","#","#"]
]


[[ _ , _ , _ , _ , _ ], 		
 [ _ , _ , _ , _ , _ ],
 [ _ , _ , _ , _ , _ ],	
 [ _ , _ , _ , _ , _ ],
 [ _ , _ , _ , _ , _ ]],
 [[1,1], [5], [5], [3], [1]],	
[[2], [4], [4], [4], [2]],



*/



).