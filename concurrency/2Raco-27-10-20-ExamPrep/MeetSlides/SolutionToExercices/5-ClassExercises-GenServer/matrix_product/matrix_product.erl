-module(matrix_product).
-export([mult/2, dot_prod/2, transpose/1]).


dot_prod([], []) -> 0;
dot_prod([H1 | T1], [H2 | T2]) -> H1*H2 + dot_prod(T1, T2).


transpose([]) -> []; 
transpose([[]|_]) -> []; 
transpose(M) ->
	[ [H || [H | _T] <- M ] 
	| transpose([ T || [_H | T] <- M ])
	].


mult(A, B) ->
	BT = transpose(B),
	[[ dot_prod(RowA, ColB) || ColB <- BT] || RowA <- A].
