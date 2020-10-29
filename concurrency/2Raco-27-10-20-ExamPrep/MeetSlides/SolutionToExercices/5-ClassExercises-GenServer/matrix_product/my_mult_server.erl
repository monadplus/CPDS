-module(my_mult_server).
-export([loop/0]).

loop() ->
    receive
	{From, {mult, M, N}} ->
	    From ! {self(), mult(M, N)},
	    loop();
	{become, Something} ->
	    Something()
    end.
    
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


